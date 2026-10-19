"""Builder for the static Tongue-QC rating app."""

from __future__ import annotations

import argparse
import csv
import hashlib
import html as _html_esc
import json
import os
import re
import shutil
import signal
import sys
import time
import traceback
from concurrent.futures import Future, ProcessPoolExecutor, as_completed
from concurrent.futures.process import BrokenProcessPool
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Callable, Mapping, Sequence

from . import __version__
from .discovery import Scan, find_scans, read_metrics, read_postprocess, select
from .html import render_html
from .montage import LABEL_COLORS, LABEL_NAMES, PLANES, render_scan

# --------------------------------------------------------------------------
# Constants
# --------------------------------------------------------------------------

SCHEMA_VERSION = "tongueqc_v1"
HTML_NAME = "tongue_qc.html"
MANIFEST_NAME = "manifest.json"
README_NAME = "README.txt"
TEMPLATE_CSV_NAME = "ratings_template.csv"
SERVE_NAME = "qc_serve.py"
SCALE_DOC_NAME = "TONGUE_QC_SCALE.md"
IMAGES_DIR = "images"
META_NAME = "meta.json"
PIPELINE_QC_NAME = "pipeline_qc.jpg"
RATINGS_CSV_NAME = "tongue_qc_ratings.csv"

#: Fixed asset-naming rule; the app derives every image URL from it.
PLANE_PREFIX: dict[str, str] = {"sagittal": "sag", "coronal": "cor", "axial": "ax"}
ASSET_SUFFIX: dict[str, str] = {"bg": "_bg.jpg", "ov": "_ov.png", "ct": "_ct.png"}

#: Columns of the ratings CSV; keep byte-identical to CSV_HEADER in app.js.
CSV_COLUMNS: tuple[str, ...] = (
    "schema_version", "scan_id", "sub", "ses", "space", "mri_quality",
    "seg_quality", "mri_disposition", "seg_disposition", "touched", "notes",
    "tooth_artefact", "tooth_affects_volume", "tooth_affects_shape",
    "rater", "rated_at", "order",
)

#: Pass/fail per 1-5 grade, from Shaw 2025 (CIBM), Supplementary Appendix One, Table One.
DISPOSITION: dict[str, dict[int, str]] = {
    "mri": {1: "fail", 2: "fail-or-check", 3: "pass-check", 4: "pass", 5: "pass"},
    "seg": {1: "fail", 2: "fail", 3: "pass", 4: "pass", 5: "pass"},
}

#: The tongue_metrics.csv columns the app's metrics panel shows.
METRIC_KEYS: tuple[str, ...] = (
    "tongue_volume_mm3",
    "vertical_volume_mm3", "vertical_fraction",
    "superior_longitudinal_volume_mm3", "superior_longitudinal_fraction",
    "inferior_longitudinal_volume_mm3", "inferior_longitudinal_fraction",
    "genioglossus_volume_mm3", "genioglossus_fraction",
    "dorsum_mean_curvature_1pmm", "dorsum_max_curvature_1pmm", "dorsum_length_mm",
)

#: A pipeline qc.png larger than PIPELINE_QC_RECODE_BYTES is downscaled and re-encoded as JPEG.
PIPELINE_QC_RECODE_BYTES = 40_000
PIPELINE_QC_MAX_EDGE = 760
PIPELINE_QC_QUALITY = 75

#: Part of the render fingerprint; bump it whenever a code change alters the rendered pixels.
RENDER_ALGO_VERSION = "1"

#: Double-clickable launchers that start ``qc_serve.py``.
MAC_LAUNCHER_NAME = "Start rating (Mac).command"
WIN_LAUNCHER_NAME = "Start rating (Windows).bat"

#: Interpreter floor the launchers probe for before starting ``qc_serve.py``.
LAUNCHER_MIN_PY = (3, 8)

#: Typical build bytes per scan, and the qc.png share of it, for the size notes in README.txt.
BYTES_PER_SCAN = 147_000
PIPELINE_QC_BYTES_PER_SCAN = 37_000


# --------------------------------------------------------------------------
# Worker payloads (module level so they pickle)
# --------------------------------------------------------------------------

@dataclass(frozen=True)
class RenderJob:
    """Everything one worker needs; plain data, never a closure."""

    scan: Scan
    out_dir: Path
    n_frames: int
    pad_frac: float
    crop_margin_mm: float
    max_frame_px: int
    labels: tuple[int, ...]
    contour: bool
    jpeg_quality: int
    copy_pipeline_qc: bool
    #: :func:`render_fingerprint` of this job, stored in meta.json.
    fingerprint: str = ""

    @property
    def scan_id(self) -> str:
        return self.scan.scan_id


@dataclass(frozen=True)
class RenderOutcome:
    """Worker result. Carries the traceback so the parent can print it at -v."""

    scan_id: str
    ok: bool
    elapsed_s: float
    record: dict[str, Any] | None = None
    error: str = ""
    tb: str = ""


def _worker_init() -> None:
    """Ignore SIGINT in workers so Ctrl-C is handled once, in the parent."""
    signal.signal(signal.SIGINT, signal.SIG_IGN)


def _copy_pipeline_qc(src: Path, dst: Path) -> str | None:
    """Place the pipeline's own qc.png in the build; return an error or None."""
    try:
        from PIL import Image

        if src.stat().st_size <= PIPELINE_QC_RECODE_BYTES:
            with Image.open(src) as raw:
                raw.convert("RGB").save(dst, format="JPEG", quality=90, optimize=True)
            return None

        with Image.open(src) as raw:
            im = raw.convert("RGB")
        scale = PIPELINE_QC_MAX_EDGE / max(im.size)
        if scale < 1.0:
            size = (max(1, round(im.width * scale)), max(1, round(im.height * scale)))
            im = im.resize(size, Image.LANCZOS)
        im.save(dst, format="JPEG", quality=PIPELINE_QC_QUALITY,
                optimize=True, progressive=True)
        return None
    except Exception as exc:  # a missing/corrupt qc.png must not fail the scan
        return f"{type(exc).__name__}: {exc}"


def _subset_metrics(metrics: Mapping[str, str]) -> dict[str, str]:
    """Keep only the metrics the app displays, in a fixed order."""
    return {k: metrics[k] for k in METRIC_KEYS if k in metrics}


def _summarise_postprocess(pp: Mapping[str, Any]) -> dict[str, Any]:
    """Flatten postprocess.json to the handful of fields the panel shows."""
    cc = pp.get("connected_components") or {}
    sl = pp.get("sl_trim") or {}
    out: dict[str, Any] = {}
    if isinstance(cc, Mapping):
        for key in ("removed_voxels", "n_components", "n_components_kept", "note"):
            if key in cc:
                out[key] = cc[key]
    if isinstance(sl, Mapping):
        for key, alias in (("jut_columns", "sl_jut_columns"),
                           ("removed_voxels", "sl_removed_voxels")):
            if key in sl:
                out[alias] = sl[key]
    return out


#: Source files whose bytes can change what is rendered.
_SOURCE_FIELDS: tuple[str, ...] = (
    "image", "seg_native", "seg_template", "reg_warped", "pipeline_qc",
)


def _source_stamp(scan: Scan) -> list[list[Any]]:
    """``[[field, mtime_ns, size], ...]`` for every source file of a scan."""
    stamp: list[list[Any]] = []
    for fieldname in _SOURCE_FIELDS:
        path = getattr(scan, fieldname, None)
        if path is None:
            stamp.append([fieldname, -1, -1])
            continue
        try:
            st = Path(path).stat()
        except OSError:
            stamp.append([fieldname, -1, -1])
        else:
            stamp.append([fieldname, int(st.st_mtime_ns), int(st.st_size)])
    return stamp


def render_fingerprint(scan: Scan, *, n_frames: int, pad_frac: float,
                       crop_margin_mm: float, max_frame_px: int,
                       labels: Sequence[int], contour: bool, jpeg_quality: int,
                       copy_pipeline_qc: bool) -> str:
    """Stable hash of everything that determines a scan's rendered bytes."""
    payload = {
        "algo": RENDER_ALGO_VERSION,
        "n_frames": int(n_frames),
        "pad_frac": round(float(pad_frac), 9),
        "crop_margin_mm": round(float(crop_margin_mm), 9),
        "max_frame_px": int(max_frame_px),
        "labels": [int(v) for v in labels],
        "contour": bool(contour),
        "jpeg_quality": int(jpeg_quality),
        "copy_pipeline_qc": bool(copy_pipeline_qc),
        "space_requested": str(getattr(scan, "space", "") or ""),
        "sources": _source_stamp(scan),
    }
    blob = json.dumps(payload, sort_keys=True, separators=(",", ":"))
    return hashlib.sha256(blob.encode("utf-8")).hexdigest()[:32]


def render_job(job: RenderJob) -> RenderOutcome:
    """Render one scan. Never raises; failures come back as ``ok=False``."""
    t0 = time.perf_counter()
    scan = job.scan
    try:
        job.out_dir.mkdir(parents=True, exist_ok=True)
        res = render_scan(
            scan,
            job.out_dir,
            n_frames=job.n_frames,
            pad_frac=job.pad_frac,
            crop_margin_mm=job.crop_margin_mm,
            max_frame_px=job.max_frame_px,
            labels=tuple(job.labels),
            contour=job.contour,
            jpeg_quality=job.jpeg_quality,
        )
        warnings = [str(w) for w in res.warnings]

        has_qc = False
        if job.copy_pipeline_qc and scan.pipeline_qc is not None:
            err = _copy_pipeline_qc(Path(scan.pipeline_qc), job.out_dir / PIPELINE_QC_NAME)
            has_qc = err is None
            if err is not None:
                warnings.append(f"pipeline qc.png not copied ({err})")

        requested_space = str(getattr(scan, "space", "") or "native")
        actual_space = res.space
        if actual_space != requested_space:
            warnings.append(
                f"space degraded: requested {requested_space}, rendered {actual_space}")

        record: dict[str, Any] = {
            "scan_id": scan.scan_id,
            "sub": scan.sub,
            "ses": scan.ses,
            # the space actually rendered, which can differ from the one requested
            "space": actual_space,
            "space_requested": requested_space,
            "seg_empty": bool(res.seg_empty),
            "fallback": res.fallback,
            "slices": {p: [int(v) for v in res.slices.get(p, [])] for p in PLANES},
            "frame_px": {p: [int(v) for v in res.frame_px.get(p, (0, 0))] for p in PLANES},
            "n_frames": int(res.n_frames),
            "has_pipeline_qc": has_qc,
            "metrics": _subset_metrics(read_metrics(scan)),
            "postprocess": _summarise_postprocess(read_postprocess(scan)),
            "warnings": warnings,
        }
        # meta.json records the files written and their fingerprint, for resuming.
        meta = {
            "render_fingerprint": job.fingerprint,
            "files": {str(k): str(v) for k, v in res.files.items()},
            "record": record,
        }
        (job.out_dir / META_NAME).write_text(json.dumps(meta), encoding="utf-8")
        return RenderOutcome(scan.scan_id, True, time.perf_counter() - t0, record=record)
    except Exception as exc:
        return RenderOutcome(
            scan.scan_id, False, time.perf_counter() - t0,
            error=f"{type(exc).__name__}: {exc}", tb=traceback.format_exc(),
        )


# --------------------------------------------------------------------------
# Parent-side helpers
# --------------------------------------------------------------------------

def _make_log(verbose: bool) -> tuple[Callable[[str], None], Callable[[str], None]]:
    """Return ``(log, vlog)``; both flush so progress survives a pipe."""

    def log(msg: str) -> None:
        sys.stdout.write(msg + "\n")
        sys.stdout.flush()

    def vlog(msg: str) -> None:
        if verbose:
            log(msg)

    return log, vlog


def _fmt_hms(seconds: float) -> str:
    seconds = max(0, int(round(seconds)))
    h, rem = divmod(seconds, 3600)
    m, s = divmod(rem, 60)
    return f"{h:d}:{m:02d}:{s:02d}"


class _Progress:
    """One line per scan plus a running ETA."""

    def __init__(self, total: int, log: Callable[[str], None]) -> None:
        self.total = total
        self.done = 0
        self._log = log
        self._t0 = time.monotonic()

    def tick(self, scan_id: str, detail: str) -> None:
        self.done += 1
        elapsed = time.monotonic() - self._t0
        eta = ""
        if self.done and self.done < self.total:
            remaining = (elapsed / self.done) * (self.total - self.done)
            eta = f"  eta {_fmt_hms(remaining)}"
        width = len(str(self.total))
        self._log(f"[{self.done:{width}d}/{self.total}] {scan_id} ({detail}){eta}")


@dataclass
class _Collector:
    """Accumulates worker outcomes, de-duplicating replays after an interrupt."""

    progress: _Progress
    log: Callable[[str], None]
    vlog: Callable[[str], None]
    records: dict[str, dict[str, Any]] = field(default_factory=dict)
    failures: list[tuple[str, str]] = field(default_factory=list)
    seen: set[str] = field(default_factory=set)

    def add(self, outcome: RenderOutcome) -> None:
        if outcome.scan_id in self.seen:
            return
        self.seen.add(outcome.scan_id)
        if outcome.ok and outcome.record is not None:
            self.records[outcome.scan_id] = outcome.record
            self.progress.tick(outcome.scan_id, f"{outcome.elapsed_s:.1f}s")
            for warning in outcome.record.get("warnings", ()):
                self.vlog(f"    warn {outcome.scan_id}: {warning}")
        else:
            self.failures.append((outcome.scan_id, outcome.error))
            self.log(f"FAILED {outcome.scan_id}: {outcome.error}")
            if outcome.tb:
                self.vlog(outcome.tb.rstrip())
            self.progress.tick(outcome.scan_id, "failed")

    def fail(self, scan_id: str, exc: BaseException) -> None:
        """A future itself blew up (pickling, killed worker)."""
        self.add(RenderOutcome(scan_id, False, 0.0,
                               error=f"{type(exc).__name__}: {exc}",
                               tb=traceback.format_exc()))

    def fail_named(self, scan_id: str, reason: str) -> None:
        """Record a failure the parent diagnosed itself (e.g. a quarantine)."""
        self.add(RenderOutcome(scan_id, False, 0.0, error=reason))


#: Pool deaths a scan may cause before it is quarantined.
MAX_POOL_ATTEMPTS = 2
QUARANTINE_REASON = "worker-crash"


def _run_batch(jobs: Sequence[RenderJob], n_workers: int, collector: _Collector,
               log: Callable[[str], None]) -> tuple[list[RenderJob], bool, bool]:
    """Run ``jobs`` on one pool; return ``(unfinished, pool_died, interrupted)``."""
    executor = ProcessPoolExecutor(max_workers=n_workers, initializer=_worker_init)
    futures: dict[Future[RenderOutcome], RenderJob] = {
        executor.submit(render_job, job): job for job in jobs
    }
    pool_died = False
    interrupted = False
    try:
        for fut in as_completed(futures):
            try:
                collector.add(fut.result())
            except BrokenProcessPool:
                pool_died = True
                break
            except Exception as exc:
                collector.fail(futures[fut].scan_id, exc)
    except KeyboardInterrupt:
        interrupted = True
    finally:
        for fut in futures:
            fut.cancel()
        # Harvest anything that landed between the break and the cancel.
        for fut, job in futures.items():
            if fut.done() and not fut.cancelled():
                try:
                    if fut.exception() is None:
                        collector.add(fut.result())
                except BrokenProcessPool:
                    pool_died = True
                except Exception:  # pragma: no cover - defensive
                    pass
        # Wait only on a healthy pool, so its workers are reaped between batches.
        executor.shutdown(wait=not (pool_died or interrupted), cancel_futures=True)
    unfinished = [j for j in jobs if j.scan_id not in collector.seen]
    return unfinished, pool_died, interrupted


def _render_all(jobs: Sequence[RenderJob], n_workers: int, collector: _Collector,
                log: Callable[[str], None]) -> bool:
    """Run every job, quarantining scans that kill their worker; return True if interrupted."""
    if not jobs:
        return False

    if n_workers <= 1:
        try:
            for job in jobs:
                collector.add(render_job(job))
        except KeyboardInterrupt:
            log("\ninterrupted — writing a manifest for what finished")
            return True
        return False

    pending: list[RenderJob] = list(jobs)
    attempts: dict[str, int] = {}
    isolate = False

    while pending:
        batch, pending = pending, []
        width = n_workers if not isolate else 1
        if isolate:
            # One job per pool, so a pool death identifies its cause exactly.
            for i, job in enumerate(batch):
                unfinished, pool_died, interrupted = _run_batch(
                    [job], 1, collector, log)
                if interrupted:
                    log("\ninterrupted — writing a manifest for what finished")
                    return True
                if not unfinished:
                    continue
                attempts[job.scan_id] = attempts.get(job.scan_id, 0) + 1
                if not pool_died:
                    collector.fail_named(
                        job.scan_id, "worker vanished without a result")
                elif attempts[job.scan_id] >= MAX_POOL_ATTEMPTS:
                    log(f"QUARANTINED {job.scan_id}: killed its worker process "
                        f"{attempts[job.scan_id]}x — skipping it so the build finishes")
                    collector.fail_named(
                        job.scan_id,
                        f"{QUARANTINE_REASON}: killed its worker process "
                        f"{attempts[job.scan_id]} time(s)")
                else:
                    log(f"worker pool died on {job.scan_id} — retrying it alone")
                    pending.append(job)
                pending.extend(batch[i + 1:])
                isolate = False
                break
            continue

        unfinished, pool_died, interrupted = _run_batch(batch, width, collector, log)
        if interrupted:
            log("\ninterrupted — cancelling pending work and writing a partial manifest")
            return True
        if not unfinished:
            continue
        for job in unfinished:
            attempts[job.scan_id] = attempts.get(job.scan_id, 0) + 1
        if pool_died:
            worst = max(attempts.get(j.scan_id, 0) for j in unfinished)
            isolate = worst >= MAX_POOL_ATTEMPTS or len(unfinished) == 1
            log(f"worker pool died — rebuilding it and retrying {len(unfinished)} "
                f"scan(s)" + (" one at a time to find the culprit" if isolate else ""))
        else:  # pragma: no cover - a pool that neither died nor delivered
            isolate = True
            log(f"{len(unfinished)} scan(s) produced no result — retrying them "
                "one at a time")
        pending.extend(unfinished)
    return False


#: ``_load_cached`` verdicts.
CACHE_REUSE = "reuse"
CACHE_MISSING = "missing"          # nothing usable on disk
CACHE_STALE_PARAMS = "stale-params"  # rendered with different settings/inputs


def _load_cached(scan: Scan, image_dir: Path, want_pipeline_qc: bool,
                 fingerprint: str) -> tuple[dict[str, Any] | None, str]:
    """Return ``(record, verdict)`` for a previous run's assets."""
    meta_path = image_dir / META_NAME
    if not meta_path.is_file():
        return None, CACHE_MISSING
    try:
        meta = json.loads(meta_path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return None, CACHE_MISSING
    record = meta.get("record")
    files = meta.get("files")
    if not isinstance(record, dict) or not isinstance(files, dict) or not files:
        return None, CACHE_MISSING
    if any(not (image_dir / str(name)).is_file() for name in files.values()):
        return None, CACHE_MISSING
    if want_pipeline_qc and scan.pipeline_qc is not None:
        if not (image_dir / PIPELINE_QC_NAME).is_file():
            return None, CACHE_MISSING
    if str(meta.get("render_fingerprint") or "") != fingerprint:
        return None, CACHE_STALE_PARAMS
    return record, CACHE_REUSE


def _read_seed_csv(path: Path, log: Callable[[str], None]) -> str:
    """Return an existing ratings CSV verbatim, to be embedded in the app."""
    try:
        text = path.read_text(encoding="utf-8-sig")
    except OSError as exc:
        log(f"WARNING: could not read --seed-csv {path}: {exc}")
        return ""
    n = 0
    try:
        for row in csv.DictReader(text.splitlines()):
            if not (row.get("scan_id") or "").strip():
                continue
            # A tooth flag on its own counts as a usable rating.
            if (_as_score(row.get("mri_quality")) is not None
                    or _as_score(row.get("seg_quality")) is not None
                    or (row.get("notes") or "").strip()
                    or (row.get("tooth_artefact") or "").strip() == "1"):
                n += 1
    except (ValueError, csv.Error) as exc:
        log(f"WARNING: --seed-csv {path} is not a readable CSV ({exc}) — ignored")
        return ""
    if n == 0:
        log(f"WARNING: --seed-csv {path} holds no usable ratings — ignored")
        return ""
    log(f"seeded {n} existing rating(s) from {path}")
    return text


def _as_score(value: str | None) -> int | None:
    """Coerce a CSV cell to a 1-5 score, or None when blank/invalid."""
    if value is None:
        return None
    text = value.strip()
    if not text:
        return None
    try:
        score = int(float(text))
    except ValueError:
        return None
    return score if 1 <= score <= 5 else None


def _hex(rgb: tuple[float, float, float]) -> str:
    return "#" + "".join(f"{max(0, min(255, round(c * 255))):02x}" for c in rgb)


_MD_BOLD = re.compile(r"\*\*(.+?)\*\*", re.DOTALL)
_MD_ITALIC = re.compile(r"(?<![\w*])\*(?!\s)(.+?)(?<!\s)\*(?![\w*])", re.DOTALL)
_MD_CODE = re.compile(r"`([^`]+)`")
_MD_HEADING = re.compile(r"^(#{1,6})\s+(.*)$")
_MD_ULI = re.compile(r"^\s*[-*+]\s+(.*)$")
_MD_OLI = re.compile(r"^\s*\d+[.)]\s+(.*)$")
_MD_RULE = re.compile(r"^\s*(?:-{3,}|\*{3,}|_{3,})\s*$")
_MD_TABLE_SEP = re.compile(r"^\s*\|?[\s:|-]+\|[\s:|-]*$")


def _md_inline(text: str) -> str:
    """Escape a run of markdown text, then apply code / bold / italic spans."""
    out = _html_esc.escape(text.strip(), quote=False)
    out = _MD_CODE.sub(lambda m: f"<code>{m.group(1)}</code>", out)
    out = _MD_BOLD.sub(lambda m: f"<strong>{m.group(1)}</strong>", out)
    out = _MD_ITALIC.sub(lambda m: f"<em>{m.group(1)}</em>", out)
    return out


def _md_cells(line: str) -> list[str]:
    row = line.strip()
    row = row[1:] if row.startswith("|") else row
    row = row[:-1] if row.endswith("|") else row
    return [c for c in row.split("|")]


def markdown_to_html(text: str) -> str:
    """Convert the small markdown subset ``TONGUE_QC_SCALE.md`` uses to HTML."""
    lines = str(text or "").replace("\r\n", "\n").replace("\r", "\n").split("\n")
    out: list[str] = []
    list_tag: str | None = None
    para: list[str] = []
    i = 0

    def close_list() -> None:
        nonlocal list_tag
        if list_tag:
            out.append(f"</{list_tag}>")
            list_tag = None

    def close_para() -> None:
        if para:
            out.append("<p>" + "<br>".join(para) + "</p>")
            para.clear()

    while i < len(lines):
        line = lines[i]
        stripped = line.strip()

        if not stripped:
            close_para()
            close_list()
            i += 1
            continue

        # table: a header row followed by a |---|---| separator
        if (stripped.startswith("|") and i + 1 < len(lines)
                and _MD_TABLE_SEP.match(lines[i + 1] or "")
                and "|" in lines[i + 1]):
            close_para()
            close_list()
            header = _md_cells(stripped)
            out.append('<table class="scale-table"><thead><tr>')
            out.extend(f"<th>{_md_inline(c)}</th>" for c in header)
            out.append("</tr></thead><tbody>")
            i += 2
            while i < len(lines) and lines[i].strip().startswith("|"):
                out.append("<tr>")
                out.extend(f"<td>{_md_inline(c)}</td>" for c in _md_cells(lines[i]))
                out.append("</tr>")
                i += 1
            out.append("</tbody></table>")
            continue

        if _MD_RULE.match(stripped):
            close_para()
            close_list()
            out.append("<hr>")
            i += 1
            continue

        heading = _MD_HEADING.match(stripped)
        if heading:
            close_para()
            close_list()
            level = len(heading.group(1))
            out.append(f"<h{level}>{_md_inline(heading.group(2))}</h{level}>")
            i += 1
            continue

        if stripped.startswith(">"):
            close_para()
            close_list()
            quote: list[str] = []
            while i < len(lines) and lines[i].strip().startswith(">"):
                quote.append(_md_inline(lines[i].strip().lstrip(">").strip()))
                i += 1
            out.append("<blockquote><p>" + "<br>".join(quote) + "</p></blockquote>")
            continue

        item = _MD_ULI.match(line)
        tag = "ul"
        if item is None:
            item = _MD_OLI.match(line)
            tag = "ol"
        if item is not None:
            close_para()
            if list_tag != tag:
                close_list()
                out.append(f"<{tag}>")
                list_tag = tag
            body = [item.group(1)]
            i += 1
            # continuation lines of the same item (indented, not a new item)
            while (i < len(lines) and lines[i].strip()
                   and lines[i][:1] in (" ", "\t")
                   and not _MD_ULI.match(lines[i]) and not _MD_OLI.match(lines[i])):
                body.append(lines[i].strip())
                i += 1
            out.append(f"<li>{_md_inline(' '.join(body))}</li>")
            continue

        close_list()
        para.append(_md_inline(stripped))
        i += 1

    close_para()
    close_list()
    return "\n".join(out)


def _scale_html(pkg_root: Path, log: Callable[[str], None]) -> tuple[str, Path | None]:
    """Read ``TONGUE_QC_SCALE.md`` from the repo root and convert it for the ``{{SCALE}}`` slot."""
    candidate = pkg_root / SCALE_DOC_NAME
    if candidate.is_file():
        try:
            return markdown_to_html(candidate.read_text(encoding="utf-8")), candidate
        except OSError as exc:
            log(f"WARNING: could not read {candidate}: {exc}")
            return "", None
    log(f"WARNING: {SCALE_DOC_NAME} not found at {candidate} — "
        "the app's scale panel will be empty")
    return "", None


def _copy_support_file(src: Path, dst: Path, log: Callable[[str], None],
                       what: str) -> bool:
    if not src.is_file():
        log(f"WARNING: {what} not found at {src} — the build will not include it")
        return False
    shutil.copy2(src, dst)
    return True


def _human_mb(n_bytes: float) -> str:
    """Round a byte count to a number a human can act on (MB, or GB past 1 GB)."""
    if n_bytes >= 1_000_000_000:
        return f"{n_bytes / 1_000_000_000:.1f} GB"
    return f"{max(1, round(n_bytes / 1_000_000)):d} MB"


def _dir_size(path: Path) -> int:
    """Total bytes under ``path``; 0 if it cannot be walked."""
    total = 0
    try:
        for entry in path.rglob("*"):
            try:
                if entry.is_file():
                    total += entry.stat().st_size
            except OSError:
                continue
    except OSError:
        return 0
    return total


def write_launchers(out_dir: Path) -> list[Path]:
    """Write the two double-clickable launchers into the build tree."""
    ver = f"{LAUNCHER_MIN_PY[0]}.{LAUNCHER_MIN_PY[1]}"
    probe = f"import sys; sys.exit(0 if sys.version_info >= {LAUNCHER_MIN_PY} else 1)"

    mac = out_dir / MAC_LAUNCHER_NAME
    mac.write_text(
        f"""\
#!/bin/bash
# Double-click me. Opens the rating app and saves to disk as you go.
cd "$(dirname "$0")" || exit 1

# Never let this window vanish before the rater has read why it failed.
fail() {{
  echo
  echo "------------------------------------------------------------"
  echo "$1"
  echo "------------------------------------------------------------"
  echo
  echo "Press Return to close this window."
  read -r _
  exit 1
}}

PY=""
for cand in python3 python; do
  if command -v "$cand" >/dev/null 2>&1 \\
     && "$cand" -c '{probe}' >/dev/null 2>&1; then
    PY="$cand"
    break
  fi
done

if [ -z "$PY" ]; then
  fail "Python {ver} or newer was not found on this Mac, so the rating app
cannot start by itself.

Install Python from https://www.python.org/downloads/ and then
double-click this file again.

You do not have to: you can open {HTML_NAME} directly instead.
Everything works except automatic saving to disk. See {README_NAME}.
If none of that is appealing, ask the study lead."
fi

"$PY" {SERVE_NAME} "$@" \\
  || fail "The rating app stopped with the error above.
Send that message (and {README_NAME}) to the study lead."
""",
        encoding="utf-8",
    )
    mac.chmod(0o755)

    win = out_dir / WIN_LAUNCHER_NAME
    # CRLF: Windows batch files with bare LF misparse on some shells.
    win_text = f"""\
@echo off
rem Double-click me. Opens the rating app and saves to disk as you go.
cd /d "%~dp0"

set "PY="
py -3 -c "{probe}" >nul 2>&1 && set "PY=py -3"
if not defined PY (
  python -c "{probe}" >nul 2>&1 && set "PY=python"
)
if not defined PY (
  python3 -c "{probe}" >nul 2>&1 && set "PY=python3"
)

if not defined PY (
  echo.
  echo ------------------------------------------------------------
  echo  Python {ver} or newer was not found on this PC, so the
  echo  rating app cannot start by itself.
  echo.
  echo  Install Python from https://www.python.org/downloads/ and
  echo  tick "Add python.exe to PATH" during setup, then
  echo  double-click this file again.
  echo.
  echo  You do not have to: you can open {HTML_NAME}
  echo  directly instead. Everything works except automatic saving
  echo  to disk. See {README_NAME}.
  echo  If none of that is appealing, ask the study lead.
  echo ------------------------------------------------------------
  echo.
  pause
  exit /b 1
)

%PY% {SERVE_NAME} %*
if errorlevel 1 (
  echo.
  echo The rating app stopped with the error above.
  echo Send that message ^(and {README_NAME}^) to the study lead.
  echo.
  pause
  exit /b 1
)
"""
    win.write_bytes(win_text.replace("\n", "\r\n").encode("utf-8"))
    return [mac, win]


def readme_text(manifest: Mapping[str, Any], size_bytes: int | None = None) -> str:
    """Rater-facing README.txt for the build."""
    n = int(manifest.get("n_rendered", 0) or 0)
    built = manifest.get("built_utc", "")
    space = manifest.get("space", "native")
    version = manifest.get("tool_version", __version__)
    total = size_bytes if size_bytes else n * BYTES_PER_SCAN
    size = _human_mb(total)
    transfer = ("\nAt this size it is a file-transfer job — OneDrive/Dropbox/a USB\n"
                "stick — not an email attachment." if total > 25_000_000 else "")
    ck_every = 100  # the default of settings.checkpointEvery in app.js
    n_qc = sum(1 for s in manifest.get("scans", ()) if s.get("has_pipeline_qc"))
    qc_note = ""
    if n_qc:
        qc_note = (
            "\n(Note for whoever built this: rebuilding with --no-pipeline-qc\n"
            f"drops the per-scan qc.png panel and saves about "
            f"{_human_mb(n_qc * PIPELINE_QC_BYTES_PER_SCAN)} of that,\n"
            "at ~37 KB a scan.)")
    return f"""\
TONGUE QC — how to rate ({n} scans)
==================================================================
built {built} UTC   ·   tongue-qc {version}   ·   images in {space} space


1. UNZIP FIRST
------------------------------------------------------------------
If you got this as a .zip, unzip it to a real folder on your disk
before opening anything. Opening the HTML from inside a zip preview
(Windows Explorer, macOS Archive Utility preview, Google Drive) will
show a blank page or missing images: the app loads thousands of
image files that only exist once the archive is extracted.

Sizes, so nothing surprises you: this build is about {size}, and the
zip is about the same (the images are already compressed, so zipping
saves almost nothing). Unzipping needs roughly {size} of free disk on
top of the zip itself.{transfer}{qc_note}


2. START IT — JUST DOUBLE-CLICK THE LAUNCHER
------------------------------------------------------------------
  macOS    double-click   {MAC_LAUNCHER_NAME}
  Windows  double-click   {WIN_LAUNCHER_NAME}

Your browser opens by itself and you can start rating. A small
terminal window appears as well — leave it open while you rate, and
close it when you are finished.

That is the recommended way, because your ratings are then written
straight to {RATINGS_CSV_NAME} in this folder after
every single change — nothing to remember, nothing to lose. The
status line in the sidebar shows "checkpointing to disk" in green
when this is working.

(macOS may say the file is from an unidentified developer: right-
click it, choose Open, then Open again. You only do this once. If
Windows has no Python, install it from python.org and tick "Add
python.exe to PATH", or ask the study lead.)

If the launchers will not run, you can still double-click
{HTML_NAME} directly. Everything works except automatic saving,
which then falls back to:
  - "Connect CSV…" in the sidebar (Chrome/Edge) — pick a file once
    and every later change is written through to it with no further
    prompts. Do this first thing; it is as good as the server.
  - failing that, the Export CSV button, which you can press at any
    time, plus an automatic download every {ck_every} ratings (your
    browser may block the repeat downloads, so do not rely on them).

Either way your work is also kept in the browser's local storage,
so closing the tab by accident does not lose anything — reopen the
same page in the same browser and carry on.


3. THE FLOW — TWO KEYSTROKES PER SCAN
------------------------------------------------------------------
For each scan you give two scores, both 1-5 (Shaw 2025 CIBM, Appendix One,
Table One):

    first keystroke  ->  MRI QUALITY  (the raw image)
    second keystroke ->  SEG QUALITY  (the tongue segmentation)

A big indicator in the sidebar always shows which of the two your
next digit will fill. After the second digit the app moves to the
next scan by itself. So a scan is literally: look, "4", "3", look,
"5", "5", … Aim for under five seconds a scan.

If you would rather not auto-advance, press "a" (or untick the
checkbox) and use the arrow keys instead.

Press "t" if the scan has DENTAL / METAL ARTEFACT (fillings,
crowns, braces) — see section 5 below. It is a separate flag, not
a score, and you can press it at any point while the scan is up.

The viewer shows three planes — sagittal, coronal, axial — each as
a row of six slices spanning the segmentation, with the outer
slices deliberately just past its edge so you can see whether the
segmentation stops where the tongue stops. The top strip is the
image alone; the bottom strip has the labels drawn over it.


4. KEYBOARD
------------------------------------------------------------------
The full key map is on the "keys" card in the app's sidebar.


5. DENTAL / METAL ARTEFACT — THE "t" FLAG
------------------------------------------------------------------
Fillings, crowns and orthodontic metal blow a black hole with a
bright rim into the image, and it often lands right on the tongue.
That can wreck the segmentation locally even when the rest of the
scan is fine, so it gets its own flag rather than being buried in
the two scores.

Press "t" whenever you can see dental/metal artefact touching or
distorting the tongue. You can press it before the digits, between
them, or after — and "t" again turns it off.

Still score MRI and Seg exactly as you see them. "t" does not
replace a score; it records the CAUSE, so the analysis can drop
these scans separately later.

When the scan is flagged AND both digits are in, the app stops
instead of advancing and asks two questions, one at a time, each
answered with "y" or "n":

  1. Is the tooth artefact impacting the VOLUME of the
     segmentation?
     -> "y" if the artefact makes the segmentation include tissue
        that is not tongue, or miss tongue that is there.

  2. Is the tooth artefact impacting the SHAPE of the tongue?
     -> "y" if it distorts the tongue border or the left/right
        symmetry, even where the amount of tissue looks about
        right.

Both answers can be "y", both can be "n", or one of each — they
are separate questions and they are used for separate analyses.
After the second answer the app advances as usual.

Esc (or "t" again) puts the questions away without answering;
the flag stays on and you can come back to it.


6. WHAT THE TWO SCALES MEAN
------------------------------------------------------------------
The anchors are in the app — press "s". Read them once before you
start, and again whenever you hesitate. (The same text is in
{SCALE_DOC_NAME} next to this file.)


7. WHEN YOU ARE DONE (OR PAUSING)
------------------------------------------------------------------
Put your name in the "rater" box in the sidebar before you start,
so the CSV says who graded what.

  - Started from a launcher: the file is already there —
    {RATINGS_CSV_NAME} in this folder. Send that.
  - Otherwise: press "Export CSV" and send the file that downloads.

You can stop and resume whenever you like. To resume on another
machine, use "Load CSV…" to read your previous export back in.
{TEMPLATE_CSV_NAME} is an empty file with the right columns, in
case you need one to point at.

Please send the CSV back to the study lead, and mention anything
systematic you noticed (a whole site looking soft, one label
consistently wrong) — that is worth more than any single score.
"""


# --------------------------------------------------------------------------
# CLI
# --------------------------------------------------------------------------

def _default_out(root: Path, deriv_name: str) -> Path:
    """``<root>/derivatives/<deriv>-qc``, unless root already *is* that deriv dir."""
    if root.name == deriv_name and root.parent.name == "derivatives":
        return root.parent / f"{deriv_name}-qc"
    return root / "derivatives" / f"{deriv_name}-qc"


def _build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        prog="build_tongue_qc.py",
        description="Render montage strips for every tongue-seg output and emit "
                    "a single self-contained HTML rating app.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    p.add_argument("--root", type=Path, required=True,
                   help="study root, a derivatives dir, or a single scan dir")
    p.add_argument("--deriv-name", default="tongue-seg",
                   help="derivatives subdirectory holding the scan dirs")
    p.add_argument("--out", type=Path, default=None,
                   help="build directory (default: <root>/derivatives/<deriv>-qc)")
    p.add_argument("--space", choices=("native", "template", "auto"), default="auto",
                   help="which image/seg pair to render")

    p.add_argument("--sample", type=int, default=None,
                   help="render a seeded random subset of N scans (default: all)")
    p.add_argument("--seed", type=int, default=42, help="sampling seed")
    p.add_argument("--shuffle-seed", type=int, default=1234,
                   help="presentation-order seed")
    p.add_argument("--sort", action="store_true",
                   help="present scans alphabetically instead of shuffled")
    p.add_argument("--subjects", nargs="+", metavar="ID", default=None,
                   help="restrict to these scan_ids")
    p.add_argument("--limit", type=int, default=None,
                   help="stop after N scans (debugging)")

    p.add_argument("--n-frames", type=int, default=6, help="slices per plane")
    p.add_argument("--pad-frac", type=float, default=0.0,
                   help="fraction of the seg extent to pad the slice range by; "
                        "0 spans the seg exactly, >0 renders blank outer frames")
    p.add_argument("--crop-margin-mm", type=float, default=20.0,
                   help="in-plane margin around the segmentation")
    p.add_argument("--max-frame-px", type=int, default=192,
                   help="cap on the long edge of one frame")
    p.add_argument("--jpeg-quality", type=int, default=72,
                   help="background strip JPEG quality")
    p.add_argument("--no-contour", dest="contour", action="store_false",
                   help="do not render the contour-only overlay strips")
    p.add_argument("--no-pipeline-qc", dest="pipeline_qc", action="store_false",
                   help="do not copy each scan's qc.png into the build")
    p.add_argument("--labels", nargs="+", type=int, default=[1, 2, 3, 4],
                   metavar="L", help="label values to draw")

    p.add_argument("--jobs", type=int, default=max(1, (os.cpu_count() or 3) - 2),
                   help="worker processes; 1 runs in-process")
    p.add_argument("--force", action="store_true",
                   help="re-render scans that already have a complete asset set")

    p.add_argument("--rater", default="", help="pre-fill the rater name")
    p.add_argument("--seed-csv", type=Path, default=None,
                   help="existing ratings CSV to embed as pre-loaded ratings")
    p.add_argument("--zip", dest="make_zip", action="store_true",
                   help="also write <out>.zip")
    p.add_argument("-v", "--verbose", action="store_true",
                   help="print tracebacks and per-scan warnings")
    return p


def main(argv: Sequence[str] | None = None) -> int:
    """Entry point. Returns a process exit code."""
    args = _build_parser().parse_args(argv)
    log, vlog = _make_log(args.verbose)

    root = args.root.expanduser().resolve()
    if not root.is_dir():
        log(f"ERROR: --root {root} is not a directory")
        return 2

    out_dir = (args.out.expanduser().resolve() if args.out is not None
               else _default_out(root, args.deriv_name))
    labels = tuple(dict.fromkeys(int(v) for v in args.labels))

    # ---- discovery -------------------------------------------------------
    scans, skipped_pairs = find_scans(root, args.deriv_name, args.space)
    skipped: list[tuple[str, str]] = [(str(a), str(b)) for a, b in skipped_pairs]
    log(f"found {len(scans)} scan(s) under {root}")
    for name, reason in skipped:
        vlog(f"    skipped {name}: {reason}")
    if not scans:
        log(f"ERROR: no tongue-seg outputs under {root}. Expected scan directories "
            f"containing seg_native.nii.gz or seg_template.nii.gz, either directly, "
            f"in {root}/derivatives/{args.deriv_name}/, or one or two levels below.")
        if skipped:
            log(f"       ({len(skipped)} candidate director(y/ies) were skipped; "
                "re-run with -v to see why)")
        return 1

    selected = select(scans, sample=args.sample, seed=args.seed,
                      shuffle_seed=None if args.sort else args.shuffle_seed,
                      subjects=args.subjects)
    if args.limit is not None:
        selected = selected[:max(0, args.limit)]
    if not selected:
        log("ERROR: the --subjects/--sample/--limit filters selected no scans")
        return 1

    order_of = {scan.scan_id: i for i, scan in enumerate(selected)}

    images_root = out_dir / IMAGES_DIR
    images_root.mkdir(parents=True, exist_ok=True)

    # ---- resume ----------------------------------------------------------
    cached: dict[str, dict[str, Any]] = {}
    todo: list[RenderJob] = []
    n_stale = 0
    for scan in selected:
        image_dir = images_root / scan.scan_id
        fingerprint = render_fingerprint(
            scan, n_frames=args.n_frames, pad_frac=args.pad_frac,
            crop_margin_mm=args.crop_margin_mm, max_frame_px=args.max_frame_px,
            labels=labels, contour=args.contour, jpeg_quality=args.jpeg_quality,
            copy_pipeline_qc=args.pipeline_qc,
        )
        if not args.force:
            record, verdict = _load_cached(scan, image_dir, args.pipeline_qc,
                                           fingerprint)
            if record is not None:
                cached[scan.scan_id] = record
                continue
            if verdict == CACHE_STALE_PARAMS:
                n_stale += 1
                vlog(f"    re-rendering {scan.scan_id}: existing assets were made "
                     "with different settings or from different source files")
        todo.append(RenderJob(
            scan=scan, out_dir=image_dir, n_frames=args.n_frames,
            pad_frac=args.pad_frac, crop_margin_mm=args.crop_margin_mm,
            max_frame_px=args.max_frame_px, labels=labels, contour=args.contour,
            jpeg_quality=args.jpeg_quality, copy_pipeline_qc=args.pipeline_qc,
            fingerprint=fingerprint,
        ))

    n_workers = max(1, min(args.jobs, len(todo) or 1))
    log(f"rendering {len(todo)} scan(s) with {n_workers} worker(s)"
        + (f"; {len(cached)} reused (use --force to redo)" if cached else "")
        + (f"; {n_stale} re-rendered because the render parameters or the source "
           "files changed" if n_stale else ""))

    progress = _Progress(len(todo), log)
    collector = _Collector(progress=progress, log=log, vlog=vlog)
    t_start = time.monotonic()
    cut_short = _render_all(todo, n_workers, collector, log)

    records = dict(cached)
    records.update(collector.records)
    skipped.extend(collector.failures)
    if not records:
        log("ERROR: nothing rendered successfully — no app written")
        return 1

    # ---- manifest --------------------------------------------------------
    ordered = sorted(records.values(), key=lambda r: order_of.get(r["scan_id"], 1 << 30))
    for i, record in enumerate(ordered):
        record["order"] = i

    # ``space`` is the space actually rendered; ``space_requested`` is what was asked for.
    spaces = {str(r.get("space", "")) for r in ordered}
    degraded = [r["scan_id"] for r in ordered
                if r.get("space_requested") and r["space"] != r["space_requested"]]
    manifest: dict[str, Any] = {
        "schema_version": SCHEMA_VERSION,
        "built_utc": datetime.now(timezone.utc).replace(microsecond=0).isoformat(),
        "tool_version": __version__,
        # hashed so the build machine's path never reaches the rater's app
        "root": hashlib.sha256(str(root).encode("utf-8")).hexdigest()[:16],
        "space": spaces.pop() if len(spaces) == 1 else "mixed",
        "space_requested": args.space,
        "n_space_degraded": len(degraded),
        "seed": args.seed,
        "shuffle_seed": None if args.sort else args.shuffle_seed,
        "n_total_found": len(scans),
        "n_rendered": len(ordered),
        "n_skipped": len(skipped),
        "n_failed": len(collector.failures),
        "n_reused": len(cached),
        "n_rerendered_params": n_stale,
        "partial": bool(cut_short),
        "contour": bool(args.contour),
        "csv_columns": list(CSV_COLUMNS),
        "csv_name": RATINGS_CSV_NAME,
        "assets": {
            "dir": IMAGES_DIR,
            "planes": list(PLANES),
            "prefix": dict(PLANE_PREFIX),
            "suffix": dict(ASSET_SUFFIX),
            "pipeline_qc": PIPELINE_QC_NAME,
        },
        "labels": [
            {"value": lab, "name": LABEL_NAMES.get(lab, str(lab)),
             "color": _hex(LABEL_COLORS.get(lab, (1.0, 1.0, 1.0)))}
            for lab in labels
        ],
        "skipped": [list(pair) for pair in skipped],
        "scans": ordered,
    }

    # ---- build tree ------------------------------------------------------
    seed_ratings = (_read_seed_csv(args.seed_csv.expanduser(), log)
                    if args.seed_csv is not None else "")
    pkg_root = Path(__file__).resolve().parent.parent
    # The scale doc is embedded in the app and also copied alongside it.
    scale_html, scale_doc = _scale_html(pkg_root, log)
    (out_dir / HTML_NAME).write_text(
        render_html(manifest, rater=args.rater, seed_ratings=seed_ratings,
                    scale_html=scale_html),
        encoding="utf-8")
    if scale_doc is not None:
        shutil.copy2(scale_doc, out_dir / SCALE_DOC_NAME)
    (out_dir / TEMPLATE_CSV_NAME).write_text(
        ",".join(f'"{c}"' for c in CSV_COLUMNS) + "\n", encoding="utf-8")

    _copy_support_file(pkg_root / SERVE_NAME, out_dir / SERVE_NAME, log, SERVE_NAME)
    write_launchers(out_dir)

    # README last of the human-facing files, so it can quote the real build size.
    (out_dir / README_NAME).write_text(
        readme_text(manifest, _dir_size(out_dir)), encoding="utf-8")

    # manifest.json last: its presence means the build finished.
    (out_dir / MANIFEST_NAME).write_text(
        json.dumps(manifest, separators=(",", ":")), encoding="utf-8")

    if args.make_zip:
        archive = shutil.make_archive(str(out_dir), "zip",
                                      root_dir=str(out_dir.parent),
                                      base_dir=out_dir.name)
        log(f"zipped -> {archive}")

    elapsed = time.monotonic() - t_start
    log(f"done in {_fmt_hms(elapsed)}: {len(ordered)} scan(s) in the app "
        f"({len(collector.records)} rendered, {len(cached)} reused, "
        f"{n_stale} re-rendered after a parameter/source change, "
        f"{len(collector.failures)} failed, {len(skipped) - len(collector.failures)} "
        f"skipped at discovery)")
    if degraded:
        shown = ", ".join(degraded[:5]) + (" …" if len(degraded) > 5 else "")
        log(f"NOTE: {len(degraded)} scan(s) could not be rendered in the requested "
            f"space and were degraded ({shown}) — manifest.space is what was "
            "actually rendered")
    if collector.failures:
        log(f"{len(collector.failures)} scan(s) failed and are NOT in the app:")
        for scan_id, reason in collector.failures:
            log(f"  FAILED {scan_id}: {reason}")
    log(f"open it with:  double-click '{MAC_LAUNCHER_NAME}' "
        f"(or '{WIN_LAUNCHER_NAME}' on Windows) in {out_dir}")
    if cut_short:
        log("NOTE: this build was cut short — re-run the same command to resume. "
            "The app above works, it just holds fewer scans.")
        return 130
    return 0
