"""Discovery of tongue-seg output directories."""

from __future__ import annotations

import csv
import json
import os
import random
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Sequence

# --- pipeline file names ---------------------------------------------------

SEG_NATIVE = "seg_native.nii.gz"
SEG_TEMPLATE = "seg_template.nii.gz"
REG_WARPED = "reg_Warped.nii.gz"
METRICS_CSV = "tongue_metrics.csv"
POSTPROCESS_JSON = "postprocess.json"
PIPELINE_QC = "qc.png"

#: NIfTIs the pipeline writes (lower case); the one remaining ``.nii.gz`` is the source image.
NON_SOURCE_NII: frozenset[str] = frozenset(
    {
        "seg_native.nii.gz",
        "seg_template.nii.gz",
        "reg_warped.nii.gz",
        "image_cropped.nii.gz",
        "label_cropped.nii.gz",
    }
)

_SPACES = ("auto", "native", "template")

# Rule 1: an explicit session token, e.g. ``sub-004_ses-02`` or ``ABC_ses3``.
_SES_RE = re.compile(r"(?P<sub>.+?)_ses-?(?P<n>\d+)", re.IGNORECASE)
# Rule 2: a bare one- or two-digit suffix, e.g. ``ABC123_2``; ``ABC_123`` is not split.
_NUM_SUFFIX_RE = re.compile(r"(?P<sub>.+?)_(?P<n>\d{1,2})$")
# A session-only directory name, prefixed with its parent's to make a unique scan_id.
_BARE_SES_DIR_RE = re.compile(r"(?:ses[-_]?[0-9A-Za-z]+|\d{1,3})", re.IGNORECASE)


@dataclass(frozen=True)
class Scan:
    """One tongue-seg output directory and the files QC needs from it."""

    scan_id: str
    path: Path
    sub: str
    ses: str
    image: Path
    seg_native: Path | None
    seg_template: Path | None
    reg_warped: Path | None
    metrics_csv: Path | None
    postprocess_json: Path | None
    pipeline_qc: Path | None
    space: str


# --- id parsing ------------------------------------------------------------


def parse_ids(name: str) -> tuple[str, str]:
    """Split a scan name into ``(sub, ses)``."""
    stem = name
    low = stem.lower()
    if low.endswith(".nii.gz"):
        stem = stem[: -len(".nii.gz")]
    elif low.endswith(".nii"):
        stem = stem[: -len(".nii")]

    m = _SES_RE.match(stem)
    if m:
        return m.group("sub"), f"ses-{int(m.group('n')):02d}"

    m = _NUM_SUFFIX_RE.match(stem)
    if m:
        return m.group("sub"), f"ses-{int(m.group('n')):02d}"

    return stem, "ses-01"


# --- directory walking -----------------------------------------------------


def _listing(path: Path) -> tuple[set[str], list[str]] | None:
    """One ``scandir`` pass: ``(file_names, sub_directory_names)``, or None if unreadable."""
    files: set[str] = set()
    dirs: list[str] = []
    try:
        with os.scandir(path) as it:
            for entry in it:
                try:
                    if entry.is_dir():
                        dirs.append(entry.name)
                    else:
                        files.add(entry.name)
                except OSError:
                    continue
    except OSError:
        return None
    dirs.sort()
    return files, dirs


def _find_source_image(files: Iterable[str]) -> tuple[str | None, str | None]:
    """Pick the native source NIfTI. Returns ``(name, error_reason)``."""
    candidates = sorted(
        n for n in files if n.lower().endswith(".nii.gz") and n.lower() not in NON_SOURCE_NII
    )
    if not candidates:
        return None, "no source image (.nii.gz) found in scan dir"
    if len(candidates) > 1:
        return None, f"ambiguous source image, {len(candidates)} candidates: {', '.join(candidates)}"
    return candidates[0], None


def _scan_id_for(deriv: Path, path: Path) -> str:
    """Directory name, prefixed with the parent for bare-session dir names."""
    try:
        parts = path.relative_to(deriv).parts
    except ValueError:  # pragma: no cover - path is always under deriv
        parts = (path.name,)
    if not parts:
        return path.name
    if len(parts) >= 2 and _BARE_SES_DIR_RE.fullmatch(parts[-1]):
        return f"{parts[-2]}_{parts[-1]}"
    return parts[-1]


def _make_scan(scan_id: str, path: Path, files: set[str], space: str) -> tuple[Scan | None, str | None]:
    """Build a :class:`Scan` from an already-listed scan dir."""
    image_name, err = _find_source_image(files)
    if image_name is None:
        return None, err

    has_native = SEG_NATIVE in files
    has_template = SEG_TEMPLATE in files
    if space == "auto":
        resolved = "native" if has_native else "template"
    else:
        resolved = space
    if resolved == "native" and not has_native:
        return None, f"space='native' requested but {SEG_NATIVE} is missing"
    if resolved == "template" and not has_template:
        return None, f"space='template' requested but {SEG_TEMPLATE} is missing"

    sub, ses = parse_ids(scan_id)
    return (
        Scan(
            scan_id=scan_id,
            path=path,
            sub=sub or scan_id,
            ses=ses,
            image=path / image_name,
            seg_native=path / SEG_NATIVE if has_native else None,
            seg_template=path / SEG_TEMPLATE if has_template else None,
            reg_warped=path / REG_WARPED if REG_WARPED in files else None,
            metrics_csv=path / METRICS_CSV if METRICS_CSV in files else None,
            postprocess_json=path / POSTPROCESS_JSON if POSTPROCESS_JSON in files else None,
            pipeline_qc=path / PIPELINE_QC if PIPELINE_QC in files else None,
            space=resolved,
        ),
        None,
    )


def find_scans(
    root: Path,
    deriv_name: str = "tongue-seg",
    space: str = "auto",
) -> tuple[list[Scan], list[tuple[str, str]]]:
    """Discover scan dirs under *root*; return ``(scans, skipped)``."""
    if space not in _SPACES:
        raise ValueError(f"space must be one of {_SPACES}, got {space!r}")

    root = Path(root)
    deriv = root / "derivatives" / deriv_name
    if not deriv.is_dir():
        deriv = root

    scans: list[Scan] = []
    skipped: list[tuple[str, str]] = []

    listed = _listing(deriv)
    if listed is None:
        return [], [(deriv.name, f"cannot read directory: {deriv}")]
    files, subdirs = listed

    # The derivatives dir is itself a single scan dir.
    if SEG_TEMPLATE in files or SEG_NATIVE in files:
        scan, err = _make_scan(deriv.name, deriv, files, space)
        if scan is None:
            return [], [(deriv.name, err or "unusable scan dir")]
        return [scan], []

    seen: set[str] = set()

    def _add(path: Path, dir_files: set[str]) -> None:
        scan_id = _scan_id_for(deriv, path)
        if scan_id in seen:
            alt = "_".join(path.relative_to(deriv).parts)
            if alt in seen:
                skipped.append((scan_id, f"duplicate scan_id for {path}"))
                return
            scan_id = alt
        scan, err = _make_scan(scan_id, path, dir_files, space)
        seen.add(scan_id)
        if scan is None:
            skipped.append((scan_id, err or "unusable scan dir"))
        else:
            scans.append(scan)

    for name1 in subdirs:
        path1 = deriv / name1
        listed1 = _listing(path1)
        if listed1 is None:
            skipped.append((name1, f"cannot read directory: {path1}"))
            continue
        files1, subdirs1 = listed1
        if SEG_TEMPLATE in files1 or SEG_NATIVE in files1:
            _add(path1, files1)
            continue
        for name2 in subdirs1:
            path2 = path1 / name2
            listed2 = _listing(path2)
            if listed2 is None:
                continue
            files2 = listed2[0]
            if SEG_TEMPLATE in files2 or SEG_NATIVE in files2:
                _add(path2, files2)

    scans.sort(key=lambda s: s.scan_id)
    skipped.sort()
    return scans, skipped


# --- selection -------------------------------------------------------------


def select(
    scans: Sequence[Scan],
    sample: int | None = None,
    seed: int | None = None,
    shuffle_seed: int | None = None,
    subjects: Iterable[str] | None = None,
) -> list[Scan]:
    """Filter, sample and order scans deterministically."""
    pool = list(scans)
    if subjects is not None:
        wanted = set(subjects)
        pool = [s for s in pool if s.scan_id in wanted]

    # Sample from a canonical order so the draw does not depend on input order.
    pool.sort(key=lambda s: s.scan_id)
    if sample is not None and 0 <= sample < len(pool):
        pool = random.Random(seed).sample(pool, sample)

    if shuffle_seed is not None:
        random.Random(shuffle_seed).shuffle(pool)
    else:
        pool.sort(key=lambda s: s.scan_id)
    return pool


# --- side-car readers (never raise) ----------------------------------------


def read_metrics(scan: Scan) -> dict[str, str]:
    """First data row of ``tongue_metrics.csv`` as ``{column: value}``; ``{}`` on failure."""
    path = scan.metrics_csv
    if path is None:
        return {}
    try:
        with open(path, "r", encoding="utf-8-sig", newline="") as fh:
            for row in csv.DictReader(fh):
                return {
                    str(k): ("" if v is None else v if isinstance(v, str) else str(v))
                    for k, v in row.items()
                    if k is not None
                }
    except Exception:
        return {}
    return {}


def read_postprocess(scan: Scan) -> dict:
    """Parsed ``postprocess.json``; ``{}`` on failure or if it is not an object."""
    path = scan.postprocess_json
    if path is None:
        return {}
    try:
        with open(path, "r", encoding="utf-8") as fh:
            data = json.load(fh)
    except Exception:
        return {}
    return data if isinstance(data, dict) else {}
