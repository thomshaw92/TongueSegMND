"""Re-apply corrected template labels to a finished LATS run, reusing its template and registrations."""
from __future__ import annotations

import hashlib
import json
import shutil
from pathlib import Path

import nibabel as nib
import numpy as np
from nipype.interfaces.utility import IdentityInterface
from nipype.pipeline import engine as pe

from .config import LatsConfig
from .utils.paths import (intermediate_dir, jacobian_dir, labels_dir, lateral_labels_dir, manifest_path, qc_dir,
                          reg_dir, shape_dir, sst_dir, stats_dir)

DERIVED = (stats_dir, labels_dir, lateral_labels_dir, jacobian_dir, shape_dir, qc_dir,
           lambda out: sst_dir(out) / "midline.json", lambda out: sst_dir(out) / "sst_seg_lateralised.nii.gz",
           manifest_path)                      # last: pre_qa/lats_run.json marks the original outputs as fully moved


def reuse_files(config: LatsConfig) -> dict:
    out, n, reg = config.output_prefix, config.n_timepoints, reg_dir(config.output_prefix)
    nonlinear = config.reg_transform != "a"
    return {
        "template": sst_dir(out) / "T_template0.nii.gz",
        "affine": [reg / f"tp{i:02d}_0GenericAffine.mat" for i in range(n)],
        "warp": [reg / f"tp{i:02d}_1Warp.nii.gz" if nonlinear else "" for i in range(n)],
        "inverse_warp": [reg / f"tp{i:02d}_1InverseWarp.nii.gz" if nonlinear else "" for i in range(n)],
        "native_affine": [intermediate_dir(out) / "prep_crop" / f"tp{i:02d}" / "native2template_0GenericAffine.mat"
                          for i in range(n)],
    }


def qa_sidecar(config: LatsConfig) -> Path:
    return config.qa_labels.with_name(config.qa_labels.name.replace(".nii.gz", "").replace(".nii", "") + ".json")


def qa_midline(config: LatsConfig) -> str:
    side = qa_sidecar(config)
    if not side.is_file():
        return ""
    m = json.loads(side.read_text()).get("midline") or {}
    return str(side) if m.get("point_mm") and m.get("normal") else ""


def _sha256(p: Path) -> str:
    return hashlib.sha256(p.read_bytes()).hexdigest()


def prepare_reapply(config: LatsConfig) -> None:
    files = reuse_files(config)
    paths = [p for v in files.values() for p in (v if isinstance(v, list) else [v]) if p != ""]
    missing = [str(p) for p in paths if not Path(p).is_file()]
    if missing:
        raise FileNotFoundError("--qa-labels needs the finished LATS run in -o; missing: " + ", ".join(missing[:4]))
    tpl, qa = nib.load(files["template"]), nib.load(config.qa_labels)
    if tpl.shape[:3] != qa.shape[:3] or not np.allclose(tpl.affine, qa.affine, atol=1e-3):
        raise ValueError(f"{config.qa_labels.name} is not on this run's template grid")
    side = qa_sidecar(config)
    if side.is_file():
        r = json.loads(side.read_text())
        if (r.get("status"), r.get("decision")) != ("done", "include"):
            raise ValueError(f"{side.name} is not a finished include (status {r.get('status')}, decision {r.get('decision')})")
    out = config.output_prefix
    pre = out / "pre_qa"
    run = pre / "lats_run.json" if (pre / "lats_run.json").is_file() else manifest_path(out)
    if not run.is_file():
        raise FileNotFoundError(f"--qa-labels needs the run's {run.name} in -o")
    before = [Path(t["image"]).name for t in json.loads(run.read_text())["timepoints"]]
    now = [Path(tp.image).name for tp in config.timepoints]
    if before != now:
        raise ValueError(f"the sessions differ from the original run: {before} vs {now}")
    (out / "qa_applied.json").unlink(missing_ok=True)
    first = not (pre / "lats_run.json").is_file()
    pre.mkdir(exist_ok=True)
    for p in (f(out) for f in DERIVED):
        if not p.exists():
            continue
        if first and not (pre / p.name).exists():
            shutil.move(str(p), str(pre / p.name))
        else:
            shutil.rmtree(p) if p.is_dir() else p.unlink()


def write_applied(config: LatsConfig) -> None:
    side = qa_sidecar(config)
    (config.output_prefix / "qa_applied.json").write_text(json.dumps({
        "qa_labels": str(config.qa_labels),
        "sha256": _sha256(config.qa_labels),
        "sidecar_sha256": _sha256(side) if side.is_file() else None,
        "midline_from": qa_midline(config) or None,
    }, indent=2) + "\n")


def build_reuse(config: LatsConfig) -> pe.Node:
    node = pe.Node(IdentityInterface(fields=["template", "affine", "warp", "inverse_warp", "native_affine",
                                             "sst_labels"]), name="reuse")
    for k, v in reuse_files(config).items():
        setattr(node.inputs, k, [str(p) for p in v] if isinstance(v, list) else str(v))
    node.inputs.sst_labels = str(config.qa_labels)
    return node
