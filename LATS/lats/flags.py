"""Review flags for a finished run, written to qc/flags.json."""
from __future__ import annotations

import csv
import json
from pathlib import Path

import nibabel as nib
import numpy as np

from .config import LatsConfig
from .nodes.autofix import GENIO, INFLONG, SUPLONG
from .utils.paths import qc_dir, sst_dir, stats_dir

LIMITS = {"prior_removed_pct": 8.0, "cap_coverage_pct": 90.0, "il_side_ratio": 0.5, "gg_offset_mm": 3.0,
          "unpaired_pct": 3.0, "tongue_change_pct": 10.0, "seg_jac_pct_diff": 5.0}


def template_labels(config: LatsConfig) -> Path | None:
    if config.qa_labels:
        return Path(config.qa_labels)
    name = "sst_seg_autofix" if config.autofix else "sst_seg_refined" if config.prior_refine else "sst_seg"
    path = sst_dir(config.output_prefix) / f"{name}.nii.gz"
    return path if path.is_file() else None


def shape(lab, affine, plane=None):
    """Superior longitudinal cover of the dorsum, inferior longitudinal side ratio, genioglossus offset (RAS order)."""
    fg = lab > 0
    cols = fg.any(2)
    top = lab.shape[2] - 1 - np.argmax(fg[:, :, ::-1], axis=2)
    cap = 100.0 * float((np.take_along_axis(lab, top[..., None], 2)[..., 0] == SUPLONG)[cols].mean()) if cols.any() else 0.0
    world = lambda m: np.argwhere(m) @ affine[:3, :3].T + affine[:3, 3]
    point, normal = (np.array(plane["point_mm"]), np.array(plane["normal"])) if plane else (world(fg).mean(0), np.array([1.0, 0, 0]))
    side = (world(lab == INFLONG) - point) @ normal
    left, right = int((side <= 0).sum()), int((side > 0).sum())
    gg = (world(lab == GENIO) - point) @ normal
    return {"cap_coverage_pct": round(cap, 1), "il_side_ratio": round(min(left, right) / max(left, right, 1), 3),
            "gg_offset_mm": round(float(gg.mean()), 2) if plane and gg.size else None}


def unpaired_pct(lab, affine, plane, zooms, mm=3.0):
    """Share of the tongue farther than mm from the tongue mirrored across the midline plane."""
    from scipy import ndimage as ndi

    fg = lab > 0
    n = np.asarray(plane["normal"], float) / np.linalg.norm(plane["normal"])
    w = np.argwhere(fg) @ affine[:3, :3].T + affine[:3, 3]
    v = np.rint((w - 2 * ((w - plane["point_mm"]) @ n)[:, None] * n - affine[:3, 3]) @ np.linalg.inv(affine[:3, :3]).T).astype(int)
    v = v[np.all((v >= 0) & (v < lab.shape), 1)]
    mir = np.zeros(lab.shape, bool)
    mir[tuple(v.T)] = True
    far = ndi.distance_transform_edt(~ndi.binary_closing(mir), sampling=zooms) > mm
    return round(100.0 * float((fg & far).sum()) / max(int(fg.sum()), 1), 2)


def _rows(path: Path):
    return list(csv.DictReader(open(path))) if path.is_file() else []


def write_flags(config: LatsConfig) -> dict:
    out = config.output_prefix
    values, flags, sessions = {}, [], []
    prior, run = sst_dir(out) / "prior_refine.json", out / "pre_qa" / "lats_run.json"
    refined = json.loads(run.read_text())["prior_refine"] if config.qa_labels and run.is_file() else config.prior_refine
    if refined and prior.is_file():
        p = json.loads(prior.read_text())
        values["prior_removed_pct"] = round(100.0 * (p["dl_foreground_voxels"] - p["refined_foreground_voxels"])
                                            / max(p["dl_foreground_voxels"], 1), 2)
        if values["prior_removed_pct"] > LIMITS["prior_removed_pct"]:
            flags.append("prior_removed")
    path = template_labels(config)
    if path is not None:
        img = nib.as_closest_canonical(nib.load(str(path)))
        lab = np.rint(np.asanyarray(img.dataobj)).astype(np.int16)
        values["crop_edge_voxels"] = int((lab > 0).sum() - (lab[2:-2, 2:-2, 2:-2] > 0).sum())
        midline = sst_dir(out) / "midline.json"
        plane = json.loads(midline.read_text()) if midline.is_file() else None
        values.update(shape(lab, img.affine, plane))
        if plane:
            values["unpaired_pct"] = unpaired_pct(lab, img.affine, plane, img.header.get_zooms()[:3])
            if values["unpaired_pct"] > LIMITS["unpaired_pct"]:
                flags.append("asymmetric")
        flags += [f for f, bad in (("crop_edge", values["crop_edge_voxels"] > 0),
                                   ("cap_coverage", values["cap_coverage_pct"] < LIMITS["cap_coverage_pct"]),
                                   ("il_sides", values["il_side_ratio"] < LIMITS["il_side_ratio"]),
                                   ("gg_offset", values["gg_offset_mm"] is not None
                                    and abs(values["gg_offset_mm"]) > LIMITS["gg_offset_mm"])) if bad]
    vols = {int(r["timepoint"]): r for r in _rows(stats_dir(out) / "volumes.csv") if r["label_id"] == "0"}
    for r in _rows(stats_dir(out) / "longitudinal.csv"):
        tp = int(r["timepoint"])
        if r["label_id"] != "0" or tp == 0 or not r["jac_pct_change"]:
            continue
        s = {"timepoint": tp, "tongue_change_pct": round(float(r["jac_pct_change"]), 2)}
        diff = vols.get(tp, {}).get("seg_jac_pct_diff")
        s["seg_jac_pct_diff"] = round(float(diff), 2) if diff else None
        sessions.append(s)
        if abs(s["tongue_change_pct"]) > LIMITS["tongue_change_pct"]:
            flags.append(f"tongue_change_tp{tp:02d}")
        if s["seg_jac_pct_diff"] is not None and abs(s["seg_jac_pct_diff"]) > LIMITS["seg_jac_pct_diff"]:
            flags.append(f"seg_jac_tp{tp:02d}")
    result = {"flags": flags, "values": values, "sessions": sessions, "limits": LIMITS}
    qc_dir(out).mkdir(parents=True, exist_ok=True)
    (qc_dir(out) / "flags.json").write_text(json.dumps(result, indent=2) + "\n")
    return result
