"""Per-timepoint label volumes and long-format CSV aggregation."""
from __future__ import annotations

from nipype.interfaces.utility import Function
from nipype.pipeline import engine as pe

from ..config import LatsConfig
from ..utils.paths import per_timepoint_stats_dir, stats_dir


def _stats_for_timepoint(
    timepoint_idx: int,
    warped_labels: str,
    labels_file: str,
    output_dir: str,
) -> str:
    """Count label volumes in one timepoint's native space."""
    from pathlib import Path as _P

    import nibabel as nib
    import numpy as np

    od = _P(output_dir)
    od.mkdir(parents=True, exist_ok=True)
    out = od / f"tp{timepoint_idx:02d}_volumes.txt"

    img = nib.load(warped_labels)
    data = np.rint(img.get_fdata()).astype(int)
    voxel_vol_mm3 = float(abs(np.linalg.det(img.affine[:3, :3])))

    pairs: list[tuple[int, str]] = []
    for line in _P(labels_file).read_text().splitlines():
        parts = line.split(None, 1)
        if len(parts) == 2:
            try:
                pairs.append((int(parts[0]), parts[1].strip()))
            except ValueError:
                pass

    lines: list[str] = []
    for label_id, name in pairs:
        n = int((data == label_id).sum())
        lines.append(f"{label_id}\t{name}\t{n * voxel_vol_mm3:.4f}\t{n}")
    n_whole = int((data > 0).sum())
    lines.append(f"0\twhole tongue\t{n_whole * voxel_vol_mm3:.4f}\t{n_whole}")

    out.write_text("\n".join(lines) + "\n")
    return str(out)


def _aggregate(
    stats_files: list,
    jacobian_files: list,
    sst_labels: str,
    labels_file: str,
    timepoint_images: list,
    output_dir: str,
) -> tuple[str, str]:
    """Merge per-timepoint stats + jacobian volumes into two CSVs."""
    import csv
    from pathlib import Path as _P

    import nibabel as nib
    import numpy as np

    od = _P(output_dir)
    od.mkdir(parents=True, exist_ok=True)

    def _read(path):
        """Parse a '<id>\\t<name>\\t<v1>[\\t<v2>]' file into {id: (name, v1, v2)}."""
        rows = {}
        for line in _P(path).read_text().splitlines():
            parts = line.split("\t")
            if len(parts) < 3:
                continue
            lid = int(parts[0])
            v2 = float(parts[3]) if len(parts) > 3 else None
            rows[lid] = (parts[1], float(parts[2]), v2)
        return rows

    # SST-space reference volumes.
    sst_img = nib.load(sst_labels)
    sst_data = np.rint(sst_img.get_fdata()).astype(int)
    sst_vox = float(abs(np.linalg.det(sst_img.affine[:3, :3])))
    sst_volumes = {}
    for line in _P(labels_file).read_text().splitlines():
        parts = line.split(None, 1)
        if len(parts) == 2:
            try:
                lid = int(parts[0])
            except ValueError:
                continue
            sst_volumes[lid] = float((sst_data == lid).sum()) * sst_vox
    sst_volumes[0] = float((sst_data > 0).sum()) * sst_vox

    seg_rows = [_read(p) for p in stats_files]
    jac_rows = [_read(p) for p in jacobian_files] if jacobian_files else []

    # ---- volumes.csv ------------------------------------------------------
    volumes_path = od / "volumes.csv"
    fields = [
        "timepoint", "subject_id", "input", "label_id", "label",
        "sst_volume_mm3", "seg_volume_mm3", "jac_volume_mm3", "mean_logjac",
        "seg_jac_pct_diff", "seg_prop_of_tongue", "jac_prop_of_tongue",
    ]
    records = []
    for idx, seg in enumerate(seg_rows):
        jac = jac_rows[idx] if idx < len(jac_rows) else {}
        image_name = _P(timepoint_images[idx]).name if idx < len(timepoint_images) else ""
        # Whole-tongue totals (label_id 0) for the per-muscle fractions.
        whole_seg = seg.get(0, (None, None, None))[1]
        whole_jac_entry = jac.get(0)
        whole_jac = whole_jac_entry[1] if whole_jac_entry else None
        for lid in sorted(seg):
            name, seg_vol, _ = seg[lid]
            jac_entry = jac.get(lid)
            jac_vol = jac_entry[1] if jac_entry else None
            mean_log = jac_entry[2] if jac_entry else None
            pct_diff = (
                100.0 * (seg_vol - jac_vol) / jac_vol
                if jac_vol not in (None, 0.0) else None
            )
            seg_prop = seg_vol / whole_seg if whole_seg else None
            jac_prop = (jac_vol / whole_jac
                        if (jac_vol is not None and whole_jac) else None)
            records.append({
                "timepoint": idx,
                "subject_id": f"tp{idx:02d}",
                "input": image_name,
                "label_id": lid,
                "label": name,
                "sst_volume_mm3": f"{sst_volumes.get(lid, float('nan')):.4f}",
                "seg_volume_mm3": f"{seg_vol:.4f}",
                "jac_volume_mm3": f"{jac_vol:.4f}" if jac_vol is not None else "",
                "mean_logjac": f"{mean_log:.6f}" if mean_log is not None else "",
                "seg_jac_pct_diff": f"{pct_diff:.4f}" if pct_diff is not None else "",
                "seg_prop_of_tongue": f"{seg_prop:.5f}" if seg_prop is not None else "",
                "jac_prop_of_tongue": f"{jac_prop:.5f}" if jac_prop is not None else "",
            })

    with open(volumes_path, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=fields)
        w.writeheader()
        w.writerows(records)

    # ---- longitudinal.csv -------------------------------------------------
    # Change relative to tp00, by both routes.
    long_path = od / "longitudinal.csv"
    long_fields = [
        "timepoint", "subject_id", "label_id", "label",
        "seg_volume_mm3", "seg_pct_change", "jac_volume_mm3", "jac_pct_change",
        "mean_logjac", "logjac_pct_change",
    ]
    baseline_seg = {r["label_id"]: float(r["seg_volume_mm3"])
                    for r in records if r["timepoint"] == 0}
    baseline_jac = {r["label_id"]: float(r["jac_volume_mm3"])
                    for r in records if r["timepoint"] == 0 and r["jac_volume_mm3"]}

    long_records = []
    for r in records:
        lid = r["label_id"]
        seg_v = float(r["seg_volume_mm3"])
        b_seg = baseline_seg.get(lid)
        seg_pct = 100.0 * (seg_v - b_seg) / b_seg if b_seg else None
        jac_v = float(r["jac_volume_mm3"]) if r["jac_volume_mm3"] else None
        b_jac = baseline_jac.get(lid)
        jac_pct = (100.0 * (jac_v - b_jac) / b_jac
                   if (jac_v is not None and b_jac) else None)
        # exp(mean log J) - 1: volume change relative to the SST, not tp00.
        log_pct = (100.0 * (np.exp(float(r["mean_logjac"])) - 1.0)
                   if r["mean_logjac"] else None)
        long_records.append({
            "timepoint": r["timepoint"],
            "subject_id": r["subject_id"],
            "label_id": lid,
            "label": r["label"],
            "seg_volume_mm3": r["seg_volume_mm3"],
            "seg_pct_change": f"{seg_pct:.4f}" if seg_pct is not None else "",
            "jac_volume_mm3": r["jac_volume_mm3"],
            "jac_pct_change": f"{jac_pct:.4f}" if jac_pct is not None else "",
            "mean_logjac": r["mean_logjac"],
            "logjac_pct_change": f"{log_pct:.4f}" if log_pct is not None else "",
        })

    with open(long_path, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=long_fields)
        w.writeheader()
        w.writerows(long_records)

    return str(volumes_path), str(long_path)


def build_stats(config: LatsConfig) -> pe.MapNode:
    """MapNode over timepoints. Caller wires ``warped_labels``, ``labels_file``."""
    node = pe.MapNode(
        Function(
            input_names=["timepoint_idx", "warped_labels", "labels_file", "output_dir"],
            output_names=["stats_file"],
            function=_stats_for_timepoint,
        ),
        name="stats",
        iterfield=["timepoint_idx", "warped_labels"],
    )
    node.inputs.timepoint_idx = list(range(config.n_timepoints))
    node.inputs.output_dir = str(per_timepoint_stats_dir(config.output_prefix))
    return node


def build_aggregate(config: LatsConfig) -> pe.Node:
    """Join node writing volumes.csv and longitudinal.csv."""
    node = pe.Node(
        Function(
            input_names=[
                "stats_files", "jacobian_files", "sst_labels",
                "labels_file", "timepoint_images", "output_dir",
            ],
            output_names=["volumes_csv", "longitudinal_csv"],
            function=_aggregate,
        ),
        name="aggregate",
    )
    node.inputs.timepoint_images = [str(tp.image) for tp in config.timepoints]
    node.inputs.output_dir = str(stats_dir(config.output_prefix))
    node.inputs.jacobian_files = []
    return node
