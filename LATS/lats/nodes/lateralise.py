"""Split SST labels about a mid-sagittal plane: left 100+id, right 200+id."""
from __future__ import annotations

from nipype.interfaces.utility import Function
from nipype.pipeline import engine as pe

from ..config import LatsConfig
from ..utils.paths import sst_dir


def _lateralise_sst(
    sst_labels: str,
    midline_label: int,
    midline_mode: str,
    output_dir: str,
) -> tuple[str, str]:
    """Return (lateralised_labels_path, midline_json_path)."""
    import json
    from pathlib import Path as _P

    import nibabel as nib
    import numpy as np

    from lats.utils.geometry import lateralise, midline_plane

    od = _P(output_dir)
    od.mkdir(parents=True, exist_ok=True)
    out_path = od / "sst_seg_lateralised.nii.gz"
    json_path = od / "midline.json"

    img = nib.load(sst_labels)
    data = np.rint(np.asarray(img.get_fdata())).astype(int)

    point, normal = midline_plane(data, img.affine, midline_label, mode=midline_mode)
    lat = lateralise(data, img.affine, point, normal)

    nib.save(nib.Nifti1Image(lat.astype(np.int16), img.affine, img.header), out_path)

    left = int(((lat // 100) == 1).sum())
    right = int(((lat // 100) == 2).sum())
    total = left + right
    info = {
        "midline_label": midline_label,
        "mode": midline_mode,
        "point_mm": [float(v) for v in point],
        "normal": [float(v) for v in normal],
        "sst_left_voxels": left,
        "sst_right_voxels": right,
        "sst_split_imbalance": (
            float(abs(left - right) / (0.5 * total)) if total else float("nan")
        ),
    }
    json_path.write_text(json.dumps(info, indent=2) + "\n")
    return str(out_path), str(json_path)


def build_lateralise(config: LatsConfig) -> pe.Node:
    """Return the lateralisation node. Caller wires ``sst_labels``."""
    node = pe.Node(
        Function(
            input_names=["sst_labels", "midline_label", "midline_mode", "output_dir"],
            output_names=["lateral_labels", "midline_json"],
            function=_lateralise_sst,
        ),
        name="lateralise",
    )
    node.inputs.midline_label = config.midline_label
    node.inputs.midline_mode = config.midline_mode
    node.inputs.output_dir = str(sst_dir(config.output_prefix))
    return node
