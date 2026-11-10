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
    sst_image: str = "",
    threads: int = 1,
    midline_json: str = "",
) -> tuple[str, str]:
    """Return (lateralised_labels_path, midline_json_path)."""
    import json
    from pathlib import Path as _P

    import nibabel as nib
    import numpy as np

    from lats.utils.geometry import ants_affine_ras, lateralise, midline_plane, mirror_plane

    od = _P(output_dir)
    od.mkdir(parents=True, exist_ok=True)
    out_path = od / "sst_seg_lateralised.nii.gz"
    json_path = od / "midline.json"

    img = nib.load(sst_labels)
    data = np.rint(np.asarray(img.get_fdata())).astype(int)

    if midline_json:
        m = json.loads(_P(midline_json).read_text()).get("midline")
        point, normal = np.asarray(m["point_mm"], float), np.asarray(m["normal"], float)
        normal, midline_mode = normal / np.linalg.norm(normal), "qa"
    elif midline_mode == "mirror":
        import subprocess

        sst = nib.load(sst_image)
        ax = int(np.argmax(np.abs(sst.affine[0, :3])))
        mirror = od / "sst_mirror.nii.gz"
        nib.save(nib.Nifti1Image(np.flip(np.asanyarray(sst.dataobj), ax), sst.affine, sst.header), mirror)
        prefix = od / "mirror_"
        proc = subprocess.run(["antsRegistrationSyN.sh", "-d", "3", "-f", str(sst_image), "-m", str(mirror),
                               "-o", str(prefix), "-t", "r", "-n", str(threads)], capture_output=True, text=True)
        if proc.returncode != 0:
            tail = "\n".join((proc.stderr or "").splitlines()[-40:]) or "<empty>"
            raise RuntimeError(f"mirror registration failed (exit {proc.returncode})\n--- stderr ---\n{tail}")
        flip = np.eye(4)
        flip[ax, ax], flip[ax, 3] = -1.0, sst.shape[ax] - 1
        reflection = sst.affine @ flip @ np.linalg.inv(sst.affine) @ ants_affine_ras(f"{prefix}0GenericAffine.mat")
        coords = np.argwhere(data > 0) @ img.affine[:3, :3].T + img.affine[:3, 3]
        point, normal = mirror_plane(coords, reflection)
    else:
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
        "tilt_deg": float(np.degrees(np.arccos(min(1.0, abs(normal[0]))))),
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
            input_names=["sst_labels", "midline_label", "midline_mode", "output_dir", "sst_image", "threads",
                         "midline_json"],
            output_names=["lateral_labels", "midline_json"],
            function=_lateralise_sst,
        ),
        name="lateralise",
    )
    node.inputs.midline_label = config.midline_label
    node.inputs.midline_mode = config.midline_mode
    node.inputs.output_dir = str(sst_dir(config.output_prefix))
    node.inputs.threads = config.n_procs
    node.inputs.midline_json = ""
    return node
