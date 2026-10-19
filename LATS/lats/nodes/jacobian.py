"""Log-Jacobian deformation-based volumes."""
from __future__ import annotations

from nipype.interfaces.utility import Function
from nipype.pipeline import engine as pe

from ..config import LatsConfig
from ..utils.paths import jacobian_dir


def native_scale_factors(native_affine: str) -> tuple[float, float]:
    """Return (scale_vol, add_logjac): multiply volumes by scale_vol, add add_logjac."""
    import math

    from lats.utils.checks import affine_det

    det_native = affine_det(native_affine)
    return det_native, math.log(det_native)


def _jacobian_for_timepoint(
    timepoint_idx: int,
    sst: str,
    affine: str,
    warp: str,
    sst_labels: str,
    labels_file: str,
    native_affine: str,
    output_dir: str,
    keep_tmp: bool,
) -> tuple[str, str, str]:
    """Return (jacobian_image, logjacobian_image, volumes_file)."""
    import subprocess
    from pathlib import Path as _P

    import nibabel as nib
    import numpy as np

    from lats.nodes.jacobian import native_scale_factors
    scale_vol, add_logjac = native_scale_factors(native_affine)

    od = _P(output_dir)
    od.mkdir(parents=True, exist_ok=True)
    tag = f"tp{timepoint_idx:02d}"
    composite = od / f"{tag}_composite_warp.nii.gz"
    jac_path = od / f"{tag}_jacobian.nii.gz"
    log_path = od / f"{tag}_logjacobian.nii.gz"
    vols_path = od / f"{tag}_jacobian_volumes.txt"

    def _run(cmd):
        proc = subprocess.run(cmd, capture_output=True, text=True)
        if proc.returncode != 0:
            tail = "\n".join((proc.stderr or "").splitlines()[-30:]) or "<empty>"
            raise RuntimeError(
                f"{cmd[0]} failed (exit {proc.returncode}) for {tag}\n"
                f"  cmd={' '.join(cmd)}\n--- stderr (tail) ---\n{tail}"
            )

    # 1. Collapse affine + warp into one SST-grid field, ordered as for resampling tp->SST.
    compose = [
        "antsApplyTransforms", "-d", "3",
        "-r", str(sst),
        "-o", f"[{composite},1]",
    ]
    if warp:
        compose += ["-t", str(warp)]
    compose += ["-t", str(affine)]
    _run(compose)

    # 2. Determinant, plain and log (args: dim field out doLog useGeometric).
    _run(["CreateJacobianDeterminantImage", "3", str(composite), str(jac_path), "0", "0"])
    _run(["CreateJacobianDeterminantImage", "3", str(composite), str(log_path), "1", "0"])

    # 3. Integrate over each SST-space label.
    jac_img = nib.load(str(jac_path))
    jac = jac_img.get_fdata()
    log_jac = nib.load(str(log_path)).get_fdata()
    voxel_vol_mm3 = float(abs(np.linalg.det(jac_img.affine[:3, :3])))

    lab = np.rint(nib.load(sst_labels).get_fdata()).astype(int)

    pairs: list[tuple[int, str]] = []
    for line in _P(labels_file).read_text().splitlines():
        parts = line.split(None, 1)
        if len(parts) == 2:
            try:
                pairs.append((int(parts[0]), parts[1].strip()))
            except ValueError:
                pass

    out_lines: list[str] = []
    for label_id, name in pairs:
        mask = lab == label_id
        if not mask.any():
            continue
        jac_volume_mm3 = float(jac[mask].sum() * voxel_vol_mm3) * scale_vol
        mean_logjac = float(log_jac[mask].mean()) + add_logjac
        out_lines.append(f"{label_id}\t{name}\t{jac_volume_mm3:.4f}\t{mean_logjac:.6f}")

    # Whole tongue: every labelled voxel.
    whole = lab > 0
    if whole.any():
        out_lines.append(
            f"0\twhole tongue\t{float(jac[whole].sum() * voxel_vol_mm3) * scale_vol:.4f}"
            f"\t{float(log_jac[whole].mean()) + add_logjac:.6f}"
        )

    vols_path.write_text("\n".join(out_lines) + "\n")

    # Nothing downstream reads the composite field; keep it only with keep_tmp.
    if not keep_tmp:
        composite.unlink(missing_ok=True)

    return str(jac_path), str(log_path), str(vols_path)


def build_jacobian(config: LatsConfig) -> pe.MapNode:
    """MapNode over timepoints. Caller wires the SST, transforms and labels."""
    node = pe.MapNode(
        Function(
            input_names=[
                "timepoint_idx", "sst", "affine", "warp",
                "sst_labels", "labels_file", "native_affine",
                "output_dir", "keep_tmp",
            ],
            output_names=["jacobian_image", "logjacobian_image", "volumes_file"],
            function=_jacobian_for_timepoint,
        ),
        name="jacobian",
        iterfield=["timepoint_idx", "affine", "warp", "native_affine"],
    )
    node.inputs.timepoint_idx = list(range(config.n_timepoints))
    node.inputs.output_dir = str(jacobian_dir(config.output_prefix))
    node.inputs.keep_tmp = config.keep_tmp
    return node
