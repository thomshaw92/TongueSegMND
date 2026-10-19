"""SST <-> timepoint registration (fixed = SST) and label propagation."""
from __future__ import annotations

from nipype.interfaces.utility import Function
from nipype.pipeline import engine as pe

from ..config import LatsConfig
from ..utils.paths import labels_dir, reg_dir


def _register_timepoint(
    sst: str,
    timepoint_image: str,
    timepoint_idx: int,
    transform: str,
    threads: int,
    output_dir: str,
) -> tuple[str, str, str, str]:
    """Return (affine, warp, inverse_warp, prefix); warps are empty if linear-only."""
    import os
    import subprocess
    from pathlib import Path as _P

    od = _P(output_dir)
    od.mkdir(parents=True, exist_ok=True)
    prefix = od / f"tp{timepoint_idx:02d}_"

    env = dict(os.environ)
    env["ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS"] = str(threads)

    cmd = [
        "antsRegistrationSyN.sh",
        "-d", "3",
        "-f", str(sst),
        "-m", str(timepoint_image),
        "-o", str(prefix),
        "-t", transform,
        "-n", str(threads),
    ]
    proc = subprocess.run(cmd, capture_output=True, text=True, env=env)
    if proc.returncode != 0:
        tail = "\n".join((proc.stderr or "").splitlines()[-40:]) or "<empty>"
        raise RuntimeError(
            f"antsRegistrationSyN.sh failed (exit {proc.returncode}) for tp{timepoint_idx:02d}\n"
            f"  cmd={' '.join(cmd)}\n--- stderr (tail) ---\n{tail}"
        )

    affine = _P(f"{prefix}0GenericAffine.mat")
    if not affine.is_file():
        raise RuntimeError(f"registration produced no affine: {affine}")

    warp = _P(f"{prefix}1Warp.nii.gz")
    inv = _P(f"{prefix}1InverseWarp.nii.gz")
    return (
        str(affine),
        str(warp) if warp.is_file() else "",
        str(inv) if inv.is_file() else "",
        str(prefix),
    )


def _warp_labels_to_timepoint(
    sst_labels: str,
    timepoint_image: str,
    timepoint_idx: int,
    affine: str,
    inverse_warp: str,
    native_affine: str,
    output_dir: str,
) -> str:
    """Warp SST-space labels to a session's native space in one resampling."""
    import subprocess
    from pathlib import Path as _P

    od = _P(output_dir)
    od.mkdir(parents=True, exist_ok=True)
    out = od / f"tp{timepoint_idx:02d}.nii.gz"

    cmd = [
        "antsApplyTransforms", "-d", "3",
        "-i", str(sst_labels),
        "-r", str(timepoint_image),
        "-o", str(out),
        "-n", "GenericLabel",
        "-t", f"[{affine},1]",
    ]
    if inverse_warp:
        cmd += ["-t", str(inverse_warp)]
    if native_affine:
        cmd += ["-t", f"[{native_affine},1]"]

    proc = subprocess.run(cmd, capture_output=True, text=True)
    if proc.returncode != 0:
        tail = "\n".join((proc.stderr or "").splitlines()[-30:]) or "<empty>"
        raise RuntimeError(
            f"antsApplyTransforms failed (exit {proc.returncode}) for tp{timepoint_idx:02d}\n"
            f"  cmd={' '.join(cmd)}\n--- stderr (tail) ---\n{tail}"
        )
    return str(out)


def build_register(config: LatsConfig) -> pe.MapNode:
    """SST -> each cropped session. Caller wires ``sst`` and ``timepoint_image``."""
    node = pe.MapNode(
        Function(
            input_names=[
                "sst", "timepoint_image", "timepoint_idx",
                "transform", "threads", "output_dir",
            ],
            output_names=["affine", "warp", "inverse_warp", "prefix"],
            function=_register_timepoint,
        ),
        name="register",
        iterfield=["timepoint_image", "timepoint_idx"],
    )
    node.inputs.timepoint_idx = list(range(config.n_timepoints))
    node.inputs.transform = config.reg_transform
    node.inputs.threads = config.n_procs
    node.inputs.output_dir = str(reg_dir(config.output_prefix))
    return node


def build_warp_labels(config: LatsConfig, name: str = "warp_labels",
                      output_dir: str | None = None) -> pe.MapNode:
    """Warp SST labels onto each session's native image grid."""
    node = pe.MapNode(
        Function(
            input_names=[
                "sst_labels", "timepoint_image", "timepoint_idx",
                "affine", "inverse_warp", "native_affine", "output_dir",
            ],
            output_names=["warped_labels"],
            function=_warp_labels_to_timepoint,
        ),
        name=name,
        iterfield=["timepoint_image", "timepoint_idx", "affine",
                   "inverse_warp", "native_affine"],
    )
    node.inputs.timepoint_image = [str(tp.image) for tp in config.timepoints]
    node.inputs.timepoint_idx = list(range(config.n_timepoints))
    node.inputs.output_dir = output_dir or str(labels_dir(config.output_prefix))
    return node
