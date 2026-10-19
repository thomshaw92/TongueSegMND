"""Per-session intensity preprocessing: N4 bias correction then denoising."""
from __future__ import annotations

from nipype.interfaces.utility import Function
from nipype.pipeline import engine as pe

from ..config import LatsConfig
from ..utils.paths import intermediate_dir


def _preprocess_one(
    image: str,
    timepoint_idx: int,
    do_n4: bool,
    do_denoise: bool,
    threads: int,
    output_dir: str,
) -> tuple[str, str]:
    """N4 -> denoise one session. Returns (preprocessed_path, report_json)."""
    import json
    import os
    import subprocess
    from pathlib import Path as _P

    import nibabel as nib
    import numpy as np

    from lats.utils.checks import CheckError, check_finite_nonempty, load_checked

    od = _P(output_dir)
    od.mkdir(parents=True, exist_ok=True)
    tag = f"tp{timepoint_idx:02d}"
    env = dict(os.environ)
    env["ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS"] = str(threads)

    # Input check.
    in_img = load_checked(image)
    in_shape = tuple(int(s) for s in in_img.shape)
    in_affine = np.asarray(in_img.affine)
    report = {"timepoint": timepoint_idx, "input": str(image),
              "n4": bool(do_n4), "denoise": bool(do_denoise), "steps": []}

    def _run(cmd, label):
        proc = subprocess.run(cmd, capture_output=True, text=True, env=env)
        if proc.returncode != 0:
            tail = "\n".join((proc.stderr or "").splitlines()[-30:]) or "<empty>"
            raise RuntimeError(
                f"{label} failed for {tag} (exit {proc.returncode})\n"
                f"  cmd={' '.join(cmd)}\n--- stderr ---\n{tail}"
            )

    current = str(image)
    if do_n4:
        out = od / f"{tag}_n4.nii.gz"
        _run(["N4BiasFieldCorrection", "-d", "3", "-i", current,
              "-o", str(out), "-s", "4", "-c", "[50x50x50x50,1e-7]", "-b", "[200]"],
             "N4BiasFieldCorrection")
        current = str(out)
        report["steps"].append("N4BiasFieldCorrection")

    if do_denoise:
        out = od / f"{tag}_denoised.nii.gz"
        _run(["DenoiseImage", "-d", "3", "-i", current, "-o", str(out), "-n", "Rician"],
             "DenoiseImage")
        current = str(out)
        report["steps"].append("DenoiseImage(Rician)")

    final = od / f"{tag}_preproc.nii.gz"
    if current == str(image):
        # Nothing requested; copy through so the graph has a stable output.
        import shutil
        shutil.copy(image, final)
    else:
        os.replace(current, final)

    # Output checks: geometry preserved, intensities finite and non-empty.
    out_img = nib.load(str(final))
    data = np.asarray(out_img.get_fdata())
    check_finite_nonempty(data, f"{tag} preprocessed")
    if tuple(int(s) for s in out_img.shape) != in_shape:
        raise CheckError(f"{tag}: preprocessing changed matrix size "
                         f"{in_shape} -> {tuple(out_img.shape)}")
    if not np.allclose(np.asarray(out_img.affine), in_affine, atol=1e-3):
        raise CheckError(f"{tag}: preprocessing changed the affine")

    # Soft warning: a large intensity-scale change suggests a failed N4.
    in_data = np.asarray(in_img.get_fdata())
    r0 = float(np.percentile(in_data[in_data > 0], 95)) if (in_data > 0).any() else 0.0
    r1 = float(np.percentile(data[data > 0], 95)) if (data > 0).any() else 0.0
    if r0 > 0 and (r1 / r0 > 5 or r1 / r0 < 0.2):
        report.setdefault("warnings", []).append(
            f"95th-pct intensity changed {r0:.1f} -> {r1:.1f} during preprocessing")

    (od / f"{tag}_preproc.json").write_text(json.dumps(report, indent=2) + "\n")
    return str(final), str(od / f"{tag}_preproc.json")


def build_preprocess(config: LatsConfig) -> pe.MapNode:
    """MapNode over sessions. Consumes the raw image list from config."""
    node = pe.MapNode(
        Function(
            input_names=["image", "timepoint_idx", "do_n4", "do_denoise",
                         "threads", "output_dir"],
            output_names=["preprocessed", "report_json"],
            function=_preprocess_one,
        ),
        name="preprocess",
        iterfield=["image", "timepoint_idx"],
    )
    node.inputs.image = [str(tp.image) for tp in config.timepoints]
    node.inputs.timepoint_idx = list(range(config.n_timepoints))
    node.inputs.do_n4 = config.n4
    node.inputs.do_denoise = config.denoise
    node.inputs.threads = config.n_procs
    node.inputs.output_dir = str(intermediate_dir(config.output_prefix) / "preprocess")
    return node
