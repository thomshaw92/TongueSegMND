"""Tongue-local enhancement of the crop and the native QC image."""
from __future__ import annotations

from nipype.interfaces.utility import Function
from nipype.pipeline import engine as pe

from ..config import LatsConfig
from ..utils.paths import intermediate_dir, qc_dir


def _run(cmd, label, env):
    import subprocess
    proc = subprocess.run(cmd, capture_output=True, text=True, env=env)
    if proc.returncode != 0:
        tail = "\n".join((proc.stderr or "").splitlines()[-30:]) or "<empty>"
        raise RuntimeError(f"{label} failed (exit {proc.returncode})\n"
                           f"  cmd={' '.join(cmd)}\n--- stderr ---\n{tail}")


def _n4(inp, out, env, shrink="2", conv="[50x50x50,1e-7]", bspline="[100]", mask=None):
    cmd = ["N4BiasFieldCorrection", "-d", "3", "-i", str(inp), "-o", str(out),
           "-s", shrink, "-c", conv, "-b", bspline]
    if mask:
        cmd += ["-x", str(mask)]
    _run(cmd, "N4BiasFieldCorrection", env)


def _resample_iso(inp, out, iso_mm, env, nearest=False):
    _run(["ResampleImage", "3", str(inp), str(out),
          f"{iso_mm}x{iso_mm}x{iso_mm}", "0", "1" if nearest else "0"],
         "ResampleImage(isotropic)", env)


# ---------------------------------------------------------------------------
# enhance_crop: tongue-local N4 + denoise + isotropic, on the crop
# ---------------------------------------------------------------------------

def _enhance_crop_one(cropped, timepoint_idx, do_n4, do_denoise, iso_mm,
                      threads, output_dir) -> str:
    import os
    import shutil
    from pathlib import Path as _P

    from lats.nodes.enhance import _n4, _resample_iso, _run
    from lats.utils.checks import load_checked

    od = _P(output_dir); od.mkdir(parents=True, exist_ok=True)
    tag = f"tp{timepoint_idx:02d}"
    env = dict(os.environ); env["ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS"] = str(threads)

    current = str(cropped)
    if do_n4:
        out = od / f"{tag}_cropN4.nii.gz"; _n4(current, out, env); current = str(out)
    if do_denoise:
        out = od / f"{tag}_cropDenoise.nii.gz"
        _run(["DenoiseImage", "-d", "3", "-i", current, "-o", str(out), "-n", "Rician"],
             "DenoiseImage", env)
        current = str(out)

    final = od / f"{tag}_cropEnhanced.nii.gz"
    if iso_mm and iso_mm > 0:
        _resample_iso(current, final, iso_mm, env)
    elif current != str(final):
        shutil.copy(current, final)
    load_checked(final)
    return str(final)


def build_enhance_crop(config: LatsConfig) -> pe.MapNode:
    node = pe.MapNode(
        Function(
            input_names=["cropped", "timepoint_idx", "do_n4", "do_denoise",
                         "iso_mm", "threads", "output_dir"],
            output_names=["enhanced"],
            function=_enhance_crop_one,
        ),
        name="enhance_crop",
        iterfield=["cropped", "timepoint_idx"],
    )
    node.inputs.timepoint_idx = list(range(config.n_timepoints))
    node.inputs.do_n4 = config.crop_n4
    node.inputs.do_denoise = config.crop_denoise
    node.inputs.iso_mm = config.isotropic_mm
    node.inputs.threads = config.n_procs
    node.inputs.output_dir = str(intermediate_dir(config.output_prefix) / "enhance_crop")
    return node


# ---------------------------------------------------------------------------
# native_qc_image: whole native image re-preprocessed for QC
# ---------------------------------------------------------------------------

def _native_qc_one(image, warped_labels, timepoint_idx, do_n4, do_denoise,
                   iso_mm, threads, output_dir, work_dir) -> tuple[str, str]:
    """Return (qc_image, qc_labels); intermediates go to ``work_dir``."""
    import os
    from pathlib import Path as _P

    import nibabel as nib
    import numpy as np
    from scipy import ndimage

    from lats.nodes.enhance import _n4, _resample_iso, _run
    from lats.utils.checks import load_checked

    od = _P(output_dir); od.mkdir(parents=True, exist_ok=True)
    wd = _P(work_dir); wd.mkdir(parents=True, exist_ok=True)
    tag = f"tp{timepoint_idx:02d}"
    env = dict(os.environ); env["ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS"] = str(threads)

    current = str(image)
    if do_n4:                                   # whole-image N4 (global shading)
        out = wd / f"{tag}_qcN4.nii.gz"
        _n4(current, out, env, shrink="4", conv="[50x50x50x50,1e-7]", bspline="[200]")
        current = str(out)
    if do_denoise:
        out = wd / f"{tag}_qcDenoise.nii.gz"
        _run(["DenoiseImage", "-d", "3", "-i", current, "-o", str(out), "-n", "Rician"],
             "DenoiseImage", env)
        current = str(out)
    if do_n4:                                   # tongue-masked LOCAL N4
        seg = nib.load(str(warped_labels))
        m = ndimage.binary_dilation(np.asarray(seg.get_fdata()) > 0, iterations=5)
        maskp = wd / f"{tag}_tonguemask.nii.gz"
        nib.save(nib.Nifti1Image(m.astype(np.uint8), seg.affine, seg.header), maskp)
        out = wd / f"{tag}_qcLocalN4.nii.gz"
        _n4(current, out, env, mask=maskp)
        current = str(out)

    qc_image = od / f"{tag}_qc_image.nii.gz"
    if iso_mm and iso_mm > 0:
        _resample_iso(current, qc_image, iso_mm, env)
    else:
        import shutil; shutil.copy(current, qc_image)

    # Resample the native labels onto the (possibly isotropic) QC grid.
    qc_labels = od / f"{tag}_qc_labels.nii.gz"
    _run(["antsApplyTransforms", "-d", "3", "-i", str(warped_labels),
          "-r", str(qc_image), "-o", str(qc_labels), "-n", "GenericLabel"],
         "resample labels onto QC grid", env)

    load_checked(qc_image)
    return str(qc_image), str(qc_labels)


def build_native_qc(config: LatsConfig) -> pe.MapNode:
    node = pe.MapNode(
        Function(
            input_names=["image", "warped_labels", "timepoint_idx", "do_n4",
                         "do_denoise", "iso_mm", "threads", "output_dir",
                         "work_dir"],
            output_names=["qc_image", "qc_labels"],
            function=_native_qc_one,
        ),
        name="native_qc",
        iterfield=["image", "warped_labels", "timepoint_idx"],
    )
    node.inputs.image = [str(tp.image) for tp in config.timepoints]
    node.inputs.timepoint_idx = list(range(config.n_timepoints))
    node.inputs.do_n4 = config.crop_n4
    node.inputs.do_denoise = config.crop_denoise
    node.inputs.iso_mm = config.isotropic_mm
    node.inputs.threads = config.n_procs
    node.inputs.output_dir = str(qc_dir(config.output_prefix))
    node.inputs.work_dir = str(intermediate_dir(config.output_prefix) / "native_qc")
    return node
