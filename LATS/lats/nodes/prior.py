"""Atlas-prior refinement of the SST segmentation (see ``lats.utils.prior``)."""
from __future__ import annotations

from pathlib import Path

from nipype.interfaces.utility import Function
from nipype.pipeline import engine as pe

from ..config import LatsConfig
from ..utils.paths import sst_dir

# Foreground ids in prior-channel order; must match the tongue model's labels.
FG_IDS = (1, 2, 3, 4)


def _register_atlas_to_tongue(
    dl_seg: str, atlas_foreground: str, transform: str, reg_downsample_mm: float,
    threads: int, output_dir: str,
) -> str:
    """Register the atlas tongue to the DL tongue. Returns the transform prefix."""
    import os
    import subprocess
    from pathlib import Path as _P

    import nibabel as nib
    import numpy as np
    from scipy import ndimage

    od = _P(output_dir)
    od.mkdir(parents=True, exist_ok=True)
    prefix = od / "atlas2tongue_"
    env = dict(os.environ)
    env["ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS"] = str(threads)

    # Build the driver: smoothed largest connected component of the DL fg.
    seg_img = nib.load(dl_seg)
    d = np.rint(np.asarray(seg_img.get_fdata())).astype(int)
    fg = d > 0
    if not fg.any():
        raise RuntimeError("DL segmentation is empty; cannot place the prior")
    lab, n = ndimage.label(fg)
    sizes = np.bincount(lab.ravel())[1:]
    main = (lab == int(sizes.argmax()) + 1).astype(np.float32)
    driver = od / "dl_main.nii.gz"
    nib.save(nib.Nifti1Image(ndimage.gaussian_filter(main, 1.0),
                             seg_img.affine, seg_img.header), driver)

    driver_ds = od / "dl_main_ds.nii.gz"
    ds = str(reg_downsample_mm)
    subprocess.run(["ResampleImage", "3", str(driver), str(driver_ds),
                    f"{ds}x{ds}x{ds}", "0", "0"], check=True, capture_output=True)

    cmd = [
        "antsRegistrationSyN.sh", "-d", "3",
        "-f", str(driver_ds), "-m", str(atlas_foreground),
        "-o", str(prefix), "-t", transform, "-n", str(threads), "-p", "f",
    ]
    proc = subprocess.run(cmd, capture_output=True, text=True, env=env)
    if proc.returncode != 0:
        tail = "\n".join((proc.stderr or "").splitlines()[-40:]) or "<empty>"
        raise RuntimeError(
            f"atlas->tongue registration failed (exit {proc.returncode})\n"
            f"  cmd={' '.join(cmd)}\n--- stderr ---\n{tail}"
        )
    if not _P(f"{prefix}0GenericAffine.mat").is_file():
        raise RuntimeError(f"atlas registration produced no affine at {prefix}")
    return str(prefix)


def _warp_priors_to_sst(
    prior_files: list, sst_seg: str, reg_prefix: str, output_dir: str,
) -> list:
    """Warp each soft prior into the DL-seg grid, linear interpolation."""
    import subprocess
    from pathlib import Path as _P

    od = _P(output_dir)
    od.mkdir(parents=True, exist_ok=True)
    warp = f"{reg_prefix}1Warp.nii.gz"
    affine = f"{reg_prefix}0GenericAffine.mat"
    has_warp = _P(warp).is_file()

    out_paths = []
    for pf in prior_files:
        name = _P(pf).name.replace("atlas_prob", "sstprior")
        out = od / name
        cmd = ["antsApplyTransforms", "-d", "3", "-i", str(pf), "-r", str(sst_seg),
               "-o", str(out), "-n", "Linear"]
        if has_warp:
            cmd += ["-t", warp]
        cmd += ["-t", affine]
        proc = subprocess.run(cmd, capture_output=True, text=True)
        if proc.returncode != 0:
            tail = "\n".join((proc.stderr or "").splitlines()[-30:]) or "<empty>"
            raise RuntimeError(
                f"warping prior {pf} failed (exit {proc.returncode})\n"
                f"  cmd={' '.join(cmd)}\n--- stderr ---\n{tail}"
            )
        out_paths.append(str(out))
    return out_paths


def _refine_segmentation(
    dl_seg: str, warped_priors: list, fg_ids: list,
    prior_weight: float, debris_prior_threshold: float,
    likelihood_smooth_mm: float, dilate_mm: float, output_dir: str,
    dl_probs: str = "", use_probs: bool = True,
) -> tuple[str, str]:
    """Combine DL seg + warped priors. Returns (refined_seg, report_json)."""
    import json
    from pathlib import Path as _P

    import nibabel as nib
    import numpy as np

    from lats.utils.prior import refine_with_prior

    od = _P(output_dir)
    od.mkdir(parents=True, exist_ok=True)
    out_seg = od / "sst_seg_refined.nii.gz"
    out_json = od / "prior_refine.json"

    seg_img = nib.load(dl_seg)
    dl = np.rint(np.asarray(seg_img.get_fdata())).astype(np.int16)
    zooms = tuple(float(z) for z in seg_img.header.get_zooms()[:3])

    # Stack the foreground priors in fg_ids order (filenames carry the id).
    by_id = {}
    for pf in warped_priors:
        stem = _P(pf).name
        for fid in fg_ids:
            if f"_{fid}.nii" in stem:
                by_id[fid] = np.asarray(nib.load(pf).get_fdata(), dtype=np.float32)
    priors = np.stack([by_id[fid] for fid in fg_ids], axis=-1)

    # DL softmax probabilities (channel order [bg, *fg_ids]) -> the true likelihood.
    dl_probs_arr = None
    if use_probs and dl_probs and _P(dl_probs).is_file():
        pr = np.asarray(nib.load(dl_probs).get_fdata(), dtype=np.float32)
        if pr.ndim == 4 and pr.shape[-1] == len(fg_ids) + 1 and pr.shape[:3] == dl.shape:
            dl_probs_arr = pr
        else:
            raise ValueError(
                f"dl_probs shape {pr.shape} incompatible with seg {dl.shape} "
                f"and {len(fg_ids) + 1} classes ([bg, *fg_ids])")

    refined, info = refine_with_prior(
        dl, priors, zooms, fg_ids=tuple(fg_ids),
        prior_weight=prior_weight, debris_prior_threshold=debris_prior_threshold,
        likelihood_smooth_mm=likelihood_smooth_mm, dilate_mm=dilate_mm,
        dl_probs=dl_probs_arr,
    )
    nib.save(nib.Nifti1Image(refined.astype(np.uint8), seg_img.affine, seg_img.header),
             out_seg)

    # Per-label before/after voxel counts, for the report.
    vox_mm3 = float(abs(np.linalg.det(seg_img.affine[:3, :3])))
    info["voxel_volume_mm3"] = vox_mm3
    info["per_label"] = {
        str(fid): {
            "dl_voxels": int((dl == fid).sum()),
            "refined_voxels": int((refined == fid).sum()),
        }
        for fid in fg_ids
    }
    out_json.write_text(json.dumps(info, indent=2) + "\n")
    return str(out_seg), str(out_json)


def build_register_atlas(config: LatsConfig) -> pe.Node:
    node = pe.Node(
        Function(
            input_names=["dl_seg", "atlas_foreground", "transform",
                         "reg_downsample_mm", "threads", "output_dir"],
            output_names=["reg_prefix"],
            function=_register_atlas_to_tongue,
        ),
        name="register_atlas",
    )
    node.inputs.atlas_foreground = str(Path(config.atlas_dir) / "atlas_foreground.nii.gz")
    node.inputs.transform = config.atlas_reg_transform
    node.inputs.reg_downsample_mm = config.atlas_reg_downsample_mm
    node.inputs.threads = config.n_procs
    node.inputs.output_dir = str(sst_dir(config.output_prefix) / "prior")
    return node


def build_warp_priors(config: LatsConfig) -> pe.Node:
    prior_files = [str(Path(config.atlas_dir) / f"atlas_prob_{k}.nii.gz") for k in FG_IDS]
    node = pe.Node(
        Function(
            input_names=["prior_files", "sst_seg", "reg_prefix", "output_dir"],
            output_names=["warped_priors"],
            function=_warp_priors_to_sst,
        ),
        name="warp_priors",
    )
    node.inputs.prior_files = prior_files
    node.inputs.output_dir = str(sst_dir(config.output_prefix) / "prior")
    return node


def build_refine(config: LatsConfig) -> pe.Node:
    node = pe.Node(
        Function(
            input_names=[
                "dl_seg", "warped_priors", "fg_ids", "prior_weight",
                "debris_prior_threshold", "likelihood_smooth_mm", "dilate_mm",
                "output_dir", "dl_probs", "use_probs",
            ],
            output_names=["refined_seg", "report_json"],
            function=_refine_segmentation,
        ),
        name="refine_seg",
    )
    node.inputs.fg_ids = list(FG_IDS)
    node.inputs.prior_weight = config.prior_weight
    node.inputs.debris_prior_threshold = config.debris_prior_threshold
    node.inputs.likelihood_smooth_mm = config.prior_likelihood_smooth_mm
    node.inputs.dilate_mm = config.prior_dilate_mm
    node.inputs.use_probs = config.prior_use_probs
    # dl_probs is wired from segment_sst.sst_probs; "" falls back to one-hot.
    node.inputs.output_dir = str(sst_dir(config.output_prefix))
    return node
