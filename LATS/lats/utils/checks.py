"""Sanity checks: ``check_*`` raise CheckError; ``warn_*`` return warnings."""
from __future__ import annotations

from pathlib import Path

import numpy as np

# Plausible whole-tongue volume envelope (mm^3).
TONGUE_VOLUME_MIN_MM3 = 15_000.0
TONGUE_VOLUME_MAX_MM3 = 200_000.0

# Registration affines with |det| outside this band are treated as collapsed.
AFFINE_DET_MIN = 0.40
AFFINE_DET_MAX = 2.50


class CheckError(RuntimeError):
    """A hard sanity-check failure."""


def load_checked(path):
    """Load a NIfTI and assert it is readable and 3D."""
    import nibabel as nib

    p = Path(path)
    if not p.is_file():
        raise CheckError(f"file does not exist: {p}")
    try:
        img = nib.load(str(p))
        _ = img.shape
    except Exception as exc:  # noqa: BLE001 - surface any nibabel failure
        raise CheckError(f"cannot read NIfTI {p}: {exc}") from exc
    if len(img.shape) != 3:
        raise CheckError(f"expected a 3D image, got shape {img.shape} for {p}")
    return img


def check_affine_nondegenerate(affine: np.ndarray, name: str) -> None:
    """Raise if the affine is singular or wildly anisotropically scaled."""
    m = np.asarray(affine)[:3, :3]
    det = float(np.linalg.det(m))
    if not np.isfinite(det) or abs(det) < 1e-6:
        raise CheckError(f"{name}: affine is singular (det={det:.3g})")
    sv = np.linalg.svd(m, compute_uv=False)
    cond = float(sv.max() / max(sv.min(), 1e-12))
    if cond > 20.0:
        raise CheckError(
            f"{name}: affine is extremely anisotropic (condition number "
            f"{cond:.1f}); registration has likely collapsed"
        )


def affine_det(mat_path) -> float:
    """``|det|`` of the linear part of an ANTs ``.mat`` affine."""
    import scipy.io as sio

    m = sio.loadmat(mat_path)
    # Key is AffineTransform_<float|double>_3_3 or a MatrixOffset variant.
    key = next((k for k in m if "Transform" in k and k.endswith("_3_3")), None)
    if key is None:
        raise CheckError(f"no affine transform found in {mat_path} "
                         f"(keys: {[k for k in m if not k.startswith('__')]})")
    # ANTs stores this as a (12,1) column: 9 matrix entries then 3 translation.
    A = np.asarray(m[key]).ravel()[:9].reshape(3, 3)
    return float(abs(np.linalg.det(A)))


def check_registration_affine(affine_mat: str, name: str) -> float:
    """Check an ANTs .mat's volumetric scale is physiological; return its |det|."""
    det = affine_det(affine_mat)
    if not (AFFINE_DET_MIN <= det <= AFFINE_DET_MAX):
        raise CheckError(
            f"{name}: registration scale |det|={det:.3f} is outside the "
            f"physiological band [{AFFINE_DET_MIN}, {AFFINE_DET_MAX}] — the "
            f"transform has likely collapsed. Check the input and modality."
        )
    return det


def check_finite_nonempty(data: np.ndarray, name: str) -> None:
    """Raise if an image is all-zero, or contains NaN/Inf."""
    if not np.isfinite(data).all():
        n_bad = int((~np.isfinite(data)).sum())
        raise CheckError(f"{name}: contains {n_bad} non-finite voxel(s) (NaN/Inf)")
    if not np.any(data):
        raise CheckError(f"{name}: image is entirely zero")


def check_labels(data: np.ndarray, name: str) -> None:
    """Raise if a label map has values outside 0-4 or is empty."""
    vals = set(int(v) for v in np.unique(np.rint(data)))
    unexpected = vals - {0, 1, 2, 3, 4}
    if unexpected:
        raise CheckError(f"{name}: unexpected label values {sorted(unexpected)}")
    if vals <= {0}:
        raise CheckError(f"{name}: segmentation is empty (background only)")


def warn_tongue_volume(data: np.ndarray, zooms, name: str) -> list[str]:
    """Warn if the whole-tongue volume is outside the plausible envelope."""
    vox = float(abs(np.prod(zooms[:3])))
    vol = float((np.rint(data) > 0).sum() * vox)
    out = []
    if vol < TONGUE_VOLUME_MIN_MM3:
        out.append(f"{name}: tongue volume {vol/1000:.1f} mL is implausibly small")
    elif vol > TONGUE_VOLUME_MAX_MM3:
        out.append(f"{name}: tongue volume {vol/1000:.1f} mL is implausibly large")
    return out


def warn_header_consistency(images: list) -> list[str]:
    """Warn if sessions differ markedly in geometry."""
    import nibabel as nib

    out: list[str] = []
    shapes, zooms, ornts = [], [], []
    for p in images:
        img = nib.load(str(p))
        shapes.append(tuple(int(s) for s in img.shape[:3]))
        zooms.append(tuple(round(float(z), 2) for z in img.header.get_zooms()[:3]))
        ornts.append("".join(nib.aff2axcodes(img.affine)))
    if len(set(shapes)) > 1:
        out.append(f"sessions differ in matrix size: {shapes}")
    if len(set(zooms)) > 1:
        out.append(f"sessions differ in voxel size: {zooms} — expected with "
                   f"mixed protocols; preprocessing normalises to template")
    if len(set(ornts)) > 1:
        out.append(f"sessions differ in orientation: {ornts}")
    return out


def preflight(image_paths: list) -> list[str]:
    """Run all input-side checks. Raises on hard failures, returns warnings."""
    for p in image_paths:
        img = load_checked(p)
        check_affine_nondegenerate(img.affine, f"input {Path(p).name}")
    return warn_header_consistency(image_paths)
