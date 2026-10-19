"""Tests for the sanity-check layer."""
from __future__ import annotations

import nibabel as nib
import numpy as np
import pytest

from lats.utils import checks
from lats.utils.checks import CheckError


def _save(tmp_path, name, data, affine=None):
    affine = np.eye(4) if affine is None else affine
    p = tmp_path / name
    nib.save(nib.Nifti1Image(data, affine), p)
    return p


def test_load_checked_rejects_missing(tmp_path):
    with pytest.raises(CheckError, match="does not exist"):
        checks.load_checked(tmp_path / "nope.nii.gz")


def test_load_checked_rejects_4d(tmp_path):
    p = _save(tmp_path, "x.nii.gz", np.zeros((4, 4, 4, 2), dtype=np.float32))
    with pytest.raises(CheckError, match="3D"):
        checks.load_checked(p)


def test_degenerate_affine_is_caught():
    aff = np.eye(4)
    aff[2, 2] = 1e-9                       # near-singular
    with pytest.raises(CheckError, match="singular"):
        checks.check_affine_nondegenerate(aff, "test")


def test_anisotropic_affine_is_caught():
    aff = np.diag([1.0, 1.0, 0.02, 1.0])   # condition number 50
    with pytest.raises(CheckError, match="anisotropic"):
        checks.check_affine_nondegenerate(aff, "test")


def test_registration_affine_scale_band(tmp_path):
    import scipy.io as sio

    # A collapsed affine (det ~0.31), below the physiological band.
    A = np.diag([1.154, 0.736, 0.367])     # det ~0.31
    p = tmp_path / "bad.mat"
    sio.savemat(p, {"AffineTransform_double_3_3":
                    np.r_[A.ravel(), [0, 0, 0]].reshape(12, 1)})
    with pytest.raises(CheckError, match="collapsed"):
        checks.check_registration_affine(str(p), "atlas->head")

    A2 = np.eye(3)                          # det 1.0, fine
    p2 = tmp_path / "ok.mat"
    sio.savemat(p2, {"AffineTransform_double_3_3":
                     np.r_[A2.ravel(), [0, 0, 0]].reshape(12, 1)})
    assert abs(checks.check_registration_affine(str(p2), "ok") - 1.0) < 1e-6


def test_finite_nonempty():
    with pytest.raises(CheckError, match="non-finite"):
        checks.check_finite_nonempty(np.array([1.0, np.nan, 2.0]), "x")
    with pytest.raises(CheckError, match="entirely zero"):
        checks.check_finite_nonempty(np.zeros((3, 3)), "x")
    checks.check_finite_nonempty(np.ones((3, 3)), "x")   # ok


def test_check_labels():
    with pytest.raises(CheckError, match="unexpected label"):
        checks.check_labels(np.array([0, 1, 7]), "seg")
    with pytest.raises(CheckError, match="empty"):
        checks.check_labels(np.zeros(5), "seg")
    checks.check_labels(np.array([0, 1, 2, 3, 4]), "seg")  # ok


def test_warn_tongue_volume():
    small = np.ones((5, 5, 5))             # 125 mm^3 at 1mm -> tiny
    assert checks.warn_tongue_volume(small, (1, 1, 1), "seg")
    ok = np.ones((45, 45, 45))             # ~91 mL
    assert not checks.warn_tongue_volume(ok, (1, 1, 1), "seg")


def test_warn_header_consistency(tmp_path):
    a = _save(tmp_path, "a.nii.gz", np.zeros((10, 10, 10), np.float32))
    b = _save(tmp_path, "b.nii.gz", np.zeros((12, 12, 12), np.float32),
              affine=np.diag([0.8, 0.8, 0.8, 1.0]))
    warns = checks.warn_header_consistency([a, b])
    assert any("matrix size" in w for w in warns)
    assert any("voxel size" in w for w in warns)
