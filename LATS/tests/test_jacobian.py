"""Unit guard for the native-scale restoration arithmetic."""
from __future__ import annotations

import math

import numpy as np
import pytest
import scipy.io as sio

from lats.nodes.jacobian import native_scale_factors


def _write_affine(path, scales) -> str:
    """Write an ANTs-style .mat whose linear part is ``diag(scales)``."""
    sx, sy, sz = scales
    linear = np.diag([sx, sy, sz]).astype(float)
    params = np.concatenate([linear.ravel(), np.zeros(3)]).reshape(12, 1)
    sio.savemat(str(path), {"AffineTransform_double_3_3": params})
    return str(path)


def test_isotropic_scale_recovers_cubed_ratio(tmp_path):
    # A 1.05x-larger session: |det| = 1.05^3 = 1.157625.
    mat = _write_affine(tmp_path / "aff.mat", (1.05, 1.05, 1.05))
    scale_vol, add_logjac = native_scale_factors(mat)
    assert scale_vol == pytest.approx(1.157625)
    assert add_logjac == pytest.approx(math.log(1.157625))
    # Multiply, NOT divide: a larger session must scale the volume UP.
    assert scale_vol > 1.0
    assert add_logjac > 0.0


def test_identity_is_neutral(tmp_path):
    mat = _write_affine(tmp_path / "id.mat", (1.0, 1.0, 1.0))
    scale_vol, add_logjac = native_scale_factors(mat)
    assert scale_vol == pytest.approx(1.0)
    assert add_logjac == pytest.approx(0.0)


def test_anisotropic_det_is_product_of_scales(tmp_path):
    # |det| of diag(1.1, 1.0, 0.9) = 0.99 (a slightly-smaller session).
    mat = _write_affine(tmp_path / "aniso.mat", (1.1, 1.0, 0.9))
    scale_vol, add_logjac = native_scale_factors(mat)
    assert scale_vol == pytest.approx(0.99)
    assert add_logjac == pytest.approx(math.log(0.99))
    assert scale_vol < 1.0
    assert add_logjac < 0.0


def test_longitudinal_change_is_jacobian_only(tmp_path):
    import csv

    import nibabel as nib

    from lats.nodes.stats import _aggregate

    (tmp_path / "labels.txt").write_text("1 vertical\n")
    nib.save(nib.Nifti1Image(np.ones((2, 2, 2), np.uint8), np.eye(4)), str(tmp_path / "sst.nii.gz"))
    stats, jacs = [], []
    for t, (seg, jac) in enumerate([(1000.0, 1000.0), (1000.0, 900.0)]):
        s, j = tmp_path / f"s{t}.txt", tmp_path / f"j{t}.txt"
        s.write_text(f"1\tvertical\t{seg}\t1000\n0\twhole tongue\t{seg}\t1000\n")
        j.write_text(f"1\tvertical\t{jac}\t0.0\n0\twhole tongue\t{jac}\t0.0\n")
        stats.append(str(s)); jacs.append(str(j))
    _, long_path = _aggregate(stats, jacs, str(tmp_path / "sst.nii.gz"), str(tmp_path / "labels.txt"),
                              ["a.nii.gz", "b.nii.gz"], str(tmp_path / "out"))
    rows = list(csv.DictReader(open(long_path)))
    assert "seg_pct_change" not in rows[0]
    tp1 = next(r for r in rows if r["timepoint"] == "1" and r["label_id"] == "1")
    assert float(tp1["jac_pct_change"]) == pytest.approx(-10.0)
    assert float(tp1["seg_volume_mm3"]) == pytest.approx(1000.0)
