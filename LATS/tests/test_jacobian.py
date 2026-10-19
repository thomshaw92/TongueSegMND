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
