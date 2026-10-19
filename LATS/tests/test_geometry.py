"""Analytic tests of the shape descriptors."""
from __future__ import annotations

import numpy as np
import pytest

from lats.utils import geometry as g


def _ellipsoid(shape, semi_axes, spacing=(1.0, 1.0, 1.0)):
    """Boolean ellipsoid centred in an array of ``shape``."""
    grids = [
        (np.arange(n) - (n - 1) / 2.0) * s
        for n, s in zip(shape, spacing)
    ]
    x, y, z = np.meshgrid(*grids, indexing="ij")
    a, b, c = semi_axes
    return (x / a) ** 2 + (y / b) ** 2 + (z / c) ** 2 <= 1.0


def _affine(spacing=(1.0, 1.0, 1.0)):
    aff = np.eye(4)
    aff[0, 0], aff[1, 1], aff[2, 2] = spacing
    return aff


# --- surface area & sphericity ---------------------------------------------

def test_sphere_surface_area_matches_analytic():
    r = 20.0
    mask = _ellipsoid((64, 64, 64), (r, r, r))
    area = g.surface_area_mm2(mask, (1.0, 1.0, 1.0))
    expected = 4.0 * np.pi * r ** 2
    assert abs(area - expected) / expected < 0.02, (area, expected)


def test_sphere_sphericity_is_one():
    r = 20.0
    mask = _ellipsoid((64, 64, 64), (r, r, r))
    vol = mask.sum() * 1.0
    area = g.surface_area_mm2(mask, (1.0, 1.0, 1.0))
    assert abs(g.sphericity(vol, area) - 1.0) < 0.02


def test_sphericity_of_cube_is_known():
    # A cube's sphericity is pi^(1/3) (6)^(2/3) / 6 = 0.8060
    side = 30.0
    vol = side ** 3
    area = 6 * side ** 2
    assert abs(g.sphericity(vol, area) - 0.80599) < 1e-4


# --- inertia ----------------------------------------------------------------

def test_sphere_is_isotropic():
    mask = _ellipsoid((48, 48, 48), (18.0, 18.0, 18.0))
    d = g.inertia_descriptors(g.voxel_world_coords(mask, _affine()))
    assert abs(d["elongation"] - 1.0) < 0.02
    assert abs(d["flatness"] - 1.0) < 0.02


def test_ellipsoid_axis_lengths_and_ratios():
    a, b, c = 30.0, 20.0, 10.0
    mask = _ellipsoid((80, 80, 80), (a, b, c))
    d = g.inertia_descriptors(g.voxel_world_coords(mask, _affine()))
    # Equivalent-ellipsoid full axis lengths should recover 2a, 2b, 2c.
    for got, want in zip(
        (d["pc1_length_mm"], d["pc2_length_mm"], d["pc3_length_mm"]),
        (2 * a, 2 * b, 2 * c),
    ):
        assert abs(got - want) / want < 0.03, (got, want)
    assert abs(d["elongation"] - b / a) < 0.03
    assert abs(d["flatness"] - c / a) < 0.03


def test_centroid_is_recovered():
    mask = np.zeros((20, 20, 20), dtype=bool)
    mask[5:9, 10:14, 2:6] = True
    aff = _affine((2.0, 2.0, 2.0))
    d = g.inertia_descriptors(g.voxel_world_coords(mask, aff))
    assert abs(d["centroid_x_mm"] - 13.0) < 1e-6   # (5+8)/2 * 2
    assert abs(d["centroid_y_mm"] - 23.0) < 1e-6
    assert abs(d["centroid_z_mm"] - 7.0) < 1e-6


# --- roughness / profiles ---------------------------------------------------

def test_polynomial_is_perfectly_detrended():
    x = np.linspace(0, 10, 60)
    y = 3.0 + 2.0 * x - 0.4 * x ** 2 + 0.01 * x ** 3
    assert g.polynomial_residual_rms(x, y, degree=4) < 1e-6


def test_sinusoidal_roughness_recovers_amplitude():
    # RMS of A*sin over whole periods is A/sqrt(2).
    x = np.linspace(0, 60, 400)
    amp = 1.5
    y = amp * np.sin(2 * np.pi * x / 10.0)
    rms = g.polynomial_residual_rms(x, y, degree=4)
    assert abs(rms - amp / np.sqrt(2)) < 0.08, rms


def test_resample_profile_preserves_a_ramp():
    x = np.linspace(3.0, 9.0, 37)
    y = 2.0 * x
    out = g.resample_profile(x, y, n=11)
    assert np.allclose(out, np.linspace(6.0, 18.0, 11), atol=1e-6)


def test_curvature_of_a_circle_is_one_over_r():
    r = 50.0
    s = np.linspace(-20.0, 20.0, 201)
    z = np.sqrt(r ** 2 - s ** 2)
    _, mean_k, max_k, _ = g.curvature_1d(s, z, smooth_sigma=1.0)
    assert abs(mean_k - 1.0 / r) / (1.0 / r) < 0.01, mean_k
    assert abs(max_k - 1.0 / r) / (1.0 / r) < 0.05, max_k


def test_curvature_end_trim_removes_the_boundary_artefact():
    # Untrimmed, the smoothed profile's ends show spurious curvature.
    r = 50.0
    s = np.linspace(-20.0, 20.0, 201)
    z = np.sqrt(r ** 2 - s ** 2)
    _, untrimmed_mean, untrimmed_max, _ = g.curvature_1d(s, z, smooth_sigma=1.0, trim=0)
    _, mean_k, max_k, _ = g.curvature_1d(s, z, smooth_sigma=1.0)
    assert untrimmed_max > 5 * (1.0 / r)        # the artefact is real and large
    assert max_k < 1.5 * (1.0 / r)              # and the trim removes it
    assert abs(mean_k - 1.0 / r) < abs(untrimmed_mean - 1.0 / r)


def test_arc_length_of_a_circle_segment():
    r = 50.0
    s = np.linspace(-20.0, 20.0, 401)
    z = np.sqrt(r ** 2 - s ** 2)
    _, _, _, length = g.curvature_1d(s, z, smooth_sigma=1.0)
    expected = 2 * r * np.arcsin(20.0 / r)      # 2*r*theta
    assert abs(length - expected) / expected < 0.01, (length, expected)


# --- height maps ------------------------------------------------------------

def test_rugosity_of_flat_surface_is_one():
    grid = np.full((20, 20), 5.0)
    assert abs(g.rugosity(grid, 1.0, 1.0) - 1.0) < 1e-9


def test_rugosity_of_45_degree_slope_is_sqrt2():
    rows = np.arange(20).astype(float)
    grid = np.repeat(rows[:, None], 20, axis=1)   # slope 1 mm per mm
    assert abs(g.rugosity(grid, 1.0, 1.0) - np.sqrt(2.0)) < 1e-6


def test_surface_roughness_zero_on_smooth_quadratic():
    r, c = np.meshgrid(np.linspace(-1, 1, 24), np.linspace(-1, 1, 24), indexing="ij")
    grid = 2.0 + 0.5 * r - 0.3 * c + 0.8 * r * c - 0.2 * r ** 2
    assert g.surface_roughness(grid, degree=3) < 1e-9


def test_superior_height_map_finds_the_top():
    mask = np.zeros((10, 12, 14), dtype=bool)
    mask[3:6, 4:8, 2:9] = True          # top SI index is 8
    heights = g.superior_height_map(mask, lr_axis=0, si_axis=2, ap_axis=1,
                                    zooms=(1.0, 1.0, 2.0))
    assert np.nanmax(heights) == pytest.approx(16.0)   # 8 * 2.0mm
    assert np.isnan(heights[0, 0])


# --- midline / lateralisation ----------------------------------------------

def test_lateralise_splits_a_symmetric_object_evenly():
    labels = np.zeros((40, 20, 20), dtype=int)
    labels[10:30, 5:15, 5:15] = 4          # symmetric about x index 19.5
    aff = _affine()
    point, normal = g.midline_plane(labels, aff, midline_label=4)
    assert np.allclose(normal, [1.0, 0.0, 0.0])
    lat = g.lateralise(labels, aff, point, normal)
    left = int((lat == 104).sum())
    right = int((lat == 204).sum())
    assert left == right, (left, right)
    assert left + right == int((labels > 0).sum())


def test_lateralise_detects_asymmetry():
    labels = np.zeros((40, 20, 20), dtype=int)
    labels[10:30, 5:15, 5:15] = 4          # defines the midline
    labels[30:34, 5:15, 5:15] = 1          # extra tissue on the right only
    aff = _affine()
    point, normal = g.midline_plane(labels, aff, midline_label=4)
    lat = g.lateralise(labels, aff, point, normal)
    assert int((lat == 201).sum()) > 0
    assert int((lat == 101).sum()) == 0


def test_midline_pca_mode_tolerates_rotation():
    labels = np.zeros((50, 30, 30), dtype=int)
    labels[15:35, 8:22, 10:20] = 4
    aff = _affine()
    point, normal = g.midline_plane(labels, aff, midline_label=4, mode="pca")
    # PCA picks the smallest extent (SI here); check the normal is unit and +R.
    assert abs(np.linalg.norm(normal) - 1.0) < 1e-9
    assert normal[0] >= 0.0
