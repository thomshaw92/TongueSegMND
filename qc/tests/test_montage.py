"""Tests for :mod:`tongue_qc.montage`."""

from __future__ import annotations

import inspect
from pathlib import Path
from typing import Callable, Sequence

import nibabel as nib
import numpy as np
import pytest
from PIL import Image

from tongue_qc.discovery import Scan
from tongue_qc.montage import (
    LABEL_COLORS,
    LABEL_NAMES,
    PLANE_AXIS,
    PLANES,
    MontageResult,
    render_scan,
)

from .conftest import (
    NATIVE_SHAPE,
    NATIVE_ZOOMS,
    diagonal_affine,
    scan_from_dir,
)

GUTTER_PX = 2


# --------------------------------------------------------------------------- #
# Helpers
# --------------------------------------------------------------------------- #


def _seg_path(scan: Scan) -> Path:
    path = scan.seg_native if scan.space == "native" else scan.seg_template
    assert path is not None, f"scan has no seg for space {scan.space!r}"
    return path


def _ras_labels(path: Path) -> np.ndarray:
    """Load a label map the way the renderer must read it: canonical RAS, rounded."""
    img = nib.as_closest_canonical(nib.load(str(path)))
    return np.rint(np.asarray(img.get_fdata())).astype(np.int16)


def _ras_zooms(path: Path) -> tuple[float, float, float]:
    img = nib.as_closest_canonical(nib.load(str(path)))
    z = img.header.get_zooms()[:3]
    return (float(z[0]), float(z[1]), float(z[2]))


def _bbox_1d(mask: np.ndarray, axis: int) -> tuple[int, int]:
    others = tuple(a for a in range(3) if a != axis)
    hits = np.where(mask.any(axis=others))[0]
    assert hits.size, f"mask empty along axis {axis}"
    return int(hits[0]), int(hits[-1])


#: Read from ``render_scan`` so the tests follow its default padding.
DEFAULT_PAD_FRAC: float = inspect.signature(render_scan).parameters["pad_frac"].default


def _expected_slices(
    mask: np.ndarray, axis: int, dim: int, n_frames: int = 6,
    pad_frac: float = DEFAULT_PAD_FRAC,
) -> list[int]:
    """The expected slice indices: evenly spaced over the padded bbox, endpoints included."""
    lo, hi = _bbox_1d(mask, axis)
    span = hi - lo
    p = int(round(pad_frac * span))
    a, b = max(lo - p, 0), min(hi + p, dim - 1)
    return [int(round(v)) for v in np.linspace(a, b, n_frames)]


def _strip(out_dir: Path, result: MontageResult, key: str) -> Image.Image:
    assert key in result.files, f"MontageResult.files is missing {key!r}"
    path = out_dir / result.files[key]
    assert path.is_file(), f"declared file {path} was not written"
    return Image.open(path)


def _rgba(out_dir: Path, result: MontageResult, key: str) -> np.ndarray:
    with _strip(out_dir, result, key) as im:
        assert im.mode == "RGBA", f"{key} must be RGBA, got {im.mode}"
        return np.asarray(im)


def _grey(out_dir: Path, result: MontageResult, key: str) -> np.ndarray:
    with _strip(out_dir, result, key) as im:
        return np.asarray(im.convert("L"))


def _expected_rgb() -> dict[int, tuple[int, int, int]]:
    return {
        lab: tuple(int(round(c * 255)) for c in col)  # type: ignore[misc]
        for lab, col in LABEL_COLORS.items()
    }


def _matches_a_label(rgb: Sequence[int], labels: Sequence[int]) -> bool:
    expected = _expected_rgb()
    return any(
        max(abs(int(rgb[k]) - expected[lab][k]) for k in range(3)) <= 1 for lab in labels
    )


# --------------------------------------------------------------------------- #
# Module-level constants
# --------------------------------------------------------------------------- #


def test_plane_axes_and_label_colours_are_fixed() -> None:
    assert PLANES == ("sagittal", "coronal", "axial")
    assert PLANE_AXIS == {"sagittal": 0, "coronal": 1, "axial": 2}
    assert LABEL_COLORS == {
        1: (1.00, 0.25, 0.25),
        2: (0.20, 0.55, 1.00),
        3: (0.20, 0.80, 0.30),
        4: (1.00, 0.75, 0.10),
    }
    assert LABEL_NAMES == {
        1: "vertical",
        2: "sup. longitudinal",
        3: "inf. longitudinal",
        4: "genioglossus",
    }


def test_montage_result_fields(normal_scan: Scan, out_dir: Path) -> None:
    res = render_scan(normal_scan, out_dir)

    assert isinstance(res, MontageResult)
    assert res.n_frames == 6
    assert res.seg_empty is False
    assert res.fallback is None
    assert isinstance(res.warnings, list)
    assert set(res.slices) == set(PLANES)
    assert set(res.frame_px) == set(PLANES)
    for plane in PLANES:
        assert len(res.slices[plane]) == 6
        assert all(isinstance(i, int) for i in res.slices[plane])
        w, h = res.frame_px[plane]
        assert w > 0 and h > 0


# --------------------------------------------------------------------------- #
# Strip layout
# --------------------------------------------------------------------------- #


@pytest.mark.parametrize("n_frames", [3, 6, 8])
def test_strip_width_is_frames_plus_gutters(
    normal_scan: Scan, out_dir: Path, n_frames: int
) -> None:
    res = render_scan(normal_scan, out_dir, n_frames=n_frames)

    assert res.n_frames == n_frames
    for plane in PLANES:
        fw, fh = res.frame_px[plane]
        expected_w = n_frames * fw + (n_frames - 1) * GUTTER_PX
        with _strip(out_dir, res, f"{plane}_bg") as bg:
            assert bg.size == (expected_w, fh), f"{plane} bg strip is mis-sized"
        assert len(res.slices[plane]) == n_frames


def test_bg_and_overlay_strips_are_pixel_identical_in_size(
    normal_scan: Scan, out_dir: Path
) -> None:
    res = render_scan(normal_scan, out_dir, contour=True)

    for plane in PLANES:
        with (
            _strip(out_dir, res, f"{plane}_bg") as bg,
            _strip(out_dir, res, f"{plane}_ov") as ov,
            _strip(out_dir, res, f"{plane}_ct") as ct,
        ):
            assert bg.size == ov.size, f"{plane}: bg {bg.size} != ov {ov.size}"
            assert ct.size == bg.size, f"{plane}: ct {ct.size} != bg {bg.size}"


def test_file_formats_and_modes(normal_scan: Scan, out_dir: Path) -> None:
    res = render_scan(normal_scan, out_dir, contour=True)

    for plane in PLANES:
        with _strip(out_dir, res, f"{plane}_bg") as bg:
            assert bg.format == "JPEG"
            assert bg.mode == "L", "the background must be greyscale, not RGB"
        for key in (f"{plane}_ov", f"{plane}_ct"):
            with _strip(out_dir, res, key) as im:
                assert im.format == "PNG"
                assert im.mode == "RGBA"


def test_one_strip_per_plane_not_one_file_per_frame(normal_scan: Scan, out_dir: Path) -> None:
    render_scan(normal_scan, out_dir, contour=True)

    written = sorted(
        p.name for p in out_dir.iterdir() if p.suffix.lower() in {".png", ".jpg", ".jpeg"}
    )
    assert len(written) <= 3 * len(PLANES), f"too many image files written: {written}"
    assert len(written) >= 2 * len(PLANES)


def test_gutters_are_black_in_the_background_strip(normal_scan: Scan, out_dir: Path) -> None:
    res = render_scan(normal_scan, out_dir)

    # The strip is a lossy JPEG, so the gutter need only read as black.
    for plane in PLANES:
        fw, _fh = res.frame_px[plane]
        arr = _grey(out_dir, res, f"{plane}_bg")
        for k in range(res.n_frames - 1):
            x0 = (k + 1) * fw + k * GUTTER_PX
            gutter = arr[:, x0 : x0 + GUTTER_PX]
            assert gutter.max() <= 24, f"{plane} gutter at x={x0} is not black"
            assert gutter.mean() <= 4, f"{plane} gutter at x={x0} carries image content"


def test_no_contour_file_when_contour_is_false(normal_scan: Scan, out_dir: Path) -> None:
    render_scan(normal_scan, out_dir, contour=False)
    assert not list(out_dir.glob("*_ct.png"))


# --------------------------------------------------------------------------- #
# Slice selection
# --------------------------------------------------------------------------- #


def test_slice_indices_are_evenly_spaced_over_the_padded_bbox(
    normal_scan: Scan, out_dir: Path
) -> None:
    res = render_scan(normal_scan, out_dir)
    seg = _ras_labels(_seg_path(normal_scan))
    mask = seg > 0

    for plane in PLANES:
        axis = PLANE_AXIS[plane]
        assert res.slices[plane] == _expected_slices(mask, axis, seg.shape[axis])


def test_slices_are_monotonic_in_bounds_and_span_the_padded_bbox(
    normal_scan: Scan, out_dir: Path
) -> None:
    res = render_scan(normal_scan, out_dir)
    seg = _ras_labels(_seg_path(normal_scan))
    mask = seg > 0

    for plane in PLANES:
        axis = PLANE_AXIS[plane]
        dim = seg.shape[axis]
        idx = res.slices[plane]
        lo, hi = _bbox_1d(mask, axis)
        span = hi - lo
        p = int(round(DEFAULT_PAD_FRAC * span))
        a, b = max(lo - p, 0), min(hi + p, dim - 1)

        assert idx == sorted(idx), f"{plane} slice indices are not monotonic"
        assert all(0 <= i < dim for i in idx), f"{plane} slice index out of bounds"
        # Both endpoints are included.
        assert idx[0] == a
        assert idx[-1] == b
        assert idx[0] <= lo and idx[-1] >= hi


def test_slices_clamp_when_the_seg_touches_the_volume_edge(
    edge_seg_scan: Scan, out_dir: Path
) -> None:
    res = render_scan(edge_seg_scan, out_dir)
    seg = _ras_labels(_seg_path(edge_seg_scan))
    mask = seg > 0

    # Fixture guarantee: the blob is clipped by the axis-0 low face ...
    assert _bbox_1d(mask, 0)[0] == 0
    # ... and by the axis-2 high face.
    assert _bbox_1d(mask, 2)[1] == seg.shape[2] - 1

    assert res.slices["sagittal"][0] == 0
    assert res.slices["axial"][-1] == seg.shape[2] - 1
    for plane in PLANES:
        axis = PLANE_AXIS[plane]
        assert all(0 <= i < seg.shape[axis] for i in res.slices[plane])
        assert res.slices[plane] == _expected_slices(mask, axis, seg.shape[axis])


def test_edge_scan_strips_are_still_well_formed(edge_seg_scan: Scan, out_dir: Path) -> None:
    res = render_scan(edge_seg_scan, out_dir)
    for plane in PLANES:
        fw, fh = res.frame_px[plane]
        with _strip(out_dir, res, f"{plane}_bg") as bg, _strip(out_dir, res, f"{plane}_ov") as ov:
            assert bg.size == ov.size
            assert bg.size == (res.n_frames * fw + (res.n_frames - 1) * GUTTER_PX, fh)


# --------------------------------------------------------------------------- #
# Empty segmentation
# --------------------------------------------------------------------------- #


def test_empty_seg_falls_back_to_centre_and_still_renders(
    empty_seg_scan: Scan, out_dir: Path
) -> None:
    res = render_scan(empty_seg_scan, out_dir)
    seg = _ras_labels(_seg_path(empty_seg_scan))
    assert not (seg > 0).any(), "fixture is meant to be empty"
    assert not (empty_seg_scan.path / "work").exists(), "no raw seg, so 'raw' cannot be chosen"

    assert res.seg_empty is True
    assert res.fallback == "centre"
    assert isinstance(res.warnings, list)

    for plane in PLANES:
        axis = PLANE_AXIS[plane]
        dim = seg.shape[axis]
        idx = res.slices[plane]
        assert len(idx) == res.n_frames
        assert idx == sorted(idx)
        assert all(0 <= i < dim for i in idx)
        # Middle 50 % of the axis: centred and away from the volume faces.
        assert idx[0] > 0 and idx[-1] < dim - 1
        assert (idx[0] + idx[-1]) / 2 == pytest.approx(dim / 2, abs=2)
        assert dim * 0.4 <= idx[-1] - idx[0] <= dim * 0.7

        fw, fh = res.frame_px[plane]
        with _strip(out_dir, res, f"{plane}_bg") as bg, _strip(out_dir, res, f"{plane}_ov") as ov:
            assert bg.size == ov.size
            assert bg.size == (res.n_frames * fw + (res.n_frames - 1) * GUTTER_PX, fh)


def test_empty_seg_overlay_is_fully_transparent(empty_seg_scan: Scan, out_dir: Path) -> None:
    res = render_scan(empty_seg_scan, out_dir)
    for plane in PLANES:
        assert int(_rgba(out_dir, res, f"{plane}_ov")[..., 3].max()) == 0


# --------------------------------------------------------------------------- #
# Overlay pixels
# --------------------------------------------------------------------------- #


def test_overlay_alpha_is_binary_and_colours_are_label_colours(
    normal_scan: Scan, out_dir: Path
) -> None:
    res = render_scan(normal_scan, out_dir)
    seen: set[tuple[int, int, int]] = set()

    for plane in PLANES:
        arr = _rgba(out_dir, res, f"{plane}_ov")
        alpha = arr[..., 3]
        assert set(np.unique(alpha).tolist()) <= {0, 255}, (
            f"{plane}: overlay alpha must be 0 or 255 (CSS opacity does the blending)"
        )
        opaque = alpha > 0
        assert opaque.any(), f"{plane}: overlay is empty for a non-empty seg"
        # Nearest-neighbour resizing means no interpolated in-between colours.
        for rgb in np.unique(arr[opaque][:, :3], axis=0):
            assert _matches_a_label(rgb, (1, 2, 3, 4)), f"{plane}: stray colour {tuple(rgb)}"
            seen.add(tuple(int(v) for v in rgb))  # type: ignore[arg-type]

    assert len(seen) == 4, f"expected all four label colours across the planes, got {seen}"


def test_overlay_transparent_where_there_is_no_label(normal_scan: Scan, out_dir: Path) -> None:
    res = render_scan(normal_scan, out_dir)
    for plane in PLANES:
        alpha = _rgba(out_dir, res, f"{plane}_ov")[..., 3]
        assert (alpha == 0).any(), f"{plane}: nothing outside the labels is transparent"
        assert float((alpha > 0).mean()) < 0.9


def test_labels_argument_restricts_the_overlay(normal_scan: Scan, out_dir: Path) -> None:
    full = render_scan(normal_scan, out_dir, labels=(1, 2, 3, 4))
    full_counts = {
        p: int((_rgba(out_dir, full, f"{p}_ov")[..., 3] > 0).sum()) for p in PLANES
    }

    only_one_dir = out_dir.parent / "one_label"
    only_one_dir.mkdir(parents=True, exist_ok=True)
    one = render_scan(normal_scan, only_one_dir, labels=(1,))

    seen: set[tuple[int, int, int]] = set()
    for plane in PLANES:
        arr = _rgba(only_one_dir, one, f"{plane}_ov")
        opaque = arr[..., 3] > 0
        assert int(opaque.sum()) < full_counts[plane]
        for rgb in np.unique(arr[opaque][:, :3], axis=0):
            seen.add(tuple(int(v) for v in rgb))  # type: ignore[arg-type]
    assert len(seen) == 1 and _matches_a_label(next(iter(seen)), (1,))


def test_contour_is_a_strict_subset_of_the_fill(normal_scan: Scan, out_dir: Path) -> None:
    res = render_scan(normal_scan, out_dir, contour=True)

    for plane in PLANES:
        fill = _rgba(out_dir, res, f"{plane}_ov")[..., 3] > 0
        contour = _rgba(out_dir, res, f"{plane}_ct")[..., 3] > 0
        assert contour.shape == fill.shape
        assert not (contour & ~fill).any(), f"{plane}: contour paints outside the fill"
        assert 0 < int(contour.sum()) < int(fill.sum()), f"{plane}: contour is not thinner"


def test_float32_label_map_is_rounded_not_truncated(
    float32_seg_scan: Scan, out_dir: Path
) -> None:
    raw = np.asarray(nib.load(str(_seg_path(float32_seg_scan))).dataobj)
    assert raw.dtype == np.float32
    assert not np.array_equal(raw, np.rint(raw)), "fixture should carry sub-integer jitter"

    res = render_scan(float32_seg_scan, out_dir)
    assert res.seg_empty is False
    assert res.fallback is None

    seg = _ras_labels(_seg_path(float32_seg_scan))
    for plane in PLANES:
        axis = PLANE_AXIS[plane]
        assert res.slices[plane] == _expected_slices(seg > 0, axis, seg.shape[axis])

    seen: set[tuple[int, int, int]] = set()
    for plane in PLANES:
        arr = _rgba(out_dir, res, f"{plane}_ov")
        opaque = arr[..., 3] > 0
        for rgb in np.unique(arr[opaque][:, :3], axis=0):
            assert _matches_a_label(rgb, (1, 2, 3, 4))
            seen.add(tuple(int(v) for v in rgb))  # type: ignore[arg-type]
    assert len(seen) == 4


# --------------------------------------------------------------------------- #
# Geometry: anisotropy correction
# --------------------------------------------------------------------------- #


def test_aspect_correction_makes_the_on_screen_pixel_isotropic(
    normal_scan: Scan, out_dir: Path
) -> None:
    """Sagittal is already isotropic (0.5/0.5); coronal and axial need 0.8/0.5."""
    res = render_scan(normal_scan, out_dir, crop_margin_mm=10_000.0)

    seg_file = _seg_path(normal_scan)
    shape = _ras_labels(seg_file).shape
    zooms = _ras_zooms(seg_file)

    ratios: dict[str, float] = {}
    for plane in PLANES:
        axis = PLANE_AXIS[plane]
        a1, a2 = [a for a in range(3) if a != axis]
        expected = (shape[a1] * zooms[a1]) / (shape[a2] * zooms[a2])
        w, h = res.frame_px[plane]
        got = w / h
        ratios[plane] = got
        assert got == pytest.approx(expected, rel=0.05), (
            f"{plane}: frame aspect {got:.3f} != physical aspect {expected:.3f} "
            f"(frame_px={(w, h)}, zooms={zooms})"
        )

    # Sagittal must NOT be stretched by the 1.6 anisotropy factor.
    assert ratios["sagittal"] == pytest.approx(1.0, rel=0.05)
    # Coronal/axial must be: uncorrected they would read 40/80 = 0.5.
    assert ratios["coronal"] == pytest.approx(0.8, rel=0.05)
    assert ratios["axial"] == pytest.approx(0.8, rel=0.05)


def test_long_edge_is_capped(normal_scan: Scan, out_dir: Path) -> None:
    res = render_scan(normal_scan, out_dir, max_frame_px=32, crop_margin_mm=10_000.0)
    for plane in PLANES:
        w, h = res.frame_px[plane]
        assert max(w, h) <= 32, f"{plane} frame {w}x{h} exceeds max_frame_px"


# --------------------------------------------------------------------------- #
# Windowing
# --------------------------------------------------------------------------- #


def test_windowing_uses_nonzero_voxels_only(narrow_band_scan: Scan, out_dir: Path) -> None:
    """Tissue lives in a narrow band far from zero on a mostly-empty grid."""
    res = render_scan(narrow_band_scan, out_dir)

    for plane in PLANES:
        arr = _grey(out_dir, res, f"{plane}_bg").astype(np.int16)
        mid = float(((arr >= 60) & (arr <= 190)).mean())
        assert mid > 0.05, (
            f"{plane}: only {mid:.3%} of pixels are mid-grey — the window looks like it "
            "included the zero background"
        )
        assert int(arr.max()) > 200, f"{plane}: strip never reaches the bright end"
        assert int(arr.min()) < 30, f"{plane}: background is not dark"


def test_render_does_not_crash_on_a_nearly_blank_image(
    scan_dir_factory: Callable[..., Path], out_dir: Path
) -> None:
    """Fewer than 100 nonzero voxels: the window falls back to min/max."""
    d = scan_dir_factory("BLANK01_1")
    shape = NATIVE_SHAPE
    affine = diagonal_affine(shape, NATIVE_ZOOMS)
    seg = np.zeros(shape, dtype=np.float32)
    seg[19:23, 39:43, 39:43] = 1.0
    img = np.zeros(shape, dtype=np.float32)
    img[20, 40, 40] = 900.0
    nib.save(nib.Nifti1Image(seg, affine), str(d / "seg_native.nii.gz"))
    nib.save(nib.Nifti1Image(img, affine), str(d / "image_T1w.nii.gz"))

    res = render_scan(scan_from_dir(d, "native"), out_dir)
    assert res.seg_empty is False
    for plane in PLANES:
        with _strip(out_dir, res, f"{plane}_bg") as bg:
            assert bg.size[0] > 0 and bg.size[1] > 0


# --------------------------------------------------------------------------- #
# Template space and determinism
# --------------------------------------------------------------------------- #


def test_template_space_renders_from_the_warped_pair(
    template_scan: Scan, out_dir: Path
) -> None:
    res = render_scan(template_scan, out_dir)

    assert res.seg_empty is False
    seg = _ras_labels(_seg_path(template_scan))
    for plane in PLANES:
        axis = PLANE_AXIS[plane]
        assert res.slices[plane] == _expected_slices(seg > 0, axis, seg.shape[axis])
        with _strip(out_dir, res, f"{plane}_bg") as bg, _strip(out_dir, res, f"{plane}_ov") as ov:
            assert bg.size == ov.size


def test_result_space_is_the_space_actually_rendered(
    normal_scan: Scan, template_scan: Scan, out_dir: Path, tmp_path: Path
) -> None:
    """``MontageResult.space`` is what build.py records."""
    assert render_scan(normal_scan, out_dir).space == "native"

    t_dir = tmp_path / "template_out"
    t_dir.mkdir()
    assert render_scan(template_scan, t_dir).space == "template"


def test_result_space_reports_native_when_template_degrades(
    scan_dir_factory: Callable[..., Path], out_dir: Path
) -> None:
    """A template request with no warped pair falls back, and says so."""
    path = scan_dir_factory("DEGRADE_1", with_seg_template=False, with_reg_warped=False)
    scan = scan_from_dir(path, "template")

    res = render_scan(scan, out_dir)

    assert res.space == "native"
    assert any("rendered in native space" in w for w in res.warnings)


def test_rendering_is_byte_for_byte_deterministic(normal_scan: Scan, tmp_path: Path) -> None:
    a_dir, b_dir = tmp_path / "run_a", tmp_path / "run_b"
    a_dir.mkdir()
    b_dir.mkdir()

    a = render_scan(normal_scan, a_dir)
    b = render_scan(normal_scan, b_dir)

    assert a.slices == b.slices
    assert a.frame_px == b.frame_px
    assert a.files == b.files
    for key, name in a.files.items():
        assert (a_dir / name).read_bytes() == (b_dir / name).read_bytes(), f"{key} differs"
