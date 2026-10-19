"""Montage rendering for Tongue-QC."""

from __future__ import annotations

import gzip
from dataclasses import dataclass, field
from pathlib import Path
from typing import Sequence

import nibabel as nib
import numpy as np
from PIL import Image
from scipy import ndimage

from .discovery import Scan

LABEL_COLORS: dict[int, tuple[float, float, float]] = {
    1: (1.00, 0.25, 0.25),   # vertical              red
    2: (0.20, 0.55, 1.00),   # superior longitudinal blue
    3: (0.20, 0.80, 0.30),   # inferior longitudinal green
    4: (1.00, 0.75, 0.10),   # genioglossus          amber
}
LABEL_NAMES: dict[int, str] = {
    1: "vertical",
    2: "sup. longitudinal",
    3: "inf. longitudinal",
    4: "genioglossus",
}
PLANES: tuple[str, str, str] = ("sagittal", "coronal", "axial")
PLANE_AXIS: dict[str, int] = {"sagittal": 0, "coronal": 1, "axial": 2}
#: Short filename stem per plane: ``images/<scan_id>/{sag,cor,ax}_{bg,ov,ct}.*``.
PLANE_CODE: dict[str, str] = {"sagittal": "sag", "coronal": "cor", "axial": "ax"}

#: Black separator between frames within a strip.
GUTTER_PX = 2
#: Intensity window percentiles, and the asymmetric widening applied to them.
_WIN_LO_PCT = 1.0
_WIN_HI_PCT = 99.0
_WIN_WIDEN_LO = 0.05
_WIN_WIDEN_HI = 0.10
#: Below this many nonzero voxels the percentile window is meaningless.
_WIN_MIN_NONZERO = 100
#: Last-resort window when a frame carries no usable (finite) intensity at all.
_WIN_FALLBACK = (0.0, 1.0)
#: Chunk size (voxels along RAS axis 0) used when rounding a label volume.
_ROUND_CHUNK = 16
#: Slab size (voxels along the slowest file axis) used when streaming a .nii.gz.
_READ_CHUNK = 16
#: Erosion iterations behind ``*_ct.png``, i.e. the inner contour width in pixels.
_CONTOUR_ERODE_ITERS = 2


@dataclass
class MontageResult:
    """Everything ``build.py`` needs to describe one rendered scan."""

    files: dict[str, str]
    slices: dict[str, list[int]]
    frame_px: dict[str, tuple[int, int]]
    n_frames: int
    seg_empty: bool
    fallback: str | None
    #: The space actually rendered (``"native"`` or ``"template"``), not the one requested.
    space: str = "native"
    warnings: list[str] = field(default_factory=list)


# ---------------------------------------------------------------------------
# NIfTI access: RAS-canonical, zero-copy where possible
# ---------------------------------------------------------------------------
def _ras_geom(img: nib.Nifti1Image) -> tuple[tuple[int, int, int], tuple[float, float, float]]:
    """RAS-canonical shape and zooms, read from the header alone (no data I/O)."""
    ornt = nib.orientations.io_orientation(img.affine)
    perm = np.argsort(ornt[:, 0].astype(int))  # RAS axis j <- file axis perm[j]
    shape = img.shape[:3]
    zooms = img.header.get_zooms()[:3]
    return (
        (int(shape[perm[0]]), int(shape[perm[1]]), int(shape[perm[2]])),
        (float(zooms[perm[0]]), float(zooms[perm[1]]), float(zooms[perm[2]])),
    )


def _load_array(img: nib.Nifti1Image) -> np.ndarray:
    """The whole volume as one array, read with a bounded transient."""
    proxy = img.dataobj
    try:
        filename = img.file_map["image"].filename
        shape = tuple(int(s) for s in img.shape)
        dtype = np.dtype(proxy.dtype)
        offset = int(proxy.offset)
        usable = (
            isinstance(filename, str)
            and filename.endswith(".gz")
            and len(shape) == 3
            and float(proxy.slope) == 1.0
            and float(proxy.inter) == 0.0
            and getattr(proxy, "order", "F") == "F"
        )
    except Exception:  # pragma: no cover - defensive: unknown proxy flavour
        usable = False
    if not usable:
        return np.asanyarray(proxy)

    # NIfTI data is Fortran-ordered, so a contiguous byte run is a slab along the last axis.
    plane_items = shape[0] * shape[1]
    out = np.empty(shape, dtype=dtype, order="F")
    with gzip.open(filename, "rb") as fh:
        remaining = offset
        while remaining > 0:  # skip the header without buffering it
            remaining -= len(fh.read(min(remaining, 1 << 20)))
        for k0 in range(0, shape[2], _READ_CHUNK):
            k = min(_READ_CHUNK, shape[2] - k0)
            buf = fh.read(k * plane_items * dtype.itemsize)
            if len(buf) != k * plane_items * dtype.itemsize:  # truncated file
                del buf, out
                return np.asanyarray(proxy)
            out[:, :, k0 : k0 + k] = np.frombuffer(buf, dtype=dtype).reshape(
                (shape[0], shape[1], k), order="F"
            )
            del buf
    return out


def _ras_view(img: nib.Nifti1Image) -> np.ndarray:
    """Load the volume once and return an RAS-canonical *view* of it."""
    ornt = nib.orientations.io_orientation(img.affine)
    return nib.orientations.apply_orientation(_load_array(img), ornt)


def _rounded_mask(vol: np.ndarray) -> np.ndarray:
    """``np.rint(vol) > 0`` as a bool array, rounded in chunks."""
    mask = np.empty(vol.shape, dtype=bool)
    for i in range(0, vol.shape[0], _ROUND_CHUNK):
        sub = vol[i : i + _ROUND_CHUNK]
        mask[i : i + _ROUND_CHUNK] = np.rint(sub) > 0
    return mask


def _bbox(mask: np.ndarray) -> list[tuple[int, int]] | None:
    """Inclusive per-axis ``[lo, hi]`` bounding box of a bool volume, or None."""
    if not mask.any():
        return None
    out: list[tuple[int, int]] = []
    for axis in range(3):
        proj = mask.any(axis=tuple(a for a in range(3) if a != axis))
        idx = np.flatnonzero(proj)
        out.append((int(idx[0]), int(idx[-1])))
    return out


# ---------------------------------------------------------------------------
# Slice / crop planning
# ---------------------------------------------------------------------------
def _slice_indices(lo: int, hi: int, dim: int, n_frames: int, pad_frac: float) -> list[int]:
    """Evenly spaced slice indices spanning a padded bbox range, endpoints included."""
    span = hi - lo
    p = int(round(pad_frac * span))
    a, b = max(lo - p, 0), min(hi + p, dim - 1)
    return [int(round(v)) for v in np.linspace(a, b, n_frames)]


def _in_plane_crop(
    bbox: list[tuple[int, int]],
    axis: int,
    shape: tuple[int, int, int],
    zooms: tuple[float, float, float],
    margin_mm: float,
) -> tuple[slice, slice, slice]:
    """Crop slices for the two axes that are *not* the plane axis."""
    sl: list[slice] = [slice(None), slice(None), slice(None)]
    for other in range(3):
        if other == axis:
            continue
        margin = int(round(margin_mm / max(zooms[other], 1e-6)))
        lo = max(bbox[other][0] - margin, 0)
        hi = min(bbox[other][1] + margin, shape[other] - 1)
        sl[other] = slice(lo, hi + 1)
    return (sl[0], sl[1], sl[2])


def _frames(
    vol: np.ndarray,
    axis: int,
    indices: Sequence[int],
    crop: tuple[slice, slice, slice],
) -> list[np.ndarray]:
    """Extract and orient the display frames (origin at bottom-left) as contiguous copies."""
    sl = list(crop)
    out: list[np.ndarray] = []
    for k in indices:
        sl[axis] = slice(k, k + 1)
        plane = np.squeeze(vol[tuple(sl)], axis=axis)
        out.append(np.ascontiguousarray(plane.T[::-1]))
    return out


# ---------------------------------------------------------------------------
# Intensity windowing
# ---------------------------------------------------------------------------
def _window(frames: Sequence[np.ndarray]) -> tuple[float, float, int]:
    """Return ``(lo, hi, n_nonfinite)``: a percentile window over nonzero, finite voxels."""
    stack = np.concatenate([f.ravel() for f in frames]).astype(np.float64, copy=False)
    finite = np.isfinite(stack)
    n_nonfinite = int(stack.size) - int(finite.sum())
    good = stack if n_nonfinite == 0 else stack[finite]
    nz = good[good != 0]

    lo = hi = float("nan")
    if nz.size >= _WIN_MIN_NONZERO:
        lo, hi = (float(v) for v in np.nanpercentile(nz, [_WIN_LO_PCT, _WIN_HI_PCT]))
        rng = hi - lo
        lo -= _WIN_WIDEN_LO * rng
        hi += _WIN_WIDEN_HI * rng
    elif good.size:
        lo, hi = float(good.min()), float(good.max())

    if not (np.isfinite(lo) and np.isfinite(hi)):
        lo, hi = _WIN_FALLBACK
    if hi <= lo:
        hi = lo + 1.0
    return lo, hi, n_nonfinite


# ---------------------------------------------------------------------------
# Frame sizing (anisotropy correction) and strip assembly
# ---------------------------------------------------------------------------
def _frame_size(
    n_rows: int, n_cols: int, z_row: float, z_col: float, max_frame_px: int
) -> tuple[int, int]:
    """Displayed ``(w, h)`` for a frame whose voxel is ``z_col`` x ``z_row`` mm."""
    px_per_mm = 1.0 / max(min(z_row, z_col), 1e-6)
    w = n_cols * z_col * px_per_mm
    h = n_rows * z_row * px_per_mm
    longest = max(w, h)
    if longest > max_frame_px:
        scale = max_frame_px / longest
        w *= scale
        h *= scale
    return max(1, int(round(w))), max(1, int(round(h)))


def _strip_width(n_frames: int, frame_w: int) -> int:
    return n_frames * frame_w + max(0, n_frames - 1) * GUTTER_PX


def _paste_strip(tiles: Sequence[Image.Image], size: tuple[int, int], mode: str) -> Image.Image:
    """Butt frames together left-to-right with a black/transparent gutter."""
    fw, fh = size
    bg = 0 if mode == "L" else (0, 0, 0, 0)
    strip = Image.new(mode, (_strip_width(len(tiles), fw), fh), bg)
    for i, tile in enumerate(tiles):
        strip.paste(tile, (i * (fw + GUTTER_PX), 0))
    return strip


def _bg_tile(frame: np.ndarray, lo: float, hi: float, size: tuple[int, int]) -> Image.Image:
    arr = frame.astype(np.float32)
    if not np.isfinite(arr).all():
        # A NaN/inf voxel becomes one dark (or saturated) pixel.
        arr = np.nan_to_num(arr, nan=lo, posinf=hi, neginf=lo)
    norm = (np.clip(arr, lo, hi) - lo) / (hi - lo)
    tile = Image.fromarray((norm * 255.0 + 0.5).astype(np.uint8))
    return tile.resize(size, Image.Resampling.BILINEAR)


def _label_tiles(
    frame: np.ndarray, labels: Sequence[int], size: tuple[int, int]
) -> dict[int, np.ndarray]:
    """Per-label boolean masks resampled to display size with nearest neighbour."""
    out: dict[int, np.ndarray] = {}
    for lab in labels:
        m = frame == lab
        if not m.any():
            continue
        resized = Image.fromarray(m.view(np.uint8) * 255).resize(
            size, Image.Resampling.NEAREST
        )
        arr = np.asarray(resized) > 0
        if arr.any():
            out[lab] = arr
    return out


def _rgba_from_masks(
    masks: dict[int, np.ndarray], size: tuple[int, int], edges: bool
) -> Image.Image:
    """Flat label colours at alpha 255 (CSS opacity does the blending)."""
    fw, fh = size
    rgba = np.zeros((fh, fw, 4), dtype=np.uint8)
    for lab, mask in masks.items():
        colour = LABEL_COLORS[lab]
        if edges:
            eroded = ndimage.binary_erosion(mask, iterations=_CONTOUR_ERODE_ITERS)
            drawn = mask ^ eroded
        else:
            drawn = mask
        if not drawn.any():
            continue
        rgba[drawn, 0] = int(round(colour[0] * 255))
        rgba[drawn, 1] = int(round(colour[1] * 255))
        rgba[drawn, 2] = int(round(colour[2] * 255))
        rgba[drawn, 3] = 255
    return Image.fromarray(rgba)


# ---------------------------------------------------------------------------
# Input resolution
# ---------------------------------------------------------------------------
def _resolve_pair(scan: Scan, warnings: list[str]) -> tuple[Path, Path, str]:
    """Pick the (image, seg) pair for ``scan.space``, degrading if it is absent."""
    pairs: dict[str, tuple[Path | None, Path | None]] = {
        "native": (getattr(scan, "image", None), getattr(scan, "seg_native", None)),
        "template": (getattr(scan, "reg_warped", None), getattr(scan, "seg_template", None)),
    }
    want = getattr(scan, "space", "native") or "native"
    order = [want] + [s for s in ("native", "template") if s != want]
    for space in order:
        image, seg = pairs[space]
        if image is not None and seg is not None:
            if space != want:
                warnings.append(f"{want} pair incomplete; rendered in {space} space")
            return Path(image), Path(seg), space
    raise ValueError(
        f"{getattr(scan, 'scan_id', '?')}: no complete (image, seg) pair "
        f"for space {want!r}"
    )


def _raw_seg_path(scan: Scan, space: str) -> Path | None:
    """The pipeline's pre-post-processing mask, used only as a bbox fallback."""
    work = Path(getattr(scan, "path", ".")) / "work"
    preferred = work / f"seg_{space}_raw.nii.gz"
    if preferred.is_file():
        return preferred
    candidates = sorted(work.glob("seg_*_raw.nii.gz")) if work.is_dir() else []
    return candidates[0] if candidates else None


def _centre_bbox(shape: tuple[int, int, int]) -> list[tuple[int, int]]:
    """Middle 50 % of every axis: the last-resort framing for an empty seg."""
    return [(int(0.25 * n), max(int(0.25 * n), int(0.75 * n) - 1)) for n in shape]


# ---------------------------------------------------------------------------
# Public entry point
# ---------------------------------------------------------------------------
def render_scan(
    scan: Scan,
    out_dir: Path,
    *,
    n_frames: int = 6,
    # 0.0 puts the outer two frames on the seg's first and last slice.
    pad_frac: float = 0.0,
    crop_margin_mm: float = 20.0,
    max_frame_px: int = 192,
    labels: Sequence[int] = (1, 2, 3, 4),
    contour: bool = True,
    jpeg_quality: int = 72,
) -> MontageResult:
    """Render one scan's montage strips into ``out_dir``."""
    warnings: list[str] = []
    n_frames = max(1, int(n_frames))
    draw_labels = [int(v) for v in labels if int(v) in LABEL_COLORS]
    dropped = sorted({int(v) for v in labels} - set(draw_labels))
    if dropped:
        warnings.append(f"ignored labels without a colour: {dropped}")

    image_path, seg_path, space = _resolve_pair(scan, warnings)
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    image_nii = nib.load(str(image_path))
    seg_nii = nib.load(str(seg_path))
    img_shape, img_zooms = _ras_geom(image_nii)
    seg_shape, seg_zooms = _ras_geom(seg_nii)
    if img_shape != seg_shape:
        raise ValueError(
            f"{getattr(scan, 'scan_id', '?')}: image/segmentation grid mismatch — "
            f"{image_path.name} is {img_shape} in RAS, {seg_path.name} is {seg_shape}; "
            f"resampling is out of scope"
        )
    if not np.allclose(img_zooms, seg_zooms, atol=1e-3):
        warnings.append(f"zoom mismatch: image {img_zooms} vs seg {seg_zooms}")
    shape, zooms = seg_shape, seg_zooms

    # --- segmentation: bbox, then the frames that will actually be drawn -----
    seg_vol = _ras_view(seg_nii)
    seg_mask = _rounded_mask(seg_vol)
    bbox = _bbox(seg_mask)
    del seg_mask
    seg_empty = bbox is None
    fallback: str | None = None

    if bbox is None:
        raw_path = _raw_seg_path(scan, space)
        if raw_path is not None:
            raw_nii = nib.load(str(raw_path))
            if _ras_geom(raw_nii)[0] == shape:
                raw_vol = _ras_view(raw_nii)
                raw_mask = _rounded_mask(raw_vol)
                bbox = _bbox(raw_mask)
                del raw_mask, raw_vol, raw_nii
                if bbox is not None:
                    fallback = "raw"
                    warnings.append(f"empty segmentation; framed from {raw_path.name}")
            else:
                warnings.append(f"{raw_path.name} is on a different grid; ignored")
    if bbox is None:
        fallback = "centre"
        bbox = _centre_bbox(shape)
        warnings.append("empty segmentation; framed on the volume centre")

    plans: dict[str, tuple[list[int], tuple[slice, slice, slice]]] = {}
    for plane in PLANES:
        axis = PLANE_AXIS[plane]
        lo, hi = bbox[axis]
        indices = _slice_indices(lo, hi, shape[axis], n_frames, pad_frac)
        if fallback == "centre":
            crop = (slice(None), slice(None), slice(None))
        else:
            crop = _in_plane_crop(bbox, axis, shape, zooms, crop_margin_mm)
        plans[plane] = (indices, crop)

    seg_frames: dict[str, list[np.ndarray]] = {
        plane: [
            np.rint(f).astype(np.int16)
            for f in _frames(seg_vol, PLANE_AXIS[plane], plans[plane][0], plans[plane][1])
        ]
        for plane in PLANES
    }
    del seg_vol, seg_nii

    # --- image: same planning, then release the volume before drawing --------
    image_vol = _ras_view(image_nii)
    img_frames: dict[str, list[np.ndarray]] = {
        plane: _frames(image_vol, PLANE_AXIS[plane], plans[plane][0], plans[plane][1])
        for plane in PLANES
    }
    del image_vol, image_nii

    files: dict[str, str] = {}
    slices: dict[str, list[int]] = {}
    frame_px: dict[str, tuple[int, int]] = {}

    for plane in PLANES:
        axis = PLANE_AXIS[plane]
        code = PLANE_CODE[plane]
        indices, _ = plans[plane]
        bg_frames = img_frames.pop(plane)
        ov_frames = seg_frames.pop(plane)
        n_rows, n_cols = bg_frames[0].shape

        # Rows/cols carry the two non-plane axes; see ``_frames``.
        others = [a for a in range(3) if a != axis]
        z_col, z_row = zooms[others[0]], zooms[others[1]]
        size = _frame_size(n_rows, n_cols, z_row, z_col, max_frame_px)

        lo, hi, n_nonfinite = _window(bg_frames)
        if n_nonfinite:
            warnings.append(
                f"{plane}: {n_nonfinite} non-finite voxel(s) in the drawn frames; "
                "excluded from the intensity window"
            )
        bg = _paste_strip([_bg_tile(f, lo, hi, size) for f in bg_frames], size, "L")
        bg_name = f"{code}_bg.jpg"
        bg.save(
            out_dir / bg_name,
            format="JPEG",
            quality=int(jpeg_quality),
            optimize=True,
            progressive=True,
        )

        masks = [_label_tiles(f, draw_labels, size) for f in ov_frames]
        ov = _paste_strip([_rgba_from_masks(m, size, edges=False) for m in masks], size, "RGBA")
        ov_name = f"{code}_ov.png"
        ov.save(out_dir / ov_name, format="PNG", optimize=True)

        files[f"{plane}_bg"] = bg_name
        files[f"{plane}_ov"] = ov_name
        if contour:
            ct = _paste_strip([_rgba_from_masks(m, size, edges=True) for m in masks], size, "RGBA")
            ct_name = f"{code}_ct.png"
            ct.save(out_dir / ct_name, format="PNG", optimize=True)
            files[f"{plane}_ct"] = ct_name

        slices[plane] = list(indices)
        frame_px[plane] = size

    return MontageResult(
        files=files,
        slices=slices,
        frame_px=frame_px,
        n_frames=n_frames,
        seg_empty=seg_empty,
        fallback=fallback,
        space=space,
        warnings=warnings,
    )
