"""Pure-numpy shape descriptors and midline geometry (no Nipype, no I/O)."""
from __future__ import annotations

import numpy as np


# ---------------------------------------------------------------------------
# Coordinates
# ---------------------------------------------------------------------------

def voxel_world_coords(mask: np.ndarray, affine: np.ndarray) -> np.ndarray:
    """World (mm) coordinates of every True voxel in ``mask``. Shape (N, 3)."""
    idx = np.argwhere(mask)
    if idx.size == 0:
        return np.zeros((0, 3))
    homog = np.hstack([idx, np.ones((idx.shape[0], 1))])
    return (affine @ homog.T).T[:, :3]


def voxel_volume_mm3(affine: np.ndarray) -> float:
    return float(abs(np.linalg.det(affine[:3, :3])))


# ---------------------------------------------------------------------------
# Inertia / principal axes
# ---------------------------------------------------------------------------

def inertia_descriptors(coords: np.ndarray) -> dict:
    """Centroid, principal-axis lengths, elongation and flatness of a point cloud."""
    n = coords.shape[0]
    empty = {
        "centroid_x_mm": np.nan, "centroid_y_mm": np.nan, "centroid_z_mm": np.nan,
        "pc1_length_mm": np.nan, "pc2_length_mm": np.nan, "pc3_length_mm": np.nan,
        "elongation": np.nan, "flatness": np.nan,
    }
    if n < 4:
        return empty
    centroid = coords.mean(axis=0)
    centred = coords - centroid
    cov = (centred.T @ centred) / n
    evals = np.linalg.eigvalsh(cov)[::-1]          # descending
    evals = np.clip(evals, 0.0, None)
    lengths = 2.0 * np.sqrt(5.0 * evals)
    major = evals[0]
    return {
        "centroid_x_mm": float(centroid[0]),
        "centroid_y_mm": float(centroid[1]),
        "centroid_z_mm": float(centroid[2]),
        "pc1_length_mm": float(lengths[0]),
        "pc2_length_mm": float(lengths[1]),
        "pc3_length_mm": float(lengths[2]),
        "elongation": float(np.sqrt(evals[1] / major)) if major > 0 else np.nan,
        "flatness": float(np.sqrt(evals[2] / major)) if major > 0 else np.nan,
    }


# ---------------------------------------------------------------------------
# Surface
# ---------------------------------------------------------------------------

# Gaussian pre-smoothing (voxels) before marching cubes.
SURFACE_SMOOTH_SIGMA = 1.0


def surface_area_mm2(mask: np.ndarray, zooms) -> float:
    """Marching-cubes surface area of a binary mask, in mm^2."""
    from skimage import measure

    try:
        verts, faces = mesh_from_mask(mask, zooms)
    except (ValueError, RuntimeError):
        return float("nan")
    return float(measure.mesh_surface_area(verts, faces))


def sphericity(volume_mm3: float, area_mm2: float) -> float:
    """Sphere-equivalent surface area / actual area (1.0 for a sphere)."""
    if not np.isfinite(volume_mm3) or not np.isfinite(area_mm2) or area_mm2 <= 0:
        return float("nan")
    return float((np.pi ** (1.0 / 3.0)) * ((6.0 * volume_mm3) ** (2.0 / 3.0)) / area_mm2)


def mesh_from_mask(mask: np.ndarray, zooms, sigma: float = SURFACE_SMOOTH_SIGMA):
    """(verts, faces) marching-cubes mesh of a binary mask, in mm."""
    from scipy import ndimage
    from skimage import measure

    # Pad so objects touching the array edge still produce a closed surface.
    padded = np.pad(mask.astype(float), 3, mode="constant")
    if sigma and sigma > 0:
        padded = ndimage.gaussian_filter(padded, sigma)
    verts, faces, _, _ = measure.marching_cubes(
        padded, level=0.5, spacing=tuple(float(z) for z in zooms)
    )
    return verts, faces


def write_stl(path, verts: np.ndarray, faces: np.ndarray) -> str:
    """Minimal binary STL writer."""
    import struct

    tri = verts[faces]                                   # (F, 3, 3)
    n = np.cross(tri[:, 1] - tri[:, 0], tri[:, 2] - tri[:, 0])
    norms = np.linalg.norm(n, axis=1, keepdims=True)
    n = np.divide(n, norms, out=np.zeros_like(n), where=norms > 0)
    with open(path, "wb") as f:
        f.write(b"\0" * 80)
        f.write(struct.pack("<I", tri.shape[0]))
        for i in range(tri.shape[0]):
            f.write(struct.pack("<3f", *n[i]))
            for v in tri[i]:
                f.write(struct.pack("<3f", *v))
            f.write(struct.pack("<H", 0))
    return str(path)


# ---------------------------------------------------------------------------
# Profiles and roughness
# ---------------------------------------------------------------------------

def polynomial_residual_rms(x: np.ndarray, y: np.ndarray, degree: int = 4) -> float:
    """RMS of y after removing a smooth degree-``degree`` trend."""
    good = np.isfinite(x) & np.isfinite(y)
    if good.sum() <= degree + 1:
        return float("nan")
    xx, yy = x[good], y[good]
    # Normalise x to keep the Vandermonde matrix well conditioned.
    span = xx.max() - xx.min()
    xn = (xx - xx.mean()) / (span if span > 0 else 1.0)
    coeffs = np.polyfit(xn, yy, degree)
    return float(np.sqrt(np.mean((yy - np.polyval(coeffs, xn)) ** 2)))


def resample_profile(x: np.ndarray, y: np.ndarray, n: int = 50) -> np.ndarray:
    """Resample y(x) onto ``n`` evenly spaced *normalised* positions in [0, 1]."""
    good = np.isfinite(x) & np.isfinite(y)
    if good.sum() < 2:
        return np.full(n, np.nan)
    xx, yy = x[good], y[good]
    order = np.argsort(xx)
    xx, yy = xx[order], yy[order]
    span = xx.max() - xx.min()
    if span <= 0:
        return np.full(n, np.nan)
    xn = (xx - xx.min()) / span
    return np.interp(np.linspace(0.0, 1.0, n), xn, yy)


def curvature_1d(s_mm: np.ndarray, z_mm: np.ndarray, smooth_sigma: float = 2.0,
                 trim: int | None = None):
    """Return (kappa, mean, max, arc_length); mean and max are over the interior."""
    from scipy import ndimage

    if s_mm.size < 7:
        return np.array([]), float("nan"), float("nan"), float("nan")
    sigma = max(smooth_sigma, 1e-6)
    z_s = ndimage.gaussian_filter1d(z_mm, sigma=sigma, mode="nearest")
    dz = np.gradient(z_s, s_mm)
    ddz = np.gradient(dz, s_mm)
    kappa = np.abs(ddz) / np.power(1.0 + dz ** 2, 1.5)
    length = float(np.sum(np.sqrt(np.diff(s_mm) ** 2 + np.diff(z_s) ** 2)))

    if trim is None:
        trim = int(np.ceil(2.0 * sigma)) + 2
    trim = int(min(trim, max(0, (kappa.size - 3) // 2)))
    interior = kappa[trim:kappa.size - trim] if trim > 0 else kappa
    if interior.size == 0:
        interior = kappa
    return kappa, float(np.mean(interior)), float(np.max(interior)), length


# ---------------------------------------------------------------------------
# Height maps (superior surfaces)
# ---------------------------------------------------------------------------

def superior_height_map(mask: np.ndarray, lr_axis: int, si_axis: int, ap_axis: int,
                        zooms) -> np.ndarray:
    """Superior-most surface height (mm) per (LR, AP) column. NaN where empty."""
    m = np.moveaxis(mask, (lr_axis, ap_axis, si_axis), (0, 1, 2))
    # Highest True index along SI per column.
    return np.where(m.any(2),
                    (m * np.arange(m.shape[2])).max(2) * float(zooms[si_axis]),
                    np.nan)


def resample_grid(grid: np.ndarray, n_rows: int, n_cols: int) -> np.ndarray:
    """Resample a height map onto a fixed normalised grid over its own extent."""
    finite = np.isfinite(grid)
    if not finite.any():
        return np.full((n_rows, n_cols), np.nan)
    rows = np.where(finite.any(axis=1))[0]
    cols = np.where(finite.any(axis=0))[0]
    sub = grid[rows.min():rows.max() + 1, cols.min():cols.max() + 1]
    src_r = np.linspace(0.0, 1.0, sub.shape[0])
    src_c = np.linspace(0.0, 1.0, sub.shape[1])
    dst_r = np.linspace(0.0, 1.0, n_rows)
    dst_c = np.linspace(0.0, 1.0, n_cols)
    # Separable linear interpolation, NaN-tolerant along each axis.
    tmp = np.empty((n_rows, sub.shape[1]))
    for j in range(sub.shape[1]):
        col = sub[:, j]
        good = np.isfinite(col)
        tmp[:, j] = np.interp(dst_r, src_r[good], col[good]) if good.sum() >= 2 else np.nan
    out = np.empty((n_rows, n_cols))
    for i in range(n_rows):
        row = tmp[i, :]
        good = np.isfinite(row)
        out[i, :] = np.interp(dst_c, src_c[good], row[good]) if good.sum() >= 2 else np.nan
    return out


def surface_roughness(grid: np.ndarray, degree: int = 3) -> float:
    """RMS residual of a height map after removing a smooth 2-D polynomial."""
    finite = np.isfinite(grid)
    if finite.sum() < (degree + 1) ** 2 + 1:
        return float("nan")
    n_r, n_c = grid.shape
    rr, cc = np.meshgrid(np.linspace(-1, 1, n_r), np.linspace(-1, 1, n_c), indexing="ij")
    terms = [(rr ** i) * (cc ** j)
             for i in range(degree + 1) for j in range(degree + 1) if i + j <= degree]
    A = np.stack([t[finite].ravel() for t in terms], axis=1)
    y = grid[finite].ravel()
    coeffs, *_ = np.linalg.lstsq(A, y, rcond=None)
    return float(np.sqrt(np.mean((y - A @ coeffs) ** 2)))


def rugosity(grid: np.ndarray, row_spacing_mm: float, col_spacing_mm: float) -> float:
    """True surface area / projected area of a height map. 1.0 = perfectly flat."""
    finite = np.isfinite(grid)
    if finite.sum() < 4:
        return float("nan")
    filled = np.where(finite, grid, np.nan)
    dr, dc = np.gradient(filled, row_spacing_mm, col_spacing_mm)
    local = np.sqrt(1.0 + dr ** 2 + dc ** 2)
    good = np.isfinite(local)
    return float(np.mean(local[good])) if good.any() else float("nan")


# ---------------------------------------------------------------------------
# Midline
# ---------------------------------------------------------------------------

def midline_plane(labels: np.ndarray, affine: np.ndarray, midline_label: int,
                  mode: str = "centroid") -> tuple[np.ndarray, np.ndarray]:
    """Return (point_mm, unit_normal) of the mid-sagittal plane."""
    mask = labels == midline_label
    if not mask.any():
        mask = labels > 0
    coords = voxel_world_coords(mask, affine)
    if coords.shape[0] == 0:
        raise ValueError("cannot define a midline: no labelled voxels")
    point = coords.mean(axis=0)

    if mode == "pca":
        all_coords = voxel_world_coords(labels > 0, affine)
        centred = all_coords - all_coords.mean(axis=0)
        cov = (centred.T @ centred) / centred.shape[0]
        evals, evecs = np.linalg.eigh(cov)
        normal = evecs[:, 0]                     # smallest eigenvalue
        if normal[0] < 0:                        # orient toward Right
            normal = -normal
    else:
        normal = np.array([1.0, 0.0, 0.0])       # RAS world x = Right

    normal = normal / np.linalg.norm(normal)
    return point, normal


def lateralise(labels: np.ndarray, affine: np.ndarray, point: np.ndarray,
               normal: np.ndarray) -> np.ndarray:
    """Split a label map about a plane into left (+100) and right (+200) ids."""
    out = np.zeros_like(labels, dtype=np.int16)
    mask = labels > 0
    signed = (voxel_world_coords(mask, affine) - point) @ normal
    out[mask] = labels[mask] + np.where(signed > 0, 200, 100)
    return out
