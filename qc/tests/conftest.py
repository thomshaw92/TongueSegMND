"""Shared pytest fixtures: synthetic tongue-seg output trees."""

from __future__ import annotations

import json
from pathlib import Path
from typing import TYPE_CHECKING, Callable, Sequence

import nibabel as nib
import numpy as np
import pytest
from PIL import Image

if TYPE_CHECKING:  # pragma: no cover - typing only
    from tongue_qc.discovery import Scan


# --------------------------------------------------------------------------- #
# Constants
# --------------------------------------------------------------------------- #

#: Native-space grid: anisotropic, zoom ratio 1.6.
NATIVE_SHAPE: tuple[int, int, int] = (40, 80, 80)
NATIVE_ZOOMS: tuple[float, float, float] = (0.8, 0.5, 0.5)

#: Template-space grid: isotropic 1 mm, as the pipeline emits.
TEMPLATE_SHAPE: tuple[int, int, int] = (24, 32, 32)
TEMPLATE_ZOOMS: tuple[float, float, float] = (1.0, 1.0, 1.0)

#: Filenames that are never the native source image (the pipeline's own outputs).
NON_SOURCE_NIFTIS: frozenset[str] = frozenset(
    {
        "seg_native.nii.gz",
        "seg_template.nii.gz",
        "reg_Warped.nii.gz",
        "image_cropped.nii.gz",
        "label_cropped.nii.gz",
    }
)

#: A synthetic metrics row: label volumes sum to the tongue volume, fractions to 1.
_METRICS_HEADER = (
    "participant,session,modality,input,space,voxel_volume_mm3,tongue_volume_mm3,"
    "vertical_volume_mm3,vertical_fraction,superior_longitudinal_volume_mm3,"
    "superior_longitudinal_fraction,inferior_longitudinal_volume_mm3,"
    "inferior_longitudinal_fraction,genioglossus_volume_mm3,genioglossus_fraction,"
    "dorsum_mean_curvature_1pmm,dorsum_max_curvature_1pmm,dorsum_length_mm"
)
_METRICS_ROW = (
    "NA,NA,T1w,{image},native,0.2,60000.0,21000.0,0.35,18000.0,0.30,"
    "9000.0,0.15,12000.0,0.20,0.025,0.05,70.0"
)

_POSTPROCESS = {
    "input": "/out/seg_template.nii.gz",
    "config": {
        "enable_cc": True,
        "open_iter": 1,
        "enable_sl_trim": True,
        "sl_med_mm": 15.0,
        "sl_kmad": 3.0,
        "sl_min_jut_mm": 3.0,
        "sl_trim_keep_mm": 1.0,
        "mid_halfwidth_mm": 4.0,
        "curv_smooth_mm": 4.0,
    },
    "connected_components": {
        "removed_voxels": 30000,
        "n_components": 10,
        "n_components_kept": 1,
    },
    "sl_trim": {"jut_columns": 0, "removed_voxels": 0, "flagged": []},
}


# --------------------------------------------------------------------------- #
# Volume construction
# --------------------------------------------------------------------------- #


def diagonal_affine(
    shape: Sequence[int],
    zooms: Sequence[float],
    *,
    oblique_deg: float = 0.0,
) -> np.ndarray:
    """A world-centred RAS affine, pure-diagonal unless ``oblique_deg`` is given."""
    zoom = np.asarray(zooms, dtype=np.float64)
    rot = np.eye(3)
    if oblique_deg:
        th = np.deg2rad(oblique_deg)
        c, s = np.cos(th), np.sin(th)
        rot = np.array([[1.0, 0.0, 0.0], [0.0, c, -s], [0.0, s, c]])
    affine = np.eye(4)
    affine[:3, :3] = rot @ np.diag(zoom)
    affine[:3, 3] = -affine[:3, :3] @ (np.asarray(shape, dtype=np.float64) / 2.0)
    return affine


def _ellipsoid(
    shape: Sequence[int],
    centre: Sequence[float],
    radii: Sequence[float],
) -> np.ndarray:
    idx = np.indices(tuple(shape), dtype=np.float64)
    r2 = sum(((idx[a] - centre[a]) / radii[a]) ** 2 for a in range(3))
    return np.asarray(r2 <= 1.0)


def _normalised_grid(shape: Sequence[int]) -> list[np.ndarray]:
    idx = np.indices(tuple(shape), dtype=np.float64)
    return [idx[a] / max(shape[a] - 1, 1) for a in range(3)]


def make_seg(
    shape: Sequence[int] = NATIVE_SHAPE,
    kind: str = "normal",
    *,
    dtype: np.dtype | type = np.float32,
    jitter: bool = False,
) -> np.ndarray:
    """A blob-shaped four-label tongue segmentation."""
    shape = tuple(int(s) for s in shape)
    seg = np.zeros(shape, dtype=np.float64)
    if kind == "empty":
        return seg.astype(dtype)

    if kind == "normal":
        centre = (0.50 * shape[0], 0.50 * shape[1], 0.45 * shape[2])
        radii = (0.30 * shape[0], 0.28 * shape[1], 0.22 * shape[2])
    elif kind == "edge":
        centre = (0.20 * shape[0], 0.50 * shape[1], 0.80 * shape[2])
        radii = (0.35 * shape[0], 0.28 * shape[1], 0.30 * shape[2])
    else:  # pragma: no cover - guarded by callers
        raise ValueError(f"unknown seg kind {kind!r}")

    mask = _ellipsoid(shape, centre, radii)
    z = np.indices(shape, dtype=np.float64)[2]
    lo, hi = float(z[mask].min()), float(z[mask].max())
    t = (z - lo) / max(hi - lo, 1.0)
    # Inferior -> superior: genioglossus, inferior long., vertical, superior long.
    for lab, (t0, t1) in zip((4, 3, 1, 2), ((0.0, 0.25), (0.25, 0.5), (0.5, 0.75), (0.75, 1.01))):
        seg[mask & (t >= t0) & (t < t1)] = float(lab)

    if jitter:
        rng = np.random.default_rng(7)
        nz = seg > 0
        seg[nz] += rng.uniform(-4e-3, 4e-3, size=int(nz.sum()))
    return seg.astype(dtype)


def make_image(
    shape: Sequence[int] = NATIVE_SHAPE,
    *,
    intensity: tuple[float, float] = (200.0, 3000.0),
    noise: float = 0.02,
    seed: int = 0,
) -> np.ndarray:
    """A head-shaped ellipsoid of tissue on an exactly-zero background."""
    shape = tuple(int(s) for s in shape)
    head = _ellipsoid(shape, tuple(0.5 * s for s in shape), tuple(0.42 * s for s in shape))
    n0, n1, n2 = _normalised_grid(shape)
    lo, hi = intensity
    vals = lo + (hi - lo) * (n0 + n1 + n2) / 3.0
    if noise:
        rng = np.random.default_rng(seed)
        vals = vals * (1.0 + noise * rng.standard_normal(shape))
    return np.where(head, vals, 0.0).astype(np.float32)


def _save(data: np.ndarray, affine: np.ndarray, path: Path) -> None:
    # Nifti1Image derives the header zooms from the affine's column norms.
    nib.save(nib.Nifti1Image(data, affine), str(path))


# --------------------------------------------------------------------------- #
# Scan-directory construction
# --------------------------------------------------------------------------- #


def build_scan_dir(
    path: Path,
    *,
    shape: Sequence[int] = NATIVE_SHAPE,
    zooms: Sequence[float] = NATIVE_ZOOMS,
    seg_kind: str = "normal",
    seg_dtype: np.dtype | type = np.float32,
    seg_jitter: bool = False,
    intensity: tuple[float, float] = (200.0, 3000.0),
    source_names: Sequence[str] = ("image_T1w.nii.gz",),
    with_seg_native: bool = True,
    with_seg_template: bool = True,
    with_reg_warped: bool = True,
    with_metrics: bool = True,
    with_postprocess: bool = True,
    with_qc: bool = True,
    with_work: bool = False,
    template_seg_kind: str = "normal",
) -> Path:
    """Write one synthetic tongue-seg output directory and return its path."""
    path.mkdir(parents=True, exist_ok=True)
    affine = diagonal_affine(shape, zooms)

    seg = make_seg(shape, seg_kind, dtype=seg_dtype, jitter=seg_jitter)
    image = make_image(shape, intensity=intensity)
    for name in source_names:
        _save(image, affine, path / name)
    if with_seg_native:
        _save(seg, affine, path / "seg_native.nii.gz")

    if with_seg_template or with_reg_warped:
        t_affine = diagonal_affine(TEMPLATE_SHAPE, TEMPLATE_ZOOMS, oblique_deg=5.0)
        if with_seg_template:
            t_seg = make_seg(TEMPLATE_SHAPE, template_seg_kind, dtype=np.uint8)
            _save(t_seg, t_affine, path / "seg_template.nii.gz")
        if with_reg_warped:
            _save(make_image(TEMPLATE_SHAPE, intensity=intensity), t_affine,
                  path / "reg_Warped.nii.gz")

    if with_metrics:
        row = _METRICS_ROW.format(image=source_names[0] if source_names else "NA")
        (path / "tongue_metrics.csv").write_text(f"{_METRICS_HEADER}\n{row}\n", encoding="utf-8")
    if with_postprocess:
        (path / "postprocess.json").write_text(json.dumps(_POSTPROCESS, indent=2), encoding="utf-8")
    if with_qc:
        Image.new("RGBA", (60, 40), (24, 24, 28, 255)).save(path / "qc.png")

    if with_work:
        work = path / "work"
        work.mkdir(exist_ok=True)
        raw = make_seg(shape, "normal" if seg_kind == "empty" else seg_kind, dtype=np.uint8)
        _save(raw, affine, work / "seg_native_raw.nii.gz")
        _save(image, affine, work / "cropped.nii.gz")
        (work / "crop.json").write_text(json.dumps({"x": [0, 1], "y": [0, 1], "z": [0, 1]}),
                                        encoding="utf-8")
    return path


def find_source_image(path: Path) -> Path | None:
    """Mirror of discovery's source-image rule, for fixture use only."""
    cands = [p for p in sorted(path.glob("*.nii.gz")) if p.name not in NON_SOURCE_NIFTIS]
    return cands[0] if len(cands) == 1 else None


def scan_from_dir(path: Path, space: str = "native") -> "Scan":
    """Build a :class:`~tongue_qc.discovery.Scan` for ``path`` without discovery."""
    from tongue_qc.discovery import Scan

    scan_id = path.name
    sub, _, tail = scan_id.partition("_")
    image = find_source_image(path)
    if image is None:  # pragma: no cover - fixture misuse
        raise AssertionError(f"{path} has no unambiguous source image")

    def opt(name: str) -> Path | None:
        p = path / name
        return p if p.exists() else None

    return Scan(
        scan_id=scan_id,
        path=path,
        sub=sub or scan_id,
        ses=f"ses-{int(tail):02d}" if tail.isdigit() else "ses-01",
        image=image,
        seg_native=opt("seg_native.nii.gz"),
        seg_template=opt("seg_template.nii.gz"),
        reg_warped=opt("reg_Warped.nii.gz"),
        metrics_csv=opt("tongue_metrics.csv"),
        postprocess_json=opt("postprocess.json"),
        pipeline_qc=opt("qc.png"),
        space=space,
    )


# --------------------------------------------------------------------------- #
# Fixtures: factories
# --------------------------------------------------------------------------- #


@pytest.fixture
def scan_dir_factory(tmp_path: Path) -> Callable[..., Path]:
    """``factory(name, **build_scan_dir_kwargs) -> Path`` rooted at ``tmp_path``."""

    def factory(name: str = "ABC123_1", *, parent: Path | None = None, **kw: object) -> Path:
        base = parent if parent is not None else tmp_path
        return build_scan_dir(base / name, **kw)  # type: ignore[arg-type]

    return factory


@pytest.fixture
def fake_scans() -> Callable[[int], list["Scan"]]:
    """Cheap file-less ``Scan`` records, for exercising ``select``."""

    def factory(n: int = 40, prefix: str = "SCAN") -> list["Scan"]:
        from tongue_qc.discovery import Scan

        return [
            Scan(
                scan_id=f"{prefix}{i:03d}_1",
                path=Path(f"/nowhere/{prefix}{i:03d}_1"),
                sub=f"{prefix}{i:03d}",
                ses="ses-01",
                image=Path(f"/nowhere/{prefix}{i:03d}_1/image_T1w.nii.gz"),
                seg_native=None,
                seg_template=None,
                reg_warped=None,
                metrics_csv=None,
                postprocess_json=None,
                pipeline_qc=None,
                space="native",
            )
            for i in range(n)
        ]

    return factory


@pytest.fixture
def out_dir(tmp_path: Path) -> Path:
    """A clean, existing output directory for montage renders."""
    d = tmp_path / "montage_out"
    d.mkdir(parents=True, exist_ok=True)
    return d


# --------------------------------------------------------------------------- #
# Fixtures: single scan variants
# --------------------------------------------------------------------------- #


@pytest.fixture
def normal_scan_dir(scan_dir_factory: Callable[..., Path]) -> Path:
    return scan_dir_factory("ABC123_1", with_work=True)


@pytest.fixture
def normal_scan(normal_scan_dir: Path) -> "Scan":
    return scan_from_dir(normal_scan_dir, "native")


@pytest.fixture
def template_scan(scan_dir_factory: Callable[..., Path]) -> "Scan":
    return scan_from_dir(scan_dir_factory("TPL001_1"), "template")


@pytest.fixture
def empty_seg_scan(scan_dir_factory: Callable[..., Path]) -> "Scan":
    """Empty seg **and no ``work/`` raw seg**, so the fallback must be ``centre``."""
    return scan_from_dir(scan_dir_factory("EMPTY01_1", seg_kind="empty", with_work=False), "native")


@pytest.fixture
def edge_seg_scan(scan_dir_factory: Callable[..., Path]) -> "Scan":
    """Seg clipped by the axis-0 low face and the axis-2 high face."""
    return scan_from_dir(scan_dir_factory("EDGE001_1", seg_kind="edge"), "native")


@pytest.fixture
def float32_seg_scan(scan_dir_factory: Callable[..., Path]) -> "Scan":
    """float32 label map whose values sit either side of the integers."""
    return scan_from_dir(
        scan_dir_factory("FLOAT01_1", seg_dtype=np.float32, seg_jitter=True), "native"
    )


@pytest.fixture
def narrow_band_scan(scan_dir_factory: Callable[..., Path]) -> "Scan":
    """Tissue confined to 2000-2100 on a zero background."""
    return scan_from_dir(scan_dir_factory("NARROW1_1", intensity=(2000.0, 2100.0)), "native")


@pytest.fixture
def two_candidate_dir(scan_dir_factory: Callable[..., Path]) -> Path:
    return scan_dir_factory(
        "TWO001_1", source_names=("image_T1w.nii.gz", "image_T2w.nii.gz")
    )


@pytest.fixture
def zero_candidate_dir(scan_dir_factory: Callable[..., Path]) -> Path:
    return scan_dir_factory("ZERO001_1", source_names=())


# --------------------------------------------------------------------------- #
# Fixtures: study-tree layouts
# --------------------------------------------------------------------------- #


@pytest.fixture
def flat_root(tmp_path: Path) -> Path:
    """``root/<scan>/`` with no ``derivatives/`` at all."""
    root = tmp_path / "flat"
    for name in ("AAA111_1", "AAA111_2", "BBB222_1"):
        build_scan_dir(root / name, with_work=True)
    (root / "logs").mkdir(parents=True, exist_ok=True)
    return root


@pytest.fixture
def deriv_root(tmp_path: Path) -> Path:
    """``root/derivatives/tongue-seg/<scan>/`` plus a decoy scan dir at ``root/``."""
    root = tmp_path / "study"
    deriv = root / "derivatives" / "tongue-seg"
    for name in ("CCC333_1", "DDD444_2"):
        build_scan_dir(deriv / name)
    build_scan_dir(root / "DECOY_1")
    return root


@pytest.fixture
def bids_root(tmp_path: Path) -> Path:
    """``root/derivatives/tongue-seg/<sub>/<sub>_<ses>/``, two levels deep."""
    root = tmp_path / "bids"
    deriv = root / "derivatives" / "tongue-seg"
    build_scan_dir(deriv / "sub-001" / "sub-001_ses-01")
    build_scan_dir(deriv / "sub-002" / "sub-002_ses-01")
    return root


@pytest.fixture
def single_scan_root(scan_dir_factory: Callable[..., Path]) -> Path:
    """The root *is* the scan dir."""
    return scan_dir_factory("SOLO001_1", with_work=True)


@pytest.fixture
def empty_root(tmp_path: Path) -> Path:
    root = tmp_path / "nothing"
    (root / "a" / "b").mkdir(parents=True, exist_ok=True)
    return root
