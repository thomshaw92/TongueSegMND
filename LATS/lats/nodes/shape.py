"""Per-timepoint shape descriptors."""
from __future__ import annotations

from nipype.interfaces.utility import Function
from nipype.pipeline import engine as pe

from ..config import LatsConfig
from ..utils.paths import shape_dir, stats_dir

# Fixed sampling resolution of the normalised profiles/grids.
N_PROFILE = 50
N_GRID_LR = 24
N_GRID_AP = 32
SL_LABEL = 2          # superior longitudinal


def _shape_for_timepoint(
    timepoint_idx: int,
    warped_labels: str,
    warped_lateral_labels: str,
    labels_file: str,
    n_profile: int,
    n_grid_lr: int,
    n_grid_ap: int,
    sl_label: int,
    output_dir: str,
) -> tuple[str, str]:
    """Return (descriptors_tsv, arrays_npz)."""
    from pathlib import Path as _P

    import nibabel as nib
    import numpy as np

    from lats.utils.geometry import (
        curvature_1d,
        inertia_descriptors,
        polynomial_residual_rms,
        resample_grid,
        resample_profile,
        rugosity,
        sphericity,
        superior_height_map,
        surface_area_mm2,
        surface_roughness,
        voxel_volume_mm3,
        voxel_world_coords,
    )

    od = _P(output_dir)
    od.mkdir(parents=True, exist_ok=True)
    tag = f"tp{timepoint_idx:02d}"
    tsv_path = od / f"{tag}_shape.tsv"
    npz_path = od / f"{tag}_surfaces.npz"

    img = nib.as_closest_canonical(nib.load(warped_labels))
    data = np.rint(np.asarray(img.get_fdata())).astype(int)
    affine = img.affine
    zooms = tuple(float(z) for z in img.header.get_zooms()[:3])
    vox_mm3 = voxel_volume_mm3(affine)

    lat_img = nib.as_closest_canonical(nib.load(warped_lateral_labels))
    lat = np.rint(np.asarray(lat_img.get_fdata())).astype(int)

    pairs = []
    for line in _P(labels_file).read_text().splitlines():
        parts = line.split(None, 1)
        if len(parts) == 2:
            try:
                pairs.append((int(parts[0]), parts[1].strip()))
            except ValueError:
                pass

    # (R, A, S) after canonicalisation.
    LR, AP, SI = 0, 1, 2

    def _dorsum(mask):
        """Mid-sagittal superior profile of ``mask``: (metrics, profile)."""
        nan = ({"dorsum_length_mm": np.nan, "dorsum_mean_curvature_1pmm": np.nan,
                "dorsum_max_curvature_1pmm": np.nan, "dorsum_scalloping_mm": np.nan},
               np.full(n_profile, np.nan))
        if mask.sum() < 50:
            return nan
        # Central sagittal slab about the structure's own LR centroid.
        lr_profile = mask.sum(axis=(AP, SI))
        cx = int(round(float((lr_profile * np.arange(mask.shape[LR])).sum() / lr_profile.sum())))
        half = max(1, int(round(4.0 / max(zooms[LR], 1e-6))))   # +/- 4 mm
        slab = mask[max(0, cx - half): cx + half + 1]
        if not slab.any():
            return nan
        n_ap, n_si = mask.shape[AP], mask.shape[SI]
        si_idx = np.arange(n_si)
        sup = np.full(n_ap, np.nan)
        for yi in range(n_ap):
            cols = slab[:, yi, :]
            tops = [(c * si_idx).max() for c in cols if c.any()]
            if tops:
                sup[yi] = float(np.median(tops))
        good = np.where(np.isfinite(sup))[0]
        if good.size < 7:
            return nan
        s_mm = good * zooms[AP]
        z_mm = sup[good] * zooms[SI]
        sigma = max(1.0, 4.0 / max(zooms[AP], 1e-6))
        _, mean_k, max_k, length = curvature_1d(s_mm, z_mm, smooth_sigma=sigma)
        metrics = {
            "dorsum_length_mm": length,
            "dorsum_mean_curvature_1pmm": mean_k,
            "dorsum_max_curvature_1pmm": max_k,
            # Scalloping: RMS of the profile after removing the gross arch.
            "dorsum_scalloping_mm": polynomial_residual_rms(s_mm, z_mm, degree=4),
        }
        return metrics, resample_profile(s_mm, z_mm, n=n_profile)

    def _sl_surface(mask):
        nan = ({"sl_mean_height_mm": np.nan, "sl_roughness_mm": np.nan,
                "sl_rugosity": np.nan},
               np.full((n_grid_lr, n_grid_ap), np.nan))
        if mask.sum() < 50:
            return nan
        heights = superior_height_map(mask, lr_axis=LR, si_axis=SI, ap_axis=AP,
                                      zooms=zooms)
        grid = resample_grid(heights, n_grid_lr, n_grid_ap)
        if not np.isfinite(grid).any():
            return nan
        # Physical spacing of the *normalised* grid, for rugosity.
        finite = np.isfinite(heights)
        lr_extent = float(finite.any(axis=1).sum() * zooms[LR])
        ap_extent = float(finite.any(axis=0).sum() * zooms[AP])
        metrics = {
            "sl_mean_height_mm": float(np.nanmean(grid)),
            "sl_roughness_mm": surface_roughness(grid, degree=3),
            "sl_rugosity": rugosity(grid,
                                    lr_extent / max(n_grid_lr - 1, 1),
                                    ap_extent / max(n_grid_ap - 1, 1)),
        }
        return metrics, grid

    rows = []
    arrays = {}
    sides = {"whole": None, "left": 100, "right": 200}

    for side, offset in sides.items():
        for label_id, name in [(0, "whole tongue")] + pairs:
            if side == "whole":
                mask = (data > 0) if label_id == 0 else (data == label_id)
            else:
                if label_id == 0:
                    mask = (lat // 100) == (offset // 100)
                else:
                    mask = lat == (offset + label_id)
            n_vox = int(mask.sum())
            row = {
                "timepoint": timepoint_idx, "side": side,
                "label_id": label_id, "label": name,
                "n_voxels": n_vox, "volume_mm3": n_vox * vox_mm3,
            }
            if n_vox >= 8:
                area = surface_area_mm2(mask, zooms)
                row["surface_area_mm2"] = area
                row["sphericity"] = sphericity(n_vox * vox_mm3, area)
                row.update(inertia_descriptors(voxel_world_coords(mask, affine)))
                idx = np.argwhere(mask)
                for ax, key in ((LR, "extent_lr_mm"), (AP, "extent_ap_mm"),
                                (SI, "extent_si_mm")):
                    row[key] = float(
                        (idx[:, ax].max() - idx[:, ax].min() + 1) * zooms[ax]
                    )
            # Dorsum and SL surface only for the structures they describe.
            if label_id == 0:
                dm, prof = _dorsum(mask)
                row.update(dm)
                arrays[f"dorsum_{side}"] = prof
            if label_id == sl_label:
                sm, grid = _sl_surface(mask)
                row.update(sm)
                arrays[f"sl_surface_{side}"] = grid
            rows.append(row)

    fields = []
    for r in rows:
        for k in r:
            if k not in fields:
                fields.append(k)

    def _fmt(v):
        if v is None:
            return ""
        if isinstance(v, float):
            return "" if not np.isfinite(v) else f"{v:.6g}"
        return str(v)

    lines = ["\t".join(fields)]
    lines += ["\t".join(_fmt(r.get(f)) for f in fields) for r in rows]
    tsv_path.write_text("\n".join(lines) + "\n")
    np.savez_compressed(npz_path, **arrays)
    return str(tsv_path), str(npz_path)


def _aggregate_shape(shape_files: list, output_dir: str) -> tuple[str, str]:
    """Merge per-timepoint TSVs into shape.csv and asymmetry.csv."""
    import csv
    from pathlib import Path as _P

    od = _P(output_dir)
    od.mkdir(parents=True, exist_ok=True)
    shape_csv = od / "shape.csv"
    asym_csv = od / "asymmetry.csv"

    all_rows = []
    fields: list[str] = []
    for path in shape_files:
        lines = _P(path).read_text().splitlines()
        if not lines:
            continue
        header = lines[0].split("\t")
        for f in header:
            if f not in fields:
                fields.append(f)
        for line in lines[1:]:
            vals = line.split("\t")
            all_rows.append(dict(zip(header, vals)))

    with open(shape_csv, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=fields, restval="")
        w.writeheader()
        w.writerows(all_rows)

    # ---- asymmetry.csv ----------------------------------------------------
    # Asymmetry index: (L - R) / ((L + R) / 2), unscaled.
    by = {}
    for r in all_rows:
        by[(r["timepoint"], r["side"], r["label_id"])] = r

    metrics = ["volume_mm3", "surface_area_mm2", "sphericity", "elongation", "flatness"]
    out_fields = (["timepoint", "label_id", "label"]
                  + [f"left_{m}" for m in metrics]
                  + [f"right_{m}" for m in metrics]
                  + [f"asymmetry_{m}" for m in metrics])
    asym_rows = []
    keys = sorted({(r["timepoint"], r["label_id"]) for r in all_rows},
                  key=lambda k: (int(k[0]), int(k[1])))
    for tp, lid in keys:
        left = by.get((tp, "left", lid))
        right = by.get((tp, "right", lid))
        if not left or not right:
            continue
        row = {"timepoint": tp, "label_id": lid, "label": left.get("label", "")}
        for m in metrics:
            try:
                lv, rv = float(left.get(m, "")), float(right.get(m, ""))
            except ValueError:
                row[f"left_{m}"] = left.get(m, "")
                row[f"right_{m}"] = right.get(m, "")
                row[f"asymmetry_{m}"] = ""
                continue
            denom = (lv + rv) / 2.0
            row[f"left_{m}"] = f"{lv:.6g}"
            row[f"right_{m}"] = f"{rv:.6g}"
            row[f"asymmetry_{m}"] = f"{(lv - rv) / denom:.6g}" if denom else ""
        asym_rows.append(row)

    with open(asym_csv, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=out_fields, restval="")
        w.writeheader()
        w.writerows(asym_rows)

    return str(shape_csv), str(asym_csv)


def _sst_meshes(sst_labels: str, labels_file: str, output_dir: str) -> list:
    """Per-label STL of the SST — the subject's average shape."""
    from pathlib import Path as _P

    import nibabel as nib
    import numpy as np

    from lats.utils.geometry import mesh_from_mask, write_stl

    od = _P(output_dir)
    od.mkdir(parents=True, exist_ok=True)
    img = nib.as_closest_canonical(nib.load(sst_labels))
    data = np.rint(np.asarray(img.get_fdata())).astype(int)
    zooms = tuple(float(z) for z in img.header.get_zooms()[:3])

    pairs = [(0, "whole-tongue")]
    for line in _P(labels_file).read_text().splitlines():
        parts = line.split(None, 1)
        if len(parts) == 2:
            try:
                pairs.append((int(parts[0]), parts[1].strip().replace(" ", "-")))
            except ValueError:
                pass

    out = []
    for label_id, name in pairs:
        mask = (data > 0) if label_id == 0 else (data == label_id)
        if mask.sum() < 8:
            continue
        try:
            verts, faces = mesh_from_mask(mask, zooms)
        except (ValueError, RuntimeError):
            continue
        out.append(write_stl(od / f"sst_{label_id:02d}_{name}.stl", verts, faces))
    return out


def build_shape(config: LatsConfig) -> pe.MapNode:
    """MapNode over timepoints. Caller wires the warped and lateral labels."""
    node = pe.MapNode(
        Function(
            input_names=[
                "timepoint_idx", "warped_labels", "warped_lateral_labels",
                "labels_file", "n_profile", "n_grid_lr", "n_grid_ap",
                "sl_label", "output_dir",
            ],
            output_names=["descriptors_tsv", "arrays_npz"],
            function=_shape_for_timepoint,
        ),
        name="shape",
        iterfield=["timepoint_idx", "warped_labels", "warped_lateral_labels"],
    )
    node.inputs.timepoint_idx = list(range(config.n_timepoints))
    node.inputs.n_profile = N_PROFILE
    node.inputs.n_grid_lr = N_GRID_LR
    node.inputs.n_grid_ap = N_GRID_AP
    node.inputs.sl_label = SL_LABEL
    node.inputs.output_dir = str(shape_dir(config.output_prefix))
    return node


def build_aggregate_shape(config: LatsConfig) -> pe.Node:
    node = pe.Node(
        Function(
            input_names=["shape_files", "output_dir"],
            output_names=["shape_csv", "asymmetry_csv"],
            function=_aggregate_shape,
        ),
        name="aggregate_shape",
    )
    node.inputs.output_dir = str(stats_dir(config.output_prefix))
    return node


def build_sst_meshes(config: LatsConfig) -> pe.Node:
    node = pe.Node(
        Function(
            input_names=["sst_labels", "labels_file", "output_dir"],
            output_names=["meshes"],
            function=_sst_meshes,
        ),
        name="sst_meshes",
    )
    node.inputs.output_dir = str(shape_dir(config.output_prefix) / "meshes")
    return node
