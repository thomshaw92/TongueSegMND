"""Automatic fixes of the template labels and the session labels."""
from __future__ import annotations

import statistics

import numpy as np
from nipype.interfaces.utility import Function
from nipype.pipeline import engine as pe
from scipy import ndimage as ndi

from ..config import LatsConfig
from ..utils.paths import labels_dir, lateral_labels_dir, sst_dir

VERTICAL, SUPLONG, INFLONG, GENIO = 1, 2, 3, 4
LABELS = (1, 2, 3, 4)
FACE = ndi.generate_binary_structure(3, 1)


def _shift(a, axis, step, edge):
    out = np.full_like(a, edge)
    src, dst = [slice(None)] * 3, [slice(None)] * 3
    src[axis], dst[axis] = (slice(0, -1), slice(1, None)) if step > 0 else (slice(1, None), slice(0, -1))
    out[tuple(dst)] = a[tuple(src)]
    return out


def fill(labels, holes, ban=-1, background=False):
    """Give each hole the most common label among its decided 6-neighbours, layer by layer (ties to the lowest)."""
    out, todo = labels.copy(), holes.copy()
    out[todo] = 0
    vals = np.unique(np.append(out[~todo], 0))
    while todo.any():
        votes = np.zeros(out.shape + (len(vals),), np.int16)
        for axis in range(3):
            for step in (1, -1):
                nb, undecided = _shift(out, axis, step, -1), _shift(todo, axis, step, True)
                ok = ~undecided & (nb >= (0 if background else 1)) & (nb != ban)
                votes += ok[..., None] & (nb[..., None] == vals)
        has = todo & (votes.sum(-1) > 0)
        if not has.any():
            break
        out[has] = vals[votes.argmax(-1)][has]
        todo &= ~has
    return out


def clip_suplong(lab):
    """No superior longitudinal below the lowest vertical voxel."""
    vert = np.argwhere(lab == VERTICAL)
    if not len(vert):
        return lab
    below = (lab == SUPLONG) & (np.arange(lab.shape[2]) < vert[:, 2].min())
    out = fill(lab, below, ban=SUPLONG)
    return fill(out, below & (out == 0))


def sl_cap(lab):
    """Makes superior longitudinal a continuous cap over the dorsum, at its median thickness."""
    fg = lab > 0
    top = lab.shape[2] - 1 - np.argmax(fg[:, :, ::-1], axis=2)
    tl = np.where(fg.any(2), np.take_along_axis(lab, top[..., None], 2)[..., 0], 0)
    cap = tl == SUPLONG
    if not cap.any():
        return lab
    other = (lab != SUPLONG) & (np.arange(lab.shape[2]) <= top[..., None])
    below = np.where(other.any(2), lab.shape[2] - 1 - np.argmax(other[:, :, ::-1], axis=2), -1)
    t = int(statistics.median_low((top - below)[cap]))
    sq = np.ones((7, 7), bool)
    foot = ndi.binary_erosion(ndi.binary_dilation(cap, sq), sq, border_value=1) | ndi.binary_fill_holes(cap)
    out = lab.copy()
    for x, y in zip(*np.nonzero(foot & (tl == VERTICAL))):
        col = out[x, y, max(top[x, y] - t + 1, 0):top[x, y] + 1]
        col[col == VERTICAL] = SUPLONG
    return out


def band(image, mask, k=2.0):
    inner = ndi.binary_erosion(mask, FACE, iterations=2)
    v = image[inner if inner.sum() >= 50 else mask]
    med = float(np.median(v))
    sd = float(np.median(np.abs(v - med))) / 0.6745 + 1e-6
    return med - k * sd, med + k * sd


def trim(lab, image, label, lo, hi, layers):
    """Remove surface voxels of one muscle whose intensity is outside [lo, hi], up to `layers` layers."""
    out, outside = lab.copy(), (image < lo) | (image > hi)
    for _ in range(layers):
        fg = out > 0
        kill = fg & ~ndi.binary_erosion(fg, FACE, border_value=0) & outside & (out == label)
        if not kill.any():
            break
        out[kill] = 0
    return out


def follow_dorsum(pre, post):
    """Each column takes the labels it had before the trim, stretched to the trimmed outline."""
    out = post.copy()
    for x, y in zip(*np.nonzero((pre > 0).any(2) & (post > 0).any(2))):
        p, q = np.nonzero(pre[x, y])[0], np.nonzero(post[x, y])[0]
        p0, p1, q0, q1 = int(p[0]), int(p[-1]), int(q[0]), int(q[-1])
        for z in range(q0, q1 + 1):
            if post[x, y, z]:
                zs = p0 + (int(np.floor((z - q0) * (p1 - p0) / (q1 - q0) + 0.5)) if q1 > q0 else 0)
                if pre[x, y, zs]:
                    out[x, y, z] = pre[x, y, zs]
    return out


def order_inflong(lab):
    """No inferior longitudinal above the lowest superior longitudinal voxel of its column; those join their neighbours."""
    sl = lab == SUPLONG
    low = np.where(sl.any(2), sl.argmax(2), lab.shape[2])
    bad = (lab == INFLONG) & (np.arange(lab.shape[2])[None, None, :] > low[..., None])
    return fill(lab, bad, ban=INFLONG) if bad.any() else lab


def dark_cut(lab, image, k=3.0):
    """Removes thick dark tongue that joins the outside."""
    fg = lab > 0
    dark = ndi.binary_opening(fg & (image < band(image, fg, k)[0]), FACE, iterations=2)
    return np.where(dark & ndi.binary_propagation(~fg, FACE, mask=dark | ~fg), 0, lab).astype(lab.dtype)


def smooth(lab, zooms, labels=LABELS):
    """Smooths the outline and the muscle boundaries."""
    s = [1.0 / z for z in zooms]
    keep = ndi.gaussian_filter((lab > 0).astype(np.float32), s) >= 0.5
    w = np.stack([ndi.gaussian_filter((lab == k).astype(np.float32), s) for k in labels], -1)
    return np.where(keep, np.array(labels, lab.dtype)[w.argmax(-1)], 0).astype(lab.dtype)


def drop_pieces(lab, zooms, labels=LABELS):
    """Pieces under a quarter of the muscle's largest (inferior longitudinal: under 50 mm3) go to what surrounds them."""
    keep = np.zeros_like(lab)
    il_min = int(np.ceil(50.0 / float(np.prod(zooms))))
    for k in labels:
        cc, n = ndi.label(lab == k, FACE)
        if not n:
            continue
        sizes = np.bincount(cc.ravel())
        sizes[0] = 0
        ok = sizes >= (il_min if k % 100 == INFLONG else 0.25 * sizes.max())
        ok[0] = False
        keep[ok[cc]] = k
    return fill(keep, (lab > 0) & (keep == 0), background=True)


def largest_piece(lab):
    cc, n = ndi.label(lab > 0, FACE)
    if n < 2:
        return lab
    return np.where(cc == np.bincount(cc.ravel())[1:].argmax() + 1, lab, 0).astype(lab.dtype)


def genio_rules(lab, zooms):
    """Removes genioglossus at the side or top of the tongue, or far in front of vertical or superior longitudinal."""
    gg = lab == GENIO
    if not gg.any():
        return lab
    fg = lab > 0
    x, y, z = np.indices(lab.shape)
    mid = np.median(np.nonzero(fg)[0])

    def extent(mask, coord, axes, high):
        e = np.where(mask, coord, -np.inf if high else np.inf)
        e = e.max(axis=axes) if high else e.min(axis=axes)
        return np.where(np.isfinite(e), e, np.nan)

    with np.errstate(invalid="ignore"):
        right, left = extent(fg & ~gg & (x >= mid), x, (0, 2), True), extent(fg & ~gg & (x < mid), x, (0, 2), False)
        bad = (x >= mid) & ((x - right[None, :, None]) * zooms[0] > 0.5)
        bad |= (x < mid) & ((left[None, :, None] - x) * zooms[0] > 0.5)
        bad |= z > extent(lab == VERTICAL, z, (0, 2), True)[None, :, None]
        bad |= (y - extent(lab == VERTICAL, y, (1, 2), True)[:, None, None]) * zooms[1] > 4.0
        bad |= (y - extent(lab == SUPLONG, y, (1, 2), True)[:, None, None]) * zooms[1] > 11.0
    bad &= gg
    if not bad.any():
        return lab
    out = np.where(bad, 0, lab).astype(lab.dtype)
    enclosed = bad & ndi.binary_fill_holes(out > 0)
    return fill(out, enclosed) if enclosed.any() else out


def shave_side(lab, labels=(VERTICAL, SUPLONG)):
    """The outermost side voxel of vertical and superior longitudinal (background next to it left or right) goes."""
    fg = lab > 0
    side = np.zeros_like(fg)
    side[1:-1] = fg[1:-1] & (~fg[:-2] | ~fg[2:])
    return np.where(side & np.isin(lab, labels), 0, lab).astype(lab.dtype)


def autofix(lab, image, zooms, sl_layers=2, il_layers=3, k=2.0, tongue_prob=None, prob_cut=0.0, dark_sd=3.0, shave=True):
    """Returns the fixed labels and the voxels each step changed; arrays in RAS voxel order."""
    changed = {}

    def step(name, new, old):
        changed[name] = int((new != old).sum())
        return new

    out = step("clip", clip_suplong(lab), lab)
    out = step("cap", sl_cap(out), out)
    out = step("trim", follow_dorsum(out, trim(out, image, SUPLONG, *band(image, out == SUPLONG, k), sl_layers)), out)
    out = step("trim_il", trim(out, image, INFLONG, *band(image, out == INFLONG, k), il_layers), out)
    out = step("order", order_inflong(out), out)
    if dark_sd:
        out = step("dark", dark_cut(out, image, dark_sd), out)
    if prob_cut:
        out = step("prob_cut", fill(out, (out > 0) & (tongue_prob < prob_cut), background=True), out)
    out = step("smooth", smooth(out, zooms), out)
    out = step("genio", genio_rules(out, zooms), out)
    if shave:
        out = step("shave", shave_side(out), out)
    out = step("pieces", drop_pieces(out, zooms), out)
    out = step("largest", largest_piece(out), out)
    return out, changed


def _tongue_n4(image_path, labels, output_dir):
    """N4 within the tongue, for the intensity trims."""
    import subprocess
    from pathlib import Path

    import nibabel as nib

    z = labels.header.get_zooms()[:3]
    mask = ndi.binary_dilation(np.asanyarray(labels.dataobj) > 0.5, FACE, iterations=max(1, round(5 / min(z))))
    mpath, opath = Path(output_dir) / "autofix_n4_mask.nii.gz", Path(output_dir) / "T_template0_tongueN4.nii.gz"
    nib.save(nib.Nifti1Image(mask.astype(np.uint8), labels.affine), str(mpath))
    subprocess.run(["N4BiasFieldCorrection", "-d", "3", "-i", str(image_path), "-x", str(mpath), "-o", str(opath),
                    "-s", "1", "-c", "[100x100x100x100,1e-7]", "-b", "[30]"], check=True, capture_output=True)
    img = np.asanyarray(nib.load(str(opath)).dataobj).astype(np.float32)
    mpath.unlink()
    return img


def _autofix(sst_labels, sst_image, output_dir, sl_layers=2, il_layers=3, k=2.0, probs="", prob_cut=0.0, dark_sd=3.0, shave=True):
    import json
    from pathlib import Path

    import nibabel as nib
    import numpy as np
    from nibabel.orientations import apply_orientation, axcodes2ornt, io_orientation, ornt_transform

    from lats.nodes.autofix import _tongue_n4, autofix

    li, ii = nib.load(sst_labels), nib.load(sst_image)
    if li.shape[:3] != ii.shape[:3]:
        raise ValueError(f"autofix: labels {li.shape} and template {ii.shape} differ")
    own, ras = io_orientation(li.affine), axcodes2ornt("RAS")
    lab = apply_orientation(np.rint(np.asanyarray(li.dataobj)).astype(np.int16), ornt_transform(own, ras))
    img = apply_orientation(_tongue_n4(sst_image, li, output_dir), ornt_transform(own, ras))
    zooms = np.zeros(3)
    zooms[own[:, 0].astype(int)] = li.header.get_zooms()[:3]
    tp = None
    if prob_cut:
        if not probs:
            raise ValueError("--prob-cut needs the segmenter's probabilities (sst_seg_probs.nii.gz)")
        p4 = np.asanyarray(nib.load(probs).dataobj)
        if p4.shape[:3] != li.shape[:3]:
            raise ValueError(f"autofix: probabilities {p4.shape} and labels {li.shape} differ")
        tongue = 1 - p4[..., 0].astype(np.float32)
        nib.save(nib.Nifti1Image(tongue, li.affine), str(Path(output_dir) / "sst_seg_tongue_prob.nii.gz"))
        tp = apply_orientation(tongue, ornt_transform(own, ras))
    out, changed = autofix(lab, img, zooms, sl_layers, il_layers, k, tp, prob_cut, dark_sd, shave)
    hdr = li.header.copy()
    hdr.set_data_dtype(np.uint8)
    path = Path(output_dir) / "sst_seg_autofix.nii.gz"
    nib.save(nib.Nifti1Image(apply_orientation(out, ornt_transform(ras, own)).astype(np.uint8), li.affine, hdr), str(path))
    settings = {"sl_trim_layers": sl_layers, "il_trim_layers": il_layers, "trim_sd": k, "smooth_mm": 1.0, "prob_cut": prob_cut, "dark_sd": dark_sd, "side_shave": shave}
    (Path(output_dir) / "autofix.json").write_text(json.dumps({"settings": settings, "changed": changed}, indent=2) + "\n")
    return str(path)


def _fix_session(labels, lateral, timepoint_idx, labels_dir, lateral_dir):
    """Smooths one session's mapped labels, drops stray pieces and keeps one tongue."""
    from pathlib import Path

    import nibabel as nib
    import numpy as np

    from lats.nodes.autofix import drop_pieces, largest_piece, smooth

    src = nib.load(labels)
    lab = np.rint(np.asanyarray(src.dataobj)).astype(np.int16)
    out = np.zeros_like(lab)
    if lab.any():
        idx = np.argwhere(lab > 0)
        box = tuple(slice(a, b) for a, b in zip(np.maximum(idx.min(0) - 8, 0), np.minimum(idx.max(0) + 9, lab.shape)))
        z, classes = src.header.get_zooms()[:3], [int(v) for v in np.unique(lab[box]) if v > 0]
        out[box] = largest_piece(drop_pieces(smooth(lab[box], z, labels=classes), z, labels=classes))
    hdr = src.header.copy()
    hdr.set_data_dtype(np.uint8)

    def save(arr, d):
        Path(d).mkdir(parents=True, exist_ok=True)
        p = Path(d) / f"tp{timepoint_idx:02d}.nii.gz"
        nib.save(nib.Nifti1Image(arr.astype(np.uint8), src.affine, hdr), str(p))
        return str(p)

    return save(out % 100 if lateral else out, labels_dir), (save(out, lateral_dir) if lateral else "")


def build_session_fix(config: LatsConfig, lateral: bool) -> pe.MapNode:
    node = pe.MapNode(Function(input_names=["labels", "lateral", "timepoint_idx", "labels_dir", "lateral_dir"],
                               output_names=["fixed_labels", "fixed_lateral"], function=_fix_session), name="session_fix",
                      iterfield=["labels", "timepoint_idx"])
    node.inputs.timepoint_idx, node.inputs.lateral = list(range(config.n_timepoints)), lateral
    node.inputs.labels_dir = str(labels_dir(config.output_prefix))
    node.inputs.lateral_dir = str(lateral_labels_dir(config.output_prefix))
    return node


def build_autofix(config: LatsConfig) -> pe.Node:
    node = pe.Node(Function(input_names=["sst_labels", "sst_image", "output_dir", "sl_layers", "il_layers", "k", "probs", "prob_cut", "dark_sd", "shave"],
                            output_names=["fixed_seg"], function=_autofix), name="autofix")
    node.inputs.output_dir = str(sst_dir(config.output_prefix))
    node.inputs.sl_layers, node.inputs.il_layers, node.inputs.k = config.sl_trim_layers, config.il_trim_layers, config.trim_sd
    node.inputs.prob_cut, node.inputs.dark_sd, node.inputs.shave = config.prob_cut, config.dark_sd, config.side_shave
    return node
