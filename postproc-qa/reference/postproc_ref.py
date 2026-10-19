"""Reference tongue post-processing; web/js/postproc.js must match it voxel for voxel."""

import numpy as np

VERTICAL = 1
SUPLONG = 2
INFLONG = 3
GENIO = 4
LABELS = (1, 2, 3, 4)
CORE = (1, 3, 4)          # tissue-intensity reference


# --- primitive ops ---------------------------------------------------------

def erode6(mask):
    """6-connected binary erosion, out-of-bounds treated as False (border erodes)."""
    m = mask
    out = m.copy()
    out[:-1, :, :] &= m[1:, :, :]; out[1:, :, :] &= m[:-1, :, :]
    out[:, :-1, :] &= m[:, 1:, :]; out[:, 1:, :] &= m[:, :-1, :]
    out[:, :, :-1] &= m[:, :, 1:]; out[:, :, 1:] &= m[:, :, :-1]
    out[0, :, :] = False; out[-1, :, :] = False
    out[:, 0, :] = False; out[:, -1, :] = False
    out[:, :, 0] = False; out[:, :, -1] = False
    return out & m


def dilate6(mask):
    """6-connected binary dilation."""
    m = mask
    out = m.copy()
    out[1:, :, :] |= m[:-1, :, :]; out[:-1, :, :] |= m[1:, :, :]
    out[:, 1:, :] |= m[:, :-1, :]; out[:, :-1, :] |= m[:, 1:, :]
    out[:, :, 1:] |= m[:, :, :-1]; out[:, :, :-1] |= m[:, :, 1:]
    return out


def erode_n(mask, n):
    for _ in range(n):
        mask = erode6(mask)
    return mask


def dilate_n(mask, n):
    for _ in range(n):
        mask = dilate6(mask)
    return mask


def largest_cc(mask):
    """Largest 6-connected component of a boolean mask (BFS, deterministic)."""
    from collections import deque
    seen = np.zeros(mask.shape, dtype=bool)
    best = np.zeros(mask.shape, dtype=bool)
    best_n = 0
    nx, ny, nz = mask.shape
    idx = np.argwhere(mask)
    for sx, sy, sz in idx:
        if seen[sx, sy, sz]:
            continue
        comp = []
        q = deque([(sx, sy, sz)])
        seen[sx, sy, sz] = True
        while q:
            x, y, z = q.popleft()
            comp.append((x, y, z))
            for dx, dy, dz in ((1,0,0),(-1,0,0),(0,1,0),(0,-1,0),(0,0,1),(0,0,-1)):
                a, b, c = x+dx, y+dy, z+dz
                if 0 <= a < nx and 0 <= b < ny and 0 <= c < nz and mask[a,b,c] and not seen[a,b,c]:
                    seen[a,b,c] = True; q.append((a,b,c))
        if len(comp) > best_n:
            best_n = len(comp)
            best = np.zeros(mask.shape, dtype=bool)
            for x, y, z in comp:
                best[x, y, z] = True
    return best


def edt(mask, zooms):
    """Exact Euclidean distance (mm) from each True voxel to the nearest False voxel."""
    INF = 1e20
    f = np.where(mask, INF, 0.0).astype(np.float64)
    for axis, sp in enumerate(zooms):
        f = _dt_axis(f, axis, float(sp) ** 2)
    return np.sqrt(f).astype(np.float32)


def _dt_axis(f, axis, sp2):
    f = np.moveaxis(f, axis, -1)
    shape = f.shape
    flat = f.reshape(-1, shape[-1]).copy()
    n = shape[-1]
    v = np.empty(n, dtype=np.int64)
    z = np.empty(n + 1, dtype=np.float64)
    out = np.empty(n, dtype=np.float64)
    for row in flat:
        k = 0; v[0] = 0; z[0] = -1e30; z[1] = 1e30
        for q in range(1, n):
            while True:
                p = v[k]
                s = ((row[q] + q*q*sp2) - (row[p] + p*p*sp2)) / (2*sp2*(q-p))
                if s <= z[k]:
                    k -= 1
                else:
                    break
            k += 1; v[k] = q; z[k] = s; z[k+1] = 1e30
        k = 0
        for q in range(n):
            while z[k+1] < q:
                k += 1
            d = q - v[k]
            out[q] = d*d*sp2 + row[v[k]]
        row[:] = out
    return np.moveaxis(flat.reshape(shape), -1, axis)


def fill_nearest(labels, holes):
    """Fill ``holes`` by iterative majority dilation of neighbouring labels (ties -> lowest id)."""
    out = labels.copy()
    todo = holes & (out == 0)
    nx, ny, nz = out.shape
    while todo.any():
        coords = np.argwhere(todo)
        assigned = []
        for x, y, z in coords:
            counts = {}
            for dx, dy, dz in ((1,0,0),(-1,0,0),(0,1,0),(0,-1,0),(0,0,1),(0,0,-1)):
                a, b, c = x+dx, y+dy, z+dz
                if 0 <= a < nx and 0 <= b < ny and 0 <= c < nz:
                    lv = out[a, b, c]
                    if lv > 0:
                        counts[lv] = counts.get(lv, 0) + 1
            if counts:
                best = max(sorted(counts), key=lambda k: counts[k])
                assigned.append((x, y, z, best))
        if not assigned:
            break
        for x, y, z, lv in assigned:
            out[x, y, z] = lv; todo[x, y, z] = False
    return out


# --- tongue post-processing operations -------------------------------------

def tissue_band(image, labels, k=1.5):
    core = image[np.isin(labels, CORE)]
    med = float(np.median(core))
    mad = float(np.median(np.abs(core - med))) / 0.6745 + 1e-6
    return med - k * mad, med + k * mad


def thin_suplong(labels, zooms, keep_frac=0.88):
    """Keep the outer ``keep_frac`` of sup-long by depth; reassign the rest to the nearest label."""
    out = labels.copy()
    depth = edt(out > 0, zooms)
    sl = out == SUPLONG
    if sl.sum() == 0:
        return out
    t = float(np.quantile(depth[sl], keep_frac))
    reassign = sl & (depth > t)
    out[reassign] = 0
    return fill_nearest(out, reassign)


def suplong_inferior_clip(labels):
    """Sup-long cannot extend inferiorly past the lowest vertical/transverse voxel."""
    out = labels.copy()
    vert = out == VERTICAL
    if vert.sum() == 0:
        return out
    zmin = int(np.argwhere(vert)[:, 2].min())     # axis 2 = superior; inferior = low z
    zz = np.broadcast_to(np.arange(out.shape[2]), out.shape)
    below = (out == SUPLONG) & (zz < zmin)
    out[below] = 0
    return fill_nearest(out, below)


def surface_trim(labels, image, lo, hi, iters=6, which=LABELS):
    """Trim background-facing surface voxels of ``which`` labels with intensity outside [lo, hi]."""
    out = labels.copy()
    nt = (image < lo) | (image > hi)
    whichmask = np.isin(out, which)
    for _ in range(iters):
        fg = out > 0
        surf = fg & ~erode6(fg)
        kill = surf & nt & whichmask & (out > 0)
        if not kill.any():
            break
        out[kill] = 0
        whichmask = np.isin(out, which)
    return out


def clean_labels(labels, iters=1):
    """Per-label erode -> largest connected component -> dilate, then refill holes."""
    out = np.zeros_like(labels)
    for k in LABELS:
        m = labels == k
        if m.sum() == 0:
            continue
        e = erode_n(m, iters)
        e = largest_cc(e)
        d = dilate_n(e, iters) & m
        out[d] = k
    holes = (labels > 0) & (out == 0)
    if holes.any():
        out = fill_nearest(out, holes)
    return out


def smooth_labels(labels, sigma=0.8):
    """Probabilistic-majority smoothing: blur each label mask, take the argmax."""
    fg = labels > 0
    stack = np.stack([_gaussian(labels == k, sigma) for k in LABELS], axis=-1)
    win = np.argmax(stack, axis=-1) + 1
    out = np.zeros_like(labels)
    out[fg] = win[fg].astype(labels.dtype)
    return out


def _gaussian(mask, sigma):
    x = mask.astype(np.float64)
    radius = max(1, int(4.0 * sigma + 0.5))
    r = np.arange(-radius, radius + 1)
    kern = np.exp(-(r * r) / (2 * sigma * sigma))
    kern /= kern.sum()
    for axis in range(3):
        x = _conv1d_reflect(x, kern, axis)
    return x


def _conv1d_reflect(x, kern, axis):
    x = np.moveaxis(x, axis, -1)
    pad = len(kern) // 2
    xp = np.pad(x, [(0, 0)] * (x.ndim - 1) + [(pad, pad)], mode="reflect")
    out = np.zeros_like(x)
    for i, w in enumerate(kern):
        out += w * xp[..., i:i + x.shape[-1]]
    return np.moveaxis(out, -1, axis)


# --- pipeline --------------------------------------------------------------

def run_pipeline(labels, image, zooms, steps):
    """Apply the selected steps in order. ``steps`` is a dict of booleans/values."""
    out = labels.copy()
    if steps.get("thin"):
        out = thin_suplong(out, zooms, steps.get("keep_frac", 0.88))
    if steps.get("inferior_clip"):
        out = suplong_inferior_clip(out)
    if steps.get("surface_trim"):
        lo, hi = tissue_band(image, out, steps.get("trim_k", 1.5))
        which = LABELS if steps.get("trim_all", True) else (SUPLONG,)
        out = surface_trim(out, image, lo, hi, which=which)
    if steps.get("clean"):
        out = clean_labels(out, steps.get("clean_iters", 1))
    if steps.get("smooth"):
        out = smooth_labels(out, steps.get("sigma", 0.8))
    return out
