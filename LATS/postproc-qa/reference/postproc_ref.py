"""Reference tongue post-processing; web/js/postproc.js must match it voxel for voxel."""

import math

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


def major_cc(mask, frac=0.25, min_size=0):
    """6-connected components at least ``frac`` of the largest one and ``min_size`` voxels (BFS, deterministic)."""
    from collections import deque
    seen = np.zeros(mask.shape, dtype=bool)
    comps = []
    nx, ny, nz = mask.shape
    for sx, sy, sz in np.argwhere(mask):
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
        comps.append(comp)
    out = np.zeros(mask.shape, dtype=bool)
    keep = max(frac * max((len(c) for c in comps), default=0), min_size)
    for comp in comps:
        if len(comp) >= keep:
            for x, y, z in comp:
                out[x, y, z] = True
    return out


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


def fill_nearest(labels, holes, ban=-1, background=False):
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
                    if (lv > 0 or (background and not todo[a, b, c])) and lv != ban:
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
    return fill_nearest(fill_nearest(out, reassign, ban=SUPLONG), reassign)


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
    return fill_nearest(fill_nearest(out, below, ban=SUPLONG), below)


def _close2d(m, r):
    p = np.pad(m, r, constant_values=False)
    d = np.zeros_like(m)
    for i in range(2 * r + 1):
        for j in range(2 * r + 1):
            d |= p[i:i + m.shape[0], j:j + m.shape[1]]
    p = np.pad(d, r, constant_values=True)
    e = np.ones_like(m)
    for i in range(2 * r + 1):
        for j in range(2 * r + 1):
            e &= p[i:i + m.shape[0], j:j + m.shape[1]]
    return e


def _fill_holes2d(m):
    from collections import deque
    nx, ny = m.shape
    outside = np.zeros_like(m)
    q = deque((x, y) for x in range(nx) for y in range(ny) if (x in (0, nx - 1) or y in (0, ny - 1)) and not m[x, y])
    for x, y in q:
        outside[x, y] = True
    while q:
        x, y = q.popleft()
        for a, b in ((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)):
            if 0 <= a < nx and 0 <= b < ny and not m[a, b] and not outside[a, b]:
                outside[a, b] = True; q.append((a, b))
    return ~outside


def sl_cap(labels, r=3):
    """Superior longitudinal as a continuous dorsal cap: where vertical tops a column inside the cap's footprint, its top voxels become superior longitudinal at the cap's median thickness."""
    nz = labels.shape[2]
    fg = labels > 0
    has = fg.any(axis=2)
    top = nz - 1 - np.argmax(fg[:, :, ::-1], axis=2)
    tl = np.where(has, np.take_along_axis(labels, top[..., None], 2)[..., 0], 0)
    cap = tl == SUPLONG
    out = labels.copy()
    if not cap.any():
        return out
    runs = []
    for x, y in zip(*np.nonzero(cap)):
        z, n = top[x, y], 0
        while z >= 0 and labels[x, y, z] == SUPLONG:
            n += 1; z -= 1
        runs.append(n)
    t = sorted(runs)[(len(runs) - 1) // 2]
    foot = _close2d(cap, r) | _fill_holes2d(cap)
    for x, y in zip(*np.nonzero(foot & (tl == VERTICAL))):
        for z in range(max(top[x, y] - t + 1, 0), top[x, y] + 1):
            if out[x, y, z] == VERTICAL:
                out[x, y, z] = SUPLONG
    return out


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


def clean_labels(labels, zooms):
    """Drop pieces under a quarter of each muscle's largest (inferior longitudinal: under 50 mm3) to what surrounds them."""
    out = np.zeros_like(labels)
    il_min = int(np.ceil(50.0 / float(np.prod(zooms))))
    for k in LABELS:
        m = labels == k
        if m.any():
            out[major_cc(m, 0, il_min) if k == INFLONG else major_cc(m, 0.25)] = k
    holes = (labels > 0) & (out == 0)
    if holes.any():
        out = fill_nearest(out, holes, background=True)
    return out


def follow_dorsum(pre, post):
    """Refill each superior-inferior column of ``post`` with the ``pre`` label stack, stretched to fit."""
    out = post.copy()
    nx, ny, nz = post.shape
    for x in range(nx):
        for y in range(ny):
            p = np.nonzero(pre[x, y])[0]
            q = np.nonzero(post[x, y])[0]
            if p.size == 0 or q.size == 0:
                continue
            p0, p1, q0, q1 = int(p[0]), int(p[-1]), int(q[0]), int(q[-1])
            for z in range(q0, q1 + 1):
                if post[x, y, z] == 0:
                    continue
                zs = p0 + (int(np.floor((z - q0) * (p1 - p0) / (q1 - q0) + 0.5)) if q1 > q0 else 0)
                if pre[x, y, zs] > 0:
                    out[x, y, z] = pre[x, y, zs]
    return out


def muscle_band(image, labels, label, k=2.0):
    """Intensity band of one muscle: median +- k robust SD of its interior (2+ voxels deep)."""
    m = labels == label
    if not m.any():
        return -np.inf, np.inf
    inner = erode6(erode6(m))
    v = image[inner if inner.sum() >= 50 else m]
    med = float(np.median(v))
    sd = float(np.median(np.abs(v - med))) / 0.6745 + 1e-6
    return med - k * sd, med + k * sd


def order_inflong(labels):
    """No inferior longitudinal above the lowest superior longitudinal voxel of its column; those join their neighbours."""
    sl = labels == SUPLONG
    low = np.where(sl.any(2), sl.argmax(2), labels.shape[2])
    bad = (labels == INFLONG) & (np.arange(labels.shape[2])[None, None, :] > low[..., None])
    return fill_nearest(np.where(bad, 0, labels).astype(labels.dtype), bad, ban=INFLONG)


def largest_piece(labels):
    keep = major_cc(labels > 0, 1.0)
    return np.where(keep, labels, 0).astype(labels.dtype)


def smooth_labels(labels, zooms, sigma_mm=1.0):
    """Smooths the outline and the muscle boundaries."""
    s = [sigma_mm / z for z in zooms]
    keep = _gaussian(labels > 0, s) >= 0.5
    stack = np.stack([_gaussian(labels == k, s) for k in LABELS], axis=-1)
    win = np.argmax(stack, axis=-1) + 1
    return np.where(keep, win, 0).astype(labels.dtype)


def _gaussian(mask, sigmas):
    x = mask.astype(np.float64)
    for axis, sigma in enumerate(sigmas):
        radius = max(1, int(4.0 * sigma + 0.5))
        r = np.arange(-radius, radius + 1)
        kern = np.exp(-(r * r) / (2 * sigma * sigma))
        x = _conv1d_reflect(x, kern / kern.sum(), axis)
    return x


def _conv1d_reflect(x, kern, axis):
    x = np.moveaxis(x, axis, -1)
    pad = len(kern) // 2
    xp = np.pad(x, [(0, 0)] * (x.ndim - 1) + [(pad, pad)], mode="symmetric")
    out = np.zeros_like(x)
    for i, w in enumerate(kern):
        out += w * xp[..., i:i + x.shape[-1]]
    return np.moveaxis(out, -1, axis)


# --- pipeline --------------------------------------------------------------

def dilate6(mask):
    """6-connected binary dilation; nothing outside the volume."""
    out = mask.copy()
    out[:-1] |= mask[1:]; out[1:] |= mask[:-1]
    out[:, :-1] |= mask[:, 1:]; out[:, 1:] |= mask[:, :-1]
    out[:, :, :-1] |= mask[:, :, 1:]; out[:, :, 1:] |= mask[:, :, :-1]
    return out


def reach(seed, allowed):
    """Voxels of `allowed` 6-connected to `seed` (seed inside allowed)."""
    out = seed & allowed
    while True:
        grown = dilate6(out) & allowed
        if (grown == out).all():
            return out
        out = grown


def dark_cut(labels, image, k=3.0):
    """Removes thick dark tongue that joins the outside."""
    fg = labels > 0
    dark = dilate6(dilate6(erode6(erode6(fg & (image < muscle_band(image, fg, True, k)[0])))))
    cut = dark & reach(~fg, dark | ~fg)
    return np.where(cut, 0, labels).astype(labels.dtype)


def genio_rules(labels, zooms):
    """Removes genioglossus at the side or top of the tongue, or far in front of vertical or superior longitudinal."""
    gg = labels == GENIO
    if not gg.any():
        return labels
    fg = labels > 0
    x, y, z = np.indices(labels.shape)
    mid = np.median(np.nonzero(fg)[0])

    def extent(mask, coord, axes, high):
        e = np.where(mask, coord, -np.inf if high else np.inf)
        e = e.max(axis=axes) if high else e.min(axis=axes)
        return np.where(np.isfinite(e), e, np.nan)

    with np.errstate(invalid="ignore"):
        right, left = extent(fg & ~gg & (x >= mid), x, (0, 2), True), extent(fg & ~gg & (x < mid), x, (0, 2), False)
        bad = (x >= mid) & ((x - right[None, :, None]) * zooms[0] > 0.5)
        bad |= (x < mid) & ((left[None, :, None] - x) * zooms[0] > 0.5)
        bad |= z > extent(labels == VERTICAL, z, (0, 2), True)[None, :, None]
        bad |= (y - extent(labels == VERTICAL, y, (1, 2), True)[:, None, None]) * zooms[1] > 4.0
        bad |= (y - extent(labels == SUPLONG, y, (1, 2), True)[:, None, None]) * zooms[1] > 11.0
    bad &= gg
    if not bad.any():
        return labels
    out = np.where(bad, 0, labels).astype(labels.dtype)
    edge = np.zeros(labels.shape, bool)
    edge[[0, -1]], edge[:, [0, -1]], edge[:, :, [0, -1]] = True, True, True
    enclosed = bad & ~reach(edge, out == 0)
    return fill_nearest(out, enclosed) if enclosed.any() else out


def shave_side(labels):
    """The outermost side voxel of vertical and superior longitudinal (background next to it left or right) goes."""
    fg = labels > 0
    side = np.zeros_like(fg)
    side[1:-1] = fg[1:-1] & (~fg[:-2] | ~fg[2:])
    return np.where(side & np.isin(labels, (VERTICAL, SUPLONG)), 0, labels).astype(labels.dtype)


def mirror_cut(labels, zooms, affine, point, normal, tongue_prob, dist_mm=2.0, prob=0.9999):
    """Removes vertical with no mirror partner across the midline plane where the tongue probability is low."""
    n = np.asarray(normal, float) / np.linalg.norm(normal)
    w = world_coords(labels > 0, affine)
    wm = w - 2 * ((w - np.asarray(point, float)) @ n)[:, None] * n
    v = np.rint((wm - affine[:3, 3]) @ np.linalg.inv(affine[:3, :3]).T).astype(int)
    v = v[np.all((v >= 0) & (v < labels.shape), 1)]
    mir = np.zeros(labels.shape, bool)
    mir[tuple(v.T)] = True
    far = edt(~erode6(dilate6(mir)), zooms) > dist_mm
    unsure = np.asarray(tongue_prob, np.float64) < prob
    return np.where((labels == VERTICAL) & far & unsure, 0, labels).astype(labels.dtype)


def move_rotation(deg):
    """Rotation for [pitch, yaw, roll] in degrees (R = Rz Ry Rx), rounded to 1e-12."""
    p, y, r = (a * math.pi / 180 for a in deg)
    cp, sp, cy, sy, cr, sr = math.cos(p), math.sin(p), math.cos(y), math.sin(y), math.cos(r), math.sin(r)
    R = [[cy * cr, cy * sr * sp - sy * cp, cy * sr * cp + sy * sp],
         [sy * cr, sy * sr * sp + cy * cp, sy * sr * cp - cy * sp],
         [-sr, cr * sp, cr * cp]]
    return [[round(v * 1e12) / 1e12 for v in row] for row in R]


def tongue_centre(labels, zooms):
    idx = np.argwhere(labels > 0)
    return [float(idx[:, a].sum()) / len(idx) * float(zooms[a]) for a in range(3)]


def move_volumes(arrays, zooms, R, t, c):
    """Moves the arrays rigidly, nearest neighbour; arrays may be None."""
    shape, z = arrays[0].shape, [float(v) for v in zooms]
    g = np.meshgrid(*(np.arange(n) for n in shape), indexing="ij")
    d = [g[a] * z[a] - c[a] - t[a] for a in range(3)]
    src = [np.floor((R[0][a] * d[0] + R[1][a] * d[1] + R[2][a] * d[2] + c[a]) / z[a] + 0.5).astype(np.int64) for a in range(3)]
    ok = np.all([(s >= 0) & (s < n) for s, n in zip(src, shape)], axis=0)
    s = tuple(v[ok] for v in src)
    out = [None if a is None else np.zeros_like(a) for a in arrays]
    for o, a in zip(out, arrays):
        if o is not None:
            o[ok] = a[s]
    return out


def run_pipeline(labels, image, zooms, steps, tongue_prob=None, mirror=None):
    """Applies the selected steps in order, after any move."""
    mm, deg = steps.get("move_mm", [0, 0, 0]), steps.get("move_deg", [0, 0, 0])
    if (any(mm) or any(deg)) and (labels > 0).any():
        labels, tongue_prob = move_volumes([labels, tongue_prob], zooms, move_rotation(deg), mm, tongue_centre(labels, zooms))
    out = labels.copy()
    if steps.get("thin"):
        out = thin_suplong(out, zooms, steps.get("keep_frac", 0.88))
    if steps.get("inferior_clip"):
        out = suplong_inferior_clip(out)
    if steps.get("sl_cap"):
        out = sl_cap(out)
    pre, k = out.copy(), steps.get("trim_sd", 2.0)
    if steps.get("sl_trim"):
        out = surface_trim(out, image, *muscle_band(image, out, SUPLONG, k), iters=steps["sl_trim"], which=(SUPLONG,))
    if steps.get("follow_dorsum"):
        out = follow_dorsum(pre, out)
    if steps.get("il_trim"):
        out = surface_trim(out, image, *muscle_band(image, out, INFLONG, k), iters=steps["il_trim"], which=(INFLONG,))
    if steps.get("order"):
        out = order_inflong(out)
    if steps.get("dark_sd"):
        out = dark_cut(out, image, steps["dark_sd"])
    if steps.get("prob_cut") and tongue_prob is not None:
        unsure = (out > 0) & (tongue_prob < steps["prob_cut"])
        out = fill_nearest(np.where(unsure, 0, out), unsure, background=True)
    if steps.get("smooth"):
        out = smooth_labels(out, zooms, steps.get("sigma", 1.0))
    if steps.get("genio"):
        out = genio_rules(out, zooms)
    if steps.get("shave"):
        out = shave_side(out)
    if steps.get("clean"):
        out = largest_piece(clean_labels(out, zooms))
    if steps.get("mirror") and mirror is not None and tongue_prob is not None:
        out = mirror_cut(out, zooms, mirror["affine"], mirror["point"], mirror["normal"], tongue_prob)
        out = largest_piece(clean_labels(out, zooms))
    return out


def world_coords(mask, affine):
    return np.argwhere(mask) @ affine[:3, :3].T + affine[:3, 3]


def midline_plane(labels, affine, mode="gg"):
    """Mid-sagittal plane (point_mm, unit normal toward +x): genioglossus centroid, normal along x or tilted (PCA)."""
    m = labels == GENIO
    if not m.any():
        m = labels > 0
    point = world_coords(m, affine).mean(0)
    normal = np.array([1.0, 0.0, 0.0])
    if mode == "tilt":
        c = world_coords(labels > 0, affine)
        c = c - c.mean(0)
        normal = np.linalg.eigh(c.T @ c / len(c))[1][:, 0]
    if normal[0] < 0:
        normal = -normal
    return point, normal / np.linalg.norm(normal)


def adjust_plane(point, normal, shift_mm=0.0, yaw_deg=0.0, roll_deg=0.0):
    a, b = np.radians(yaw_deg), np.radians(roll_deg)
    rz = np.array([[np.cos(a), -np.sin(a), 0], [np.sin(a), np.cos(a), 0], [0, 0, 1]])
    ry = np.array([[np.cos(b), 0, np.sin(b)], [0, 1, 0], [-np.sin(b), 0, np.cos(b)]])
    n = rz @ ry @ normal
    n = n / np.linalg.norm(n)
    return point + shift_mm * n, n


def lateral_counts(labels, affine, point, normal):
    """Foreground voxels (left, right) of the plane; right is the +normal side."""
    s = (world_coords(labels > 0, affine) - point) @ normal
    return int((s <= 0).sum()), int((s > 0).sum())

