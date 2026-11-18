// Tongue segmentation post-processing; must match reference/postproc_ref.py voxel for voxel.
// Volumes are flat typed arrays in C order: idx = (x*ny + y)*nz + z.
// Labels: 0 bg, 1 vertical, 2 superior-long, 3 inferior-long, 4 genioglossus.

export const SUPLONG = 2, VERTICAL = 1, INFLONG = 3, GENIO = 4;
export const LABELS = [1, 2, 3, 4];
export const CORE = [1, 3, 4];

const idx = (x, y, z, ny, nz) => (x * ny + y) * nz + z;

export function erode6(m, dims) {
  const [nx, ny, nz] = dims, sx = ny * nz, sy = nz, out = new Uint8Array(m.length);
  for (let x = 0; x < nx; x++) for (let y = 0; y < ny; y++) for (let z = 0; z < nz; z++) {
    const i = idx(x, y, z, ny, nz);
    if (!m[i]) continue;
    const keep = (x > 0 && m[i - sx]) && (x < nx - 1 && m[i + sx]) &&
      (y > 0 && m[i - sy]) && (y < ny - 1 && m[i + sy]) &&
      (z > 0 && m[i - 1]) && (z < nz - 1 && m[i + 1]);
    out[i] = keep ? 1 : 0;
  }
  return out;
}

export function majorCC(mask, dims, frac = 0.25, minSize = 0) {
  const [nx, ny, nz] = dims, sx = ny * nz, sy = nz, N = mask.length;
  const seen = new Uint8Array(N), q = new Int32Array(N), comps = [];
  let big = 0;
  for (let start = 0; start < N; start++) {
    if (!mask[start] || seen[start]) continue;
    let head = 0, tail = 0; q[tail++] = start; seen[start] = 1;
    while (head < tail) {
      const i = q[head++];
      const z = i % nz, rem = (i - z) / nz, y = rem % ny, x = (rem - y) / ny;
      const nb = [];
      if (x > 0) nb.push(i - sx); if (x < nx - 1) nb.push(i + sx);
      if (y > 0) nb.push(i - sy); if (y < ny - 1) nb.push(i + sy);
      if (z > 0) nb.push(i - 1); if (z < nz - 1) nb.push(i + 1);
      for (const k of nb) if (mask[k] && !seen[k]) { seen[k] = 1; q[tail++] = k; }
    }
    comps.push(q.slice(0, tail));
    big = Math.max(big, tail);
  }
  const out = new Uint8Array(N), keep = Math.max(frac * big, minSize);
  for (const c of comps) if (c.length >= keep) for (const i of c) out[i] = 1;
  return out;
}

export function edt(mask, dims, zooms) {
  const N = mask.length, INF = 1e20, f = new Float64Array(N);
  for (let i = 0; i < N; i++) f[i] = mask[i] ? INF : 0;
  dtAxis(f, dims, 2, zooms[2] * zooms[2]);
  dtAxis(f, dims, 1, zooms[1] * zooms[1]);
  dtAxis(f, dims, 0, zooms[0] * zooms[0]);
  const out = new Float32Array(N);
  for (let i = 0; i < N; i++) out[i] = Math.sqrt(f[i]);
  return out;
}

function dtAxis(f, dims, axis, sp2) {
  const [nx, ny, nz] = dims;
  const n = dims[axis];
  const stride = axis === 0 ? ny * nz : axis === 1 ? nz : 1;
  const a1 = axis === 0 ? 1 : 0, a2 = axis === 0 ? 2 : (axis === 1 ? 2 : 1);
  const d = [nx, ny, nz];
  const line = new Float64Array(n), out = new Float64Array(n);
  const v = new Int32Array(n), z = new Float64Array(n + 1);
  for (let i1 = 0; i1 < d[a1]; i1++) for (let i2 = 0; i2 < d[a2]; i2++) {
    const c = [0, 0, 0]; c[a1] = i1; c[a2] = i2;
    const base = c[0] * ny * nz + c[1] * nz + c[2];
    for (let q = 0; q < n; q++) line[q] = f[base + q * stride];
    let k = 0; v[0] = 0; z[0] = -1e30; z[1] = 1e30;
    for (let q = 1; q < n; q++) {
      let s;
      while (true) {
        const p = v[k];
        s = ((line[q] + q * q * sp2) - (line[p] + p * p * sp2)) / (2 * sp2 * (q - p));
        if (s <= z[k]) k--; else break;
      }
      k++; v[k] = q; z[k] = s; z[k + 1] = 1e30;
    }
    k = 0;
    for (let q = 0; q < n; q++) {
      while (z[k + 1] < q) k++;
      const dd = q - v[k]; out[q] = dd * dd * sp2 + line[v[k]];
    }
    for (let q = 0; q < n; q++) f[base + q * stride] = out[q];
  }
}

function fillNearest(labels, holes, dims, ban = -1, background = false) {
  const [nx, ny, nz] = dims, sx = ny * nz, sy = nz;
  const out = Int32Array.from(labels);
  const todo = Uint8Array.from(holes);
  for (let i = 0; i < out.length; i++) if (todo[i] && out[i] !== 0) todo[i] = 0;
  while (true) {
    const assign = [];
    for (let i = 0; i < out.length; i++) {
      if (!todo[i]) continue;
      const z = i % nz, y = ((i - z) / nz) % ny, x = ((i - z) / nz - y) / ny;
      const counts = new Map();
      const nb = [];
      if (x > 0) nb.push(i - sx); if (x < nx - 1) nb.push(i + sx);
      if (y > 0) nb.push(i - sy); if (y < ny - 1) nb.push(i + sy);
      if (z > 0) nb.push(i - 1); if (z < nz - 1) nb.push(i + 1);
      for (const j of nb) { const lv = out[j]; if ((lv > 0 || (background && !todo[j])) && lv !== ban) counts.set(lv, (counts.get(lv) || 0) + 1); }
      if (counts.size) {
        let best = -1, bc = -1;
        for (const k of [...counts.keys()].sort((a, b) => a - b)) if (counts.get(k) > bc) { bc = counts.get(k); best = k; }
        assign.push([i, best]);
      }
    }
    if (!assign.length) break;
    for (const [i, lv] of assign) { out[i] = lv; todo[i] = 0; }
  }
  return out;
}

const median = (a) => { const s = Float64Array.from(a).sort(); const n = s.length; return n % 2 ? s[(n - 1) / 2] : (s[n / 2 - 1] + s[n / 2]) / 2; };
function quantile(a, q) { const s = Float64Array.from(a).sort(); const pos = q * (s.length - 1), lo = Math.floor(pos), hi = Math.ceil(pos); return s[lo] + (s[hi] - s[lo]) * (pos - lo); }

export function tissueBand(image, labels, k = 1.5) {
  const core = [];
  for (let i = 0; i < labels.length; i++) if (CORE.includes(labels[i])) core.push(image[i]);
  const med = median(core);
  const dev = core.map(v => Math.abs(v - med));
  const mad = median(dev) / 0.6745 + 1e-6;
  return [med - k * mad, med + k * mad];
}

export function thinSuplong(labels, dims, zooms, keepFrac = 0.88) {
  const out = Int32Array.from(labels);
  const fg = new Uint8Array(out.length);
  for (let i = 0; i < out.length; i++) fg[i] = out[i] > 0 ? 1 : 0;
  const depth = edt(fg, dims, zooms);
  const sld = [];
  for (let i = 0; i < out.length; i++) if (out[i] === SUPLONG) sld.push(depth[i]);
  if (!sld.length) return out;
  const t = quantile(sld, keepFrac);
  const re = new Uint8Array(out.length);
  for (let i = 0; i < out.length; i++) if (out[i] === SUPLONG && depth[i] > t) { re[i] = 1; out[i] = 0; }
  return fillNearest(fillNearest(out, re, dims, SUPLONG), re, dims);
}

export function suplongInferiorClip(labels, dims) {
  const [nx, ny, nz] = dims;
  const out = Int32Array.from(labels);
  let zmin = Infinity;
  for (let i = 0; i < out.length; i++) if (out[i] === VERTICAL) { const z = i % nz; if (z < zmin) zmin = z; }
  if (!isFinite(zmin)) return out;
  const below = new Uint8Array(out.length);
  for (let i = 0; i < out.length; i++) { const z = i % nz; if (out[i] === SUPLONG && z < zmin) { below[i] = 1; out[i] = 0; } }
  return fillNearest(fillNearest(out, below, dims, SUPLONG), below, dims);
}

function close2d(m, nx, ny, r) {
  const d = new Uint8Array(nx * ny), e = new Uint8Array(nx * ny);
  for (let x = 0; x < nx; x++) for (let y = 0; y < ny; y++) {
    let v = 0;
    for (let a = Math.max(0, x - r); a <= Math.min(nx - 1, x + r) && !v; a++)
      for (let b = Math.max(0, y - r); b <= Math.min(ny - 1, y + r); b++) if (m[a * ny + b]) { v = 1; break; }
    d[x * ny + y] = v;
  }
  for (let x = 0; x < nx; x++) for (let y = 0; y < ny; y++) {
    let v = 1;
    for (let a = Math.max(0, x - r); a <= Math.min(nx - 1, x + r) && v; a++)
      for (let b = Math.max(0, y - r); b <= Math.min(ny - 1, y + r); b++) if (!d[a * ny + b]) { v = 0; break; }
    e[x * ny + y] = v;
  }
  return e;
}

function fillHoles2d(m, nx, ny) {
  const outside = new Uint8Array(nx * ny), q = [];
  for (let x = 0; x < nx; x++) for (let y = 0; y < ny; y++)
    if ((x === 0 || x === nx - 1 || y === 0 || y === ny - 1) && !m[x * ny + y]) { outside[x * ny + y] = 1; q.push(x * ny + y); }
  for (let h = 0; h < q.length; h++) {
    const x = Math.floor(q[h] / ny), y = q[h] % ny;
    for (const [a, b] of [[x + 1, y], [x - 1, y], [x, y + 1], [x, y - 1]]) {
      const k = a * ny + b;
      if (a >= 0 && a < nx && b >= 0 && b < ny && !m[k] && !outside[k]) { outside[k] = 1; q.push(k); }
    }
  }
  return Uint8Array.from(outside, v => 1 - v);
}

export function slCap(labels, dims, r = 3) {
  const [nx, ny, nz] = dims, out = Int32Array.from(labels);
  const top = new Int32Array(nx * ny).fill(-1), tl = new Int32Array(nx * ny), cap = new Uint8Array(nx * ny);
  for (let x = 0; x < nx; x++) for (let y = 0; y < ny; y++) {
    const b = (x * ny + y) * nz;
    for (let z = nz - 1; z >= 0; z--) if (labels[b + z] > 0) { top[x * ny + y] = z; tl[x * ny + y] = labels[b + z]; break; }
    cap[x * ny + y] = tl[x * ny + y] === SUPLONG ? 1 : 0;
  }
  const runs = [];
  for (let c = 0; c < nx * ny; c++) {
    if (!cap[c]) continue;
    let z = top[c], n = 0;
    while (z >= 0 && labels[c * nz + z] === SUPLONG) { n++; z--; }
    runs.push(n);
  }
  if (!runs.length) return out;
  runs.sort((a, b) => a - b);
  const t = runs[Math.floor((runs.length - 1) / 2)];
  const closed = close2d(cap, nx, ny, r), filled = fillHoles2d(cap, nx, ny);
  for (let c = 0; c < nx * ny; c++) {
    if (!(closed[c] || filled[c]) || tl[c] !== VERTICAL) continue;
    for (let z = Math.max(top[c] - t + 1, 0); z <= top[c]; z++) if (out[c * nz + z] === VERTICAL) out[c * nz + z] = SUPLONG;
  }
  return out;
}

export function surfaceTrim(labels, image, dims, lo, hi, iters = 6, which = LABELS) {
  const out = Int32Array.from(labels);
  const nt = new Uint8Array(out.length);
  for (let i = 0; i < out.length; i++) nt[i] = (image[i] < lo || image[i] > hi) ? 1 : 0;
  const inWhich = (v) => which.includes(v);
  for (let it = 0; it < iters; it++) {
    const fg = new Uint8Array(out.length);
    for (let i = 0; i < out.length; i++) fg[i] = out[i] > 0 ? 1 : 0;
    const er = erode6(fg, dims);
    let any = false;
    const kill = new Uint8Array(out.length);
    for (let i = 0; i < out.length; i++) if (fg[i] && !er[i] && nt[i] && out[i] > 0 && inWhich(out[i])) { kill[i] = 1; any = true; }
    if (!any) break;
    for (let i = 0; i < out.length; i++) if (kill[i]) out[i] = 0;
  }
  return out;
}

// Drops pieces under a quarter of each muscle's largest (inferior longitudinal: under 50 mm3) to what surrounds them.
export function cleanLabels(labels, dims, zooms) {
  const out = new Int32Array(labels.length), ilMin = Math.ceil(50 / (zooms[0] * zooms[1] * zooms[2]));
  for (const k of LABELS) {
    const m = new Uint8Array(labels.length);
    let any = false;
    for (let i = 0; i < labels.length; i++) if (labels[i] === k) { m[i] = 1; any = true; }
    if (!any) continue;
    const d = k === INFLONG ? majorCC(m, dims, 0, ilMin) : majorCC(m, dims, 0.25);
    for (let i = 0; i < labels.length; i++) if (d[i]) out[i] = k;
  }
  const holes = new Uint8Array(labels.length);
  let anyHole = false;
  for (let i = 0; i < labels.length; i++) if (labels[i] > 0 && out[i] === 0) { holes[i] = 1; anyHole = true; }
  return anyHole ? fillNearest(out, holes, dims, -1, true) : out;
}

function gaussianKernel(sigma) {
  const radius = Math.max(1, Math.floor(4.0 * sigma + 0.5));
  const k = new Float64Array(2 * radius + 1);
  let s = 0;
  for (let r = -radius; r <= radius; r++) { const w = Math.exp(-(r * r) / (2 * sigma * sigma)); k[r + radius] = w; s += w; }
  for (let i = 0; i < k.length; i++) k[i] /= s;
  return k;
}

function gaussianBlur(mask, dims, sigmas) {
  let x = Float64Array.from(mask);
  for (let axis = 0; axis < 3; axis++) { const kern = gaussianKernel(sigmas[axis]); x = conv1dReflect(x, dims, axis, kern, (kern.length - 1) / 2); }
  return x;
}

function conv1dReflect(f, dims, axis, kern, pad) {
  const [nx, ny, nz] = dims, n = dims[axis];
  const stride = axis === 0 ? ny * nz : axis === 1 ? nz : 1;
  const a1 = axis === 0 ? 1 : 0, a2 = axis === 0 ? 2 : (axis === 1 ? 2 : 1);
  const d = [nx, ny, nz], out = new Float64Array(f.length), line = new Float64Array(n);
  const refl = (q) => { // edge repeated, as scipy.ndimage 'reflect' in LATS: abc -> ...cba|abc|cba...
    const p = 2 * n, m = ((q % p) + p) % p; return m < n ? m : p - 1 - m;
  };
  for (let i1 = 0; i1 < d[a1]; i1++) for (let i2 = 0; i2 < d[a2]; i2++) {
    const c = [0, 0, 0]; c[a1] = i1; c[a2] = i2;
    const base = c[0] * ny * nz + c[1] * nz + c[2];
    for (let q = 0; q < n; q++) line[q] = f[base + q * stride];
    for (let q = 0; q < n; q++) {
      let acc = 0;
      for (let t = 0; t < kern.length; t++) acc += kern[t] * line[refl(q + t - pad)];
      out[base + q * stride] = acc;
    }
  }
  return out;
}

export function followDorsum(pre, post, dims) {
  const [nx, ny, nz] = dims, out = Int32Array.from(post);
  for (let x = 0; x < nx; x++) for (let y = 0; y < ny; y++) {
    const b = (x * ny + y) * nz;
    let p0 = -1, p1 = -1, q0 = -1, q1 = -1;
    for (let z = 0; z < nz; z++) {
      if (pre[b + z] > 0) { if (p0 < 0) p0 = z; p1 = z; }
      if (post[b + z] > 0) { if (q0 < 0) q0 = z; q1 = z; }
    }
    if (p0 < 0 || q0 < 0) continue;
    for (let z = q0; z <= q1; z++) {
      if (post[b + z] === 0) continue;
      const zs = p0 + (q1 > q0 ? Math.floor((z - q0) * (p1 - p0) / (q1 - q0) + 0.5) : 0);
      if (pre[b + zs] > 0) out[b + z] = pre[b + zs];
    }
  }
  return out;
}

// Smooths the outline and the muscle boundaries.
export function smoothLabels(labels, dims, zooms, sigmaMm = 1.0) {
  const s = zooms.map(z => sigmaMm / z);
  const outline = gaussianBlur(Uint8Array.from(labels, v => (v > 0 ? 1 : 0)), dims, s);
  const blurred = LABELS.map(k => gaussianBlur(Uint8Array.from(labels, v => (v === k ? 1 : 0)), dims, s));
  const out = new Int32Array(labels.length);
  for (let i = 0; i < labels.length; i++) {
    if (outline[i] < 0.5) continue;
    let best = 0, bv = -Infinity;
    for (let j = 0; j < LABELS.length; j++) if (blurred[j][i] > bv) { bv = blurred[j][i]; best = LABELS[j]; }
    out[i] = best;
  }
  return out;
}

// Intensity band of one muscle: median +- k robust SD of its interior (2+ voxels deep).
export function muscleBand(image, labels, dims, label, k = 2) {
  const m = Uint8Array.from(labels, v => (v === label ? 1 : 0)), inner = erode6(erode6(m, dims), dims);
  let n = 0, nm = 0;
  for (let i = 0; i < m.length; i++) { n += inner[i]; nm += m[i]; }
  if (!nm) return [-Infinity, Infinity];
  const vals = [];
  for (let i = 0; i < m.length; i++) if (n >= 50 ? inner[i] : m[i]) vals.push(image[i]);
  const med = median(vals), sd = median(vals.map(v => Math.abs(v - med))) / 0.6745 + 1e-6;
  return [med - k * sd, med + k * sd];
}

// No inferior longitudinal above the lowest superior longitudinal voxel of its column; those join their neighbours.
export function orderInflong(labels, dims) {
  const [nx, ny, nz] = dims, out = Int32Array.from(labels), holes = new Uint8Array(labels.length);
  for (let x = 0; x < nx; x++) for (let y = 0; y < ny; y++) {
    const b = (x * ny + y) * nz;
    let low = nz;
    for (let z = 0; z < nz; z++) if (labels[b + z] === SUPLONG) { low = z; break; }
    for (let z = low + 1; z < nz; z++) if (labels[b + z] === INFLONG) { holes[b + z] = 1; out[b + z] = 0; }
  }
  return fillNearest(out, holes, dims, INFLONG);
}

export function largestPiece(labels, dims) {
  const keep = majorCC(Uint8Array.from(labels, v => (v > 0 ? 1 : 0)), dims, 1.0);
  return Int32Array.from(labels, (v, i) => (keep[i] ? v : 0));
}

function dilate6(m, dims) {
  const [nx, ny, nz] = dims, sx = ny * nz, out = Uint8Array.from(m);
  for (let x = 0; x < nx; x++) for (let y = 0; y < ny; y++) for (let z = 0; z < nz; z++) {
    const i = idx(x, y, z, ny, nz);
    if (!m[i] && ((x > 0 && m[i - sx]) || (x < nx - 1 && m[i + sx]) || (y > 0 && m[i - nz]) || (y < ny - 1 && m[i + nz]) ||
      (z > 0 && m[i - 1]) || (z < nz - 1 && m[i + 1]))) out[i] = 1;
  }
  return out;
}

// Removes thick dark tongue that joins the outside.
export function darkCut(labels, image, dims, k = 3) {
  const fg = Uint8Array.from(labels, v => (v > 0 ? 1 : 0)), [lo] = muscleBand(image, fg, dims, 1, k);
  const dark = dilate6(dilate6(erode6(erode6(Uint8Array.from(fg, (f, i) => (f && image[i] < lo ? 1 : 0)), dims), dims), dims), dims);
  const seen = reach(Uint8Array.from(fg, f => 1 - f), Uint8Array.from(fg, (f, i) => (!f || dark[i] ? 1 : 0)), dims);
  return Int32Array.from(labels, (v, i) => (dark[i] && seen[i] ? 0 : v));
}

// Voxels of allowed 6-connected to seed (seed inside allowed).
function reach(seed, allowed, dims) {
  const [nx, ny, nz] = dims, sx = ny * nz, out = Uint8Array.from(seed, (s, i) => (s && allowed[i] ? 1 : 0)), q = new Int32Array(seed.length);
  let head = 0, tail = 0;
  for (let i = 0; i < out.length; i++) if (out[i]) q[tail++] = i;
  while (head < tail) {
    const i = q[head++], z = i % nz, y = ((i - z) / nz) % ny, x = Math.floor(i / sx);
    for (const [ok, j] of [[x > 0, i - sx], [x < nx - 1, i + sx], [y > 0, i - nz], [y < ny - 1, i + nz], [z > 0, i - 1], [z < nz - 1, i + 1]])
      if (ok && !out[j] && allowed[j]) { out[j] = 1; q[tail++] = j; }
  }
  return out;
}

export function genioRules(labels, dims, zooms) {
  const [nx, ny, nz] = dims, xs = [];
  let any = false;
  for (let i = 0; i < labels.length; i++) { if (labels[i] > 0) xs.push(Math.floor(i / (ny * nz))); if (labels[i] === GENIO) any = true; }
  if (!any) return labels;
  const mid = median(xs), right = new Float64Array(ny).fill(-Infinity), left = new Float64Array(ny).fill(Infinity),
    vz = new Float64Array(ny).fill(-Infinity), vy = new Float64Array(nx).fill(-Infinity), sy = new Float64Array(nx).fill(-Infinity);
  for (let x = 0; x < nx; x++) for (let y = 0; y < ny; y++) for (let z = 0; z < nz; z++) {
    const v = labels[idx(x, y, z, ny, nz)];
    if (v > 0 && v !== GENIO) { if (x >= mid) right[y] = Math.max(right[y], x); else left[y] = Math.min(left[y], x); }
    if (v === VERTICAL) { vz[y] = Math.max(vz[y], z); vy[x] = Math.max(vy[x], y); }
    if (v === SUPLONG) sy[x] = Math.max(sy[x], y);
  }
  const out = Int32Array.from(labels), bad = new Uint8Array(labels.length);
  let n = 0;
  for (let x = 0; x < nx; x++) for (let y = 0; y < ny; y++) for (let z = 0; z < nz; z++) {
    const i = idx(x, y, z, ny, nz);
    if (labels[i] !== GENIO) continue;
    const side = x >= mid ? isFinite(right[y]) && (x - right[y]) * zooms[0] > 0.5 : isFinite(left[y]) && (left[y] - x) * zooms[0] > 0.5;
    if (side || (isFinite(vz[y]) && z > vz[y]) || (isFinite(vy[x]) && (y - vy[x]) * zooms[1] > 4) || (isFinite(sy[x]) && (y - sy[x]) * zooms[1] > 11)) {
      bad[i] = 1; out[i] = 0; n++;
    }
  }
  if (!n) return labels;
  const edge = Uint8Array.from(out, (v, i) => { const z = i % nz, y = ((i - z) / nz) % ny, x = Math.floor(i / (ny * nz)); return x === 0 || y === 0 || z === 0 || x === nx - 1 || y === ny - 1 || z === nz - 1; });
  const outside = reach(edge, Uint8Array.from(out, v => (v ? 0 : 1)), dims);
  const enclosed = Uint8Array.from(bad, (b, i) => (b && !outside[i] ? 1 : 0));
  return enclosed.some(v => v) ? fillNearest(out, enclosed, dims) : out;
}

// The outermost side voxel of vertical and superior longitudinal (background next to it left or right) goes.
export function shaveSide(labels, dims) {
  const [nx, ny, nz] = dims, sx = ny * nz;
  return Int32Array.from(labels, (v, i) => {
    const x = Math.floor(i / sx);
    return (v === VERTICAL || v === SUPLONG) && x > 0 && x < nx - 1 && (!labels[i - sx] || !labels[i + sx]) ? 0 : v;
  });
}

// Removes vertical with no mirror partner across the midline plane where the tongue probability is low.
export function mirrorCut(labels, dims, zooms, affine, { point, normal }, tongueProb, distMm = 2, prob = 0.9999) {
  const [nx, ny, nz] = dims, len = Math.hypot(...normal), n = normal.map(v => v / len), inv = inv4(affine);
  const mir = new Uint8Array(labels.length);
  forEachVoxel(labels, dims, v => v > 0, (i, j, k) => {
    const w = world(affine, i, j, k), s = 2 * ((w[0] - point[0]) * n[0] + (w[1] - point[1]) * n[1] + (w[2] - point[2]) * n[2]);
    const d = [0, 1, 2].map(r => w[r] - s * n[r] - affine[r][3]);
    const [a, b, c] = [0, 1, 2].map(r => rint(inv[r][0] * d[0] + inv[r][1] * d[1] + inv[r][2] * d[2]));
    if (a >= 0 && a < nx && b >= 0 && b < ny && c >= 0 && c < nz) mir[idx(a, b, c, ny, nz)] = 1;
  });
  const far = edt(Uint8Array.from(erode6(dilate6(mir, dims), dims), m => 1 - m), dims, zooms);
  return Int32Array.from(labels, (v, q) => (v === VERTICAL && far[q] > distMm && tongueProb[q] < prob ? 0 : v));
}

// numpy's rint: halves go to the even neighbour.
const rint = v => { const r = Math.round(v); return r - v === 0.5 && r % 2 !== 0 ? r - 1 : r; };

export function inv4(A) {   // inverse of an affine (last row 0 0 0 1), as its top three rows
  const [[a, b, d, tx], [e, f, g, ty], [h, i, k, tz]] = A, det = a * (f * k - g * i) - b * (e * k - g * h) + d * (e * i - f * h);
  const R = [[f * k - g * i, d * i - b * k, b * g - d * f], [g * h - e * k, a * k - d * h, d * e - a * g],
    [e * i - f * h, b * h - a * i, a * f - b * e]].map(r => r.map(v => v / det));
  return R.map(r => [...r, -(r[0] * tx + r[1] * ty + r[2] * tz)]);
}

// Rotation for [pitch, yaw, roll] in degrees (R = Rz Ry Rx), rounded to 1e-12.
export function moveRotation(deg) {
  const [p, y, r] = deg.map(a => a * Math.PI / 180);
  const cp = Math.cos(p), sp = Math.sin(p), cy = Math.cos(y), sy = Math.sin(y), cr = Math.cos(r), sr = Math.sin(r);
  return [[cy * cr, cy * sr * sp - sy * cp, cy * sr * cp + sy * sp], [sy * cr, sy * sr * sp + cy * cp, sy * sr * cp - cy * sp],
    [-sr, cr * sp, cr * cp]].map(row => row.map(v => Math.round(v * 1e12) / 1e12));
}

export function tongueCentre(labels, dims, zooms) {
  const [, ny, nz] = dims, s = [0, 0, 0];
  let n = 0;
  for (let q = 0; q < labels.length; q++) if (labels[q] > 0) { s[0] += Math.floor(q / (ny * nz)); s[1] += Math.floor(q / nz) % ny; s[2] += q % nz; n++; }
  return s.map((v, a) => v / n * zooms[a]);
}

// Moves the arrays rigidly, nearest neighbour; arrays may be null.
export function moveVolumes(arrays, dims, zooms, R, t, c) {
  const [nx, ny, nz] = dims, outs = arrays.map(a => a && new a.constructor(a.length));
  for (let i = 0; i < nx; i++) for (let j = 0; j < ny; j++) for (let k = 0; k < nz; k++) {
    const d0 = i * zooms[0] - c[0] - t[0], d1 = j * zooms[1] - c[1] - t[1], d2 = k * zooms[2] - c[2] - t[2];
    const si = Math.floor((R[0][0] * d0 + R[1][0] * d1 + R[2][0] * d2 + c[0]) / zooms[0] + 0.5);
    const sj = Math.floor((R[0][1] * d0 + R[1][1] * d1 + R[2][1] * d2 + c[1]) / zooms[1] + 0.5);
    const sk = Math.floor((R[0][2] * d0 + R[1][2] * d1 + R[2][2] * d2 + c[2]) / zooms[2] + 0.5);
    if (si < 0 || sj < 0 || sk < 0 || si >= nx || sj >= ny || sk >= nz) continue;
    const src = (si * ny + sj) * nz + sk, o = (i * ny + j) * nz + k;
    for (let m = 0; m < arrays.length; m++) if (outs[m]) outs[m][o] = arrays[m][src];
  }
  return outs;
}

export function runPipeline(labels, image, dims, zooms, steps, tongueProb = null, mirror = null) {
  const mm = [steps.mvX || 0, steps.mvY || 0, steps.mvZ || 0], deg = [steps.mvPitch || 0, steps.mvYaw || 0, steps.mvRoll || 0];
  if ([...mm, ...deg].some(v => v) && labels.some(v => v > 0)) {
    [labels, tongueProb] = moveVolumes([labels, tongueProb], dims, zooms, moveRotation(deg), mm, tongueCentre(labels, dims, zooms));
  }
  let out = Int32Array.from(labels);
  if (steps.thin) out = thinSuplong(out, dims, zooms, steps.keepFrac ?? 0.88);
  if (steps.inferiorClip) out = suplongInferiorClip(out, dims);
  if (steps.slCap) out = slCap(out, dims);
  const pre = Int32Array.from(out), k = steps.trimSd ?? 2;
  if (steps.slTrim) out = surfaceTrim(out, image, dims, ...muscleBand(image, out, dims, SUPLONG, k), steps.slTrim, [SUPLONG]);
  if (steps.followDorsum) out = followDorsum(pre, out, dims);
  if (steps.ilTrim) out = surfaceTrim(out, image, dims, ...muscleBand(image, out, dims, INFLONG, k), steps.ilTrim, [INFLONG]);
  if (steps.order) out = orderInflong(out, dims);
  if (steps.darkSd) out = darkCut(out, image, dims, steps.darkSd);
  if (steps.probCut && tongueProb) {
    const unsure = Uint8Array.from(out, (v, k) => (v > 0 && tongueProb[k] < steps.probCut ? 1 : 0));
    out = fillNearest(Int32Array.from(out, (v, k) => (unsure[k] ? 0 : v)), unsure, dims, -1, true);
  }
  if (steps.smooth) out = smoothLabels(out, dims, zooms, steps.sigma ?? 1);
  if (steps.genio) out = genioRules(out, dims, zooms);
  if (steps.shave) out = shaveSide(out, dims);
  if (steps.clean) out = largestPiece(cleanLabels(out, dims, zooms), dims);
  if (steps.mirror && mirror && tongueProb) out = largestPiece(cleanLabels(mirrorCut(out, dims, zooms, mirror.affine, mirror, tongueProb), dims, zooms), dims);
  return out;
}

export function qcMetrics(labels, image) {
  let fg = 0, sl = 0, slNt = 0, fgNt = 0;
  const [lo, hi] = tissueBand(image, labels);
  for (let i = 0; i < labels.length; i++) {
    const nt = image[i] < lo || image[i] > hi;
    if (labels[i] > 0) { fg++; if (nt) fgNt++; }
    if (labels[i] === SUPLONG) { sl++; if (nt) slNt++; }
  }
  return {
    suplongOverTongue: sl / Math.max(fg, 1),
    suplongNotTissueFrac: slNt / Math.max(sl, 1),
    tongueNotTissueFrac: fgNt / Math.max(fg, 1),
  };
}

const world = (affine, i, j, k) => [0, 1, 2].map(r => affine[r][0] * i + affine[r][1] * j + affine[r][2] * k + affine[r][3]);

function forEachVoxel(labels, dims, keep, fn) {
  const [nx, ny, nz] = dims;
  for (let i = 0; i < nx; i++) for (let j = 0; j < ny; j++) for (let k = 0; k < nz; k++) {
    const v = labels[(i * ny + j) * nz + k];
    if (keep(v)) fn(i, j, k, v);
  }
}

function smallestEigvec(m) {
  const a = m.map(r => r.slice()), v = [[1, 0, 0], [0, 1, 0], [0, 0, 1]];
  for (let sweep = 0; sweep < 50; sweep++) {
    if (a[0][1] ** 2 + a[0][2] ** 2 + a[1][2] ** 2 < 1e-30) break;
    for (const [p, q] of [[0, 1], [0, 2], [1, 2]]) {
      if (a[p][q] === 0) continue;
      const th = (a[q][q] - a[p][p]) / (2 * a[p][q]);
      const t = (th >= 0 ? 1 : -1) / (Math.abs(th) + Math.sqrt(th * th + 1));
      const c = 1 / Math.sqrt(t * t + 1), s = t * c;
      for (let k = 0; k < 3; k++) { const x = a[k][p], y = a[k][q]; a[k][p] = c * x - s * y; a[k][q] = s * x + c * y; }
      for (let k = 0; k < 3; k++) { const x = a[p][k], y = a[q][k]; a[p][k] = c * x - s * y; a[q][k] = s * x + c * y; }
      for (let k = 0; k < 3; k++) { const x = v[k][p], y = v[k][q]; v[k][p] = c * x - s * y; v[k][q] = s * x + c * y; }
    }
  }
  const i = [1, 2].reduce((best, k) => (a[k][k] < a[best][best] ? k : best), 0);
  return [v[0][i], v[1][i], v[2][i]];
}

const unit = (n) => { const l = Math.hypot(...n); return n.map(x => x / l); };

export function midlinePlane(labels, dims, affine, mode = 'gg') {
  const mean = (keep) => {
    const s = [0, 0, 0]; let n = 0;
    forEachVoxel(labels, dims, keep, (i, j, k) => { world(affine, i, j, k).forEach((w, d) => { s[d] += w; }); n++; });
    return n ? s.map(x => x / n) : null;
  };
  const point = mean(v => v === GENIO) || mean(v => v > 0);
  if (!point) return null;
  let normal = [1, 0, 0];
  if (mode === 'tilt') {
    const c = mean(v => v > 0), m = [[0, 0, 0], [0, 0, 0], [0, 0, 0]]; let n = 0;
    forEachVoxel(labels, dims, v => v > 0, (i, j, k) => {
      const d = world(affine, i, j, k).map((w, r) => w - c[r]); n++;
      for (let r = 0; r < 3; r++) for (let s = 0; s < 3; s++) m[r][s] += d[r] * d[s];
    });
    normal = smallestEigvec(m.map(r => r.map(x => x / n)));
  }
  if (normal[0] < 0) normal = normal.map(x => -x);
  return { point, normal: unit(normal) };
}

export function adjustPlane({ point, normal }, shiftMm = 0, yawDeg = 0, rollDeg = 0) {
  const a = yawDeg * Math.PI / 180, b = rollDeg * Math.PI / 180;
  const ry = [Math.cos(b) * normal[0] + Math.sin(b) * normal[2], normal[1], -Math.sin(b) * normal[0] + Math.cos(b) * normal[2]];
  const n = unit([Math.cos(a) * ry[0] - Math.sin(a) * ry[1], Math.sin(a) * ry[0] + Math.cos(a) * ry[1], ry[2]]);
  return { point: point.map((p, d) => p + shiftMm * n[d]), normal: n };
}

export function lateralCounts(labels, dims, affine, { point, normal }) {
  let left = 0, right = 0;
  forEachVoxel(labels, dims, v => v > 0, (i, j, k) => {
    const w = world(affine, i, j, k);
    if ((w[0] - point[0]) * normal[0] + (w[1] - point[1]) * normal[1] + (w[2] - point[2]) * normal[2] > 0) right++; else left++;
  });
  return { left, right };
}

export function planeMask(labels, dims, affine, { point, normal }, halfMm, pad = 4) {
  const [nx, ny, nz] = dims, lo = [nx, ny, nz], hi = [-1, -1, -1], out = new Uint8Array(labels.length);
  forEachVoxel(labels, dims, v => v > 0, (i, j, k) => [i, j, k].forEach((x, d) => { lo[d] = Math.min(lo[d], x); hi[d] = Math.max(hi[d], x); }));
  for (let i = Math.max(0, lo[0] - pad); i <= Math.min(nx - 1, hi[0] + pad); i++)
    for (let j = Math.max(0, lo[1] - pad); j <= Math.min(ny - 1, hi[1] + pad); j++)
      for (let k = Math.max(0, lo[2] - pad); k <= Math.min(nz - 1, hi[2] + pad); k++) {
        const w = world(affine, i, j, k);
        if (Math.abs((w[0] - point[0]) * normal[0] + (w[1] - point[1]) * normal[1] + (w[2] - point[2]) * normal[2]) <= halfMm)
          out[(i * ny + j) * nz + k] = 1;
      }
  return out;
}

export function shapeChecks(labels, dims, affine, plane = null) {
  const [nx, ny, nz] = dims;
  let cols = 0, capCols = 0;
  for (let x = 0; x < nx; x++) for (let y = 0; y < ny; y++) {
    const b = (x * ny + y) * nz;
    for (let z = nz - 1; z >= 0; z--) if (labels[b + z] > 0) { cols++; if (labels[b + z] === SUPLONG) capCols++; break; }
  }
  const ggRef = !!plane;
  if (!plane) {
    const c = [0, 0, 0]; let n = 0;
    forEachVoxel(labels, dims, v => v > 0, (i, j, k) => { world(affine, i, j, k).forEach((w, d) => { c[d] += w; }); n++; });
    plane = { point: c.map(v => v / Math.max(n, 1)), normal: [1, 0, 0] };
  }
  const { point: p, normal: m } = plane;
  let left = 0, right = 0, gg = 0, ng = 0;
  forEachVoxel(labels, dims, v => v === INFLONG || v === GENIO, (i, j, k, v) => {
    const w = world(affine, i, j, k), d = (w[0] - p[0]) * m[0] + (w[1] - p[1]) * m[1] + (w[2] - p[2]) * m[2];
    if (v === INFLONG) { if (d > 0) right++; else left++; } else { gg += d; ng++; }
  });
  return { capPct: cols ? 100 * capCols / cols : 0, ilLeft: left, ilRight: right,
    ilRatio: Math.min(left, right) / Math.max(left, right, 1), ggOffsetMm: ggRef && ng ? gg / ng : null };
}

export const SHAPE_FLAGS = { capPct: (v) => v < 90, ilRatio: (v) => v < 0.5, ggOffsetMm: (v) => v !== null && Math.abs(v) > 3 };

// Processing assumes RAS voxel order; rasOrder finds the voxel axis nearest each world axis, toRas/fromRas copy in and out.
export function rasOrder(dims, zooms, affine) {
  const near = [0, 1, 2].map(a => { const c = [0, 1, 2].map(w => Math.abs(affine[w][a])); return c.indexOf(Math.max(...c)); });
  if (new Set(near).size !== 3) throw new Error('voxel axes are too oblique to tell up from left-right');
  const src = [0, 1, 2].map(w => near.indexOf(w)), flip = src.map((a, w) => affine[w][a] < 0);
  const st = [dims[1] * dims[2], dims[2], 1];
  const A = affine.map(r => r.slice());
  for (let r = 0; r < 3; r++) {
    A[r][3] = affine[r][3] + src.reduce((s, a, w) => s + (flip[w] ? affine[r][a] * (dims[a] - 1) : 0), 0);
    for (let w = 0; w < 3; w++) A[r][w] = affine[r][src[w]] * (flip[w] ? -1 : 1);
  }
  return { dims: src.map(a => dims[a]), zooms: src.map(a => zooms[a]), affine: A,
    identity: src.every((a, w) => a === w) && !flip.some(Boolean),
    stride: src.map((a, w) => st[a] * (flip[w] ? -1 : 1)),
    base: src.reduce((s, a, w) => s + (flip[w] ? (dims[a] - 1) * st[a] : 0), 0) };
}

function rasCopy(arr, o, into) {
  if (o.identity) return arr;
  const [nx, ny, nz] = o.dims, [sx, sy, sz] = o.stride, out = new arr.constructor(arr.length);
  let n = 0;
  for (let i = 0; i < nx; i++) for (let j = 0; j < ny; j++) for (let k = 0, q = o.base + i * sx + j * sy; k < nz; k++, q += sz) {
    if (into) out[n++] = arr[q]; else out[q] = arr[n++];
  }
  return out;
}
export const toRas = (arr, o) => rasCopy(arr, o, true);
export const fromRas = (arr, o) => rasCopy(arr, o, false);
