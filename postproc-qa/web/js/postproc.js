// Tongue segmentation post-processing; must match reference/postproc_ref.py voxel for voxel.
// Volumes are flat typed arrays in C order: idx = (x*ny + y)*nz + z.
// Labels: 0 bg, 1 vertical, 2 superior-long, 3 inferior-long, 4 genioglossus.

export const SUPLONG = 2, VERTICAL = 1;
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

export function dilate6(m, dims) {
  const [nx, ny, nz] = dims, sx = ny * nz, sy = nz, out = new Uint8Array(m.length);
  for (let x = 0; x < nx; x++) for (let y = 0; y < ny; y++) for (let z = 0; z < nz; z++) {
    const i = idx(x, y, z, ny, nz);
    out[i] = (m[i] || (x > 0 && m[i - sx]) || (x < nx - 1 && m[i + sx]) ||
      (y > 0 && m[i - sy]) || (y < ny - 1 && m[i + sy]) ||
      (z > 0 && m[i - 1]) || (z < nz - 1 && m[i + 1])) ? 1 : 0;
  }
  return out;
}

const erodeN = (m, dims, n) => { for (let i = 0; i < n; i++) m = erode6(m, dims); return m; };
const dilateN = (m, dims, n) => { for (let i = 0; i < n; i++) m = dilate6(m, dims); return m; };

export function largestCC(mask, dims) {
  const [nx, ny, nz] = dims, sx = ny * nz, sy = nz, N = mask.length;
  const seen = new Uint8Array(N), best = new Uint8Array(N);
  const q = new Int32Array(N);
  let bestN = 0;
  for (let start = 0; start < N; start++) {
    if (!mask[start] || seen[start]) continue;
    let head = 0, tail = 0, n = 0; q[tail++] = start; seen[start] = 1;
    const comp = [];
    while (head < tail) {
      const i = q[head++]; comp.push(i); n++;
      const z = i % nz, rem = (i - z) / nz, y = rem % ny, x = (rem - y) / ny;
      const nb = [];
      if (x > 0) nb.push(i - sx); if (x < nx - 1) nb.push(i + sx);
      if (y > 0) nb.push(i - sy); if (y < ny - 1) nb.push(i + sy);
      if (z > 0) nb.push(i - 1); if (z < nz - 1) nb.push(i + 1);
      for (const j of nb) if (mask[j] && !seen[j]) { seen[j] = 1; q[tail++] = j; }
    }
    if (n > bestN) { bestN = n; best.fill(0); for (const i of comp) best[i] = 1; }
  }
  return best;
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

export function fillNearest(labels, holes, dims) {
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
      for (const j of nb) { const lv = out[j]; if (lv > 0) counts.set(lv, (counts.get(lv) || 0) + 1); }
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
  return fillNearest(out, re, dims);
}

export function suplongInferiorClip(labels, dims) {
  const [nx, ny, nz] = dims;
  const out = Int32Array.from(labels);
  let zmin = Infinity;
  for (let i = 0; i < out.length; i++) if (out[i] === VERTICAL) { const z = i % nz; if (z < zmin) zmin = z; }
  if (!isFinite(zmin)) return out;
  const below = new Uint8Array(out.length);
  for (let i = 0; i < out.length; i++) { const z = i % nz; if (out[i] === SUPLONG && z < zmin) { below[i] = 1; out[i] = 0; } }
  return fillNearest(out, below, dims);
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

export function cleanLabels(labels, dims, iters = 1) {
  const out = new Int32Array(labels.length);
  for (const k of LABELS) {
    const m = new Uint8Array(labels.length);
    let any = false;
    for (let i = 0; i < labels.length; i++) if (labels[i] === k) { m[i] = 1; any = true; }
    if (!any) continue;
    let e = erodeN(m, dims, iters);
    e = largestCC(e, dims);
    const d = dilateN(e, dims, iters);
    for (let i = 0; i < labels.length; i++) if (d[i] && m[i]) out[i] = k;
  }
  const holes = new Uint8Array(labels.length);
  let anyHole = false;
  for (let i = 0; i < labels.length; i++) if (labels[i] > 0 && out[i] === 0) { holes[i] = 1; anyHole = true; }
  return anyHole ? fillNearest(out, holes, dims) : out;
}

function gaussianKernel(sigma) {
  const radius = Math.max(1, Math.floor(4.0 * sigma + 0.5));
  const k = new Float64Array(2 * radius + 1);
  let s = 0;
  for (let r = -radius; r <= radius; r++) { const w = Math.exp(-(r * r) / (2 * sigma * sigma)); k[r + radius] = w; s += w; }
  for (let i = 0; i < k.length; i++) k[i] /= s;
  return k;
}

function gaussianBlur(mask, dims, sigma) {
  const kern = gaussianKernel(sigma), pad = (kern.length - 1) / 2;
  let x = Float64Array.from(mask);
  for (let axis = 0; axis < 3; axis++) x = conv1dReflect(x, dims, axis, kern, pad);
  return x;
}

function conv1dReflect(f, dims, axis, kern, pad) {
  const [nx, ny, nz] = dims, n = dims[axis];
  const stride = axis === 0 ? ny * nz : axis === 1 ? nz : 1;
  const a1 = axis === 0 ? 1 : 0, a2 = axis === 0 ? 2 : (axis === 1 ? 2 : 1);
  const d = [nx, ny, nz], out = new Float64Array(f.length), line = new Float64Array(n);
  const refl = (q) => { // numpy.pad 'reflect' (no edge repeat): abc -> ...b|abc|b...
    if (n === 1) return 0;
    const p = 2 * (n - 1); let m = ((q % p) + p) % p; return m < n ? m : p - m;
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

export function smoothLabels(labels, dims, sigma = 0.8) {
  const blurred = LABELS.map(k => {
    const m = new Uint8Array(labels.length);
    for (let i = 0; i < labels.length; i++) m[i] = labels[i] === k ? 1 : 0;
    return gaussianBlur(m, dims, sigma);
  });
  const out = new Int32Array(labels.length);
  for (let i = 0; i < labels.length; i++) {
    if (labels[i] === 0) continue;
    let best = 0, bv = -Infinity;
    for (let j = 0; j < LABELS.length; j++) if (blurred[j][i] > bv) { bv = blurred[j][i]; best = LABELS[j]; }
    out[i] = best;
  }
  return out;
}

export function runPipeline(labels, image, dims, zooms, steps) {
  let out = Int32Array.from(labels);
  if (steps.thin) out = thinSuplong(out, dims, zooms, steps.keepFrac ?? 0.88);
  if (steps.inferiorClip) out = suplongInferiorClip(out, dims);
  if (steps.surfaceTrim) {
    const [lo, hi] = tissueBand(image, out, steps.trimK ?? 1.5);
    const which = (steps.trimAll ?? true) ? LABELS : [SUPLONG];
    out = surfaceTrim(out, image, dims, lo, hi, 6, which);
  }
  if (steps.clean) out = cleanLabels(out, dims, steps.cleanIters ?? 1);
  if (steps.smooth) out = smoothLabels(out, dims, steps.sigma ?? 0.8);
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
