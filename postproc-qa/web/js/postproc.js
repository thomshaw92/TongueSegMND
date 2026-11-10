// Tongue segmentation post-processing; must match reference/postproc_ref.py voxel for voxel.
// Volumes are flat typed arrays in C order: idx = (x*ny + y)*nz + z.
// Labels: 0 bg, 1 vertical, 2 superior-long, 3 inferior-long, 4 genioglossus.

export const SUPLONG = 2, VERTICAL = 1, GENIO = 4;
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
  const pre = Int32Array.from(out);
  if (steps.surfaceTrim) {
    const [lo, hi] = tissueBand(image, out, steps.trimK ?? 1.5);
    const which = (steps.trimAll ?? true) ? LABELS : [SUPLONG];
    out = surfaceTrim(out, image, dims, lo, hi, 6, which);
  }
  if (steps.clean) out = cleanLabels(out, dims, steps.cleanIters ?? 1);
  if (steps.followDorsum) out = followDorsum(pre, out, dims);
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
