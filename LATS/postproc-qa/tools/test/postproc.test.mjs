// Parity: the JS port must match reference/postproc_ref.py on the golden phantom.
// Run: npm test (regenerates the fixtures first)
import { test } from 'node:test';
import assert from 'node:assert';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';
import * as P from '../../web/js/postproc.js';

const G = join(dirname(fileURLToPath(import.meta.url)), '..', 'golden');
const load = (n) => JSON.parse(readFileSync(join(G, n + '.json'), 'utf8'));

const meta = load('meta');
const zooms = meta.zooms;
const lab = load('phantom_label');
const dims = lab.dims;
const L = Int32Array.from(lab.data);
const img = Float32Array.from(load('phantom_image').data);
const CUTS = { inferiorClip: true, slTrim: 2, ilTrim: 3, trimSd: 2, followDorsum: true, order: true, darkSd: 3, probCut: 0.99, smooth: true, sigma: 1, genio: true, shave: true, clean: true };
const GENTLE = { thin: true, inferiorClip: true, slCap: true, slTrim: 2, ilTrim: 3, trimSd: 2, followDorsum: true, order: true, smooth: true, sigma: 1, genio: true, clean: true };
const u8 = (a) => Uint8Array.from(a.map(v => v ? 1 : 0));

const eqInt = (a, golden, name) => {
  const g = Int32Array.from(golden.data);
  let diff = 0;
  for (let i = 0; i < g.length; i++) if (a[i] !== g[i]) diff++;
  assert.strictEqual(diff, 0, `${name}: ${diff} voxels differ`);
};

test('erode6', () => eqInt(P.erode6(u8(lab.data), dims), load('erode6'), 'erode6'));
test('majorCC (sup-long, and a paired muscle with a speck)', () => {
  const sl = Uint8Array.from(lab.data.map(v => v === 2 ? 1 : 0));
  eqInt(P.majorCC(sl, dims), load('major_cc'), 'majorCC');
  const pair = Uint8Array.from(load('pair_mask').data), kept = P.majorCC(pair, dims);
  eqInt(kept, load('major_cc_pair'), 'majorCC pair');
  const n = (a) => a.reduce((s, v) => s + (v ? 1 : 0), 0);
  assert.strictEqual(n(kept), 6 * 7 * 6 + 5 * 7 * 6, 'both bundles kept, speck dropped');
});
test('edt (max abs err < 1e-4 mm)', () => {
  const e = P.edt(u8(lab.data), dims, zooms), g = Float32Array.from(load('edt').data);
  let m = 0; for (let i = 0; i < g.length; i++) m = Math.max(m, Math.abs(e[i] - g[i]));
  assert.ok(m < 1e-4, `edt max err ${m}`);
});
test('tissueBand', () => {
  const [lo, hi] = P.tissueBand(img, L), g = load('tissue_band');
  assert.ok(Math.abs(lo - g.lo) < 1e-6 && Math.abs(hi - g.hi) < 1e-6, `band [${lo},${hi}] vs [${g.lo},${g.hi}]`);
});
test('thinSuplong', () => eqInt(P.thinSuplong(L, dims, zooms, 0.88), load('thin'), 'thin'));
test('suplongInferiorClip', () => eqInt(P.suplongInferiorClip(L, dims), load('inferior_clip'), 'inferior_clip'));
test('slCap fills gaps and holes in the superior longitudinal cap, not its edge', () => {
  const g = load('cap_label'), C = Int32Array.from(g.data), out = P.slCap(C, g.dims);
  eqInt(out, load('sl_cap'), 'sl_cap');
  const [, ny, nz] = g.dims, at = (x, y, z) => out[(x * ny + y) * nz + z];
  assert.strictEqual(at(10, 11, 16), 2, 'gap filled');
  assert.strictEqual(at(8, 16, 16), 2, 'hole filled');
  assert.strictEqual(at(3, 10, 16), 1, 'edge row left as vertical');
  assert.deepStrictEqual([at(10, 11, 13), at(10, 11, 14)], [1, 2], 'gap filled at the median cap thickness, not the thickest');
});
test('surfaceTrim', () => {
  const g = load('tissue_band');
  eqInt(P.surfaceTrim(L, img, dims, g.lo, g.hi), load('surface_trim'), 'surface_trim');
});
test('cleanLabels', () => eqInt(P.cleanLabels(L, dims, zooms), load('clean'), 'clean'));
test('followDorsum', () => {
  const post = Int32Array.from(load('follow_post').data);
  eqInt(P.followDorsum(L, post, dims), load('follow_dorsum'), 'follow_dorsum');
});
test('smoothLabels', () => eqInt(P.smoothLabels(L, dims, zooms, 1.0), load('smooth'), 'smooth'));

test('muscleBand and orderInflong match the reference', () => {
  const g = JSON.parse(readFileSync(join(G, 'muscle_band.json'), 'utf8'));
  for (const k of [2, 3]) P.muscleBand(img, L, dims, k, 2).forEach((v, j) => assert.ok(Math.abs(v - g[k][j]) < 1e-6 * Math.max(1, Math.abs(v))));
  eqInt(P.orderInflong(Int32Array.from(load('order_label').data), dims), load('order_inflong'), 'order_inflong');
  eqInt(P.genioRules(Int32Array.from(load('genio_label').data), dims, zooms), load('genio_rules'), 'genio_rules');
  const gf = load('genio_front_label');
  eqInt(P.genioRules(Int32Array.from(gf.data), gf.dims, zooms), load('genio_front'), 'genio_front');
  const I2 = Float32Array.from(load('il_image').data), t = P.surfaceTrim(L, I2, dims, ...P.muscleBand(I2, L, dims, 3, 2), 3, [3]);
  eqInt(t, load('il_trim'), 'il_trim');
  assert.ok(t.filter(v => v === 3).length < L.filter(v => v === 3).length, 'the trim removed inferior longitudinal surface');
});
test('darkCut and the confidence cut match the reference', () => {
  const dl = load('dark_label');
  eqInt(P.darkCut(Int32Array.from(dl.data), Float64Array.from(load('dark_image').data), dl.dims, 3), load('dark_cut'), 'dark_cut');
  eqInt(P.shaveSide(L, dims), load('shave_side'), 'shave_side');
  eqInt(P.runPipeline(L, img, dims, zooms, CUTS, Float64Array.from(load('tongue_prob').data)), load('pipeline_cuts'), 'pipeline_cuts');
});
test('the move matches the reference, and the pipeline with it', () => {
  const tp = Float64Array.from(load('tongue_prob').data), deg = [3, -4, 2], mm = [1, -2, 1];
  const [m, mp] = P.moveVolumes([L, tp], dims, zooms, P.moveRotation(deg), mm, P.tongueCentre(L, dims, zooms));
  eqInt(m, load('move_label'), 'move_label');
  assert.deepStrictEqual(Array.from(mp), load('move_prob').data, 'the confidence map moves with the labels');
  assert.ok(load('move_label').data.some((v, i) => v !== L[i]), 'the move changes the labels');
  eqInt(P.runPipeline(L, img, dims, zooms, { ...CUTS, mvX: 1, mvY: -2, mvZ: 1, mvPitch: 3, mvYaw: -4, mvRoll: 2 }, tp), load('pipeline_move'), 'pipeline_move');
  const [, ny, nz] = dims, up = P.moveVolumes([L], dims, zooms, P.moveRotation([0, 0, 0]), [0, 0, zooms[2]], [0, 0, 0])[0];
  assert.ok(Array.from(up).every((v, q) => v === (q % nz === 0 ? 0 : L[q - 1])), 'up is +z, one voxel');
  const one = new Int32Array(9 ** 3), at = (x, y, z) => (x * 9 + y) * 9 + z;
  one[at(6, 4, 4)] = 1;
  const yaw = P.moveVolumes([one], [9, 9, 9], [1, 1, 1], P.moveRotation([0, 90, 0]), [0, 0, 0], [4, 4, 4])[0];
  assert.deepStrictEqual([yaw[at(4, 6, 4)], yaw.reduce((a, b) => a + b)], [1, 1], 'yaw +90: right turns to forward');
});
test('mirror cut and the pipeline with it match the reference', () => {
  const g = load('mirror_plane'), ml = load('mirror_label'), M = Int32Array.from(ml.data), tp = Float64Array.from(load('mirror_prob').data);
  eqInt(P.mirrorCut(M, ml.dims, zooms, g.affine, g, tp), load('mirror_cut'), 'mirror_cut');
  eqInt(P.runPipeline(M, img, ml.dims, zooms, { shave: true, clean: true, mirror: true }, tp, g), load('pipeline_mirror'), 'pipeline_mirror');
  const off = P.runPipeline(M, img, ml.dims, zooms, { shave: true, clean: true, mirror: false }, tp, g);
  assert.ok(off.filter(v => v === 1).length > load('pipeline_mirror').data.filter(v => v === 1).length, 'off by default: nothing cut');
});
test('runPipeline gentle (full)', () => eqInt(P.runPipeline(L, img, dims, zooms, GENTLE), load('pipeline_gentle'), 'pipeline_gentle'));

test('midline planes, adjustment and left/right counts', () => {
  const g = load('midline'), ml = load('midline_label'), M = Int32Array.from(ml.data);
  const close = (a, b, name) => a.forEach((x, i) => assert.ok(Math.abs(x - b[i]) < 1e-6, `${name}: ${a} vs ${b}`));
  for (const mode of ['gg', 'tilt']) {
    const p = P.midlinePlane(M, ml.dims, g.affine, mode), e = g[mode];
    close(p.point, e.point, mode + ' point'); close(p.normal, e.normal, mode + ' normal');
    const c = P.lateralCounts(M, ml.dims, g.affine, p);
    assert.deepStrictEqual([c.left, c.right], e.counts, mode + ' counts');
    const a = P.adjustPlane(p, 1.5, 7, -4);
    close(a.point, e.adj_point, mode + ' adjusted point'); close(a.normal, e.adj_normal, mode + ' adjusted normal');
    const ca = P.lateralCounts(M, ml.dims, g.affine, a);
    assert.deepStrictEqual([ca.left, ca.right], e.adj_counts, mode + ' adjusted counts');
  }
});

test('shapeChecks: cap coverage, inferior longitudinal sides, genioglossus offset', () => {
  const dims = [10, 10, 8], [nx, ny, nz] = dims, L = new Int32Array(nx * ny * nz), at = (x, y, z) => (x * ny + y) * nz + z;
  for (let x = 1; x <= 8; x++) for (let y = 1; y <= 8; y++) for (let z = 1; z <= 5; z++) L[at(x, y, z)] = z === 5 ? 2 : 1;
  for (const [x, y] of [[1, 1], [1, 2], [2, 1], [2, 2]]) L[at(x, y, 5)] = 1;
  for (const y of [3, 4, 5, 6]) L[at(2, y, 2)] = 3;
  for (const y of [3, 4]) L[at(7, y, 2)] = 3;
  L[at(5, 5, 2)] = 4; L[at(6, 5, 2)] = 4;
  const eye = [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]];
  const c = P.shapeChecks(L, dims, eye, { point: [4.5, 0, 0], normal: [1, 0, 0] });
  assert.strictEqual(c.capPct, 100 * 60 / 64);
  assert.deepStrictEqual([c.ilLeft, c.ilRight, c.ilRatio], [4, 2, 0.5]);
  assert.strictEqual(c.ggOffsetMm, 1);
  assert.deepStrictEqual(Object.keys(P.SHAPE_FLAGS).filter(k => P.SHAPE_FLAGS[k](c[k])), []);
  assert.ok(P.SHAPE_FLAGS.ilRatio(0.49) && P.SHAPE_FLAGS.capPct(89.9) && P.SHAPE_FLAGS.ggOffsetMm(-3.1));
});

const count = (a, v) => a.reduce((n, x) => n + (x === v ? 1 : 0), 0);
test('thin and clip relabel removed superior longitudinal as another muscle', () => {
  const thin = P.thinSuplong(L, dims, zooms, 0.88), d = P.edt(Uint8Array.from(L, v => v > 0 ? 1 : 0), dims, zooms);
  const sl = []; L.forEach((v, i) => { if (v === 2) sl.push(d[i]); }); sl.sort((a, b) => a - b);
  const deep = L.map((v, i) => v === 2 && d[i] > sl[Math.ceil(0.88 * (sl.length - 1))] ? 1 : 0);
  assert.ok(count(deep, 1) > 100 && thin.every((v, i) => !deep[i] || v !== 2), 'deep superior longitudinal relabelled');
  const clip = P.suplongInferiorClip(L, dims), [, ny, nz] = dims;
  let zmin = nz; L.forEach((v, i) => { if (v === 1) zmin = Math.min(zmin, i % nz); });
  assert.strictEqual(clip.filter((v, i) => v === 2 && i % nz < zmin).length, 0, 'no superior longitudinal below the lowest vertical');
  assert.strictEqual(count(clip, 0), count(L, 0) + 8, 'only the detached 8-voxel speck is removed');
});

test('clean keeps a small (atrophied) inferior longitudinal side', () => {
  const dims = [24, 14, 12], [nx, ny, nz] = dims, V = new Int32Array(nx * ny * nz), at = (x, y, z) => (x * ny + y) * nz + z;
  for (let x = 1; x < 23; x++) for (let y = 1; y < 13; y++) for (let z = 1; z < 11; z++) V[at(x, y, z)] = 1;
  for (let x = 3; x < 9; x++) for (let y = 3; y < 10; y++) for (let z = 3; z < 9; z++) V[at(x, y, z)] = 3;     // healthy side
  for (let x = 16; x < 19; x++) for (let y = 4; y < 9; y++) for (let z = 4; z < 9; z++) V[at(x, y, z)] = 3;    // thin side
  const out = P.cleanLabels(V, dims, [1, 1, 1]);
  let small = 0; for (let x = 16; x < 19; x++) for (let y = 4; y < 9; y++) for (let z = 4; z < 9; z++) small += out[at(x, y, z)] === 3;
  assert.ok(small > 0, 'thin side kept');
});

test('clean keeps a 2-voxel-thick genioglossus side and drops specks', () => {
  const dims = [24, 14, 12], [nx, ny, nz] = dims, V = new Int32Array(nx * ny * nz), at = (x, y, z) => (x * ny + y) * nz + z;
  const box = (x0, x1, y0, y1, z0, z1, v) => { for (let x = x0; x < x1; x++) for (let y = y0; y < y1; y++) for (let z = z0; z < z1; z++) V[at(x, y, z)] = v; };
  box(1, 23, 1, 13, 1, 11, 1);
  box(3, 9, 3, 10, 3, 9, 4);        // healthy genioglossus side, 252 voxels
  box(16, 18, 3, 10, 3, 9, 4);      // atrophied side, 2 voxels thick, 84 voxels (a third)
  box(12, 14, 2, 3, 9, 10, 4);      // genioglossus speck, 2 voxels
  box(12, 13, 11, 12, 2, 7, 3);     // inferior longitudinal speck, 5 voxels
  const out = P.cleanLabels(V, dims, [1, 1, 1]), n = (v) => out.filter((w) => w === v).length;
  assert.strictEqual(n(4), 252 + 84, 'both genioglossus sides kept whole, speck dropped');
  assert.strictEqual(n(3), 0, 'inferior longitudinal speck dropped');
  assert.strictEqual(out.filter((w) => w > 0).length, V.filter((w) => w > 0).length, 'no tissue lost');
});

test('clean removes a blob outside the tongue and gives an inner speck to its neighbour', () => {
  const dims = [16, 16, 16], [, ny, nz] = dims, V = new Int32Array(16 ** 3), at = (x, y, z) => (x * ny + y) * nz + z;
  for (let x = 2; x < 14; x++) for (let y = 2; y < 14; y++) for (let z = 2; z < 10; z++) V[at(x, y, z)] = x < 7 ? 4 : 1;
  for (let x = 9; x < 11; x++) for (let y = 7; y < 9; y++) for (let z = 10; z < 13; z++) V[at(x, y, z)] = 4;
  V[at(12, 10, 5)] = 4;
  const out = P.cleanLabels(V, dims, [1, 1, 1]);
  assert.strictEqual([9, 10].flatMap(x => [7, 8].flatMap(y => [10, 11, 12].map(z => out[at(x, y, z)]))).filter(v => v).length, 0);
  assert.strictEqual(out[at(12, 10, 5)], 1);
});

test('rasOrder: a permuted and flipped copy processes like the original', () => {
  const [nx, ny, nz] = dims, D = [nz, nx, ny], Q = new Int32Array(L.length), I = new Float32Array(img.length);
  for (let x = 0; x < nx; x++) for (let y = 0; y < ny; y++) for (let z = 0; z < nz; z++) {   // new axes: -z, x, -y
    const j = ((nz - 1 - z) * nx + x) * ny + (ny - 1 - y), i = (x * ny + y) * nz + z;
    Q[j] = L[i]; I[j] = img[i];
  }
  const A = [[0, zooms[0], 0, 0], [0, 0, -zooms[1], (ny - 1) * zooms[1]], [-zooms[2], 0, 0, (nz - 1) * zooms[2]], [0, 0, 0, 1]];
  const o = P.rasOrder(D, [zooms[2], zooms[0], zooms[1]], A);
  assert.deepStrictEqual([o.dims, o.zooms, o.identity], [dims, zooms, false]);
  o.affine.forEach((r, k) => r.forEach((v, c) => assert.ok(Math.abs(v - (k === c ? (k < 3 ? zooms[k] : 1) : 0)) < 1e-9, 'affine')));
  eqInt(P.toRas(Q, o), { data: Array.from(L) }, 'toRas');
  assert.deepStrictEqual(P.fromRas(P.toRas(Q, o), o), Q, 'fromRas undoes toRas');
  eqInt(P.toRas(P.fromRas(P.runPipeline(P.toRas(Q, o), P.toRas(I, o), o.dims, o.zooms, GENTLE), o), o), load('pipeline_gentle'), 'pipeline');
  assert.ok(P.rasOrder(dims, zooms, [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]).identity);
});

test('no midline from an empty label map; no genioglossus offset without a midline', () => {
  const eye = [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]];
  assert.strictEqual(P.midlinePlane(new Int32Array(27), [3, 3, 3], eye, 'tilt'), null);
  const c = P.shapeChecks(L, dims, eye, null);
  assert.strictEqual(c.ggOffsetMm, null);
  assert.ok(!P.SHAPE_FLAGS.ggOffsetMm(c.ggOffsetMm));
});
