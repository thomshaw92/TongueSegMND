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
const u8 = (a) => Uint8Array.from(a.map(v => v ? 1 : 0));

const eqInt = (a, golden, name) => {
  const g = Int32Array.from(golden.data);
  let diff = 0;
  for (let i = 0; i < g.length; i++) if (a[i] !== g[i]) diff++;
  assert.strictEqual(diff, 0, `${name}: ${diff} voxels differ`);
};

test('erode6', () => eqInt(P.erode6(u8(lab.data), dims), load('erode6'), 'erode6'));
test('dilate6', () => eqInt(P.dilate6(u8(lab.data), dims), load('dilate6'), 'dilate6'));
test('largestCC (sup-long)', () => {
  const sl = Uint8Array.from(lab.data.map(v => v === 2 ? 1 : 0));
  eqInt(P.largestCC(sl, dims), load('largest_cc'), 'largestCC');
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
test('surfaceTrim', () => {
  const g = load('tissue_band');
  eqInt(P.surfaceTrim(L, img, dims, g.lo, g.hi), load('surface_trim'), 'surface_trim');
});
test('cleanLabels', () => eqInt(P.cleanLabels(L, dims, 1), load('clean'), 'clean'));
test('smoothLabels', () => eqInt(P.smoothLabels(L, dims, 0.8), load('smooth'), 'smooth'));
test('runPipeline gentle (full)', () => {
  const out = P.runPipeline(L, img, dims, zooms, {
    thin: true, inferiorClip: true, surfaceTrim: true, clean: true, smooth: true,
  });
  eqInt(out, load('pipeline_gentle'), 'pipeline_gentle');
});
