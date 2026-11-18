import { test } from 'node:test';
import assert from 'node:assert';
import { brushVoxels, applyTool } from '../../web/js/edit.js';

const ijk = (i, dims) => { const [, ny, nz] = dims, z = i % nz, y = ((i - z) / nz) % ny; return [(i - z - y * nz) / (ny * nz), y, z]; };
const world = (A, v) => [0, 1, 2].map(r => A[r][0] * v[0] + A[r][1] * v[1] + A[r][2] * v[2] + A[r][3]);

test('brush: a disc in the displayed slice, for any voxel orientation', () => {
  const dims = [12, 12, 12], I = [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]];
  assert.strictEqual(brushVoxels(dims, I, [5, 5, 5], 2, 1).idx.length, 1);
  const b = brushVoxels(dims, I, [5, 5, 5], 2, 3);
  assert.strictEqual(b.idx.length, 9);
  assert.ok(b.idx.every(i => ijk(i, dims)[2] === 5), 'axial disc stays on one slice');
  // stored PIR: i runs posterior (-y), j runs inferior (-z), k runs right (+x)
  const P = [[0, 0, 1, -6], [-1, 0, 0, 6], [0, -1, 0, 6], [0, 0, 0, 1]];
  for (const axis of [0, 1, 2]) {
    const { idx } = brushVoxels(dims, P, [0, 0, 0], axis, 3);
    assert.strictEqual(idx.length, 9, `axis ${axis}`);
    const w = idx.map(i => world(P, ijk(i, dims))[axis]);
    assert.ok(w.every(v => v === w[0]), `axis ${axis}: every voxel on the same world slice`);
  }
  // 3 mm slices: a coronal disc 3 voxels across spans one slice in z
  const T = [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 3, 0], [0, 0, 0, 1]];
  assert.strictEqual(brushVoxels(dims, T, [5, 5, 15], 1, 3).idx.length, 3);
  assert.strictEqual(brushVoxels(dims, I, [50, 5, 5], 2, 3).idx.length, 0, 'outside the volume');
  // oblique (tilted 10 degrees about x, as LATS templates can be): an axial disc stays on one voxel slice, as NiiVue shows it
  const t = 10 * Math.PI / 180, O = [[1, 0, 0, 0], [0, Math.cos(t), -Math.sin(t), 0], [0, Math.sin(t), Math.cos(t), 0], [0, 0, 0, 1]];
  const ob = brushVoxels([12, 12, 12], O, world(O, [5, 5, 5]), 2, 11).idx;
  let disc = 0; for (let a = -5; a <= 5; a++) for (let b2 = -5; b2 <= 5; b2++) disc += Math.hypot(a, b2) <= 5.5 && a + 5 < 12 && b2 + 5 < 12;
  assert.strictEqual(ob.length, disc);
  assert.ok(ob.every(i => ijk(i, dims)[2] === 5), 'oblique axial disc on one voxel slice');
});

test('tools: paint, erase, one-layer erode and dilate', () => {
  const dims = [9, 9, 9], [, ny, nz] = dims, at = (x, y, z) => (x * ny + y) * nz + z, all = [...Array(9 ** 3).keys()];
  const cube = () => { const L = new Int32Array(9 ** 3); for (let x = 2; x < 7; x++) for (let y = 2; y < 7; y++) for (let z = 2; z < 7; z++) L[at(x, y, z)] = x < 4 ? 2 : 1; return L; };
  let L = cube();
  applyTool(L, dims, [at(4, 4, 4)], 'paint', [3]); assert.strictEqual(L[at(4, 4, 4)], 3);
  applyTool(L, dims, [at(4, 4, 4)], 'erase', [3]); assert.strictEqual(L[at(4, 4, 4)], 0);
  L = cube();
  const er = applyTool(L, dims, all, 'erode', [1]), n1 = cube().filter(v => v === 1).length;
  assert.ok(er.length > 0 && er.every(([, from, to]) => from === 1 && to === 0), 'only muscle 1 changes, to background');
  assert.strictEqual(L[at(5, 4, 4)], 1, 'interior kept: one layer per pass');
  assert.strictEqual(L[at(4, 4, 4)], 1, 'the boundary with muscle 2 stays');
  assert.strictEqual(L[at(6, 4, 4)], 0, 'outer surface goes to background');
  assert.ok(L.filter(v => v === 1).length < n1);
  L = cube();
  const di = applyTool(L, dims, all, 'dilate', [1]);
  assert.ok(di.length > 0 && di.every(([, from, to]) => from === 0 && to === 1), 'grows only into background');
  assert.strictEqual(L[at(3, 4, 4)], 2, 'the other muscle stays');
  assert.strictEqual(L[at(7, 4, 4)], 1); assert.strictEqual(L[at(8, 4, 4)], 0, 'one layer');
  L = cube();
  const both = applyTool(L, dims, all, 'erode', [1, 2]);
  assert.ok(both.some(([, f]) => f === 1) && both.some(([, f]) => f === 2) && both.every(([, , to]) => to === 0), 'erodes both chosen muscles');
  assert.deepStrictEqual([L[at(3, 4, 4)], L[at(4, 4, 4)]], [2, 1], 'the boundary between them stays');
  for (const tool of ['erode', 'dilate']) {
    const once = cube(), twice = cube(), ref = cube();
    applyTool(once, dims, all, tool, [1]);
    applyTool(twice, dims, all, tool, [1], ref); applyTool(twice, dims, all, tool, [1], ref);
    assert.deepStrictEqual(twice, once, `${tool}: passes decided from the stroke's start do one layer in all`);
  }
});

test('dilate with several muscles: contested background goes to the one with more voxels next to it', () => {
  const dims = [3, 3, 3], at = (x, y, z) => (x * 3 + y) * 3 + z, c = at(1, 1, 1), L = new Int32Array(27);
  L[at(0, 1, 1)] = 1; L[at(2, 1, 1)] = 2; L[at(1, 0, 1)] = 2;
  applyTool(L, dims, [c], 'dilate', [1, 2]); assert.strictEqual(L[c], 2, 'two neighbours of 2 beat one of 1');
  L[c] = 0; applyTool(L, dims, [c], 'dilate', [1]); assert.strictEqual(L[c], 1, 'an unchosen muscle never wins');
  L[c] = 0; L[at(1, 0, 1)] = 0; applyTool(L, dims, [c], 'dilate', [1, 2]); assert.strictEqual(L[c], 1, 'tie: the first chosen');
});
