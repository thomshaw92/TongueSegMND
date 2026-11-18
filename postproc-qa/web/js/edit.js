// Brush and tools for hand editing. Labels are in C order (z fastest).
import { inv4 } from './postproc.js';

// A disc `size` voxels across at world point `mm` on the displayed slice: the voxel plane closest to world axis `axis`
// (NiiVue does not reslice, also for oblique volumes).
export function brushVoxels(dims, affine, mm, axis, size) {
  const [nx, ny, nz] = dims, Ai = inv4(affine);
  const c = Ai.map(row => Math.round(row[0] * mm[0] + row[1] * mm[1] + row[2] * mm[2] + row[3]));
  if (c.some((v, j) => v < 0 || v >= dims[j])) return { idx: [] };
  const vox = [0, 1, 2].map(j => Math.hypot(affine[0][j], affine[1][j], affine[2][j]));
  const through = [0, 1, 2].reduce((b, j) => (Math.abs(affine[axis][j]) / vox[j] > Math.abs(affine[axis][b]) / vox[b] ? j : b), 0);
  const radius = size / 2 * Math.min(...vox), reach = vox.map((v, j) => (j === through ? 0 : Math.floor(radius / v))), idx = [];
  for (let x = Math.max(0, c[0] - reach[0]); x <= Math.min(nx - 1, c[0] + reach[0]); x++)
    for (let y = Math.max(0, c[1] - reach[1]); y <= Math.min(ny - 1, c[1] + reach[1]); y++)
      for (let z = Math.max(0, c[2] - reach[2]); z <= Math.min(nz - 1, c[2] + reach[2]); z++)
        if (Math.hypot((x - c[0]) * vox[0], (y - c[1]) * vox[1], (z - c[2]) * vox[2]) <= radius) idx.push((x * ny + y) * nz + z);
  return { idx };
}

// applyTool changes one layer of the chosen muscles ks (paint uses ks[0]) and returns [index, from, to] changes.
export function applyTool(lab, dims, idx, tool, ks, ref = lab) {
  const [nx, ny, nz] = dims, sx = ny * nz, changes = [];
  const around = (i) => {
    const z = i % nz, y = ((i - z) / nz) % ny, x = (i - z - y * nz) / sx, n = [];
    if (x > 0) n.push(i - sx); if (x < nx - 1) n.push(i + sx);
    if (y > 0) n.push(i - nz); if (y < ny - 1) n.push(i + nz);
    if (z > 0) n.push(i - 1); if (z < nz - 1) n.push(i + 1);
    return n;
  };
  for (const i of idx) {
    const v = ref[i];
    let to = lab[i];
    if (tool === 'paint') to = ks[0];
    else if (tool === 'erase') to = 0;
    else if (tool === 'erode' && ks.includes(v)) {
      const n = around(i);
      if (n.length < 6 || n.some(j => ref[j] === 0)) to = 0;
    } else if (tool === 'dilate' && v === 0) {
      const n = around(i).map(j => ref[j]);
      let most = 0;
      for (const k of ks) { const c = n.filter(x => x === k).length; if (c > most) { most = c; to = k; } }
    }
    if (to !== lab[i]) changes.push([i, lab[i], to]);
  }
  for (const [i, , to] of changes) lab[i] = to;
  return changes;
}
