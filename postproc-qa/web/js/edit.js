// Brush and tools for hand editing. Labels are in C order (z fastest).

function inv4(A) {   // inverse of an affine (last row 0 0 0 1), as its top three rows
  const [[a, b, d, tx], [e, f, g, ty], [h, i, k, tz]] = A, det = a * (f * k - g * i) - b * (e * k - g * h) + d * (e * i - f * h);
  const R = [[f * k - g * i, d * i - b * k, b * g - d * f], [g * h - e * k, a * k - d * h, d * e - a * g],
    [e * i - f * h, b * h - a * i, a * f - b * e]].map(r => r.map(v => v / det));
  return R.map(r => [...r, -(r[0] * tx + r[1] * ty + r[2] * tz)]);
}

// A disc `size` voxels across at world point `mm` on the displayed slice: the voxel plane closest to world axis `axis`
// (NiiVue does not reslice, also for oblique volumes).
export function brushVoxels(dims, affine, mm, axis, size) {
  const [nx, ny, nz] = dims, Ai = inv4(affine);
  const c = Ai.map(row => Math.round(row[0] * mm[0] + row[1] * mm[1] + row[2] * mm[2] + row[3]));
  if (c.some((v, j) => v < 0 || v >= dims[j])) return { centre: -1, idx: [] };
  const vox = [0, 1, 2].map(j => Math.hypot(affine[0][j], affine[1][j], affine[2][j]));
  const through = [0, 1, 2].reduce((b, j) => (Math.abs(affine[axis][j]) / vox[j] > Math.abs(affine[axis][b]) / vox[b] ? j : b), 0);
  const radius = size / 2 * Math.min(...vox), reach = vox.map((v, j) => (j === through ? 0 : Math.floor(radius / v))), idx = [];
  for (let x = Math.max(0, c[0] - reach[0]); x <= Math.min(nx - 1, c[0] + reach[0]); x++)
    for (let y = Math.max(0, c[1] - reach[1]); y <= Math.min(ny - 1, c[1] + reach[1]); y++)
      for (let z = Math.max(0, c[2] - reach[2]); z <= Math.min(nz - 1, c[2] + reach[2]); z++)
        if (Math.hypot((x - c[0]) * vox[0], (y - c[1]) * vox[1], (z - c[2]) * vox[2]) <= radius) idx.push((x * ny + y) * nz + z);
  return { centre: (c[0] * ny + c[1]) * nz + c[2], idx };
}

// One layer per call, decided from the labels before it. erode: edge voxels of k take their most common other
// neighbour, background included; dilate: other muscles next to k become k. Returns [index, from, to] changes.
export function applyTool(lab, dims, idx, tool, k) {
  const [nx, ny, nz] = dims, sx = ny * nz, changes = [];
  const around = (i) => {
    const z = i % nz, y = ((i - z) / nz) % ny, x = (i - z - y * nz) / sx, n = [];
    if (x > 0) n.push(i - sx); if (x < nx - 1) n.push(i + sx);
    if (y > 0) n.push(i - nz); if (y < ny - 1) n.push(i + nz);
    if (z > 0) n.push(i - 1); if (z < nz - 1) n.push(i + 1);
    return n;
  };
  for (const i of idx) {
    const v = lab[i];
    let to = v;
    if (tool === 'paint') to = k;
    else if (tool === 'erase') to = 0;
    else if (tool === 'erode' && v === k) {
      const n = around(i), votes = new Map([[0, 6 - n.length]]);
      for (const j of n) if (lab[j] !== k) votes.set(lab[j], (votes.get(lab[j]) || 0) + 1);
      const best = [...votes].filter(([, c]) => c > 0).sort((a, b) => b[1] - a[1] || a[0] - b[0])[0];
      if (best) to = best[0];
    } else if (tool === 'dilate' && v > 0 && v !== k && around(i).some(j => lab[j] === k)) to = k;
    if (to !== v) changes.push([i, v, to]);
  }
  for (const [i, , to] of changes) lab[i] = to;
  return changes;
}
