// The NIfTI reader must reproduce the nibabel-written phantoms voxel for voxel.
import { test } from 'node:test';
import assert from 'node:assert';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';
import { readNifti, writeNiftiLabels } from '../../web/js/nifti.js';

const G = join(dirname(fileURLToPath(import.meta.url)), '..', 'golden');
const nii = (n) => { const b = readFileSync(join(G, n)); return readNifti(b.buffer.slice(b.byteOffset, b.byteOffset + b.length)); };
const json = (n) => JSON.parse(readFileSync(join(G, n), 'utf8'));

// exact match: same dims and no differing voxel
const same = (got, want, name) => {
  assert.deepStrictEqual(got.dims, want.dims, `${name}: dims`);
  let diff = 0;
  for (let i = 0; i < want.data.length; i++) if (got.dataC[i] !== want.data[i]) diff++;
  assert.strictEqual(diff, 0, `${name}: ${diff} voxels differ`);
};

test('readNifti: uint8 label phantom', () => same(nii('phantom_label.nii'), json('phantom_label.json'), 'labels'));
test('readNifti: float32 image phantom', () => same(nii('phantom_image.nii'), json('phantom_image.json'), 'image'));
test('writeNiftiLabels -> readNifti round trip', () => {
  const lab = nii('phantom_label.nii');
  const bytes = writeNiftiLabels(lab.header, lab.dataC, lab.dims);
  same(readNifti(bytes.buffer), json('phantom_label.json'), 'round trip');
});
