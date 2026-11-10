import { test } from 'node:test';
import assert from 'node:assert';
import * as Q from '../../web/js/qa.js';

const tree = {
  'derivatives/lats/longitudinal': ['sub-01/', 'sub-02/', 'notes.txt'],
  'derivatives/lats/longitudinal/sub-01/sst': ['T_template0.nii.gz', 'sst_seg.nii.gz', 'sst_seg_refined.nii.gz', 'midline.json'],
  'derivatives/lats/longitudinal/sub-02/sst': ['sst_seg.nii.gz'],
  'derivatives/lats/cross-sectional': ['sub-03_ses-01/'],
  'derivatives/lats/cross-sectional/sub-03_ses-01/sst': ['T_template0.nii.gz', 'sst_seg.nii.gz'],
};
const runs = {
  'derivatives/lats/longitudinal/sub-01/lats_run.json': { timepoints: [
    { index: 0, image: '/in/sub-01_ses-01_acq-mp2rage_T1w.nii.gz' }, { index: 1, image: '/in/sub-01_ses-2_T1w.nii.gz' }] },
  'derivatives/lats/cross-sectional/sub-03_ses-01/lats_run.json': { timepoints: [{ index: 0, image: '/x/sub-03_ses-01_T1w.nii.gz' }] },
};
const list = async (p) => (tree[p.join('/')] || []).map(n => ({ name: n.replace(/\/$/, ''), kind: n.endsWith('/') ? 'directory' : 'file' }));
const readJson = async (p) => runs[p.join('/')] || null;

test('names follow BIDS and the image', () => {
  assert.strictEqual(Q.labelsName('sub-01_ses-01_acq-mp2rage_T1w.nii.gz'), 'sub-01_ses-01_acq-mp2rage_desc-qa_dseg.nii.gz');
  assert.strictEqual(Q.templateLabelsName('sub-01'), 'sub-01_space-sst_desc-qa_dseg.nii.gz');
  assert.strictEqual(Q.templateLabelsName('sub-03', 'ses-01'), 'sub-03_ses-01_space-sst_desc-qa_dseg.nii.gz');
  assert.strictEqual(Q.sidecarName('a_desc-qa_dseg.nii.gz'), 'a_desc-qa_dseg.json');
});

test('discover finds templates and sessions, skips incomplete runs', async () => {
  const items = await Q.discover(list, readJson);
  assert.deepStrictEqual(items.map(i => `${i.pass}:${i.id}`),
    ['template:sub-01', 'session:sub-01_ses-01', 'session:sub-01_ses-2', 'template:sub-03_ses-01', 'session:sub-03_ses-01_single']);
  const t = items[0];
  assert.strictEqual(t.labelsIn.at(-1), 'sst_seg_refined.nii.gz');
  assert.deepStrictEqual(t.midline, ['derivatives', 'lats', 'longitudinal', 'sub-01', 'sst', 'midline.json']);
  assert.deepStrictEqual(t.qaFolder, ['derivatives', 'lats-qa', 'sub-01']);
  const s = items[2];
  assert.deepStrictEqual(s.image, ['sub-01', 'ses-2', 'anat', 'sub-01_ses-2_T1w.nii.gz']);
  assert.strictEqual(s.labelsIn.at(-1), 'tp01.nii.gz');
  assert.strictEqual(s.labels, 'sub-01_ses-2_desc-qa_dseg.nii.gz');
  assert.deepStrictEqual(items[3].qaFolder, ['derivatives', 'lats-qa', 'sub-03', 'ses-01']);
  assert.strictEqual(items[4].labels, 'sub-03_ses-01_desc-qasingle_dseg.nii.gz');
});

test('records, tsv and resume', async () => {
  const items = await Q.discover(list, readJson);
  let r = Q.newRecord(items[0]);
  r = Q.touch({ ...r, mri_quality: 4, seg_quality_raw: 2, seg_quality_final: 4, status: 'done', decision: 'include', notes: 'tab\there' }, 'TS', 'save', new Date('2026-11-11T09:00:00Z'));
  assert.strictEqual(r.rater, 'TS'); assert.strictEqual(r.updated, '2026-11-11T09:00:00.000Z'); assert.strictEqual(r.history.length, 1);
  const lines = Q.toTsv([r]).trim().split('\n');
  assert.strictEqual(lines[0].split('\t').length, Q.TSV_COLUMNS.length);
  const row = Object.fromEntries(Q.TSV_COLUMNS.map((c, i) => [c, lines[1].split('\t')[i]]));
  assert.strictEqual(row.mri_disposition, 'Pass'); assert.strictEqual(row.seg_disposition, 'Pass'); assert.strictEqual(row.notes, 'tab here');
  const recs = { 'template:sub-01': r };
  assert.strictEqual(Q.nextToDo(items, recs), 1);
  assert.strictEqual(Q.nextToDo(items, recs, 4), 1);
  const all = Object.fromEntries(items.map(i => [i.pass + ':' + i.id, { status: 'done' }]));
  assert.strictEqual(Q.nextToDo(items, all), -1);
});
