import { test } from 'node:test';
import assert from 'node:assert';
import * as Q from '../../web/js/qa.js';

const tree = {
  'derivatives/lats/longitudinal': ['sub-01/', 'sub-02/', 'notes.txt'],
  'derivatives/lats/longitudinal/sub-01/sst': ['T_template0.nii.gz', 'sst_seg.nii.gz', 'sst_seg_refined.nii.gz', 'sst_seg_autofix.nii.gz', 'T_template0_tongueN4.nii.gz', 'sst_seg_tongue_prob.nii.gz', 'midline.json', 'prior_refine.json'],
  'derivatives/lats/longitudinal/sub-01/qc': ['flags.json'],
  'derivatives/lats/longitudinal/sub-01/pre_qa': ['midline.json'],
  'derivatives/lats/cross-sectional/sub-03_ses-01': ['sst/', 'qa_applied.json'],
  'derivatives/lats/longitudinal/sub-02/sst': ['sst_seg.nii.gz'],
  'derivatives/lats/cross-sectional': ['sub-03_ses-01/'],
  'derivatives/lats/cross-sectional/sub-03_ses-01/sst': ['T_template0.nii.gz', 'sst_seg.nii.gz'],
};
const runs = {
  'derivatives/lats/longitudinal/sub-01/lats_run.json': { timepoints: [
    { index: 0, image: '/in/sub-01_ses-01_acq-mp2rage_T1w.nii.gz' }, { index: 1, image: '/in/sub-01_ses-2_T1w.nii.gz' }] },
  'derivatives/lats/cross-sectional/sub-03_ses-01/lats_run.json': { timepoints: [{ index: 0, image: '/x/sub-03_ses-01_T1w.nii.gz' }] },
  'derivatives/lats/cross-sectional/sub-03_ses-01/qa_applied.json': { sha256: 'L3', sidecar_sha256: 'S3' },
};
const QA3 = 'derivatives/lats-qa/sub-03/ses-01/anat/sub-03_ses-01_space-sst_desc-qa_dseg';
const sha = async (p) => ({ [QA3 + '.nii.gz']: 'L3', [QA3 + '.json']: 'S3' })[p.join('/')] || null;
const list = async (p) => (tree[p.join('/')] || []).map(n => ({ name: n.replace(/\/$/, ''), kind: n.endsWith('/') ? 'directory' : 'file' }));
const readJson = async (p) => runs[p.join('/')] || null;

test('names follow BIDS and the image', () => {
  assert.strictEqual(Q.labelsName('sub-01_ses-01_acq-mp2rage_T1w.nii.gz'), 'sub-01_ses-01_acq-mp2rage_desc-qa_dseg.nii.gz');
  assert.strictEqual(Q.templateLabelsName('sub-01'), 'sub-01_space-sst_desc-qa_dseg.nii.gz');
  assert.strictEqual(Q.templateLabelsName('sub-03', 'ses-01'), 'sub-03_ses-01_space-sst_desc-qa_dseg.nii.gz');
  assert.strictEqual(Q.sidecarName('a_desc-qa_dseg.nii.gz'), 'a_desc-qa_dseg.json');
});

test('discover finds templates and sessions, skips incomplete runs', async () => {
  const items = await Q.discover(list, readJson, sha);
  assert.deepStrictEqual(items.map(i => `${i.pass}:${i.id}`),
    ['template:sub-01', 'session:sub-01_ses-01', 'session:sub-01_ses-2', 'template:sub-03_ses-01', 'session:sub-03_ses-01_single']);
  const t = items[0];
  assert.strictEqual(t.labelsIn.at(-1), 'sst_seg_refined.nii.gz', 'the untrimmed labels, so the app can redo the fixes');
  assert.strictEqual(t.image.at(-1), 'T_template0.nii.gz', 'raters see the ordinary template');
  assert.strictEqual(t.fixImage.at(-1), 'T_template0_tongueN4.nii.gz', 'the fixes use the tongue-N4 image');
  assert.strictEqual(items[3].fixImage, null);
  assert.strictEqual(items[3].image.at(-1), 'T_template0.nii.gz');
  assert.strictEqual(t.prob.at(-1), 'sst_seg_tongue_prob.nii.gz', 'tongue probability map for the confidence cut');
  assert.strictEqual(items[3].prob, null);
  assert.deepStrictEqual(t.midline, ['derivatives', 'lats', 'longitudinal', 'sub-01', 'pre_qa', 'midline.json'], 'the original LATS plane, not a re-applied one');
  assert.deepStrictEqual(t.qaFolder, ['derivatives', 'lats-qa', 'sub-01', 'anat']);
  const s = items[2];
  assert.deepStrictEqual(s.image, ['sub-01', 'ses-2', 'anat', 'sub-01_ses-2_T1w.nii.gz']);
  assert.strictEqual(s.labelsIn.at(-1), 'tp01.nii.gz');
  assert.strictEqual(s.labels, 'sub-01_ses-2_desc-qa_dseg.nii.gz');
  assert.deepStrictEqual(items[3].qaFolder, ['derivatives', 'lats-qa', 'sub-03', 'ses-01', 'anat']);
  assert.deepStrictEqual([s.applied, items[4].applied, items[4].appliedSha], [false, true, 'L3 S3']);
  const edited = await Q.discover(list, readJson, async (p) => (p.at(-1).endsWith('.json') ? 'S3' : 'L3 edited'));
  assert.deepStrictEqual([edited[4].applied, edited[4].appliedSha], [false, null], 'labels changed since LATS re-applied them');
  assert.deepStrictEqual(t.sessions.map(x => x.id), ['sub-01_ses-01', 'sub-01_ses-2'], 'the template knows its sessions');
  assert.deepStrictEqual(items[3].sessions, [items[4]]);
  assert.deepStrictEqual(t.prior, ['derivatives', 'lats', 'longitudinal', 'sub-01', 'sst', 'prior_refine.json']);
  assert.deepStrictEqual(s.prior, t.prior, 'sessions carry their run\'s prior record');
  assert.strictEqual(items[3].prior, null);
  assert.deepStrictEqual(t.flags, ['derivatives', 'lats', 'longitudinal', 'sub-01', 'qc', 'flags.json']);
  assert.strictEqual(items[3].flags, null);
  assert.strictEqual(Q.priorRemovedPct({ dl_foreground_voxels: 1000, refined_foreground_voxels: 900 }), 10);
  assert.strictEqual(Q.priorRemovedPct(null), null);
  assert.strictEqual(items[4].labels, 'sub-03_ses-01_desc-qasingle_dseg.nii.gz');
});

test('records, tsv and resume', async () => {
  const items = await Q.discover(list, readJson, sha);
  let r = Q.newRecord(items[0]);
  r = Q.touch({ ...r, mri_quality: 4, seg_quality_final: 4, status: 'done', decision: 'include', notes: 'tab\there',
    options_changed: 'slTrim=3; mirror=on', midline: { mode: 'lats', shift_mm: 1.5, yaw_deg: -4, roll_deg: 0 } }, 'TS', 'save', new Date('2026-11-11T09:00:00Z'));
  assert.strictEqual(r.rater, 'TS'); assert.strictEqual(r.updated, '2026-11-11T09:00:00.000Z'); assert.strictEqual(r.history.length, 1);
  const lines = Q.toTsv([r]).trim().split('\n');
  assert.strictEqual(lines[0].split('\t').length, Q.TSV_COLUMNS.length);
  const row = Object.fromEntries(Q.TSV_COLUMNS.map((c, i) => [c, lines[1].split('\t')[i]]));
  assert.strictEqual(row.mri_disposition, 'Pass'); assert.strictEqual(row.seg_disposition, 'Pass'); assert.strictEqual(row.notes, 'tab here');
  assert.strictEqual(row.exclude_reason, 'n/a');
  assert.deepStrictEqual([row.options_changed, row.midline_mode, row.midline_shift_mm, row.midline_yaw_deg, row.midline_roll_deg],
    ['slTrim=3; mirror=on', 'lats', '1.5', '-4', '0']);
  assert.ok(!Q.TSV_COLUMNS.includes('seg_quality_raw'), 'labels are rated once, after the fixes');
  assert.ok(Q.TSV_COLUMNS.includes('prior_removed_pct'));
  const recs = { 'template:sub-01': r };
  assert.strictEqual(Q.nextToDo(items, recs), 1);
  assert.strictEqual(Q.nextToDo(items, recs, 4), 1);
  const all = Object.fromEntries(items.map(i => [i.pass + ':' + i.id, { status: 'done' }]));
  assert.strictEqual(Q.nextToDo(items, all), -1);
});
