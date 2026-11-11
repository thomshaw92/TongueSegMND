export const QA_DIR = ['derivatives', 'lats-qa'];
export const LATS_DIR = ['derivatives', 'lats'];

export const SCALES = {
  mri: {
    1: ['Unacceptable Quality.', "The tongue is not visible, there are large artifacts obscuring the tongue, extreme motion artifacts are present, or there's a complete lack of contrast or signal.", 'Fail'],
    2: ['Poor Quality.', 'Smaller artifacts like those described in 1, or minor motion artifacts. However, the tongue is still identifiable despite these issues.', 'Fail or check'],
    3: ['Moderate Quality.', 'The tongue is shown with adequate quality, but with noticeable imperfections. These may include moderate artifacts, slight motion blur, or some issues with contrast. The overall structure and some of the internal structures of the tongue are discernible, but the image is not ideal for detailed segmentation.', 'Pass + check'],
    4: ['Good Quality.', 'Minor errors in image quality are present in one or more of the domains (CNR, SNR, ringing, etc.). However, these do not significantly impair the visibility of the tongue structure.', 'Pass'],
    5: ['Excellent Quality.', 'Perfect image quality across all domains, including CNR, SNR, absence of ringing, and other artefacts. The entire tongue is visible, and the image is ideal for segmentation.', 'Pass'],
  },
  seg: {
    1: ['Unusable Quality.', 'Either completely unusable segmentations or labels that do not fit the anatomy.', 'Fail'],
    2: ['Poor Quality.', 'Missing large sections of the required labels or having labels in incorrect locations. Alternatively, this level can also include many (5+) small errors in segmentation.', 'Fail'],
    3: ['Moderate Quality.', 'More than three errors, but these errors are not so severe as to make the segmentation unusable. The errors could be minor misplacements or inaccuracies in labelling. Despite these errors, the segmentation is generally accurate.', 'Pass'],
    4: ['Good Quality.', 'The overall quality of the segmentation is good, with either minor errors present or perhaps one large error. However, these errors do not substantially impact the overall utility of the segmentation.', 'Pass'],
    5: ['Excellent Quality.', 'Virtually error-free. It represents the anatomy accurately without any noticeable mistakes.', 'Pass'],
  },
};

export const TSV_COLUMNS = ['pass', 'item', 'sub', 'ses', 'status', 'decision', 'exclude_reason',
  'mri_quality', 'mri_disposition', 'seg_quality_raw', 'seg_quality_final', 'seg_disposition',
  'touched_up', 'asymmetry_pct', 'midline_mode', 'cap_coverage_pct', 'il_side_ratio', 'gg_offset_mm', 'shape_flags',
  'prior_removed_pct', 'labels', 'rater', 'updated', 'notes'];

// Share of the network's tongue the atlas prior removed (LATS sst/prior_refine.json); the threshold is provisional.
export const PRIOR_FLAG_PCT = 8;
export const priorRemovedPct = (j) => (j && j.dl_foreground_voxels > 0
  ? 100 * (j.dl_foreground_voxels - j.refined_foreground_voxels) / j.dl_foreground_voxels : null);

const entities = (name) => name.replace(/\.nii(\.gz)?$/, '').split('_');

export function labelsName(imageName, desc = 'qa') {
  const parts = entities(imageName);
  return [...parts.slice(0, -1), `desc-${desc}`, 'dseg'].join('_') + '.nii.gz';
}

export function templateLabelsName(sub, ses) {
  return [sub, ses, 'space-sst', 'desc-qa', 'dseg'].filter(Boolean).join('_') + '.nii.gz';
}

export const sidecarName = (labels) => labels.replace(/\.nii\.gz$/, '.json');

export function sesOf(imageName) {
  const m = imageName.match(/(?:^|_)(ses-[^_]+)/);
  return m ? m[1] : null;
}

export async function discover(list, readJson) {
  const items = [];
  for (const kind of ['longitudinal', 'cross-sectional']) {
    for (const run of (await list([...LATS_DIR, kind])).filter(e => e.kind === 'directory' && e.name.startsWith('sub-')).map(e => e.name).sort()) {
      const base = [...LATS_DIR, kind, run];
      const sst = (await list([...base, 'sst'])).map(e => e.name);
      const seg = ['sst_seg_refined.nii.gz', 'sst_seg.nii.gz'].find(n => sst.includes(n));
      if (!sst.includes('T_template0.nii.gz') || !seg) continue;
      const [sub, ses] = run.split('_');
      const qaFolder = [...QA_DIR, sub, ...(ses ? [ses] : []), 'anat'];
      const pre = (await list([...base, 'pre_qa'])).map(e => e.name);
      const labels = templateLabelsName(sub, ses), prior = sst.includes('prior_refine.json') ? [...base, 'sst', 'prior_refine.json'] : null;
      items.push({ pass: 'template', id: run, sub, ses: ses || null, image: [...base, 'sst', 'T_template0.nii.gz'], prior,
        labelsIn: [...base, 'sst', seg],
        midline: pre.includes('midline.json') ? [...base, 'pre_qa', 'midline.json'] : sst.includes('midline.json') ? [...base, 'sst', 'midline.json'] : null,
        qaFolder, labels, sidecar: sidecarName(labels) });
      const runInfo = await readJson([...base, 'lats_run.json']);
      const applied = (await list(base)).some(e => e.name === 'qa_applied.json');
      for (const tp of (runInfo && runInfo.timepoints) || []) {
        const imageName = String(tp.image).split('/').pop();
        const tses = sesOf(imageName);
        if (!tses) continue;
        const single = kind === 'cross-sectional';
        const sLabels = labelsName(imageName, single ? 'qasingle' : 'qa');
        items.push({ pass: 'session', id: `${sub}_${tses}${single ? '_single' : ''}`, sub, ses: tses, image: [sub, tses, 'anat', imageName],
          labelsIn: [...base, 'labels', `tp${String(tp.index).padStart(2, '0')}.nii.gz`], midline: null, prior,
          qaFolder: [...QA_DIR, sub, tses, 'anat'], labels: sLabels, sidecar: sidecarName(sLabels), applied });
      }
    }
  }
  return items;
}

export function newRecord(item) {
  return { pass: item.pass, item: item.id, sub: item.sub, ses: item.ses, image: item.image.join('/'),
    labels_in: item.labelsIn.join('/'), status: 'to do', decision: null, exclude_reason: null,
    mri_quality: null, seg_quality_raw: null, seg_quality_final: null, touched_up: false,
    steps: null, midline: null, asymmetry_pct: null, labels: null, notes: '', rater: null, updated: null, history: [] };
}

export function touch(record, rater, action, now = new Date()) {
  const date = now.toISOString();
  return { ...record, rater, updated: date, history: [...record.history, { rater, date, action }] };
}

export function disposition(scale, grade) {
  return grade ? SCALES[scale][grade][2] : '';
}

export function tsvRow(r) {
  const v = { ...r, mri_disposition: disposition('mri', r.mri_quality),
    seg_disposition: disposition('seg', r.seg_quality_final || r.seg_quality_raw),
    midline_mode: r.midline ? r.midline.mode : '' };
  return TSV_COLUMNS.map(c => (v[c] === null || v[c] === undefined || v[c] === '' ? 'n/a' : String(v[c]).replace(/[\t\n\r]+/g, ' '))).join('\t');
}

export const toTsv = (records) => [TSV_COLUMNS.join('\t'), ...records.map(tsvRow)].join('\n') + '\n';

export function nextToDo(items, records, from = -1) {
  for (let k = 1; k <= items.length; k++) {
    const i = (from + k) % items.length;
    if ((records[items[i].pass + ':' + items[i].id] || {}).status !== 'done') return i;
  }
  return -1;
}
