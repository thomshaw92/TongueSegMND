import { readNifti, writeNiftiLabels, gunzipIfNeeded } from './nifti.js';
import { runPipeline, qcMetrics, midlinePlane, adjustPlane, lateralCounts, planeMask } from './postproc.js';
import * as Q from './qa.js';

const $ = (id) => document.getElementById(id);
const LABEL_LUT = {
  R: [0, 230, 38, 51, 250], G: [0, 38, 204, 115, 204], B: [0, 38, 64, 242, 26], A: [0, 255, 255, 255, 255],
  I: [0, 1, 2, 3, 4], labels: ['bg', 'vertical', 'sup-long', 'inf-long', 'genioglossus'],
};
const MIDLINE_LUT = { R: [0, 255], G: [0, 255], B: [0, 255], A: [0, 255], I: [0, 1], labels: ['bg', 'midline'] };
const GENTLE = { thin: true, keepFrac: 0.88, inferiorClip: true, surfaceTrim: true, trimK: 1.5, trimAll: true,
  clean: true, cleanIters: 1, followDorsum: true, smooth: true, sigma: 0.8 };
const OFF = { ...GENTLE, thin: false, inferiorClip: false, surfaceTrim: false, clean: false, followDorsum: false, smooth: false };
const TOGGLES = ['thin', 'inferiorClip', 'surfaceTrim', 'trimAll', 'clean', 'followDorsum', 'smooth'];
const SLIDERS = [['keepFrac', 'keepFracV'], ['trimK', 'trimKV'], ['cleanIters', 'cleanItersV'], ['sigma', 'sigmaV']];
const ML_SLIDERS = [['mlShift', 'mlShiftV'], ['mlYaw', 'mlYawV'], ['mlRoll', 'mlRollV']];

const scaleInfo = (scale) => Object.entries(Q.SCALES[scale])
  .map(([g, [t, d, disp]]) => `<p><b>${g}. ${t}</b> ${d} <i>(${disp})</i></p>`).join('');
const LANDMARKS = `<p><b>What to look for.</b> Axial: superior longitudinal shows a "W" shape, becoming a larger sheet
superiorly. Coronal: the "M" shape of superior longitudinal over transverse/vertical. Sagittal: superior longitudinal is a
band across the top quarter of the tongue from the front of the palate; the air between tongue and palate should not be
labelled. Inferior longitudinal lies alongside the salivary glands and mylohyoid; the mylohyoid (the floor of the mouth) is
not a tongue muscle.</p>`;
const STEPS = [
  { key: 'mri', short: 'Scan', title: 'Rate the scan',
    what: 'Look through the tongue in all three views and rate the image quality from 1 to 5.',
    info: '<p>Anchors from Shaw et al. 2025 (Computers in Biology and Medicine), used verbatim:</p>' + scaleInfo('mri') },
  { key: 'raw', short: 'Labels', title: 'Rate the labels from LATS',
    what: 'Rate the segmentation as LATS made it, from 1 to 5. If it is already right, accept it as it is.',
    info: scaleInfo('seg') + LANDMARKS },
  { key: 'surface', short: 'Surface', title: 'Correct the surface',
    what: 'Gentle corrections are on. Press B to compare before and after, and untick anything that makes it worse.',
    info: `<p><b>Thin superior longitudinal</b> keeps its outer part (the share set by keep) and gives its deepest voxels to
the nearest other muscle, for a superior longitudinal that is too thick.</p><p><b>Clip</b> stops superior longitudinal
wrapping under the tongue: none may sit below the lowest vertical voxel.</p><p><b>Trim</b> removes surface voxels whose
intensity is outside the tissue band (the median of vertical, inferior longitudinal and genioglossus, plus or minus k robust
standard deviations), up to 6 layers. Untick all muscles to trim superior longitudinal only.</p>` },
  { key: 'smooth', short: 'Smooth', title: 'Follow the dorsum and smooth',
    what: 'Check that the layers follow the corrected top of the tongue and that the edges are smooth.',
    info: `<p><b>Follow the dorsum</b>: each top-to-bottom column takes the labels from before the trim, stretched to the
corrected outline, so the other muscles move down with the new top.</p><p><b>Clean</b> erodes each muscle, keeps its largest
piece and grows it back, removing specks.</p><p><b>Smooth</b> is a Gaussian majority vote between muscles; a larger sigma
gives smoother boundaries.</p>` },
  { key: 'midline', short: 'Midline', title: 'Check the midline', template: true,
    what: 'The white line should run down the middle of the tongue in the coronal and axial views. Shift or rotate it if it is off.',
    info: `<p><b>LATS mirror plane</b>: LATS registers the template to its left-right mirror image and splits at the plane
halfway. It is used when LATS wrote midline.json.</p><p><b>Genioglossus centroid</b>: a left-right plane through the centre of
genioglossus. <b>Tilted</b>: the plane across the tongue's shortest axis.</p><p>Asymmetry is very sensitive to the midline: a
2 mm shift can move it by more than 20 points.</p>` },
  { key: 'final', short: 'Recheck', title: 'Rate the corrected labels',
    what: 'Rate the segmentation as it is now, from 1 to 5.', info: scaleInfo('seg') },
  { key: 'decide', short: 'Decide', title: 'Decide',
    what: 'Include saves the labels and moves on. Touch up opens them in ITK-SNAP for a quick fix. Exclude records why.',
    info: `<p><b>Include</b> writes the labels and a record (ratings, steps, midline, notes, your initials and the time) to
derivatives/lats-qa, then loads the next item.</p><p><b>Touch up</b> saves the labels first. Paste the command into a
terminal, fix the labels in ITK-SNAP, save over the same file, then press Reload edited labels and Include.</p><p><b>Exclude</b>
records the reason; no labels are written. Aim for under 5 minutes per item.</p>` },
];

const state = { root: null, items: [], records: {}, pass: 'template', idx: -1, step: 0, image: null, label: null, raw: null,
  corrected: null, frozen: false, lats: null, plane: null, settings: { ...GENTLE }, nv: null, overlayUrl: null,
  midlineUrl: null, writes: Promise.resolve() };

const status = (t) => { $('status').textContent = t; };
const keyOf = (it) => it.pass + ':' + it.id;
const passItems = () => state.items.filter(i => i.pass === state.pass);
const cur = () => passItems()[state.idx];
const rec = (it = cur()) => state.records[keyOf(it)];
const steps = () => STEPS.filter(s => !(s.template && state.pass === 'session'));
const stepKey = () => (steps()[state.step] || {}).key;
const rater = () => $('rater').value.trim().toUpperCase();

async function dirAt(parts, create = false) {
  let d = state.root;
  for (const p of parts) d = await d.getDirectoryHandle(p, { create });
  return d;
}
async function list(parts) {
  try {
    const out = [];
    for await (const [name, h] of (await dirAt(parts)).entries()) out.push({ name, kind: h.kind });
    return out;
  } catch { return []; }
}
const readFile = async (parts) => (await (await dirAt(parts.slice(0, -1))).getFileHandle(parts.at(-1))).getFile();
async function readJson(parts) {
  try { return JSON.parse(await (await readFile(parts)).text()); } catch { return null; }
}
function writeFile(parts, data) {
  state.writes = state.writes.then(async () => {
    const fh = await (await dirAt(parts.slice(0, -1), true)).getFileHandle(parts.at(-1), { create: true });
    const w = await fh.createWritable();
    await w.write(data);
    await w.close();
  }).catch(e => status('Could not write ' + parts.at(-1) + ': ' + e.message));
  return state.writes;
}
const gzip = async (bytes) => new Uint8Array(await new Response(new Blob([bytes]).stream().pipeThrough(new CompressionStream('gzip'))).arrayBuffer());
const loadNifti = async (parts) => readNifti(await gunzipIfNeeded(await (await readFile(parts)).arrayBuffer()));

function idb(mode, fn) {
  return new Promise((resolve, reject) => {
    const open = indexedDB.open('tongue-qa', 1);
    open.onupgradeneeded = () => open.result.createObjectStore('kv');
    open.onerror = () => reject(open.error);
    open.onsuccess = () => {
      const tx = open.result.transaction('kv', mode), req = fn(tx.objectStore('kv'));
      tx.oncomplete = () => resolve(req.result);
      tx.onerror = () => reject(tx.error);
    };
  });
}

async function openFolder(handle) {
  state.root = handle;
  $('folderName').textContent = handle.name;
  $('reopenFolder').hidden = true;
  $('rootPath').value = localStorage.getItem('tongueqa_root:' + handle.name) || '';
  await idb('readwrite', s => s.put(handle, 'root')).catch(() => {});
  status('Looking for LATS outputs...');
  state.items = await Q.discover(list, readJson);
  if (!state.items.length) { status('No LATS outputs found under derivatives/lats in this folder.'); return; }
  state.records = {};
  for (const it of state.items) state.records[keyOf(it)] = (await readJson([...it.qaFolder, it.sidecar])) || Q.newRecord(it);
  if (!(await readJson([...Q.QA_DIR, 'dataset_description.json']))) {
    await writeFile([...Q.QA_DIR, 'dataset_description.json'], JSON.stringify({ Name: 'Tongue QA of LATS outputs',
      BIDSVersion: '1.9.0', DatasetType: 'derivative', GeneratedBy: [{ Name: 'postproc-qa' }] }, null, 2) + '\n');
  }
  const templatesLeft = state.items.some(i => i.pass === 'template' && state.records[keyOf(i)].status !== 'done');
  await setPass(templatesLeft ? 'template' : 'session');
}

async function setPass(pass) {
  state.pass = pass;
  $('passTemplate').classList.toggle('on', pass === 'template');
  $('passSession').classList.toggle('on', pass === 'session');
  const next = Q.nextToDo(passItems(), state.records);
  await loadItem(next < 0 ? 0 : next);
}

function renderItemBar() {
  const items = passItems(), done = items.filter(i => rec(i).status === 'done').length;
  $('progress').textContent = `${done} of ${items.length} ${state.pass === 'template' ? 'templates' : 'sessions'} done`;
  $('itemList').innerHTML = items.map((it, i) => {
    const s = rec(it).status;
    return `<option value="${i}"${i === state.idx ? ' selected' : ''}>${s === 'done' ? '[done] ' : s === 'in progress' ? '[open] ' : ''}${it.id}</option>`;
  }).join('');
  const s = rec().status, chip = $('itemStatus');
  chip.textContent = s === 'done' ? `done: ${rec().decision}` : s;
  chip.className = 'chip ' + (s === 'done' ? 'done' : s === 'in progress' ? 'progress' : '');
}

function writeControls(s) {
  TOGGLES.forEach(id => { $(id).checked = !!s[id]; });
  SLIDERS.forEach(([id, v]) => { $(id).value = s[id]; $(v).textContent = s[id]; });
}
function readControls() {
  const s = {};
  TOGGLES.forEach(id => { s[id] = $(id).checked; });
  SLIDERS.forEach(([id]) => { s[id] = +$(id).value; });
  return s;
}
function setMidlineControls(m) {
  $('midlineMode').value = m.mode;
  [['mlShift', m.shift_mm], ['mlYaw', m.yaw_deg], ['mlRoll', m.roll_deg]].forEach(([id, v], k) => {
    $(id).value = v; $(ML_SLIDERS[k][1]).textContent = v;
  });
}
const defaultMidline = () => ({ mode: state.lats ? 'lats' : 'gg', shift_mm: 0, yaw_deg: 0, roll_deg: 0 });

async function loadItem(i) {
  await state.writes;
  const items = passItems();
  if (!items.length) { status(`No ${state.pass} items found.`); return; }
  state.idx = (i + items.length) % items.length;
  const it = cur(), r = rec(it);
  status(`Loading ${it.id}...`);
  let image, label;
  try {
    image = await loadNifti(it.image);
    label = await loadNifti(it.labelsIn);
  } catch (e) {
    renderItemBar(); status(`Could not load ${it.id}: ${e.message}`); return;
  }
  if (image.dims.some((d, k) => d !== label.dims[k])) {
    renderItemBar(); status(`Grid mismatch for ${it.id}: image ${image.dims} vs labels ${label.dims}.`); return;
  }
  state.image = image; state.label = label;
  state.raw = Int32Array.from(label.dataC, v => Math.round(v));
  state.lats = null;
  const j = it.midline && await readJson(it.midline);
  if (j && [j.point_mm, j.normal].every(a => Array.isArray(a) && a.length === 3)) state.lats = { point: j.point_mm, normal: j.normal };
  $('midlineMode').querySelector('option[value=lats]').disabled = !state.lats;
  state.settings = r.steps ? { ...GENTLE, ...r.steps } : { ...(it.pass === 'template' ? GENTLE : OFF) };
  writeControls(state.settings);
  setMidlineControls(r.midline && (r.midline.mode !== 'lats' || state.lats) ? r.midline : defaultMidline());
  state.frozen = false;
  if (r.touched_up && r.labels) {
    try {
      state.corrected = Int32Array.from((await loadNifti([...it.qaFolder, r.labels])).dataC, v => Math.round(v));
      state.frozen = true;
    } catch { r.touched_up = false; }
  }
  $('notes').value = r.notes || '';
  $('touchPanel').hidden = !state.frozen; $('excludePanel').hidden = true;
  state.step = r.status === 'done' ? steps().length - 1 : Math.min(r.step || 0, steps().length - 1);
  await initViewer(it);
  renderItemBar();
  renderStep();
  await recompute();
  status(`${it.id}: ${r.status === 'done' ? `done (${r.decision}); open for review, and saving again replaces it` : 'ready'}.`);
}

async function initViewer(it) {
  if (!state.nv) {
    state.nv = new niivue.Niivue({ show3Dcrosshair: false, backColor: [0.1, 0.1, 0.12, 1] });
    await state.nv.attachTo('gl');
  }
  const f = await readFile(it.image);
  await state.nv.loadVolumes([{ url: URL.createObjectURL(f), name: f.name.endsWith('.gz') ? 'image.nii.gz' : 'image.nii' }]);
}

function currentPlane() {
  const mode = $('midlineMode').value;
  const base = mode === 'lats' && state.lats ? state.lats
    : midlinePlane(state.corrected, state.label.dims, state.label.affine, mode === 'lats' ? 'gg' : mode);
  return adjustPlane(base, +$('mlShift').value, +$('mlYaw').value, +$('mlRoll').value);
}
const midlineRecord = () => (state.pass === 'template' && state.plane ? {
  mode: $('midlineMode').value, shift_mm: +$('mlShift').value, yaw_deg: +$('mlYaw').value, roll_deg: +$('mlRoll').value,
  point_mm: state.plane.point, normal: state.plane.normal } : null);
const shown = () => ($('showCorrected').checked ? state.corrected : state.raw);

async function recompute() {
  if (!state.label) return;
  const { dims, zooms } = state.label;
  if (!state.frozen) state.corrected = runPipeline(state.raw, state.image.dataC, dims, zooms, state.settings);
  state.plane = state.pass === 'template' ? currentPlane() : null;
  updateQc();
  await refreshOverlay();
}

function asymmetry() {
  if (!state.plane) return null;
  const lr = lateralCounts(state.corrected, state.label.dims, state.label.affine, state.plane);
  return { ...lr, pct: 100 * (lr.left - lr.right) / Math.max((lr.left + lr.right) / 2, 1) };
}

function updateQc() {
  const lab = shown(), m = qcMetrics(lab, state.image.dataC);
  $('qcRatio').textContent = m.suplongOverTongue.toFixed(3);
  $('qcRatio').className = (m.suplongOverTongue >= 0.237 && m.suplongOverTongue <= 0.392) ? 'ok' : 'flag';
  $('qcSlNt').textContent = (100 * m.suplongNotTissueFrac).toFixed(1) + '%';
  $('qcFgNt').textContent = (100 * m.tongueNotTissueFrac).toFixed(1) + '%';
  const vox = state.label.zooms[0] * state.label.zooms[1] * state.label.zooms[2] / 1000;
  let sl = 0; for (let i = 0; i < lab.length; i++) if (lab[i] === 2) sl++;
  $('qcVol').textContent = (sl * vox).toFixed(1);
  const a = asymmetry();
  $('qcLR').textContent = a ? `${(a.left * vox).toFixed(1)} / ${(a.right * vox).toFixed(1)}` : '-';
  $('qcAsym').textContent = a ? a.pct.toFixed(1) + '%' : '-';
}

async function refreshOverlay() {
  const { dims, zooms, affine, header } = state.label, nv = state.nv;
  if (state.overlayUrl) URL.revokeObjectURL(state.overlayUrl);
  state.overlayUrl = URL.createObjectURL(new Blob([writeNiftiLabels(header, shown(), dims)]));
  while (nv.volumes.length > 1) nv.removeVolumeByIndex(1);
  const v = await nv.addVolumeFromUrl({ url: state.overlayUrl, name: 'labels.nii', opacity: +$('opacity').value });
  if (v && v.setColormapLabel) v.setColormapLabel(LABEL_LUT);
  if (state.plane && $('showMidline').checked) {
    if (state.midlineUrl) URL.revokeObjectURL(state.midlineUrl);
    const mask = planeMask(state.corrected, dims, affine, state.plane, 0.5 * Math.max(...zooms));
    state.midlineUrl = URL.createObjectURL(new Blob([writeNiftiLabels(header, mask, dims)]));
    const m = await nv.addVolumeFromUrl({ url: state.midlineUrl, name: 'midline.nii', opacity: 0.9 });
    if (m && m.setColormapLabel) m.setColormapLabel(MIDLINE_LUT);
  }
  nv.updateGLVolume();
}

function renderStep() {
  const list = steps(), s = list[state.step], r = rec();
  $('stepper').innerHTML = list.map((x, i) => `<li data-i="${i}" class="${i === state.step ? 'on' : i < state.step || r.status === 'done' ? 'done' : ''}">${i + 1} ${x.short}</li>`).join('');
  $('stepTitle').textContent = `${state.step + 1}. ${s.title}`;
  $('stepWhat').textContent = s.what;
  $('stepInfo').innerHTML = s.info;
  document.querySelectorAll('.stepbody').forEach(b => b.classList.toggle('on', b.dataset.step === s.key));
  $('showCorrected').checked = !['mri', 'raw'].includes(s.key);
  [...TOGGLES, ...SLIDERS.map(x => x[0])].forEach(id => { $(id).disabled = state.frozen; });
  if (state.frozen && ['surface', 'smooth'].includes(s.key)) $('stepWhat').textContent = 'These labels were touched up in ITK-SNAP, so corrections are frozen. Undo the touch-up (step 7) to change them.';
  renderGrades();
  $('backStep').disabled = state.step === 0;
  $('nextStep').disabled = state.step === list.length - 1;
}

const GRADE_FIELD = { mri: 'mri_quality', seg_raw: 'seg_quality_raw', seg_final: 'seg_quality_final' };
function renderGrades() {
  document.querySelectorAll('.grades').forEach(g => {
    const field = GRADE_FIELD[g.dataset.scale], val = rec()[field];
    if (!g.children.length) g.innerHTML = [1, 2, 3, 4, 5].map(n => `<button data-g="${n}">${n}</button>`).join('');
    [...g.children].forEach(b => b.classList.toggle('sel', +b.dataset.g === val));
    const a = document.querySelector(`[data-anchor="${g.dataset.scale}"]`), sc = g.dataset.scale === 'mri' ? 'mri' : 'seg';
    a.textContent = val ? `${val}. ${Q.SCALES[sc][val][0]} ${Q.SCALES[sc][val][2]}.` : 'Not rated yet.';
  });
}

function edit(patch, action) {
  const it = cur(), r = rec(it);
  const a = asymmetry();
  state.records[keyOf(it)] = Q.touch({ ...r, ...patch, status: 'in progress', step: state.step, steps: state.settings,
    midline: midlineRecord(), asymmetry_pct: a ? +a.pct.toFixed(2) : null, notes: $('notes').value }, rater(), action);
  renderItemBar();
  return writeFile([...it.qaFolder, it.sidecar], JSON.stringify(state.records[keyOf(it)], null, 2) + '\n');
}

function needRater() {
  if (rater()) return true;
  status('Enter your initials first.'); $('rater').focus(); return false;
}

async function rate(field, grade) {
  if (!state.label || !needRater()) return;
  await edit({ [field]: grade }, `rate ${field} ${grade}`);
  renderGrades();
  setTimeout(() => goStep(state.step + 1), 250);
}

async function goStep(i) {
  const n = steps().length;
  if (!state.label || i < 0 || i >= n) return;
  state.step = i;
  renderStep();
  await refreshOverlay();
  updateQc();
}

async function finish(patch, action) {
  const it = cur(), a = asymmetry();
  state.records[keyOf(it)] = Q.touch({ ...rec(it), ...patch, status: 'done', step: steps().length - 1, steps: state.settings,
    midline: midlineRecord(), asymmetry_pct: a ? +a.pct.toFixed(2) : null, notes: $('notes').value,
    touched_up: state.frozen }, rater(), action);
  await writeFile([...it.qaFolder, it.sidecar], JSON.stringify(state.records[keyOf(it)], null, 2) + '\n');
  const done = Object.values(state.records).filter(r => r.status !== 'to do').sort((a, b) => (a.pass + a.item).localeCompare(b.pass + b.item));
  await writeFile([...Q.QA_DIR, 'qa.tsv'], Q.toTsv(done));
  status(`${it.id} saved (${patch.decision}).`);
  const next = Q.nextToDo(passItems(), state.records, state.idx);
  if (next < 0) { renderItemBar(); renderStep(); status(`All ${state.pass} items are done.`); return; }
  await loadItem(next);
}

const saveLabels = async () => writeFile([...cur().qaFolder, cur().labels], await gzip(writeNiftiLabels(state.label.header, state.corrected, state.label.dims)));

async function include() {
  if (!state.label || !needRater()) return;
  await saveLabels();
  await finish({ decision: 'include', exclude_reason: null, labels: cur().labels }, 'include');
}

async function acceptRaw() {
  if (!state.label || !needRater()) return;
  state.settings = { ...OFF }; state.frozen = false; writeControls(state.settings);
  await recompute();
  await edit({ seg_quality_final: rec().seg_quality_raw }, 'accept as is');
  await goStep(steps().findIndex(s => s.key === (state.pass === 'template' ? 'midline' : 'decide')));
}

function itkCommand() {
  const it = cur(), root = $('rootPath').value.trim().replace(/\/$/, '') || '<BIDS folder>';
  return `itksnap -g "${root}/${it.image.join('/')}" -s "${root}/${[...it.qaFolder, it.labels].join('/')}"`;
}

async function touchUp() {
  if (!state.label || !needRater()) return;
  await saveLabels();
  await edit({ labels: cur().labels }, 'touch-up started');
  $('touchPanel').hidden = false; $('excludePanel').hidden = true;
  $('itkCmd').textContent = itkCommand();
  status('Labels saved. Copy the command, fix and save in ITK-SNAP, then press Reload edited labels.');
}

async function reloadEdit() {
  const it = cur();
  try {
    const edited = await loadNifti([...it.qaFolder, it.labels]);
    if (edited.dims.some((d, k) => d !== state.label.dims[k])) throw new Error('grid differs from the image');
    state.corrected = Int32Array.from(edited.dataC, v => Math.round(v));
  } catch (e) { status('Could not reload: ' + e.message); return; }
  state.frozen = true;
  await edit({ touched_up: true }, 'touch-up reloaded');
  renderStep(); await recompute();
  status('Edited labels loaded. Include saves them.');
}

async function undoTouch() {
  state.frozen = false;
  await edit({ touched_up: false }, 'touch-up undone');
  $('touchPanel').hidden = true;
  renderStep(); await recompute();
}

async function confirmExclude() {
  if (!state.label || !needRater()) return;
  await finish({ decision: 'exclude', exclude_reason: $('excludeReason').value, labels: null }, 'exclude');
}

$('openFolder').addEventListener('click', async () => {
  if (!window.showDirectoryPicker) { status('This browser cannot open folders. Use Chrome or Edge.'); return; }
  try { await openFolder(await window.showDirectoryPicker({ id: 'lats-bids', mode: 'readwrite' })); }
  catch (e) { if (e.name !== 'AbortError') status('Could not open the folder: ' + e.message); }
});
$('reopenFolder').addEventListener('click', async () => {
  const h = await idb('readonly', s => s.get('root'));
  if (h && (await h.requestPermission({ mode: 'readwrite' })) === 'granted') await openFolder(h);
});
$('rater').value = localStorage.getItem('tongueqa_rater') || '';
$('rater').addEventListener('change', () => { $('rater').value = rater(); localStorage.setItem('tongueqa_rater', rater()); });
$('rootPath').addEventListener('change', () => { localStorage.setItem('tongueqa_root:' + (state.root ? state.root.name : ''), $('rootPath').value); $('itkCmd').textContent = itkCommand(); });
$('passTemplate').addEventListener('click', () => state.root && setPass('template'));
$('passSession').addEventListener('click', () => state.root && setPass('session'));
$('prevItem').addEventListener('click', () => state.root && loadItem(state.idx - 1));
$('nextItem').addEventListener('click', () => state.root && loadItem(state.idx + 1));
$('nextTodo').addEventListener('click', () => { const n = state.root ? Q.nextToDo(passItems(), state.records, state.idx) : -1; if (n >= 0) loadItem(n); });
$('itemList').addEventListener('change', () => loadItem(+$('itemList').value));
$('stepper').addEventListener('click', (e) => { const li = e.target.closest('li'); if (li) goStep(+li.dataset.i); });
$('backStep').addEventListener('click', () => goStep(state.step - 1));
$('nextStep').addEventListener('click', () => goStep(state.step + 1));
$('infoBtn').addEventListener('click', () => { $('stepInfo').hidden = !$('stepInfo').hidden; });
document.querySelectorAll('.grades').forEach(g => g.addEventListener('click', (e) => {
  const b = e.target.closest('button'); if (b) rate(GRADE_FIELD[g.dataset.scale], +b.dataset.g);
}));
TOGGLES.forEach(id => $(id).addEventListener('change', async () => { state.settings = readControls(); await recompute(); await edit({}, 'settings'); }));
SLIDERS.forEach(([id, v]) => {
  $(id).addEventListener('input', () => { $(v).textContent = $(id).value; });
  $(id).addEventListener('change', async () => { state.settings = readControls(); await recompute(); await edit({}, 'settings'); });
});
ML_SLIDERS.forEach(([id, v]) => {
  $(id).addEventListener('input', () => { $(v).textContent = $(id).value; });
  $(id).addEventListener('change', async () => { await recompute(); await edit({}, 'midline'); });
});
$('midlineMode').addEventListener('change', async () => { await recompute(); await edit({}, 'midline'); });
$('mlReset').addEventListener('click', async () => { setMidlineControls(defaultMidline()); await recompute(); await edit({}, 'midline reset'); });
['showCorrected', 'showMidline'].forEach(id => $(id).addEventListener('change', async () => { updateQc(); await refreshOverlay(); }));
$('opacity').addEventListener('change', () => state.label && refreshOverlay());
$('notes').addEventListener('change', () => state.label && edit({}, 'notes'));
$('acceptRaw').addEventListener('click', acceptRaw);
document.querySelectorAll('.toExclude').forEach(b => b.addEventListener('click', async () => { await goStep(steps().length - 1); $('excludePanel').hidden = false; $('touchPanel').hidden = true; }));
$('include').addEventListener('click', include);
$('touchUp').addEventListener('click', touchUp);
$('reloadEdit').addEventListener('click', reloadEdit);
$('undoTouch').addEventListener('click', undoTouch);
$('copyCmd').addEventListener('click', () => navigator.clipboard.writeText($('itkCmd').textContent));
$('exclude').addEventListener('click', () => { $('excludePanel').hidden = false; $('touchPanel').hidden = true; });
$('confirmExclude').addEventListener('click', confirmExclude);

document.addEventListener('keydown', (e) => {
  if (e.target.matches('textarea, select, input:not([type=range]):not([type=checkbox])') || e.metaKey || e.ctrlKey || !state.label) return;
  const k = e.key.toLowerCase(), sk = stepKey();
  const scale = { mri: 'mri_quality', raw: 'seg_quality_raw', final: 'seg_quality_final' }[sk];
  if (/^[1-5]$/.test(k) && scale) rate(scale, +k);
  else if (k === 'enter') goStep(state.step + 1);
  else if (k === 'b') { $('showCorrected').checked = !$('showCorrected').checked; updateQc(); refreshOverlay(); }
  else if (k === 'a' && sk === 'raw') acceptRaw();
  else if (k === 's' && sk === 'decide') include();
  else if (k === '[') loadItem(state.idx - 1);
  else if (k === ']') loadItem(state.idx + 1);
  else return;
  e.preventDefault();
});

idb('readonly', s => s.get('root')).then(h => {
  if (h) { $('reopenFolder').textContent = `Reopen ${h.name}`; $('reopenFolder').hidden = false; }
}).catch(() => {});
