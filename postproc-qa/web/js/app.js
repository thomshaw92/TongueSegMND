import { readNifti, writeNiftiLabels, gunzipIfNeeded } from './nifti.js';
import { runPipeline, qcMetrics, midlinePlane, adjustPlane, lateralCounts, planeMask, shapeChecks, SHAPE_FLAGS, rasOrder, toRas, fromRas } from './postproc.js';
import * as Q from './qa.js';
import { brushVoxels, applyTool } from './edit.js';

const $ = (id) => document.getElementById(id);
const LABEL_LUT = {
  R: [0, 230, 38, 51, 250], G: [0, 38, 204, 115, 204], B: [0, 38, 64, 242, 26], A: [0, 255, 255, 255, 255],
  I: [0, 1, 2, 3, 4], labels: ['bg', 'vertical', 'sup-long', 'inf-long', 'genioglossus'],
};
const MIDLINE_LUT = { R: [0, 255], G: [0, 255], B: [0, 255], A: [0, 255], I: [0, 1], labels: ['bg', 'midline'] };
const GENTLE = { thin: true, keepFrac: 0.88, inferiorClip: true, slCap: true, surfaceTrim: true, trimK: 1.5, trimAll: false,
  clean: true, followDorsum: true, smooth: true, sigma: 0.8 };
const OFF = { ...GENTLE, thin: false, inferiorClip: false, slCap: false, surfaceTrim: false, clean: false, followDorsum: false, smooth: false };
const TOGGLES = ['thin', 'inferiorClip', 'slCap', 'surfaceTrim', 'trimAll', 'clean', 'followDorsum', 'smooth'];
const SLIDERS = [['keepFrac', 'keepFracV'], ['trimK', 'trimKV'], ['sigma', 'sigmaV']];
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
    what: 'Rate the segmentation as LATS made it, from 1 to 5. Then press Enter to correct it, or A to accept it as it is.',
    info: scaleInfo('seg') + LANDMARKS },
  { key: 'surface', short: 'Surface', title: 'Correct the surface',
    what: 'Gentle corrections are on. Press B to compare before and after, and untick anything that makes it worse.',
    whatSession: 'Corrections are off for sessions, whose labels come from the corrected template. Tick one only if it helps; B compares.',
    info: `<p><b>Thin superior longitudinal</b> keeps its outer part (the share set by keep) and gives its deepest voxels to
the nearest other muscle, for a superior longitudinal that is too thick.</p><p><b>Clip</b> stops superior longitudinal
wrapping under the tongue: none may sit below the lowest vertical voxel.</p><p><b>Cap</b> keeps superior longitudinal
continuous over the top of the tongue: where vertical breaks through it, the top voxels of those columns become superior
longitudinal at the cap's usual thickness. Only gaps and holes inside the cap are filled, not its edges.</p><p><b>Trim</b> removes surface voxels whose
intensity is outside the tissue band (the median of vertical, inferior longitudinal and genioglossus, plus or minus k robust
standard deviations), up to 6 layers, on superior longitudinal only by default. Ticking all muscles trims every muscle's
surface, which shrank the whole tongue by 14 to 18% on our examples.</p>` },
  { key: 'smooth', short: 'Smooth', title: 'Follow the dorsum and smooth',
    what: 'Check that the layers follow the corrected top of the tongue and that the edges are smooth.',
    info: `<p><b>Follow the dorsum</b>: each top-to-bottom column takes the labels from before the trim, stretched to the
corrected outline, so the other muscles move down with the new top.</p><p><b>Clean</b> drops pieces of each muscle under a quarter
of its largest piece, and inferior longitudinal pieces under 50 mm³, and gives them to the neighbouring muscle. It never
shaves a muscle, so a thin, atrophied side is kept.</p><p><b>Smooth</b> is a Gaussian majority vote between muscles; a larger sigma
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
    what: 'Include saves the labels and moves on. Edit here (E) fixes them by hand; Touch up opens them in ITK-SNAP instead. Exclude records why.',
    whatSession: 'Include marks the session usable and moves on; exclude it if the labels do not fit this scan. LATS takes its labels from the template, so fix label errors on the template.',
    info: `<p><b>Include</b> writes the labels and a record (ratings, steps, midline, notes, your initials and the time) to
derivatives/lats-qa, then loads the next item.</p><p><b>Edit here</b> works on the slice under the cursor. Paint (P) gives the
brush area the chosen muscle (1 to 4), Erase (X) clears it, Erode peels one layer off the chosen muscle and gives it to its
neighbours, and Dilate grows the chosen muscle one layer into the muscles next to it. Z undoes a stroke and N goes back to
navigating; the mouse wheel still moves through slices. Each stroke is saved, and the corrections are then frozen.</p><p><b>Touch up</b> saves the labels first. Paste the command into a
terminal, fix the labels in ITK-SNAP, save over the same file, then press Include (it reloads your edits).</p><p><b>Exclude</b>
records the reason; no labels are written. Aim for under 5 minutes per item.</p>` },
];

const state = { root: null, items: [], records: {}, pass: 'template', idx: -1, step: 0, image: null, label: null, raw: null,
  corrected: null, ras: null, rawR: null, imageR: null, correctedR: null, frozen: false, lats: null, plane: null, settings: { ...GENTLE }, nv: null, overlayUrl: null,
  midlineUrl: null, writes: Promise.resolve(), seen: {}, busy: false, priorPct: null, edit: { tool: 'nav', stroke: null, last: -1, undo: [] } };

const status = (t) => { $('status').textContent = t; };
const keyOf = (it) => it.pass + ':' + it.id;
const passItems = () => state.items.filter(i => i.pass === state.pass);
const cur = () => passItems()[state.idx];
const rec = (it = cur()) => state.records[keyOf(it)];
const steps = () => STEPS.filter(s => !(s.template && state.pass === 'session'));
const stepKey = () => (steps()[state.step] || {}).key;
const rater = () => $('rater').value.trim().toUpperCase();
const stepStore = (it) => 'tongueqa_step:' + (state.root ? state.root.name : '') + ':' + keyOf(it);

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
  const p = state.writes.then(async () => {
    const fh = await (await dirAt(parts.slice(0, -1), true)).getFileHandle(parts.at(-1), { create: true });
    const w = await fh.createWritable();
    await w.write(data);
    await w.close();
  });
  state.writes = p.catch(() => {});
  return p.catch(e => { const m = `Could not write ${parts.at(-1)}: ${e.message}. It is not saved.`; status(m); alert(m); throw e; });
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
  state.seen = Object.fromEntries(Object.entries(state.records).map(([k, r]) => [k, r.updated]));
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
  state.label = null;
  await state.writes;
  const items = passItems();
  if (!items.length) { status(`No ${state.pass} items found.`); return; }
  const wrapped = i >= items.length || i < 0;
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
  try { state.ras = rasOrder(label.dims, label.zooms, label.affine); } catch (e) {
    renderItemBar(); status(`${it.id}: ${e.message}.`); return;
  }
  state.image = image; state.label = label;
  state.raw = Int32Array.from(label.dataC, v => Math.round(v));
  state.rawR = toRas(state.raw, state.ras); state.imageR = toRas(image.dataC, state.ras);
  state.lats = null;
  state.priorPct = Q.priorRemovedPct(it.prior && await readJson(it.prior));
  const j = it.midline && await readJson(it.midline);
  if (j && j.mode !== 'qa' && [j.point_mm, j.normal].every(a => Array.isArray(a) && a.length === 3)) state.lats = { point: j.point_mm, normal: j.normal };
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
  $('touchPanel').hidden = !r.touch_pending; $('excludePanel').hidden = true; $('editPanel').hidden = true;
  state.edit = { tool: 'nav', stroke: null, last: -1, undo: [] }; setTool('nav');
  $('itkCmd').textContent = itkCommand();
  state.step = r.status === 'done' ? steps().length - 1 : Math.min(+localStorage.getItem(stepStore(it)) || r.step || 0, steps().length - 1);
  await initViewer(it);
  renderItemBar();
  renderStep();
  await recompute();
  status(`${wrapped ? 'Back to the start of the list. ' : ''}${it.id}: ${r.status === 'done' ? `done (${r.decision}); open for review, and saving again replaces it`
    : 'ready'}.${it.pass === 'session' && !it.applied ? ' Its labels are from before the template QA: LATS has not re-applied this run yet.' : ''}`);
}

async function initViewer(it) {
  if (!state.nv) {
    state.nv = new niivue.Niivue({ show3Dcrosshair: false, backColor: [0.1, 0.1, 0.12, 1] });
    await state.nv.attachTo('gl');
  }
  const f = await readFile(it.image);
  await state.nv.loadVolumes([{ url: URL.createObjectURL(f), name: f.name.endsWith('.gz') ? 'image.nii.gz' : 'image.nii' }]);
  const { dims, affine } = state.label, [, ny, nz] = dims, c = [0, 0, 0];
  let n = 0;
  state.raw.forEach((v, q) => { if (v > 0) { c[0] += Math.floor(q / (ny * nz)); c[1] += Math.floor(q / nz) % ny; c[2] += q % nz; n++; } });
  if (n) {
    const mm = [0, 1, 2].map(r => [0, 1, 2].reduce((s, k) => s + affine[r][k] * c[k] / n, affine[r][3]));
    state.nv.scene.crosshairPos = state.nv.mm2frac(mm);
  }
}

function currentPlane() {
  const mode = $('midlineMode').value;
  const base = mode === 'lats' && state.lats ? state.lats
    : midlinePlane(state.corrected, state.label.dims, state.label.affine, mode === 'lats' ? 'gg' : mode);
  return base && adjustPlane(base, +$('mlShift').value, +$('mlYaw').value, +$('mlRoll').value);
}
const midlineRecord = () => (state.pass === 'template' && state.plane ? {
  mode: $('midlineMode').value, shift_mm: +$('mlShift').value, yaw_deg: +$('mlYaw').value, roll_deg: +$('mlRoll').value,
  point_mm: state.plane.point, normal: state.plane.normal } : null);
const shown = () => ($('showCorrected').checked ? state.corrected : state.raw);
const shownR = () => ($('showCorrected').checked ? state.correctedR : state.rawR);

async function recompute() {
  if (!state.label) return;
  if (!state.frozen) {
    state.correctedR = runPipeline(state.rawR, state.imageR, state.ras.dims, state.ras.zooms, state.settings);
    state.corrected = fromRas(state.correctedR, state.ras);
  } else state.correctedR = toRas(state.corrected, state.ras);
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
  const sc = shapes(shownR());
  [['qcCap', sc.capPct.toFixed(1) + '%', 'capPct'], ['qcIl', `${sc.ilRatio.toFixed(2)} (${(sc.ilLeft * vox).toFixed(1)} / ${(sc.ilRight * vox).toFixed(1)} mL)`, 'ilRatio'],
    ['qcGg', sc.ggOffsetMm === null ? '-' : sc.ggOffsetMm.toFixed(1) + ' mm', 'ggOffsetMm']].forEach(([id, text, k]) => {
    $(id).textContent = text; $(id).className = sc.ggOffsetMm === null && k === 'ggOffsetMm' ? '' : SHAPE_FLAGS[k](sc[k]) ? 'flag' : 'ok';
  });
  const pp = state.priorPct;
  $('qcPrior').textContent = pp === null ? '-' : pp.toFixed(1) + '%';
  $('qcPrior').className = pp === null ? '' : pp > Q.PRIOR_FLAG_PCT ? 'flag' : 'ok';
  $('qcTitle').textContent = `QC of the ${$('showCorrected').checked ? 'corrected' : 'raw'} labels`;
  const f = shapeRecord().shape_flags;
  $('flagNote').textContent = f ? `Flags: ${f}. Check them before including.` : 'No flags.';
}

function shapes(labR) {
  const sc = shapeChecks(labR, state.ras.dims, state.ras.affine, state.plane);
  if (!(state.pass === 'template' && $('midlineMode').value === 'lats')) sc.ggOffsetMm = null;
  return sc;
}

function shapeRecord() {
  const sc = shapes(state.correctedR);
  const pp = state.priorPct, flags = Object.keys(SHAPE_FLAGS).filter(k => SHAPE_FLAGS[k](sc[k]));
  if (pp !== null && pp > Q.PRIOR_FLAG_PCT) flags.push('prior_removed');
  return { cap_coverage_pct: +sc.capPct.toFixed(1), il_side_ratio: +sc.ilRatio.toFixed(3), gg_offset_mm: sc.ggOffsetMm === null ? null : +sc.ggOffsetMm.toFixed(2),
    shape_flags: flags.join(','), prior_removed_pct: pp === null ? null : +pp.toFixed(2) };
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
  $('stepWhat').textContent = (state.pass === 'session' && s.whatSession) || s.what;
  $('stepInfo').innerHTML = s.info;
  document.querySelectorAll('.stepbody').forEach(b => b.classList.toggle('on', b.dataset.step === s.key));
  $('showCorrected').checked = !['mri', 'raw'].includes(s.key);
  [...TOGGLES, ...SLIDERS.map(x => x[0])].forEach(id => { $(id).disabled = state.frozen; });
  if (state.frozen && ['surface', 'smooth'].includes(s.key)) $('stepWhat').textContent = `These labels were edited by hand, so corrections are frozen. Undo the hand edits (step ${list.length}) to change them.`;
  $('frozenRow').hidden = !state.frozen; $('editHere').hidden = state.pass === 'session';
  renderGrades();
  $('backStep').disabled = state.step === 0;
  $('nextStep').disabled = state.step === list.length - 1;
  if (state.nv) state.nv.setCrosshairWidth(s.key === 'midline' ? 0 : 1);
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

async function unchangedOnDisk(it) {
  const disk = await readJson([...it.qaFolder, it.sidecar]);
  if (!disk || disk.updated === state.seen[keyOf(it)]) return true;
  const m = `${it.id} was changed elsewhere (by ${disk.rater} at ${disk.updated}). Reopen the folder before changing it.`;
  status(m); alert(m); return false;
}

async function edit(patch, action) {
  const it = cur(), r = rec(it);
  if (r.status !== 'done' && !(await unchangedOnDisk(it))) return;
  const a = asymmetry();
  const done = r.status === 'done';
  state.records[keyOf(it)] = Q.touch({ ...r, ...patch, status: done ? 'done' : 'in progress', step: state.step, steps: state.settings,
    midline: midlineRecord(), asymmetry_pct: a ? +a.pct.toFixed(2) : null, ...shapeRecord(), notes: $('notes').value }, rater(), action);
  renderItemBar();
  if (done) { status(`${it.id} is done; this change is not saved until you include or exclude it again.`); return Promise.resolve(); }
  return writeFile([...it.qaFolder, it.sidecar], JSON.stringify(state.records[keyOf(it)], null, 2) + '\n')
    .then(() => { state.seen[keyOf(it)] = state.records[keyOf(it)].updated; }).catch(() => {});
}

function needRater() {
  if (/^[A-Z]{2,8}$/.test(rater())) return true;
  status('Enter your initials (letters only) first.'); $('rater').focus(); return false;
}

async function rate(field, grade) {
  if (!state.label || !needRater()) return;
  await edit({ [field]: grade }, `rate ${field} ${grade}`);
  renderGrades();
  const from = state.step;
  if (field !== 'seg_quality_raw') setTimeout(() => state.step === from && goStep(from + 1), 250);
}

async function goStep(i) {
  const n = steps().length;
  if (!state.label || i < 0 || i >= n) return;
  state.step = i;
  if (state.root) localStorage.setItem(stepStore(cur()), i);
  renderStep();
  await refreshOverlay();
  updateQc();
}

async function finish(patch, action) {
  const it = cur(), a = asymmetry();
  if (!(await unchangedOnDisk(it))) throw new Error('changed elsewhere');
  state.records[keyOf(it)] = Q.touch({ ...rec(it), ...patch, status: 'done', step: steps().length - 1, steps: state.settings,
    midline: midlineRecord(), asymmetry_pct: a ? +a.pct.toFixed(2) : null, ...shapeRecord(), notes: $('notes').value,
    touched_up: state.frozen }, rater(), action);
  await writeFile([...it.qaFolder, it.sidecar], JSON.stringify(state.records[keyOf(it)], null, 2) + '\n');
  state.seen[keyOf(it)] = state.records[keyOf(it)].updated;
  localStorage.removeItem(stepStore(it));
  for (const i of state.items) {
    const d = i !== it && await readJson([...i.qaFolder, i.sidecar]);
    if (d) { state.records[keyOf(i)] = d; state.seen[keyOf(i)] = d.updated; }
  }
  const done = Object.values(state.records).filter(r => r.status !== 'to do').sort((a, b) => (a.pass + a.item).localeCompare(b.pass + b.item));
  await writeFile([...Q.QA_DIR, 'qa.tsv'], Q.toTsv(done));
  status(`${it.id} saved (${patch.decision}).`);
  const next = Q.nextToDo(passItems(), state.records, state.idx);
  if (next < 0) {
    renderItemBar(); renderStep();
    status(state.pass === 'template' ? 'All templates are done. Re-apply them with LATS (--qa), then start Sessions.' : 'All sessions are done.');
    return;
  }
  await loadItem(next);
}

const saveLabels = async () => writeFile([...cur().qaFolder, cur().labels], await gzip(writeNiftiLabels(state.label.header, state.corrected, state.label.dims)));

function missingRating() {
  const r = rec(), need = [['mri', 'mri_quality'], ['final', 'seg_quality_final']].find(([, f]) => !r[f]);
  if (!need) return false;
  status('Rate this first, then include.'); goStep(steps().findIndex(s => s.key === need[0])); return true;
}

async function include() {
  if (!state.label || state.busy || !needRater() || missingRating()) return;
  state.busy = true;
  try {
    if (rec().touch_pending) { if (!(await reloadEdit())) return; } else await saveLabels();
    await finish({ decision: 'include', exclude_reason: null, labels: cur().labels, touch_pending: false }, 'include');
  } catch { /* shown by writeFile or unchangedOnDisk */ } finally { state.busy = false; }
}

async function acceptRaw() {
  if (!state.label || !needRater()) return;
  if (!rec().seg_quality_raw) { status('Rate the labels first, then accept them.'); return; }
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
  try { await saveLabels(); } catch { return; }
  await edit({ labels: cur().labels, touch_pending: true }, 'touch-up started');
  $('touchPanel').hidden = false; $('excludePanel').hidden = true; $('editPanel').hidden = true; setTool('nav');
  $('itkCmd').textContent = itkCommand();
  status('Labels saved. Copy the command, fix and save in ITK-SNAP, then press Reload edited labels.');
}

async function reloadEdit() {
  const it = cur();
  try {
    const edited = await loadNifti([...it.qaFolder, it.labels]);
    if (edited.dims.some((d, k) => d !== state.label.dims[k])) throw new Error('grid differs from the image');
    state.corrected = Int32Array.from(edited.dataC, v => Math.round(v));
  } catch (e) { status('Could not reload: ' + e.message); return false; }
  state.frozen = true;
  await edit({ touched_up: true }, 'touch-up reloaded');
  renderStep(); await recompute();
  status('Edited labels loaded. Include saves them.');
  return true;
}

async function undoTouch() {
  state.frozen = false; state.edit.undo = [];
  await edit({ touched_up: false, touch_pending: false }, 'hand edits undone');
  $('touchPanel').hidden = true;
  renderStep(); await recompute();
}

async function confirmExclude() {
  if (!state.label || state.busy || !needRater()) return;
  const it = cur();
  try {
    await finish({ decision: 'exclude', exclude_reason: $('excludeReason').value, labels: null, touch_pending: false, touched_up: false }, 'exclude');
    await (await dirAt(it.qaFolder)).removeEntry(it.labels).catch(() => {});
  } catch { /* shown by writeFile or unchangedOnDisk */ }
}

// Hand editing (templates): strokes change state.corrected, save on release and freeze the corrections.
const editing = () => state.label && state.edit.tool !== 'nav' && stepKey() === 'decide' && !$('editPanel').hidden && $('showCorrected').checked;
function setTool(t) {
  state.edit.tool = t;
  document.querySelectorAll('#editPanel [data-tool]').forEach(b => b.classList.toggle('on', b.dataset.tool === t));
  if (t === 'nav') $('brushCursor').style.display = 'none';
}
function canvasPoint(e) {
  const nv = state.nv, dpr = nv.uiData.dpr || 1, r = $('gl').getBoundingClientRect();
  const x = (e.clientX - r.left) * dpr, y = (e.clientY - r.top) * dpr, t = nv.tileIndex(x, y);
  return t < 0 || nv.screenSlices[t].axCorSag > 2 ? null : { x, y, dpr, axis: [2, 1, 0][nv.screenSlices[t].axCorSag] };
}
function moveCursor(e) {
  const c = $('brushCursor'), p = editing() && canvasPoint(e), nv = state.nv;
  const f1 = p && nv.canvasPos2frac([p.x, p.y]), f2 = p && nv.canvasPos2frac([p.x + 10, p.y]);
  if (!p || f1[0] < 0 || f2[0] < 0) { c.style.display = 'none'; return; }
  const a = nv.frac2mm(f1), b = nv.frac2mm(f2), mmPerPx = Math.hypot(a[0] - b[0], a[1] - b[1], a[2] - b[2]) / 10;
  const d = +$('brush').value * Math.min(...state.label.zooms) / mmPerPx / p.dpr, r = $('nv').getBoundingClientRect();
  Object.assign(c.style, { display: 'block', width: d + 'px', height: d + 'px', left: e.clientX - r.left + 'px', top: e.clientY - r.top + 'px' });
}
let redraw = 0;
function stroke(e) {
  const p = canvasPoint(e), f = p && state.nv.canvasPos2frac([p.x, p.y]);
  if (!p || f[0] < 0) return;
  const mm = state.nv.frac2mm(f, 0, true), { dims, affine } = state.label;
  const { centre, idx } = brushVoxels(dims, affine, [mm[0], mm[1], mm[2]], p.axis, +$('brush').value);
  const t = state.edit.tool;
  if (!idx.length || (centre === state.edit.last && (t === 'erode' || t === 'dilate'))) return;
  state.edit.last = centre;
  const ch = applyTool(state.corrected, dims, idx, t, +$('editLabel').value), v = state.nv.volumes[1], [nx, ny, nz] = dims;
  for (const [i, from, to] of ch) {
    if (!state.edit.stroke.has(i)) state.edit.stroke.set(i, from);
    const z = i % nz, y = ((i - z) / nz) % ny, x = (i - z - y * nz) / (ny * nz);
    if (v) v.img[x + nx * (y + ny * z)] = to;
  }
  if (ch.length && !redraw) redraw = requestAnimationFrame(() => { redraw = 0; state.nv.updateGLVolume(); });
}
async function afterEdit(action) {
  const first = !state.frozen;
  state.frozen = true;
  if (rec().status !== 'done') try { await saveLabels(); } catch { return; }
  if (first) await edit({ touched_up: true, labels: cur().labels }, action);
  renderStep(); await recompute();
}
async function endStroke() {
  const s = state.edit.stroke;
  state.edit.stroke = null;
  if (!s || !s.size) return;
  state.edit.undo.push(s);
  await afterEdit('edited in the app');
  status(`${s.size} voxels changed${rec().status === 'done' ? '; not saved until you include it again' : ' and saved'}. Z undoes it.`);
}
async function undoStroke() {
  const s = state.edit.undo.pop();
  if (!s) { status('Nothing to undo.'); return; }
  for (const [i, from] of s) state.corrected[i] = from;
  await afterEdit('edit undone');
  status(`Stroke undone (${s.size} voxels).`);
}
['mousedown', 'mousemove', 'mouseup'].forEach(type => $('nv').addEventListener(type, (e) => {
  if (type === 'mousemove') moveCursor(e);
  if (!editing()) return;
  e.stopPropagation(); e.preventDefault();
  if (type === 'mousedown' && e.button === 0) { state.edit.stroke = new Map(); state.edit.last = -1; stroke(e); }
  else if (type === 'mousemove' && state.edit.stroke) stroke(e);
  else if (type === 'mouseup') endStroke();
}, true));
document.addEventListener('mouseup', () => state.edit.stroke && endStroke());
$('editHere').addEventListener('click', () => {
  if (!state.label || !needRater()) return;
  $('editPanel').hidden = !$('editPanel').hidden; $('touchPanel').hidden = true; $('excludePanel').hidden = true;
  setTool($('editPanel').hidden ? 'nav' : 'paint');
});
document.querySelectorAll('#editPanel [data-tool]').forEach(b => b.addEventListener('click', () => setTool(b.dataset.tool)));
$('brush').addEventListener('input', () => { $('brushV').textContent = $('brush').value; });
$('editUndo').addEventListener('click', undoStroke);

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
$('itemList').addEventListener('change', () => { $('itemList').blur(); loadItem(+$('itemList').value); });
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
$('midlineMode').addEventListener('change', async () => { $('midlineMode').blur(); await recompute(); await edit({}, 'midline'); });
$('mlReset').addEventListener('click', async () => { setMidlineControls(defaultMidline()); await recompute(); await edit({}, 'midline reset'); });
['showCorrected', 'showMidline'].forEach(id => $(id).addEventListener('change', async () => { updateQc(); await refreshOverlay(); }));
$('opacity').addEventListener('change', () => state.label && refreshOverlay());
$('notes').addEventListener('change', () => state.label && edit({}, 'notes'));
$('acceptRaw').addEventListener('click', acceptRaw);
document.querySelectorAll('.toExclude').forEach(b => b.addEventListener('click', async () => {
  $('excludeReason').value = stepKey() === 'mri' ? 'MRI unusable' : 'segmentation unusable'; await goStep(steps().length - 1); $('excludePanel').hidden = false; $('touchPanel').hidden = true; }));
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
  else if (k === 's' && sk === 'decide' && $('excludePanel').hidden) include();
  else if (k === 'e' && sk === 'decide' && state.pass === 'template') $('editHere').click();
  else if (!$('editPanel').hidden && sk === 'decide' && 'npx'.includes(k)) setTool({ n: 'nav', p: 'paint', x: 'erase' }[k]);
  else if (!$('editPanel').hidden && sk === 'decide' && /^[1-4]$/.test(k)) $('editLabel').value = k;
  else if (!$('editPanel').hidden && sk === 'decide' && k === 'z') undoStroke();
  else if (k === '[') loadItem(state.idx - 1);
  else if (k === ']') loadItem(state.idx + 1);
  else return;
  e.preventDefault();
});

idb('readonly', s => s.get('root')).then(h => {
  if (h) { $('reopenFolder').textContent = `Reopen ${h.name}`; $('reopenFolder').hidden = false; }
}).catch(() => {});
