import { readNifti, writeNiftiLabels, gunzipIfNeeded } from './nifti.js';
import { runPipeline, largestPiece, moveRotation, moveVolumes, tongueCentre, qcMetrics, midlinePlane, adjustPlane, lateralCounts, planeMask, shapeChecks, SHAPE_FLAGS, rasOrder, toRas, fromRas } from './postproc.js';
import * as Q from './qa.js';
import { brushVoxels, applyTool } from './edit.js';

const $ = (id) => document.getElementById(id);
const LABEL_LUT = {
  R: [0, 230, 38, 51, 250], G: [0, 38, 204, 115, 204], B: [0, 38, 64, 242, 26], A: [0, 255, 255, 255, 255],
  I: [0, 1, 2, 3, 4], labels: ['bg', 'vertical', 'sup-long', 'inf-long', 'genioglossus'],
};
const MIDLINE_LUT = { R: [0, 255], G: [0, 255], B: [0, 255], A: [0, 255], I: [0, 1], labels: ['bg', 'midline'] };
const GENTLE = { thin: false, keepFrac: 0.88, inferiorClip: true, slCap: true, slTrim: 2, ilTrim: 3, trimSd: 2, darkSd: 3, probCut: 0.99,
  followDorsum: true, order: true, smooth: true, sigma: 1, genio: true, shave: true, clean: true, mirror: false,
  mvX: 0, mvY: 0, mvZ: 0, mvPitch: 0, mvYaw: 0, mvRoll: 0 };
const TOGGLES = Object.keys(GENTLE).filter(k => typeof GENTLE[k] === 'boolean');
const SLIDERS = Object.keys(GENTLE).filter(k => typeof GENTLE[k] === 'number'), MOVE = SLIDERS.filter(k => k.startsWith('mv'));
const ML_SLIDERS = ['mlShift', 'mlYaw', 'mlRoll'];
const optionsChanged = (s) => Object.keys(GENTLE).filter(k => s[k] !== GENTLE[k])
  .map(k => `${k}=${typeof s[k] === 'boolean' ? (s[k] ? 'on' : 'off') : s[k]}`).join('; ');

const scaleInfo = (scale) => Object.entries(Q.SCALES[scale])
  .map(([g, [t, d, disp]]) => `<p><b>${g}. ${t}</b> ${d} <i>(${disp})</i></p>`).join('');
const LANDMARKS = `<p><b>What to look for.</b> Axial: superior longitudinal shows a "W" shape, becoming a larger sheet
superiorly. Coronal: the "M" shape of superior longitudinal over transverse/vertical. Sagittal: superior longitudinal is a
band across the top quarter of the tongue from the front of the palate; the air between tongue and palate should not be
labelled. Inferior longitudinal lies alongside the salivary glands and mylohyoid; the mylohyoid (the floor of the mouth) is
not a tongue muscle.</p>`;
const INFO = {
  thin: `<p><b>Thin superior longitudinal</b> (off by default) keeps the share of superior longitudinal set by keep, nearest
the tongue surface, and gives the deeper superior longitudinal voxels to the nearest other muscle.</p>`,
  clip: '<p><b>Clip</b> stops superior longitudinal wrapping under the tongue: none may sit below the lowest vertical voxel.</p>',
  cap: `<p><b>Cap</b> keeps superior longitudinal continuous over the dorsum: where vertical reaches the top of the tongue
inside the area superior longitudinal covers, its top voxels become superior longitudinal at the cap's median thickness.</p>`,
  trims: `<p><b>Trims</b> remove superior and inferior longitudinal voxels on the tongue's outer surface whose intensity is
outside that muscle's own band (its median plus or minus the band's robust SDs, from its interior), up to the set number of layers; 0 turns a trim
off. The image is bias-corrected within the tongue first.</p>`,
  dark: `<p><b>Dark cut</b> removes tongue darker than the tongue's median minus the set robust SDs, where the dark region is
thick (it survives an opening two voxels deep) and joins the outside; 0 turns it off.</p>`,
  prob: `<p><b>Confidence cut</b>: tongue whose segmenter probability is below the set value goes to what surrounds it,
background included, so unsure tongue at the edge is removed (needs the probability map LATS saves); 0 turns it off.
Smoothing comes after the dark and confidence cuts.</p>`,
  follow: `<p><b>Follow the dorsum</b>: each top-to-bottom column takes the labels from before the trim, stretched to the
corrected outline, so the other muscles move down with the new top.</p>`,
  order: `<p><b>No inferior longitudinal above superior longitudinal</b>: in each column, inferior longitudinal above the lowest
superior longitudinal voxel joins the muscles around it.</p>`,
  smooth: `<p><b>Smooth</b> blurs the outline and each muscle by sigma (mm) and keeps the tongue where the outline weight is at
least half, each voxel going to the muscle with the most weight.</p>`,
  genio: `<p><b>Genioglossus rules</b>: genioglossus may not form the side of the tongue or lie above vertical in its coronal
slice, nor reach more than 4 mm in front of vertical or 11 mm in front of superior longitudinal in its sagittal slice
(limits the hand-drawn maps never break). Such voxels are removed; any enclosed by tongue join their neighbours.</p>`,
  shave: `<p><b>Shave side</b> removes the outermost side voxel of vertical and superior longitudinal, which LATS draws about
a voxel wider than raters; inferior longitudinal and genioglossus are left alone.</p>`,
  clean: `<p><b>Clean</b> gives pieces of vertical, superior longitudinal and genioglossus under a quarter of that muscle's
largest piece, and inferior longitudinal pieces under 50 mm³ (so both its sides stay), to whatever surrounds them, and
keeps one connected tongue.</p>`,
  mirror: `<p><b>Mirror cut</b> (off unless you turn it on) removes vertical more than 2 mm from the tongue mirrored across
LATS's midline where the segmenter's tongue probability is below 0.9999, then cleans again. Use it for one-sided errors such as
vertical hanging below the tongue on one side, not for real asymmetry: on a wasted tongue it trims the healthy side. LATS
flags a scan "asymmetric" when more than 3% of its tongue has no mirror partner within 3 mm; such scans are worth checking
either way. It needs LATS's midline.json and probabilities.</p>`,
  move: `<p><b>Move</b> shifts and turns the whole segmentation before the fixes run, for when the segmenter is off by a
little. The sliders move it right or left, forward or back, and up or down, 1 mm at a time, along the axes the views show.
The dials turn it about the centre of the tongue, up to 10 degrees: pitch shows in the sagittal view, yaw in the axial view
and roll in the coronal view, and the line in each dial leans the way the labels turn in that view. The segmenter's
confidence map moves with the labels; the scan does not. While you drag, the view shows the corrected labels moved; the
fixes run again when you let go. Turning resamples the labels, which roughens their edges a little; the smoothing step
evens them out.</p>`,
  midline: `<p><b>LATS mirror plane</b>: LATS registers the template to its left-right mirror image and splits at the plane
halfway. It is used when LATS wrote midline.json.</p><p><b>Genioglossus centroid</b>: a left-right plane through the centre of
genioglossus. <b>Tilted</b>: the plane through the centre of genioglossus at right angles to the tongue's shortest
axis.</p><p><b>Shift</b> moves the plane left or right.
Turn the <b>dials</b> by dragging round them: the line in each dial leans the way the white midline leans in that view, yaw
in the axial view and roll in the coronal view, up to 20 degrees either way.</p><p>Asymmetry is very sensitive to the
midline: a 2 mm shift can move it by more than 20 points. LATS uses the midline saved here when it re-applies the
template, so every session gets it.</p>`,
  qc: `<p><b>sup-long / tongue</b>: superior longitudinal's share of the tongue, against the expected range in the norm
column. <b>Not-tissue</b>: the share of superior longitudinal, or of the whole tongue, whose intensity lies outside the tissue
band (the median of vertical, inferior longitudinal and genioglossus, plus or minus 1.5 robust SD); a high tongue value
points to the scan. <b>Left / right</b> and <b>asymmetry</b>: tongue volume either side of the midline, and left minus right
over their mean. <b>Sup-long cap</b>: the share of tongue columns whose top voxel is superior longitudinal. <b>Inf-long
L/R</b>: the smaller side of inferior longitudinal over the larger. <b>Genioglossus off midline</b>: the mean signed distance
of genioglossus from the midline (LATS mirror plane only). <b>Prior removed</b>: the share of the segmenter's tongue the atlas
prior removed. Flagged values show in orange; LATS's own flags are listed on the Decide step.</p>`,
  view: `<p><b>Segmentation (H)</b> shows or hides the labels. <b>After fixes (B)</b> shows the labels after the fix steps as
set; untick it to see the template labels before the fixes. <b>Opacity</b> sets how strongly the labels cover the scan.
<b>Midline</b> shows the white midline (templates).</p><p>The arrows on each view, or + and - with the mouse over a view,
move one slice: up or down in the axial view, forward or back in the coronal view, right or left in the sagittal view. The
mouse wheel moves one slice per notch.</p>`,
  tools: `<p>Paint, Erase, Erode and Dilate are brushes: choose one, choose the muscle with the buttons or keys 1 to 4, then
drag over the image. Each brush works on the slice under the cursor, inside the brush circle; the brush slider sets the
circle's width. <b>Paint</b> (P) gives the area one chosen muscle and <b>Erase</b> (X) clears the area. <b>Erode</b> removes
one layer of each chosen muscle where that muscle touches the outside of the tongue, and <b>Dilate</b> grows each chosen
muscle one layer outwards. Erode and Dilate can work on several muscles at once: a muscle button adds that muscle, and a
second click removes it. Where background touches two chosen muscles, Dilate gives the background voxel to the muscle with
more voxels next to it. Erode and Dilate change no other muscle; to move a boundary between muscles, use Paint. One stroke
changes one layer, however often the brush passes over a spot; for another layer, do another stroke. <b>Navigate</b> (N)
stops editing; the mouse wheel moves through slices. Z, Cmd+Z or Ctrl+Z undoes a stroke. Each stroke is saved (on a finished template, only when
you include it again), and the corrections are then frozen.</p>`,
};
const STEPS = [
  { key: 'scans', short: 'Scans', title: 'Rate the scans', template: true,
    what: "Rate the template and each session's scan from 1 to 5; no labels are shown. Click a name to view that scan.",
    info: '<p>Anchors from Shaw et al. 2025 (Computers in Biology and Medicine), used verbatim:</p>' + scaleInfo('mri') },
  { key: 'surface', short: 'Surface', title: 'Correct the surface', template: true,
    what: "These are LATS's automatic fixes, redone here from its unfixed labels. If the whole segmentation sits off, move it first. If a trim takes too much or too little, change its layers; B compares.",
    info: ['move', 'clip', 'cap', 'trims', 'dark', 'prob', 'thin'].map(k => INFO[k]).join('') },
  { key: 'smooth', short: 'Smooth', title: 'Follow the dorsum and smooth', template: true,
    what: 'Check that the layers follow the corrected top of the tongue and that the edges are smooth.',
    info: ['follow', 'order', 'smooth', 'genio', 'shave', 'clean', 'mirror'].map(k => INFO[k]).join('') },
  { key: 'midline', short: 'Midline', title: 'Check the midline', template: true,
    what: 'The white line should run down the middle of the tongue in the coronal and axial views. Shift it, or turn the dials, if it is off.',
    info: INFO.midline },
  { key: 'decide', short: 'Decide', title: 'Rate the labels and decide', titleSession: 'Rate the fit and decide',
    what: 'Rate the corrected labels from 1 to 5, then press Include (S) to finish this template. Edit here (E) fixes the labels by hand; Touch up opens them in ITK-SNAP instead. Exclude records why.',
    whatSession: "Rate how well the labels fit this scan from 1 to 5, then press Include (S) to finish this session, or Exclude if the labels do not fit: a misfit means this session's measured change is wrong. Labels are not edited here.",
    info: scaleInfo('seg') + LANDMARKS + `<p><b>Include</b> writes the labels and a record (ratings, settings, midline, notes, your
initials and the time) to derivatives/lats-qa, then loads the next item.</p><p><b>Edit here</b> opens the brushes (see their
i).</p><p><b>Keep largest piece</b> removes tongue that is not joined to the main piece, such as islands left by editing. It
counts as a hand edit, and Z undoes it.</p><p><b>Touch up</b> saves the labels first; on a finished template, this reopens it until you include it again. Paste the command into a terminal, fix the labels in ITK-SNAP, save over the
same file, then press Include (it reloads your edits).</p><p><b>Exclude</b> records the reason; no labels are written. Aim
for under 5 minutes per item.</p>`,
    infoSession: scaleInfo('seg') + LANDMARKS + `<p><b>Include</b> writes this session's labels and a record (rating, notes, your
initials and the time) to derivatives/lats-qa, then loads the next session. <b>Exclude</b> records the reason. The scan's
quality was rated in the template stream.</p>` },
];

const state = { root: null, items: [], records: {}, pass: 'template', idx: -1, step: 0, image: null, label: null, raw: null,
  corrected: null, ras: null, rawR: null, imageR: null, probR: null, correctedR: null, frozen: false, lats: null, plane: null, settings: { ...GENTLE }, nv: null, overlayUrl: null,
  midlineUrl: null, bg: null, bgUrl: null, scan: 0, labelsHidden: false, allDone: false,
  writes: Promise.resolve(), seen: {}, busy: false, stepping: false, priorPct: null, latsFlags: [], edit: { tool: 'nav', stroke: null, undo: [], muscles: [1] } };

const status = (t) => { $('status').textContent = t; };
const keyOf = (it) => it.pass + ':' + it.id;
const passItems = () => state.items.filter(i => i.pass === state.pass && (i.pass === 'template' || i.applied));
const cur = () => passItems()[state.idx];
const rec = (it = cur()) => state.records[keyOf(it)];
const steps = () => STEPS.filter(s => !(s.template && state.pass === 'session'));
const stepKey = () => (steps()[state.step] || {}).key;
const rater = () => $('rater').value.trim().toUpperCase();
const stepStore = (it) => 'tongueqa_step:' + (state.root ? state.root.name : '') + ':' + keyOf(it);
const scanTargets = (it = cur()) => [{ name: 'template', item: it }, ...it.sessions.map(s => ({ name: s.ses, item: s }))];
const waitingRuns = () => state.items.filter(i => i.pass === 'template' && i.sessions.length && !i.sessions[0].applied && rec(i).decision !== 'exclude').length;

let busyN = 0;
async function busy(f) {
  if (!busyN++) $('spin').hidden = false;
  await new Promise(r => requestAnimationFrame(() => setTimeout(r, 0)));
  try { return await f(); } finally { if (!--busyN) $('spin').hidden = true; }
}

// goItem waits for every save and recompute before switching items.
async function goItem(f) {
  if (!state.root || state.switching) return;
  state.switching = true;
  try {
    while (busyN || state.busy || state.stepping) await new Promise(r => setTimeout(r, 50));
    await saving; await state.writes; await viewRun;
    await f();
  } finally { state.switching = false; }
}

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
async function sha256(parts) {
  try { return [...new Uint8Array(await crypto.subtle.digest('SHA-256', await (await readFile(parts)).arrayBuffer()))].map(b => b.toString(16).padStart(2, '0')).join(''); }
  catch { return null; }
}
const gzip = async (bytes) => new Uint8Array(await new Response(new Blob([bytes]).stream().pipeThrough(new CompressionStream('gzip'))).arrayBuffer());
const saveRecord = (it) => writeFile([...it.qaFolder, it.sidecar], JSON.stringify(rec(it), null, 2) + '\n').then(() => { state.seen[keyOf(it)] = rec(it).updated; });
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
  state.items = await Q.discover(list, readJson, sha256);
  if (!state.items.length) { status('No LATS outputs found under derivatives/lats in this folder.'); return; }
  state.records = {};
  for (const it of state.items) state.records[keyOf(it)] = (await readJson([...it.qaFolder, it.sidecar])) || Q.newRecord(it);
  state.seen = Object.fromEntries(Object.entries(state.records).map(([k, r]) => [k, r.updated]));
  // Sessions rated against an earlier re-apply go back to the list; the scan rating stays.
  const stale = state.items.filter(i => i.applied && state.records[keyOf(i)].applied_sha256 && state.records[keyOf(i)].applied_sha256 !== i.appliedSha);
  for (const i of stale) {
    const r = state.records[keyOf(i)];
    state.records[keyOf(i)] = Q.touch({ ...Q.newRecord(i), mri_quality: r.mri_quality, status: r.mri_quality ? 'in progress' : 'to do', history: r.history },
      rater() || null, 'reopened: LATS re-applied the template since');
    await saveRecord(i).catch(() => {});
  }
  if (stale.length) await writeTsv();
  if (!(await readJson([...Q.QA_DIR, 'dataset_description.json']))) {
    await writeFile([...Q.QA_DIR, 'dataset_description.json'], JSON.stringify({ Name: 'Tongue QA of LATS outputs',
      BIDSVersion: '1.9.0', DatasetType: 'derivative', GeneratedBy: [{ Name: 'postproc-qa' }] }, null, 2) + '\n');
  }
  const sessions = gateSessions();
  const templatesLeft = state.items.some(i => i.pass === 'template' && state.records[keyOf(i)].status !== 'done');
  await setPass(templatesLeft || !sessions ? 'template' : 'session');
}

function gateSessions() {
  const open = state.items.some(i => i.pass === 'session' && i.applied);
  $('passSession').disabled = !open;
  $('passSession').title = open ? '' : "Sessions open once LATS has re-applied at least one QA'd template.";
  return open;
}
const closeSessions = (it) => { it.sessions.forEach(s => { s.applied = false; }); gateSessions(); };

async function setPass(pass) {
  state.pass = pass;
  $('passTemplate').classList.toggle('on', pass === 'template');
  $('passSession').classList.toggle('on', pass === 'session');
  if (!passItems().length) { status(`No ${pass} items yet.`); return; }
  const next = Q.nextToDo(passItems(), state.records);
  await loadItem(next < 0 ? 0 : next);
  if (next < 0) showAllDone();
}

function renderItemBar() {
  const items = passItems(), done = items.filter(i => rec(i).status === 'done').length, w = waitingRuns();
  $('progress').textContent = `${done} of ${items.length} ${state.pass === 'template' ? 'templates' : 'sessions'} done`
    + (state.pass === 'session' && w ? `; sessions of ${w} more runs appear once LATS re-applies their templates` : '');
  $('itemList').innerHTML = items.map((it, i) => {
    const s = rec(it).status;
    return `<option value="${i}"${i === state.idx ? ' selected' : ''}>${s === 'done' ? '[done] ' : s === 'in progress' ? '[open] ' : ''}${it.id}</option>`;
  }).join('');
  const s = rec().status, chip = $('itemStatus');
  chip.textContent = s === 'done' ? `done: ${rec().decision}` : s;
  chip.className = 'chip ' + (s === 'done' ? 'done' : s === 'in progress' ? 'progress' : '');
}

function showAllDone() {
  const n = passItems().length, w = waitingRuns();
  state.allDone = true; $('stepCard').hidden = true; $('doneCard').hidden = false; $('editPanel').hidden = true; setTool('nav');
  $('doneText').innerHTML = state.pass === 'template'
    ? `All ${n} templates are done. Next, LATS re-applies them: <code>lats_submit.sh --qa</code> on a cluster, or
<code>--qa-labels</code> for one run. LATS re-applies only templates marked include. Then open <b>2. Sessions</b>: every
session must be checked.`
    : `All ${n} sessions are done.${w ? ` Sessions of ${w} more runs appear once LATS re-applies their templates.` : ''} The
ratings and decisions are in derivatives/lats-qa/qa.tsv.`;
  $('doneBack').textContent = `Back to ${cur().id}`;
  $('goSessions').hidden = state.pass !== 'template' || $('passSession').disabled;
  renderItemBar();
  status('Everything in this list is done. Pick an item from the list to review it.');
}

const lastOne = () => status(`${cur().id} is the last one still to do. Rate the labels on the Decide step, then press Include (S) to finish ${cur().id}, or Exclude.`);

async function nextItem() {
  if (!state.root || !state.items.length) return;
  if (state.idx + 1 < passItems().length) return loadItem(state.idx + 1);
  const n = Q.nextToDo(passItems(), state.records, -1);
  if (n < 0) { showAllDone(); return; }
  if (n === state.idx) { lastOne(); return; }
  await loadItem(n);
  const left = passItems().filter(i => rec(i).status !== 'done').length;
  status(`End of the list: ${left} of ${passItems().length} still to do; ${cur().id} is the first of them.`);
}

function writeControls(s) {
  TOGGLES.forEach(id => { $(id).checked = !!s[id]; });
  SLIDERS.forEach(id => { $(id).value = s[id]; $(id + 'V').textContent = s[id]; });
  drawDials();
}
function readControls() {
  const s = {};
  TOGGLES.forEach(id => { s[id] = $(id).checked; });
  SLIDERS.forEach(id => { s[id] = +$(id).value; });
  return s;
}
function drawDials() {
  document.querySelectorAll('.dial').forEach(d => {
    const v = +$(d.dataset.for).value;
    d.querySelector('g').setAttribute('transform', `rotate(${(+d.dataset.sign || 1) * v})`);
  });
}
function setMidlineControls(m) {
  $('midlineMode').value = m.mode;
  [['mlShift', m.shift_mm], ['mlYaw', m.yaw_deg], ['mlRoll', m.roll_deg]].forEach(([id, v], k) => {
    $(id).value = v; $(id + 'V').textContent = v;
  });
  drawDials();
}
const defaultMidline = () => ({ mode: state.lats ? 'lats' : 'gg', shift_mm: 0, yaw_deg: 0, roll_deg: 0 });

function loadItem(i) {
  return busy(async () => {
    state.label = null; state.allDone = false; $('doneCard').hidden = true; $('stepCard').hidden = false;
    await state.writes;
    const items = passItems();
    if (!items.length) { status(`No ${state.pass} items found.`); return; }
    state.idx = (i + items.length) % items.length;
    const it = cur();
    if (rec(it).status === 'done' && held(it)) {   // a change to a finished item not included again is dropped
      const d = await readJson([...it.qaFolder, it.sidecar]);
      if (d) { state.records[keyOf(it)] = d; state.seen[keyOf(it)] = d.updated; }
    }
    const r = rec(it);
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
    state.raw = Int32Array.from(label.dataC, v => Math.round(v));
    const fixImg = it.fixImage && await loadNifti(it.fixImage).catch(() => null);
    state.rawR = toRas(state.raw, state.ras);
    state.imageR = toRas((fixImg && fixImg.dims.every((d, k) => d === label.dims[k]) ? fixImg : image).dataC, state.ras);
    const prob = it.prob && await loadNifti(it.prob).catch(() => null);
    state.probR = prob && prob.dims.every((d, k) => d === label.dims[k]) ? toRas(prob.dataC, state.ras) : null;
    state.lats = null;
    state.priorPct = Q.priorRemovedPct(it.prior && await readJson(it.prior));
    state.latsFlags = ((it.flags && await readJson(it.flags)) || {}).flags || [];
    const j = it.midline && await readJson(it.midline);
    if (j && j.mode !== 'qa' && [j.point_mm, j.normal].every(a => Array.isArray(a) && a.length === 3)) state.lats = { point: j.point_mm, normal: j.normal };
    $('midlineMode').querySelector('option[value=lats]').disabled = !state.lats;
    state.settings = { ...GENTLE, ...r.steps };
    if (!state.probR) state.settings.probCut = 0;
    writeControls(state.settings);
    setMidlineControls(r.midline && (r.midline.mode !== 'lats' || state.lats) ? r.midline : defaultMidline());
    state.frozen = false; state.preview = null;
    if (r.touched_up && r.labels) {
      try {
        state.corrected = Int32Array.from((await loadNifti([...it.qaFolder, r.labels])).dataC, v => Math.round(v));
        state.frozen = true;
      } catch { r.touched_up = false; }
    }
    $('notes').value = r.notes || '';
    $('touchPanel').hidden = !r.touch_pending; $('excludePanel').hidden = true; $('editPanel').hidden = true;
    state.edit = { tool: 'nav', stroke: null, undo: [], muscles: state.edit.muscles }; setTool('nav');
    $('itkCmd').textContent = itkCommand();
    state.scan = it.pass === 'template' ? Math.max(0, scanTargets(it).findIndex(x => !rec(x.item).mri_quality)) : 0;
    state.step = r.status === 'done' ? steps().length - 1 : Math.min(+localStorage.getItem(stepStore(it)) || r.step || 0, steps().length - 1);
    state.bg = null;
    if (!state.nv) {
      state.nv = new niivue.Niivue({ show3Dcrosshair: false, backColor: [0.1, 0.1, 0.12, 1], isNearestInterpolation: true });
      const interp = state.nv.updateInterpolation.bind(state.nv);
      state.nv.updateInterpolation = (layer, linear = false) => interp(layer, linear || layer === 0);
      await state.nv.attachTo('gl');
    }
    state.image = image; state.label = label;
    renderItemBar();
    renderStep();
    await recompute();
    status(`${it.id}: ${r.status === 'done' ? `done (${r.decision}); open for review, and saving again replaces it` : 'ready'}.`);
  });
}

async function showImage(item) {
  const f = await readFile(item.image);
  if (state.bgUrl) URL.revokeObjectURL(state.bgUrl);
  state.bgUrl = URL.createObjectURL(f);
  await state.nv.loadVolumes([{ url: state.bgUrl, name: f.name.endsWith('.gz') ? 'image.nii.gz' : 'image.nii' }]);
  state.bg = item;
  const lab = item === cur() ? { dataC: state.raw, dims: state.label.dims, affine: state.label.affine } : await loadNifti(item.labelsIn).catch(() => null);
  const c = lab && tongueCentre(lab.dataC, lab.dims, [1, 1, 1]);
  if (c && !isNaN(c[0])) state.nv.scene.crosshairPos = state.nv.mm2frac([0, 1, 2].map(r => c.reduce((s, v, k) => s + lab.affine[r][k] * v, lab.affine[r][3])));
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
const shown = () => state.preview || ($('showCorrected').checked ? state.corrected : state.raw);
const shownR = () => ($('showCorrected').checked ? state.correctedR : state.rawR);
const labelOpacity = () => (state.labelsHidden ? 0 : +$('opacity').value);

async function recompute() {
  if (!state.label) return;
  if (state.pass === 'session') { state.corrected = state.raw; state.correctedR = state.rawR; }
  else if (!state.frozen) {
    state.correctedR = runPipeline(state.rawR, state.imageR, state.ras.dims, state.ras.zooms, state.settings, state.probR,
      state.lats && { affine: state.ras.affine, ...state.lats });
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
  if (!state.label) return;
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
  $('qcTitle').textContent = state.pass === 'session' ? 'QC of the labels' : `QC ${$('showCorrected').checked ? 'after' : 'before'} the fixes`;
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
    shape_flags: [...new Set([...flags, ...state.latsFlags])].join(','), prior_removed_pct: pp === null ? null : +pp.toFixed(2) };
}

// updateView runs one viewer update at a time; want: 1 the midline moved, 2 the labels changed.
let viewRun = null, want = 0;
function updateView(bits) {
  want |= bits;
  if (!viewRun) viewRun = (async () => {
    try {
      while (want) {
        const w = want; want = 0;
        if (!state.label) continue;
        if (w & 1 && stepKey() !== 'scans') { state.plane = currentPlane(); updateQc(); }
        if (w & 2) await drawOverlay(); else if (stepKey() !== 'scans') { await addMidline(); state.nv.updateGLVolume(); }
      }
    } finally { viewRun = null; }
  })();
  return viewRun;
}
const refreshOverlay = () => updateView(2);
const midlineLive = () => (state.label && stepKey() !== 'scans' ? updateView(1) : Promise.resolve());

async function drawOverlay() {
  const nv = state.nv, scan = stepKey() === 'scans' ? scanTargets()[state.scan] : null, bg = scan ? scan.item : cur();
  if (state.bg !== bg) await showImage(bg);
  while (nv.volumes.length > 1) nv.removeVolumeByIndex(1);
  if (!scan) {
    const { dims, header } = state.label;
    if (state.overlayUrl) URL.revokeObjectURL(state.overlayUrl);
    state.overlayUrl = URL.createObjectURL(new Blob([writeNiftiLabels(header, shown(), dims)]));
    const v = await nv.addVolumeFromUrl({ url: state.overlayUrl, name: 'labels.nii', opacity: labelOpacity() });
    if (v && v.setColormapLabel) v.setColormapLabel(LABEL_LUT);
    await addMidline();
  }
  nv.updateGLVolume();
  requestAnimationFrame(placeSliceButtons);
}

async function addMidline() {
  const nv = state.nv;
  while (nv.volumes.length > 2) nv.removeVolumeByIndex(2);
  if (!(state.plane && $('showMidline').checked)) return;
  const { dims, zooms, affine, header } = state.label;
  if (state.midlineUrl) URL.revokeObjectURL(state.midlineUrl);
  const mask = planeMask(state.corrected, dims, affine, state.plane, 0.5 * Math.max(...zooms));
  state.midlineUrl = URL.createObjectURL(new Blob([writeNiftiLabels(header, mask, dims)]));
  const m = await nv.addVolumeFromUrl({ url: state.midlineUrl, name: 'midline.nii', opacity: 0.9 });
  if (m && m.setColormapLabel) m.setColormapLabel(MIDLINE_LUT);
}

function setOpacity() {
  if (state.nv && state.nv.volumes.length > 1 && stepKey() !== 'scans') state.nv.setOpacity(1, labelOpacity());
}
function toggleSeg() {
  state.labelsHidden = !state.labelsHidden;
  $('toggleSeg').textContent = `Segmentation: ${state.labelsHidden ? 'off' : 'on'} (H)`;
  $('toggleSeg').classList.toggle('primary', !state.labelsHidden);
  setOpacity();
}

function renderScans() {
  const t = scanTargets();
  $('scanList').innerHTML = t.map((x, k) => {
    const g = rec(x.item).mri_quality;
    return `<div class="scanrow${k === state.scan ? ' on' : ''}" data-k="${k}"><button class="scanname">${x.name}</button>`
      + [1, 2, 3, 4, 5].map(n => `<button data-g="${n}"${n === g ? ' class="sel"' : ''}>${n}</button>`).join('') + '</div>';
  }).join('');
  const g = rec(t[state.scan].item).mri_quality;
  $('scanAnchor').textContent = `${t[state.scan].name}: ${g ? `${g}. ${Q.SCALES.mri[g][0]} ${Q.SCALES.mri[g][2]}.` : 'not rated yet.'}`;
}

function renderStep() {
  const list = steps(), s = list[state.step], r = rec(), tmpl = state.pass === 'template';
  $('stepper').innerHTML = list.map((x, i) => `<li data-i="${i}" class="${i === state.step ? 'on' : i < state.step || r.status === 'done' ? 'done' : ''}"><b>${i + 1}</b><span>${x.short}</span></li>`).join('');
  $('stepTitle').textContent = `${state.step + 1}. ${(!tmpl && s.titleSession) || s.title}`;
  $('stepWhat').textContent = (!tmpl && s.whatSession) || s.what;
  $('stepInfo').innerHTML = (!tmpl && s.infoSession) || s.info;
  document.querySelectorAll('.stepbody').forEach(b => b.classList.toggle('on', b.dataset.step === s.key));
  $('showCorrected').checked = true; $('afterFixes').hidden = !tmpl;
  $('showMidline').checked = s.key !== 'decide'; $('midlineToggle').hidden = !tmpl;
  [...TOGGLES, ...SLIDERS].forEach(id => { $(id).disabled = state.frozen || (id === 'mirror' && !(state.lats && state.probR)) || (id === 'probCut' && !state.probR); });
  if (state.frozen && ['surface', 'smooth'].includes(s.key)) $('stepWhat').textContent = `These labels were edited by hand, so corrections are frozen. Undo the hand edits (step ${list.length}) to change them.`;
  $('mvReset').disabled = state.frozen; $('frozenRow').hidden = !state.frozen; $('editHere').hidden = !tmpl; $('keepLargest').hidden = !tmpl; $('touchUp').hidden = !tmpl;
  if (s.key === 'scans') renderScans();
  renderGrades();
  $('backStep').disabled = state.step === 0;
  $('nextStep').disabled = state.step === list.length - 1;
  if (state.nv) state.nv.setCrosshairWidth(s.key === 'midline' ? 0 : 1);
}

function renderGrades() {
  const g = document.querySelector('.grades'), val = rec().seg_quality_final;
  [...g.children].forEach(b => b.classList.toggle('sel', +b.dataset.g === val));
  $('include').classList.toggle('ready', !!val && rec().status !== 'done');
  document.querySelector('[data-anchor="seg_final"]').textContent = val ? `${val}. ${Q.SCALES.seg[val][0]} ${Q.SCALES.seg[val][2]}.` : 'Not rated yet.';
}

async function unchangedOnDisk(it) {
  const disk = await readJson([...it.qaFolder, it.sidecar]);
  if (!disk || disk.updated === state.seen[keyOf(it)]) return true;
  const m = `${it.id} was changed elsewhere (by ${disk.rater} at ${disk.updated}). Reopen the folder before changing it.`;
  status(m); alert(m); return false;
}

// serial runs saves one at a time.
let saving = Promise.resolve();
const serial = (f) => { const p = saving.then(f); saving = p.catch(() => {}); return p; };

const itemFields = () => {
  const a = asymmetry(), tmpl = state.pass === 'template';
  return { steps: tmpl ? state.settings : null, options_changed: tmpl ? optionsChanged(state.settings) : null, midline: midlineRecord(),
    asymmetry_pct: a ? +a.pct.toFixed(2) : null, ...shapeRecord(), applied_sha256: tmpl ? null : cur().appliedSha, notes: $('notes').value };
};

function edit(patch, action) {
  if (!state.label) return Promise.resolve();
  const it = cur(), fields = { step: state.step, ...itemFields() };
  return serial(async () => {
    const r = rec(it);
    if (r.status !== 'done' && !(await unchangedOnDisk(it))) return;
    const done = r.status === 'done';
    state.records[keyOf(it)] = Q.touch({ ...r, ...patch, status: done ? 'done' : 'in progress', ...fields }, rater(), action);
    renderItemBar();
    if (done) { status(`${it.id} is done; this change is not saved until you include or exclude it again.`); return; }
    await saveRecord(it).catch(() => {});
  });
}

function rateSession(s, grade) {
  return serial(async () => {
    if (!(await unchangedOnDisk(s))) return;
    const r = rec(s);
    state.records[keyOf(s)] = Q.touch({ ...r, mri_quality: grade, status: r.status === 'to do' ? 'in progress' : r.status }, rater(), `rate mri_quality ${grade}`);
    await saveRecord(s).catch(() => {});
    if (r.status === 'done') await writeTsv().catch(() => {});
  });
}

function needRater() {
  if (/^[A-Z]{2,8}$/.test(rater())) return true;
  status('Enter your initials (letters only) first.'); $('rater').focus(); return false;
}

async function rateScan(k, grade) {
  if (!state.label || state.stepping || !needRater()) return;
  const t = scanTargets(), x = t[k], it = cur();
  state.stepping = true;
  status(`You chose ${grade} (${Q.SCALES.mri[grade][0].replace(/\.$/, '')}) for the ${x.name} scan of ${it.id}.`);
  try {
    if (k === 0) await edit({ mri_quality: grade }, `rate mri_quality ${grade}`);
    else await rateSession(x.item, grade);
    const todo = t.map((y, j) => j).filter(j => !rec(t[j].item).mri_quality), next = todo.find(j => j > k) ?? todo[0];
    if (next !== undefined) { state.scan = next; renderScans(); await busy(refreshOverlay); }
    else { renderScans(); await new Promise(r => setTimeout(r, 250)); if (stepKey() === 'scans') await goStep(state.step + 1); }
  } finally { state.stepping = false; }
}

async function rate(grade) {
  if (!state.label || !needRater()) return;
  const it = cur(), chose = `You chose ${grade} (${Q.SCALES.seg[grade][0].replace(/\.$/, '')}) for the labels on ${it.id}.`;
  await edit({ seg_quality_final: grade }, `rate seg_quality_final ${grade}`);
  renderGrades();
  if (rec(it).seg_quality_final === grade && rec(it).status !== 'done') status(`${chose} Press Include (S) to finish ${it.id}, or Exclude.`);
}

async function goStep(i) {
  const n = steps().length;
  if (!state.label || i < 0 || i >= n) return;
  state.step = i;
  if (state.root) localStorage.setItem(stepStore(cur()), i);
  renderStep();
  await busy(refreshOverlay);
  updateQc();
}

async function finish(patch, action) {
  const it = cur(), fields = itemFields();
  await serial(async () => {
    if (!(await unchangedOnDisk(it))) throw new Error('changed elsewhere');
    state.records[keyOf(it)] = Q.touch({ ...rec(it), touched_up: state.frozen, ...patch, status: 'done', step: steps().length - 1,
      ...fields }, rater(), action);
    await saveRecord(it);
  });
  if (it.pass === 'template') closeSessions(it);
  localStorage.removeItem(stepStore(it));
  await writeTsv();
  status(`${it.id} saved (${patch.decision}).`);
  const next = Q.nextToDo(passItems(), state.records, state.idx);
  if (next < 0) { renderStep(); showAllDone(); return; }
  await loadItem(next);
}

// writeTsv writes qa.tsv from the records on disk, except items with changes not saved yet.
const held = (i) => state.records[keyOf(i)].updated !== state.seen[keyOf(i)];
async function writeTsv() {
  const rows = [];
  for (const i of state.items) {
    const d = await readJson([...i.qaFolder, i.sidecar]);
    if (!d) continue;
    if (!held(i)) { state.records[keyOf(i)] = d; state.seen[keyOf(i)] = d.updated; }
    if (d.status !== 'to do') rows.push(d);
  }
  await writeFile([...Q.QA_DIR, 'qa.tsv'], Q.toTsv(rows.sort((a, b) => (a.pass + a.item).localeCompare(b.pass + b.item))));
}

const saveLabels = async (it = cur()) => writeFile([...it.qaFolder, it.labels], await gzip(writeNiftiLabels(state.label.header, state.corrected, state.label.dims)));
const onDiskOk = (it) => serial(() => unchangedOnDisk(it));

function missingRating() {
  if (state.pass === 'template') {
    const k = scanTargets().findIndex(x => !rec(x.item).mri_quality);
    if (k >= 0) { status('Rate every scan first, then include.'); state.scan = k; goStep(0); return true; }
  }
  if (rec().seg_quality_final) return false;
  status('Rate the labels from 1 to 5 first, then include.'); return true;
}

async function include() {
  if (!state.label || state.busy || !needRater() || missingRating()) return;
  const it = cur(), g = rec(it).seg_quality_final;
  if (g <= 2 && !confirm(`The labels on ${it.id} are rated ${g} (${Q.SCALES.seg[g][0].replace(/\.$/, '')}), a fail. Include them anyway?`)) return;
  state.busy = true;
  try {
    await busy(async () => {
      if (!(await onDiskOk(it))) return;
      if (rec(it).touch_pending) { if (!(await reloadEdit())) return; } else await saveLabels(it);
      await finish({ decision: 'include', exclude_reason: null, labels: it.labels, touch_pending: false }, 'include');
    });
  } catch { /* shown by writeFile or unchangedOnDisk */ } finally { state.busy = false; }
}

function itkCommand() {
  const it = cur(), root = $('rootPath').value.trim().replace(/\/$/, '') || '<BIDS folder>';
  return `itksnap -g "${root}/${it.image.join('/')}" -s "${root}/${[...it.qaFolder, it.labels].join('/')}"`;
}

async function touchUp() {
  if (!state.label || !needRater()) return;
  const it = cur();
  await busy(async () => {
    if (!(await onDiskOk(it))) return;
    if (!rec(it).touch_pending) try { await saveLabels(it); } catch { return; }
    state.edit.undo = [];
    const reopen = rec(it).status === 'done';
    if (reopen) state.records[keyOf(it)] = { ...rec(it), status: 'in progress' };
    await edit({ labels: it.labels, touch_pending: true }, reopen ? 'reopened for touch-up' : 'touch-up started');
    if (reopen) { closeSessions(it); await writeTsv(); }
    $('touchPanel').hidden = false; $('excludePanel').hidden = true; $('editPanel').hidden = true; setTool('nav');
    $('itkCmd').textContent = itkCommand();
    status(`Labels saved${reopen ? `; ${it.id} is open again until you include it` : ''}. Copy the command, fix and save in ITK-SNAP, then press Reload edited labels.`);
  });
}

async function reloadEdit() {
  const it = cur();
  try {
    const edited = await loadNifti([...it.qaFolder, it.labels]);
    if (edited.dims.some((d, k) => d !== state.label.dims[k])) throw new Error('grid differs from the image');
    state.corrected = Int32Array.from(edited.dataC, v => Math.round(v));
  } catch (e) { status('Could not reload: ' + e.message); return false; }
  state.frozen = true; state.edit.undo = [];
  await edit({ touched_up: true, touch_pending: false }, 'touch-up reloaded');
  renderStep(); await recompute();
  status('Edited labels loaded. Include saves them.');
  return true;
}

async function undoTouch() {
  if (!state.label) return;
  state.frozen = false; state.edit.undo = [];
  await edit({ touched_up: false, touch_pending: false }, 'hand edits undone');
  $('touchPanel').hidden = true;
  renderStep(); await busy(recompute);
}

async function confirmExclude() {
  if (!state.label || state.busy || !needRater()) return;
  const it = cur();
  try {
    await busy(async () => {
      await finish({ decision: 'exclude', exclude_reason: $('excludeReason').value, labels: null, touch_pending: false, touched_up: false }, 'exclude');
      await (await dirAt(it.qaFolder)).removeEntry(it.labels).catch(() => {});
    });
  } catch { /* shown by writeFile or unchangedOnDisk */ }
}

// Hand editing (templates): strokes change state.corrected, save on release and freeze the corrections.
const editing = () => state.label && state.edit.tool !== 'nav' && stepKey() === 'decide' && !$('editPanel').hidden && $('showCorrected').checked;
const drawMuscles = () => document.querySelectorAll('#muscles [data-k]').forEach(b => b.classList.toggle('on', state.edit.muscles.includes(+b.dataset.k)));
function pickMuscle(k) {
  const m = state.edit.muscles;
  state.edit.muscles = !['erode', 'dilate'].includes(state.edit.tool) ? [k] : !m.includes(k) ? [...m, k].sort() : m.length > 1 ? m.filter(x => x !== k) : m;
  drawMuscles();
}
function setTool(t) {
  state.edit.tool = t;
  if (t === 'paint') state.edit.muscles = state.edit.muscles.slice(0, 1);
  drawMuscles();
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
  const { idx } = brushVoxels(dims, affine, [mm[0], mm[1], mm[2]], p.axis, +$('brush').value);
  const t = state.edit.tool;
  if (!idx.length) return;
  const ch = applyTool(state.corrected, dims, idx, t, state.edit.muscles, state.edit.ref || state.corrected), v = state.nv.volumes[1], [nx, ny, nz] = dims;
  for (const [i, from, to] of ch) {
    if (!state.edit.stroke.has(i)) state.edit.stroke.set(i, from);
    const z = i % nz, y = ((i - z) / nz) % ny, x = (i - z - y * nz) / (ny * nz);
    if (v) v.img[x + nx * (y + ny * z)] = to;
  }
  if (ch.length && !redraw) redraw = requestAnimationFrame(() => { redraw = 0; state.nv.updateGLVolume(); });
}
const touchPending = () => !!rec().touch_pending && (status('Press Reload edited labels first, so the edits saved in ITK-SNAP are kept.'), true);
function afterEdit(action) {
  if (!state.label) return Promise.resolve();
  const it = cur(), first = !state.frozen;
  state.frozen = true;
  return busy(async () => {
    if (rec(it).status !== 'done') {
      if (!(await onDiskOk(it))) return;
      try { await saveLabels(it); } catch { return; }
    }
    if (first) await edit({ touched_up: true, labels: it.labels }, action);
    renderStep(); await recompute();
  });
}
async function endStroke() {
  const s = state.edit.stroke;
  state.edit.stroke = null;
  if (!s || !s.size) return;
  state.edit.undo.push(s);
  await afterEdit('edited in the app');
  status(`${s.size} voxels changed${rec().status === 'done' ? '; not saved until you include it again' : ' and saved'}. Z or Cmd+Z undoes the stroke.`);
}
async function keepLargest() {
  if (!state.label || state.busy || !needRater()) return;
  if (busyN) { status('Still working; press Keep largest piece again when the spinner stops.'); return; }
  if (touchPending()) return;
  const kept = largestPiece(state.corrected, state.label.dims), s = new Map();
  kept.forEach((v, i) => { if (v !== state.corrected[i]) s.set(i, state.corrected[i]); });
  if (!s.size) { status('The tongue is already one piece.'); return; }
  for (const i of s.keys()) state.corrected[i] = 0;
  state.edit.undo.push(s);
  await afterEdit('kept the largest piece');
  status(`${s.size} voxels not joined to the tongue removed${rec().status === 'done' ? '; not saved until you include it again' : ' and saved'}. Z or Cmd+Z undoes this.`);
}
async function undoStroke() {
  if (!state.label) return;
  if (touchPending()) return;
  const s = state.edit.undo.pop();
  if (!s) { status('Nothing to undo.'); return; }
  for (const [i, from] of s) state.corrected[i] = from;
  await afterEdit('edit undone');
  status(`Stroke undone (${s.size} voxels).`);
}
['mousedown', 'mousemove', 'mouseup'].forEach(type => $('nv').addEventListener(type, (e) => {
  if (e.target.closest('#sliceBtns')) return;
  if (type === 'mousemove') { moveCursor(e); state.hoverAxis = state.label ? (canvasPoint(e) || {}).axis : undefined; }
  if (!editing()) return;
  e.stopPropagation(); e.preventDefault();
  if (type === 'mousedown' && e.button === 0) {
    state.edit.stroke = new Map();
    state.edit.ref = ['erode', 'dilate'].includes(state.edit.tool) ? state.corrected.slice() : null;
    stroke(e);
  }
  else if (type === 'mousemove' && state.edit.stroke) stroke(e);
  else if (type === 'mouseup') endStroke();
}, true));
document.addEventListener('mouseup', () => state.edit.stroke && endStroke());
$('nv').addEventListener('mouseleave', () => { state.hoverAxis = undefined; });

// moveSlice moves one slice per arrow, + / - key or WHEEL_STEP of wheel travel; axis 0 x, 1 y, 2 z in RAS voxels.
const WHEEL_STEP = 100;
function moveSlice(axis, d) {
  if (!state.nv || !state.label || axis === undefined) return;
  const v = [0, 0, 0]; v[axis] = d; state.nv.moveCrosshairInVox(...v);
}
let wheelAcc = 0;
$('nv').addEventListener('wheel', (e) => {
  const p = state.label && !e.ctrlKey && canvasPoint(e);
  if (!p) return;
  e.preventDefault(); e.stopPropagation();
  wheelAcc += e.deltaY;
  while (Math.abs(wheelAcc) >= WHEEL_STEP) { const d = Math.sign(wheelAcc); wheelAcc -= d * WHEEL_STEP; moveSlice(p.axis, d); }
}, { capture: true, passive: false });
const SLICE_DIR = [['Right', 'Left'], ['Forward', 'Back'], ['Up', 'Down']];
function placeSliceButtons() {
  const nv = state.nv, dpr = (nv && nv.uiData.dpr) || 1;
  const html = (nv ? nv.screenSlices : []).filter(t => t.axCorSag <= 2).map(t => {
    const [l, top, w] = t.leftTopWidthHeight.map(v => v / dpr), a = [2, 1, 0][t.axCorSag];
    return `<div class="slicebtns" style="left:${l + w - 34}px;top:${top + 4}px"><button data-axis="${a}" data-d="1" title="${SLICE_DIR[a][0]} one slice (+)">\u25B2</button>`
      + `<button data-axis="${a}" data-d="-1" title="${SLICE_DIR[a][1]} one slice (-)">\u25BC</button></div>`;
  }).join('');
  if ($('sliceBtns').innerHTML !== html) $('sliceBtns').innerHTML = html;
}
$('sliceBtns').addEventListener('click', (e) => { const b = e.target.closest('[data-axis]'); if (b) { moveSlice(+b.dataset.axis, +b.dataset.d); b.blur(); } });
new ResizeObserver(() => requestAnimationFrame(placeSliceButtons)).observe($('nv'));
$('editHere').addEventListener('click', () => {
  if (!state.label || !needRater()) return;
  if (touchPending()) return;
  $('editPanel').hidden = !$('editPanel').hidden; $('touchPanel').hidden = true; $('excludePanel').hidden = true;
  setTool($('editPanel').hidden ? 'nav' : 'paint');
});
document.querySelectorAll('#editPanel [data-tool]').forEach(b => b.addEventListener('click', () => setTool(b.dataset.tool)));
$('brush').addEventListener('input', () => { $('brushV').textContent = $('brush').value; });
$('editUndo').addEventListener('click', undoStroke);
$('keepLargest').addEventListener('click', keepLargest);
$('muscles').addEventListener('click', (e) => { const b = e.target.closest('[data-k]'); if (b) pickMuscle(+b.dataset.k); });
$('doneBack').addEventListener('click', () => goItem(() => loadItem(state.idx)));
$('goSessions').addEventListener('click', () => goItem(() => setPass('session')));

document.querySelectorAll('.dial svg').forEach(svg => {
  const dial = svg.closest('.dial'), input = $(dial.dataset.for);
  const turn = (e) => {
    const r = svg.getBoundingClientRect();
    let th = Math.atan2(e.clientX - r.left - r.width / 2, r.top + r.height / 2 - e.clientY) * 180 / Math.PI;
    if (th > 90) th -= 180; else if (th < -90) th += 180;
    const v = Math.max(+input.min, Math.min(+input.max, Math.round((+dial.dataset.sign || 1) * th))) || 0;
    if (+input.value !== v) { input.value = v; input.dispatchEvent(new Event('input')); }
  };
  svg.addEventListener('pointerdown', (e) => { if (!state.label || input.disabled) return; svg.setPointerCapture(e.pointerId); turn(e); });
  svg.addEventListener('pointermove', (e) => { if (svg.hasPointerCapture(e.pointerId)) turn(e); });
  svg.addEventListener('pointerup', (e) => {
    if (!svg.hasPointerCapture(e.pointerId)) return;
    svg.releasePointerCapture(e.pointerId); input.dispatchEvent(new Event('change'));
  });
});

document.addEventListener('click', (e) => {
  const b = e.target.closest('[data-info]');
  if (!b) return;
  const row = b.parentElement;
  let box = row.nextElementSibling;
  if (!box || !box.classList.contains('infobox')) { box = document.createElement('div'); box.className = 'infobox'; box.hidden = true; row.after(box); }
  box.innerHTML = INFO[b.dataset.info]; box.hidden = !box.hidden;
});
$('scanList').addEventListener('click', async (e) => {
  const row = e.target.closest('.scanrow'), b = e.target.closest('[data-g]');
  if (!row) return;
  if (b) { rateScan(+row.dataset.k, +b.dataset.g); return; }
  if (+row.dataset.k !== state.scan) { state.scan = +row.dataset.k; renderScans(); await busy(refreshOverlay); }
});
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
$('passTemplate').addEventListener('click', () => goItem(() => setPass('template')));
$('passSession').addEventListener('click', () => goItem(() => setPass('session')));
$('prevItem').addEventListener('click', () => goItem(() => loadItem(state.idx - 1)));
$('nextItem').addEventListener('click', () => goItem(nextItem));
$('nextTodo').addEventListener('click', () => goItem(async () => {
  const n = Q.nextToDo(passItems(), state.records, state.idx);
  if (n === state.idx) lastOne(); else if (n >= 0) await loadItem(n); else showAllDone();
}));
$('itemList').addEventListener('change', () => { $('itemList').blur(); goItem(() => loadItem(+$('itemList').value)); });
$('stepper').addEventListener('click', (e) => { const li = e.target.closest('li'); if (li) goStep(+li.dataset.i); });
$('backStep').addEventListener('click', () => goStep(state.step - 1));
$('nextStep').addEventListener('click', () => goStep(state.step + 1));
$('infoBtn').addEventListener('click', () => { $('stepInfo').hidden = !$('stepInfo').hidden; });
document.querySelector('.grades').addEventListener('click', (e) => { const b = e.target.closest('button'); if (b) rate(+b.dataset.g); });
const settingsChanged = () => busy(async () => { state.settings = readControls(); state.preview = null; await recompute(); await edit({}, 'settings'); });

// movePreview shows the fixed labels moved while a move control is dragged; letting go reruns the fixes.
let mvFrame = 0;
function movePreview() {
  drawDials();
  if (!state.label || state.frozen || state.pass !== 'template' || mvFrame) return;
  mvFrame = requestAnimationFrame(() => {
    mvFrame = 0;
    if (!state.label) return;
    const deg = s => [s.mvPitch, s.mvYaw, s.mvRoll], mm = s => [s.mvX, s.mvY, s.mvZ], a = state.settings, b = readControls();
    if (MOVE.every(k => a[k] === b[k])) { state.preview = null; refreshOverlay(); return; }
    const R0 = moveRotation(deg(a)), R1 = moveRotation(deg(b)), t0 = mm(a);
    const Rd = R1.map(r => [0, 1, 2].map(j => r[0] * R0[j][0] + r[1] * R0[j][1] + r[2] * R0[j][2]));
    const td = mm(b).map((v, i) => v - (Rd[i][0] * t0[0] + Rd[i][1] * t0[1] + Rd[i][2] * t0[2]));
    const { dims, zooms } = state.ras;
    state.preview = fromRas(moveVolumes([state.correctedR], dims, zooms, Rd, td, tongueCentre(state.rawR, dims, zooms))[0], state.ras);
    refreshOverlay();
  });
}
MOVE.forEach(id => $(id).addEventListener('input', movePreview));
$('mvReset').addEventListener('click', () => { if (state.label && !state.frozen) { MOVE.forEach(id => { $(id).value = 0; $(id + 'V').textContent = 0; }); drawDials(); settingsChanged(); } });
TOGGLES.forEach(id => $(id).addEventListener('change', settingsChanged));
SLIDERS.forEach(id => {
  $(id).addEventListener('input', () => { $(id + 'V').textContent = $(id).value; });
  $(id).addEventListener('change', settingsChanged);
});
ML_SLIDERS.forEach(id => {
  $(id).addEventListener('input', () => { $(id + 'V').textContent = $(id).value; drawDials(); midlineLive(); });
  $(id).addEventListener('change', () => busy(async () => { await midlineLive(); await edit({}, 'midline'); }));
});
$('midlineMode').addEventListener('change', () => { $('midlineMode').blur(); busy(async () => { await midlineLive(); await edit({}, 'midline'); }); });
$('mlReset').addEventListener('click', () => busy(async () => { setMidlineControls(defaultMidline()); await midlineLive(); await edit({}, 'midline reset'); }));
$('showCorrected').addEventListener('change', async () => { updateQc(); await busy(refreshOverlay); });
$('showMidline').addEventListener('change', () => midlineLive());
$('opacity').addEventListener('input', setOpacity);
$('toggleSeg').addEventListener('click', () => { toggleSeg(); $('toggleSeg').blur(); });
$('notes').addEventListener('change', () => state.label && edit({}, 'notes'));
document.querySelectorAll('.toExclude').forEach(b => b.addEventListener('click', async () => {
  $('excludeReason').value = 'MRI unusable'; await goStep(steps().length - 1); $('excludePanel').hidden = false; $('touchPanel').hidden = true; }));
$('include').addEventListener('click', include);
$('touchUp').addEventListener('click', touchUp);
$('reloadEdit').addEventListener('click', () => busy(reloadEdit));
$('undoTouch').addEventListener('click', undoTouch);
$('copyCmd').addEventListener('click', () => navigator.clipboard.writeText($('itkCmd').textContent));
$('exclude').addEventListener('click', () => { $('excludePanel').hidden = false; $('touchPanel').hidden = true; });
$('confirmExclude').addEventListener('click', confirmExclude);

document.addEventListener('keydown', (e) => {
  if (e.target.matches('textarea, select, input:not([type=range]):not([type=checkbox])') || !state.label || state.stepping) return;
  const k = e.key.toLowerCase(), sk = stepKey(), editOpen = !$('editPanel').hidden && sk === 'decide';
  if (state.allDone && k !== '[' && k !== ']') return;
  const undoable = sk === 'decide' && state.pass === 'template';
  if (e.metaKey || e.ctrlKey) { if (undoable && k === 'z' && !e.shiftKey) { e.preventDefault(); undoStroke(); } return; }
  if (editOpen && /^[1-5]$/.test(k)) { if (k !== '5') pickMuscle(+k); }
  else if (/^[1-5]$/.test(k) && sk === 'scans') rateScan(state.scan, +k);
  else if (/^[1-5]$/.test(k) && sk === 'decide') rate(+k);
  else if (k === 'enter') goStep(state.step + 1);
  else if (k === 'b' && state.pass === 'template') { $('showCorrected').checked = !$('showCorrected').checked; updateQc(); busy(refreshOverlay); }
  else if (k === 'h') toggleSeg();
  else if (k === 's' && sk === 'decide' && $('excludePanel').hidden) include();
  else if (k === 'e' && sk === 'decide' && state.pass === 'template') $('editHere').click();
  else if (editOpen && 'npx'.includes(k)) setTool({ n: 'nav', p: 'paint', x: 'erase' }[k]);
  else if (undoable && k === 'z') undoStroke();
  else if ((k === '+' || k === '=' || k === '-') && state.hoverAxis !== undefined) moveSlice(state.hoverAxis, k === '-' ? -1 : 1);
  else if (k === '[') goItem(() => loadItem(state.idx - 1));
  else if (k === ']') goItem(nextItem);
  else return;
  e.preventDefault();
});

idb('readonly', s => s.get('root')).then(h => {
  if (h) { $('reopenFolder').textContent = `Reopen ${h.name}`; $('reopenFolder').hidden = false; }
}).catch(() => {});
