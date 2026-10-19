// Controller: load image + labels, run the pipeline live, show it in NiiVue, download the result.
import { readNifti, writeNiftiLabels, gunzipIfNeeded } from './nifti.js';
import { runPipeline, qcMetrics } from './postproc.js';

const $ = (id) => document.getElementById(id);
const state = { image: null, label: null, corrected: null, nv: null, overlayUrl: null };

// muscle colour map for the NiiVue label overlay (I = label id)
const LABEL_LUT = {
  R: [0, 230, 38, 51, 250], G: [0, 38, 204, 115, 204],
  B: [0, 38, 64, 242, 26], A: [0, 255, 255, 255, 255],
  I: [0, 1, 2, 3, 4],
  labels: ['bg', 'vertical', 'sup-long', 'inf-long', 'genioglossus'],
};

async function loadFile(fileInput) {
  const f = fileInput.files[0];
  if (!f) return null;
  const raw = await f.arrayBuffer();
  const buf = await gunzipIfNeeded(raw);
  return readNifti(buf);
}

async function tryReady() {
  if (!$('imageFile').files[0] || !$('labelFile').files[0]) return;
  $('status').textContent = 'Loading...';
  try {
    state.image = await loadFile($('imageFile'));
    state.label = await loadFile($('labelFile'));
  } catch (e) {
    $('status').textContent = 'Load failed: ' + e.message; return;
  }
  const di = state.image.dims, dl = state.label.dims;
  if (di[0] !== dl[0] || di[1] !== dl[1] || di[2] !== dl[2]) {
    $('status').textContent = `Grid mismatch: image ${di} vs labels ${dl}. They must be on the same grid.`;
    return;
  }
  $('status').textContent = `Loaded ${dl.join(' x ')}, ${state.label.zooms.map(v => v.toFixed(2)).join(' x ')} mm.`;
  await initViewer();
  apply();
  $('download').disabled = false;
  $('downloadRecipe').disabled = false;
}

async function initViewer() {
  if (!state.nv) {
    state.nv = new niivue.Niivue({ show3Dcrosshair: false, backColor: [0.1, 0.1, 0.12, 1] });
    await state.nv.attachTo('gl');
  }
  // base image from its File (NiiVue handles orientation for display)
  const imgUrl = URL.createObjectURL($('imageFile').files[0]);
  const name = $('imageFile').files[0].name.endsWith('.gz') ? 'image.nii.gz' : 'image.nii';
  await state.nv.loadVolumes([{ url: imgUrl, name }]);
}

function currentSteps() {
  return {
    thin: $('thin').checked, keepFrac: +$('keepFrac').value,
    inferiorClip: $('inferiorClip').checked,
    surfaceTrim: $('surfaceTrim').checked, trimK: +$('trimK').value, trimAll: $('trimAll').checked,
    clean: $('clean').checked, cleanIters: +$('cleanIters').value,
    smooth: $('smooth').checked, sigma: +$('sigma').value,
  };
}

async function apply() {
  if (!state.label) return;
  const steps = currentSteps();
  const { dims, zooms } = state.label;
  const labInt = Int32Array.from(state.label.dataC, v => Math.round(v));
  state.corrected = runPipeline(labInt, state.image.dataC, dims, zooms, steps);
  updateQc();
  await refreshOverlay();
}

function updateQc() {
  const m = qcMetrics(state.corrected, state.image.dataC);
  $('qcRatio').textContent = m.suplongOverTongue.toFixed(3);
  $('qcRatio').className = (m.suplongOverTongue >= 0.237 && m.suplongOverTongue <= 0.392) ? 'ok' : 'flag';
  $('qcSlNt').textContent = (100 * m.suplongNotTissueFrac).toFixed(1) + '%';
  $('qcFgNt').textContent = (100 * m.tongueNotTissueFrac).toFixed(1) + '%';
  const vox = state.label.zooms[0] * state.label.zooms[1] * state.label.zooms[2] / 1000;
  let sl = 0; for (let i = 0; i < state.corrected.length; i++) if (state.corrected[i] === 2) sl++;
  $('qcVol').textContent = (sl * vox).toFixed(1);
}

async function refreshOverlay() {
  const bytes = writeNiftiLabels(state.label.header, state.corrected, state.label.dims);
  if (state.overlayUrl) URL.revokeObjectURL(state.overlayUrl);
  state.overlayUrl = URL.createObjectURL(new Blob([bytes], { type: 'application/octet-stream' }));
  // remove a previous overlay (index 1) if present, then add the fresh one
  while (state.nv.volumes.length > 1) state.nv.removeVolumeByIndex(1);
  const vol = await state.nv.addVolumeFromUrl({ url: state.overlayUrl, name: 'labels.nii', opacity: 0.6 });
  const v = vol || state.nv.volumes[state.nv.volumes.length - 1];
  if (v && v.setColormapLabel) v.setColormapLabel(LABEL_LUT);
  state.nv.updateGLVolume();
}

function download(bytes, filename) {
  const a = document.createElement('a');
  a.href = URL.createObjectURL(new Blob([bytes], { type: 'application/octet-stream' }));
  a.download = filename; a.click();
  setTimeout(() => URL.revokeObjectURL(a.href), 1000);
}

// wire controls
['thin', 'inferiorClip', 'surfaceTrim', 'trimAll', 'clean', 'smooth'].forEach(id => $(id).addEventListener('change', apply));
[['keepFrac', 'keepFracV'], ['trimK', 'trimKV'], ['cleanIters', 'cleanItersV'], ['sigma', 'sigmaV']].forEach(([s, v]) => {
  $(s).addEventListener('input', () => { $(v).textContent = $(s).value; });
  $(s).addEventListener('change', apply);
});
$('imageFile').addEventListener('change', tryReady);
$('labelFile').addEventListener('change', tryReady);
$('presetGentle').addEventListener('click', () => {
  $('thin').checked = true; $('inferiorClip').checked = true; $('surfaceTrim').checked = true;
  $('clean').checked = true; $('smooth').checked = true; apply();
});
$('presetReset').addEventListener('click', () => {
  ['thin', 'inferiorClip', 'surfaceTrim', 'clean', 'smooth'].forEach(id => $(id).checked = false); apply();
});
$('download').addEventListener('click', () => {
  if (!state.corrected) return;
  const bytes = writeNiftiLabels(state.label.header, state.corrected, state.label.dims);
  const base = ($('labelFile').files[0].name || 'labels').replace(/\.nii(\.gz)?$/, '');
  download(bytes, base + '_postproc.nii');
});
$('downloadRecipe').addEventListener('click', () => {
  download(new TextEncoder().encode(JSON.stringify(currentSteps(), null, 2)), 'recipe.json');
});
