/* Tongue-QC rater app. Expects the MANIFEST, RATER and SEED_RATINGS globals
   injected by tongue_qc/html.py. */
(function () {
  'use strict';

  /* ------------------------------------------------------------ constants */

  var ADVANCE_MS = 180;
  var CK_DEBOUNCE_MS = 400;
  var NOTES_DEBOUNCE_MS = 300;
  var GUTTER_PX = 2;

  var UNDO_STACK_MAX = 20;
  var UNDO_NAV_MS = 250;          // floor between two undo-driven scan steps

  var LS_ROOT = 'tongueqc_v1|';
  var SETTINGS_KEY = 'tongueqc_v1__settings';
  var SCHEMA = (MANIFEST && MANIFEST.schema_version) || 'tongueqc_v1';

  /* Build identity (built_utc + root); namespaces localStorage per build. */
  function buildHash() {
    var src = String((MANIFEST && MANIFEST.built_utc) || '') + '\u0000' +
      String((MANIFEST && MANIFEST.root) || '');
    var h1 = 5381, h2 = 52711, i, c;
    for (i = 0; i < src.length; i++) {
      c = src.charCodeAt(i);
      h1 = (((h1 << 5) + h1) ^ c) >>> 0;
      h2 = (((h2 << 5) + h2) + c) >>> 0;
    }
    return h1.toString(36) + h2.toString(36);
  }

  var BUILD_HASH = buildHash();
  var LS_PREFIX = LS_ROOT + BUILD_HASH + '|';
  var SEED_KEY = 'tongueqc_v1__seeded|' + BUILD_HASH;
  var IMPORT_DISMISS_KEY = 'tongueqc_v1__importdone|' + BUILD_HASH;

  /* 'tongueqc_v1|<hash>|<scan_id>' -> {hash, id}; hash is '' when absent. */
  function splitKey(k) {
    if (!k || k.lastIndexOf(LS_ROOT, 0) !== 0) return null;
    var rest = k.slice(LS_ROOT.length);
    var p = rest.indexOf('|');
    if (p < 0) return { hash: '', id: rest };
    return { hash: rest.slice(0, p), id: rest.slice(p + 1) };
  }

  var PLANES = ['sagittal', 'coronal', 'axial'];
  var PLANE_ABBR = { sagittal: 'sag', coronal: 'cor', axial: 'ax' };

  /* RAS canonical, array origin bottom-left, as montage.py draws the frames. */
  var ORIENT = {
    sagittal: { l: 'P', r: 'A', t: 'S', b: 'I' },
    coronal: { l: 'L', r: 'R', t: 'S', b: 'I' },
    axial: { l: 'L', r: 'R', t: 'A', b: 'P' }
  };

  /* Keep byte-identical to build.py CSV_COLUMNS and tests/test_csv.py. */
  var CSV_HEADER = ['schema_version', 'scan_id', 'sub', 'ses', 'space',
    'mri_quality', 'seg_quality', 'mri_disposition', 'seg_disposition',
    'touched', 'notes',
    'tooth_artefact', 'tooth_affects_volume', 'tooth_affects_shape',
    'rater', 'rated_at', 'order'];

  var SAVE_URLS = ['/api/save', 'api/save'];
  var LOAD_URLS = ['/api/load', 'api/load'];

  var SCANS = ((MANIFEST && MANIFEST.scans) || []).slice();
  SCANS.sort(function (a, b) { return (a.order || 0) - (b.order || 0); });

  var INDEX_BY_ID = new Map();
  SCANS.forEach(function (s, i) { INDEX_BY_ID.set(s.scan_id, i); });

  /* ---------------------------------------------------------------- state */

  /* scan_id -> {mri, seg, notes, tooth, toothVol, toothShape, rated_at} */
  var records = new Map();
  var counts = { rated: 0, partial: 0 };
  var idx = 0;
  var focus = 'mri';                // which scale the next digit fills
  var toothStep = null;             // null | 'vol' | 'shape' — live follow-up
  var advanceTimer = null;
  var notesTimer = null;
  var notesScanId = null;
  var lsOK = true;
  var contourBroken = false;

  var sessionStart = Date.now();
  var sessionRated = 0;
  var sinceDownload = 0;

  var settings = {
    autoAdvance: true,
    overlayMode: 'fill',
    opacity: 55,
    checkpointEvery: 100,
    rater: '',
    qcOpen: false,
    metricsOpen: false,
    seedApplied: false
  };

  /* checkpointing */
  var tier = 'none';                // 'server' | 'fs' | 'download'
  var fsHandle = null;
  var saveUrlIdx = 0;
  var ckTimer = null;
  var ckBusy = false;
  var ckPending = false;
  var ckDirty = false;

  var preloadRing = [];

  /* undo history: last UNDO_STACK_MAX record mutations, newest last */
  var undoStack = [];
  var undoSuspended = false;        // bulk imports must not flood the stack
  var lastUndoNavAt = 0;

  /* image loading: digits are ignored until every visible strip has decoded */
  var loadToken = 0;
  var pendingLoads = 0;

  /* -------------------------------------------------------------- helpers */

  function $(id) { return document.getElementById(id); }

  function el(tag, cls, text) {
    var n = document.createElement(tag);
    if (cls) n.className = cls;
    if (text !== undefined && text !== null) n.textContent = String(text);
    return n;
  }

  function clear(node) { while (node.firstChild) node.removeChild(node.firstChild); }

  function pad2(n) { return (n < 10 ? '0' : '') + n; }

  function fmtDuration(ms) {
    var s = Math.floor(ms / 1000);
    var h = Math.floor(s / 3600);
    var m = Math.floor((s % 3600) / 60);
    return (h ? h + 'h ' : '') + pad2(m) + 'm ' + pad2(s % 60) + 's';
  }

  function nowISO() { return new Date().toISOString(); }

  var GRADE_MIN = 1, GRADE_MAX = 5;

  /* Pass/fail per grade; keep in sync with build.py and tests/test_csv.py. */
  var DISPOSITION = {
    mri: { 1: 'fail', 2: 'fail-or-check', 3: 'pass-check', 4: 'pass', 5: 'pass' },
    seg: { 1: 'fail', 2: 'fail',          3: 'pass',       4: 'pass', 5: 'pass' }
  };

  function dispositionOf(scale, grade) {
    if (grade === null || grade === undefined) return '';
    return DISPOSITION[scale][grade] || '';
  }

  function digitOf(code) {
    var m = /^(?:Digit|Numpad)([1-5])$/.exec(code);
    return m ? parseInt(m[1], 10) : null;
  }

  function intOrNull(v) {
    if (v === null || v === undefined || v === '') return null;
    var n = parseInt(v, 10);
    if (isNaN(n) || n < GRADE_MIN || n > GRADE_MAX) return null;
    return n;
  }

  /* ------------------------------------------------------------- settings */

  function loadSettings() {
    try {
      var raw = localStorage.getItem(SETTINGS_KEY);
      if (raw) {
        var v = JSON.parse(raw);
        for (var k in settings) {
          if (Object.prototype.hasOwnProperty.call(v, k)) settings[k] = v[k];
        }
      }
    } catch (e) { /* defaults are fine */ }
    if (RATER && !settings.rater) settings.rater = RATER;
  }

  function saveSettings() {
    try { localStorage.setItem(SETTINGS_KEY, JSON.stringify(settings)); } catch (e) { lsOK = false; }
  }

  /* -------------------------------------------------------------- records */

  function blankRec() {
    return { mri: null, seg: null, notes: '', tooth: false, toothVol: null, toothShape: null, rated_at: '' };
  }

  /* y/yes/1/true -> 'y', n/no/0/false -> 'n', anything else -> null. */
  function ynOrNull(v) {
    if (v === true) return 'y';
    if (v === false) return 'n';
    if (v === null || v === undefined) return null;
    var s = String(v).trim().toLowerCase();
    if (s === 'y' || s === 'yes' || s === '1' || s === 'true') return 'y';
    if (s === 'n' || s === 'no' || s === '0' || s === 'false') return 'n';
    return null;
  }

  function boolOf(v) {
    if (v === true) return true;
    if (v === null || v === undefined) return false;
    var s = String(v).trim().toLowerCase();
    return s === '1' || s === 'y' || s === 'yes' || s === 'true';
  }

  function normRec(v) {
    var tooth = boolOf(v.tooth);
    return {
      mri: intOrNull(v.mri),
      seg: intOrNull(v.seg),
      notes: v.notes === null || v.notes === undefined ? '' : String(v.notes),
      /* Answers only exist while the flag exists. */
      tooth: tooth,
      toothVol: tooth ? ynOrNull(v.toothVol) : null,
      toothShape: tooth ? ynOrNull(v.toothShape) : null,
      rated_at: v.rated_at ? String(v.rated_at) : ''
    };
  }

  /* A tooth flag alone keeps a record. */
  function isBlank(r) {
    return !r || (r.mri === null && r.seg === null && !r.notes && !r.tooth);
  }

  function classOf(r) {
    if (!r) return 'unrated';
    if (r.mri !== null && r.seg !== null) return 'rated';
    if (r.mri !== null || r.seg !== null) return 'partial';
    return 'unrated';
  }

  /* Always hand out a COPY: writeRec compares the stored class before/after. */
  function recOf(id) {
    var r = records.get(id);
    return r ? {
      mri: r.mri, seg: r.seg, notes: r.notes,
      tooth: r.tooth, toothVol: r.toothVol, toothShape: r.toothShape,
      rated_at: r.rated_at
    } : blankRec();
  }

  function stamp(r) { r.rated_at = isBlank(r) ? '' : nowISO(); }

  function pushUndo(id, prev) {
    if (undoSuspended) return;
    undoStack.push({ id: id, rec: prev ? normRec(prev) : null });
    while (undoStack.length > UNDO_STACK_MAX) undoStack.shift();
  }

  /* Ctrl/Cmd+Z: revert the most recent mutation and jump to its scan. */
  function undoLastMutation() {
    var m = undoStack.pop();
    if (!m) { setStatus('warn', 'Nothing left to undo.'); return; }
    undoSuspended = true;
    try {
      writeRec(m.id, m.rec ? normRec(m.rec) : blankRec());
    } finally {
      undoSuspended = false;
    }
    recount();
    var i = INDEX_BY_ID.get(m.id);
    if (i !== undefined && i !== idx) go(i); else { renderRatings(); renderProgress(); }
    setStatus('ok', 'Restored ' + m.id + ' · ' + undoStack.length + ' step(s) left.');
  }

  function writeRec(id, rec) {
    var prev = records.get(id);
    pushUndo(id, prev);
    var before = classOf(prev);
    if (isBlank(rec)) records.delete(id); else records.set(id, rec);
    var after = classOf(records.get(id));

    if (before !== after) {
      if (before === 'rated') counts.rated--;
      else if (before === 'partial') counts.partial--;
      if (after === 'rated') { counts.rated++; sessionRated++; sinceDownload++; }
      else if (after === 'partial') counts.partial++;
    }

    try {
      if (isBlank(rec)) localStorage.removeItem(LS_PREFIX + id);
      else localStorage.setItem(LS_PREFIX + id, JSON.stringify(rec));
    } catch (e) {
      if (lsOK) { lsOK = false; setStatus('err', 'localStorage write failed \u2014 export manually before closing.'); }
    }

    ckDirty = true;
    scheduleCheckpoint();
  }

  function hydrate() {
    var n = 0;
    try {
      for (var i = 0; i < localStorage.length; i++) {
        var k = localStorage.key(i);
        if (!k || k.lastIndexOf(LS_PREFIX, 0) !== 0) continue;
        var id = k.slice(LS_PREFIX.length);
        if (!INDEX_BY_ID.has(id)) continue;
        var v = null;
        try { v = JSON.parse(localStorage.getItem(k)); } catch (e) { continue; }
        if (!v || typeof v !== 'object') continue;
        var r = normRec(v);
        if (isBlank(r)) continue;
        records.set(id, r);
        n++;
      }
    } catch (e) { lsOK = false; }
    recount();
    return n;
  }

  function recount() {
    var rated = 0, partial = 0;
    records.forEach(function (r) {
      var c = classOf(r);
      if (c === 'rated') rated++; else if (c === 'partial') partial++;
    });
    counts.rated = rated;
    counts.partial = partial;
  }

  /* ------------------------------------------------------------ asset URLs */

  function fileFor(scan, plane, kind) {
    var f = scan.files;
    var key = plane + '_' + kind;
    if (f && typeof f === 'object' && f[key]) return f[key];
    return PLANE_ABBR[plane] + '_' + kind + (kind === 'bg' ? '.jpg' : '.png');
  }

  function urlFor(scan, plane, kind) {
    return 'images/' + encodeURIComponent(scan.scan_id) + '/' + encodeURIComponent(fileFor(scan, plane, kind));
  }

  function qcUrl(scan) {
    var assets = (MANIFEST && MANIFEST.assets) || {};
    var name = (scan.files && scan.files.pipeline_qc) || assets.pipeline_qc || 'pipeline_qc.jpg';
    return 'images/' + encodeURIComponent(scan.scan_id) + '/' + encodeURIComponent(name);
  }

  function hasContour(scan) {
    if (contourBroken) return false;
    if (scan.files && typeof scan.files === 'object') return !!scan.files.sagittal_ct;
    if (MANIFEST && MANIFEST.contour !== undefined) return !!MANIFEST.contour;
    return true;
  }

  function ovKind(scan) {
    return (settings.overlayMode === 'contour' && hasContour(scan)) ? 'ct' : 'ov';
  }

  function cur() { return SCANS.length ? SCANS[idx] : null; }

  /* ----------------------------------------------------------- plane build */

  var planeUI = {};

  function buildPlanes() {
    var host = $('planes');
    clear(host);
    PLANES.forEach(function (p) {
      var block = el('div', 'plane');

      var hd = el('div', 'plane-hd');
      hd.appendChild(el('span', null, p));
      var dim = el('span', 'hd-dim');
      hd.appendChild(dim);
      block.appendChild(hd);

      var wrap = el('div', 'stripwrap');
      var rows = el('div', 'rows');

      var rowTop = el('div', 'row plain');
      var bg = el('img', 'strip bg');
      bg.alt = p + ' image strip';
      bg.decoding = 'async';
      rowTop.appendChild(bg);
      var marksTop = el('div', 'marks');
      rowTop.appendChild(marksTop);
      rowTop.appendChild(el('span', 'rowtag', 'image'));

      var rowBot = el('div', 'row over');
      var ov = el('img', 'strip ov');
      ov.alt = p + ' segmentation overlay strip';
      ov.decoding = 'async';
      rowBot.appendChild(ov);
      var marksBot = el('div', 'marks');
      rowBot.appendChild(marksBot);
      rowBot.appendChild(el('span', 'rowtag', 'image + seg'));

      rows.appendChild(rowTop);
      rows.appendChild(rowBot);
      wrap.appendChild(rows);
      block.appendChild(wrap);
      host.appendChild(block);

      bg.addEventListener('click', function () { openLightbox(bg.src); });
      ov.addEventListener('error', onOverlayError);

      planeUI[p] = {
        block: block, rowTop: rowTop, rowBot: rowBot, bg: bg, ov: ov,
        marksTop: marksTop, marksBot: marksBot, dim: dim
      };
    });
  }

  function onOverlayError() {
    if (settings.overlayMode === 'contour' && !contourBroken) {
      contourBroken = true;
      settings.overlayMode = 'fill';
      saveSettings();
      syncOverlayButton();
      renderScan();
      setStatus('warn', 'No contour strips in this build \u2014 showing filled overlay.');
    }
  }

  function buildMarks(container, plane, n, fw, W, slices, withCaptions) {
    clear(container);
    var o = ORIENT[plane];
    var i;
    for (i = 1; i < n; i++) {
      var g = el('div', 'gutter');
      g.style.left = (((i * (fw + GUTTER_PX)) - GUTTER_PX) / W * 100) + '%';
      g.style.width = (GUTTER_PX / W * 100) + '%';
      container.appendChild(g);
    }
    if (withCaptions && slices) {
      for (i = 0; i < n && i < slices.length; i++) {
        var c = el('span', 'slicecap', String(slices[i]));
        c.style.left = (((i * (fw + GUTTER_PX)) + fw / 2) / W * 100) + '%';
        container.appendChild(c);
      }
    }
    container.appendChild(el('span', 'orient l', o.l));
    container.appendChild(el('span', 'orient r', o.r));
    container.appendChild(el('span', 'orient t', o.t));
    container.appendChild(el('span', 'orient b', o.b));
  }

  /* -------------------------------------------------------------- rendering */

  function imagesPending() { return pendingLoads > 0; }

  function setLoadingTag(on) {
    var t = $('loadingtag');
    if (t) t.hidden = !on;
  }

  function markRowLoading(u, on) {
    u.rowTop.classList.toggle('loading', !!on);
    u.rowBot.classList.toggle('loading', !!on);
  }

  /* Swap bg + overlay atomically: decode both off-DOM, then commit together. */
  function swapStrips(u, bgU, ovU, token) {
    var commit = function () {
      if (token !== loadToken) return;              // superseded by a later nav
      u.bg.src = bgU;
      u.ov.src = ovU;
      u.rowBot.style.setProperty('--bgurl', 'url("' + bgU + '")');
      markRowLoading(u, false);
      pendingLoads--;
      if (pendingLoads <= 0) { pendingLoads = 0; setLoadingTag(false); }
    };

    if (typeof Image !== 'function') { commit(); return; }   // non-browser host

    var left = 2;
    var settle = function () { if (--left === 0) commit(); };
    [bgU, ovU].forEach(function (url) {
      var im = new Image();
      im.decoding = 'async';
      im.onload = settle;
      im.onerror = settle;      // commit anyway: onOverlayError handles the swap
      im.src = url;
    });
  }

  function renderScan() {
    var s = cur();
    if (!s) return;

    var kind = ovKind(s);
    loadToken++;
    var token = loadToken;
    pendingLoads = 0;

    PLANES.forEach(function (p) {
      var u = planeUI[p];
      var fp = (s.frame_px && s.frame_px[p]) || [160, 160];
      var slices = (s.slices && s.slices[p]) || [];
      var n = s.n_frames || slices.length || 6;
      var fw = Math.max(1, fp[0] | 0);
      var fh = Math.max(1, fp[1] | 0);
      var W = n * fw + (n - 1) * GUTTER_PX;

      u.block.style.maxWidth = W + 'px';
      u.rowTop.style.aspectRatio = W + ' / ' + fh;
      u.rowBot.style.aspectRatio = W + ' / ' + fh;

      var bgU = urlFor(s, p, 'bg');
      var ovU = urlFor(s, p, kind);
      if (u.bg.getAttribute('src') === bgU && u.ov.getAttribute('src') === ovU) {
        markRowLoading(u, false);
      } else {
        markRowLoading(u, true);
        pendingLoads++;
        swapStrips(u, bgU, ovU, token);
      }

      u.dim.textContent = n + ' frames \u00b7 ' + fw + '\u00d7' + fh + ' px' +
        (slices.length ? ' \u00b7 slices ' + slices[0] + '\u2013' + slices[slices.length - 1] : '');

      buildMarks(u.marksTop, p, n, fw, W, slices, true);
      buildMarks(u.marksBot, p, n, fw, W, slices, false);
    });

    setLoadingTag(pendingLoads > 0);

    $('scanid').textContent = s.scan_id;
    $('scanmeta').textContent = (s.sub || '?') + ' \u00b7 ' + (s.ses || '?') +
      ' \u00b7 ' + (s.space || MANIFEST.space || '?') + ' space \u00b7 order ' +
      (s.order === undefined ? idx : s.order);

    var flags = $('scanflags');
    clear(flags);
    if (s.seg_empty) flags.appendChild(el('span', 'flag', 'EMPTY SEG'));
    if (s.fallback) flags.appendChild(el('span', 'flag', 'slices: ' + s.fallback));
    (s.warnings || []).forEach(function (w) { flags.appendChild(el('span', 'flag', w)); });

    renderQC(s);
    renderMetrics(s);
    renderRatings();
    renderProgress();
  }

  function renderQC(s) {
    var panel = $('qcpanel');
    var img = $('qcimg');
    var miss = $('qcmissing');
    panel.hidden = !settings.qcOpen;
    if (!settings.qcOpen) return;
    if (s.has_pipeline_qc) {
      img.classList.remove('hidden');
      miss.classList.add('hidden');
      var u = qcUrl(s);
      if (img.getAttribute('src') !== u) img.src = u;
    } else {
      img.classList.add('hidden');
      img.removeAttribute('src');
      miss.classList.remove('hidden');
    }
  }

  function addMetricRow(table, k, v) {
    var tr = document.createElement('tr');
    tr.appendChild(el('td', null, k));
    tr.appendChild(el('td', null, v));
    table.appendChild(tr);
  }

  function renderMetrics(s) {
    var panel = $('metricspanel');
    panel.classList.toggle('hidden', !settings.metricsOpen);
    if (!settings.metricsOpen) return;

    var m = s.metrics || {};
    var pp = s.postprocess || {};
    /* Accepts the flattened (build.py) and the nested postprocess.json form. */
    var cc = pp.connected_components || pp;
    var sl = pp.sl_trim || { removed_voxels: pp.sl_removed_voxels, jut_columns: pp.sl_jut_columns };

    var t = $('metricstable');
    clear(t);
    addMetricRow(t, 'tongue volume (mm\u00b3)', m.tongue_volume_mm3 !== undefined ? m.tongue_volume_mm3 : '\u2014');
    addMetricRow(t, 'vertical frac', m.vertical_fraction !== undefined ? m.vertical_fraction : '\u2014');
    addMetricRow(t, 'sup. long. frac', m.superior_longitudinal_fraction !== undefined ? m.superior_longitudinal_fraction : '\u2014');
    addMetricRow(t, 'inf. long. frac', m.inferior_longitudinal_fraction !== undefined ? m.inferior_longitudinal_fraction : '\u2014');
    addMetricRow(t, 'genioglossus frac', m.genioglossus_fraction !== undefined ? m.genioglossus_fraction : '\u2014');
    addMetricRow(t, 'dorsum mean \u03ba (1/mm)', m.dorsum_mean_curvature_1pmm !== undefined ? m.dorsum_mean_curvature_1pmm : '\u2014');
    addMetricRow(t, 'dorsum max \u03ba (1/mm)', m.dorsum_max_curvature_1pmm !== undefined ? m.dorsum_max_curvature_1pmm : '\u2014');
    addMetricRow(t, 'dorsum length (mm)', m.dorsum_length_mm !== undefined ? m.dorsum_length_mm : '\u2014');
    addMetricRow(t, 'cc removed voxels', cc.removed_voxels !== undefined ? cc.removed_voxels : '\u2014');
    addMetricRow(t, 'cc n components', cc.n_components !== undefined ? cc.n_components : '\u2014');
    addMetricRow(t, 'SL trim removed', sl.removed_voxels !== undefined ? sl.removed_voxels : '\u2014');

    var all = $('metricsall');
    clear(all);
    Object.keys(m).forEach(function (k) { addMetricRow(all, k, m[k]); });
  }

  function renderRatings() {
    var s = cur();
    var r = s ? (records.get(s.scan_id) || blankRec()) : blankRec();

    ['mri', 'seg'].forEach(function (scale) {
      var v = r[scale];
      var out = $('val-' + scale);
      out.textContent = v === null ? '\u2013' : String(v);
      out.classList.toggle('empty', v === null);
      var btns = $('btns-' + scale).children;
      for (var i = 0; i < btns.length; i++) {
        btns[i].classList.toggle('sel', v !== null && i === v - GRADE_MIN);
      }
      $('rate-' + scale).classList.toggle('active', focus === scale);
      $('cue-' + scale).textContent = focus === scale ? '\u25c0 NEXT DIGIT' : '';
    });

    var cls = classOf(records.get(s ? s.scan_id : ''));
    var bw = $('badgewrap');
    clear(bw);
    bw.appendChild(el('span', 'badge ' + cls, cls));
    if (r.tooth) bw.appendChild(el('span', 'badge tooth', 'tooth'));

    var nt = $('notes');
    if (notesScanId !== (s ? s.scan_id : null)) {
      flushNotes();
      notesScanId = s ? s.scan_id : null;
      nt.value = r.notes || '';
    }

    $('counter').textContent = (SCANS.length ? idx + 1 : 0) + ' / ' + SCANS.length;

    renderTooth(r);
  }

  /* ------------------------------------------------------ tooth artefact */

  var TOOTH_Q = {
    vol: 'Is the tooth artefact impacting the VOLUME of the segmentation?',
    shape: 'Is the tooth artefact impacting the SHAPE of the tongue?'
  };

  function ynLabel(v) { return v === 'y' ? 'Y' : v === 'n' ? 'N' : '–'; }

  function toothOpen() { return toothStep !== null; }

  function renderTooth(rec) {
    var s = cur();
    var r = rec || (s ? (records.get(s.scan_id) || blankRec()) : blankRec());

    /* Never leave a question open for a record without the tooth flag. */
    if (toothStep !== null && !r.tooth) toothStep = null;

    var box = $('toothask');
    if (box) {
      box.hidden = !toothStep;
      if (toothStep) {
        box.setAttribute('data-step', toothStep);
        $('ta-step').textContent = (toothStep === 'vol' ? '1' : '2') + ' / 2';
        $('ta-q').textContent = TOOTH_Q[toothStep];
        $('ta-sub').textContent = toothStep === 'vol'
          ? 'y = exclude from the volume analysis'
          : 'y = exclude from the shape analysis';
      }
    }
    document.body.classList.toggle('tooth-asking', !!toothStep);

    var chip = $('toothchip');
    if (chip) {
      chip.hidden = !r.tooth;
      chip.textContent = 'TOOTH · vol ' + ynLabel(r.toothVol) +
        ' · shape ' + ynLabel(r.toothShape);
      chip.classList.toggle('asking', !!toothStep);
    }

    var btn = $('toothbtn');
    if (btn) {
      btn.classList.toggle('on', !!r.tooth);
      btn.setAttribute('aria-pressed', r.tooth ? 'true' : 'false');
    }
  }

  function openTooth(step) {
    cancelAdvance();
    toothStep = step;
    renderTooth();
  }

  /* `t` dismisses an open question (flag kept); otherwise it toggles the flag. */
  function toggleTooth() {
    var s = cur();
    if (!s) return;
    if (toothOpen()) {
      toothStep = null;
      renderTooth();
      setStatus('warn', 'Tooth follow-ups dismissed — flag kept, answers left empty.');
      return;
    }
    cancelAdvance();
    var rec = recOf(s.scan_id);
    if (rec.tooth) {
      rec.tooth = false;
      rec.toothVol = null;
      rec.toothShape = null;
      stamp(rec);
      writeRec(s.scan_id, rec);
      renderRatings();
      renderProgress();
      return;
    }
    rec.tooth = true;
    stamp(rec);
    writeRec(s.scan_id, rec);
    /* Already fully rated: ask the follow-ups straight away. */
    if (rec.mri !== null && rec.seg !== null) toothStep = 'vol';
    renderRatings();
    renderProgress();
  }

  function answerTooth(ans) {
    var s = cur();
    if (!s || !toothOpen()) return;
    cancelAdvance();
    var rec = recOf(s.scan_id);
    if (!rec.tooth) { toothStep = null; renderTooth(); return; }
    if (toothStep === 'vol') {
      rec.toothVol = ans;
      toothStep = 'shape';
    } else {
      rec.toothShape = ans;
      toothStep = null;
    }
    var finished = toothStep === null;
    stamp(rec);
    writeRec(s.scan_id, rec);
    renderRatings();
    renderProgress();
    if (finished && settings.autoAdvance && rec.mri !== null && rec.seg !== null) {
      advanceTimer = setTimeout(function () { advanceTimer = null; go(idx + 1); }, ADVANCE_MS);
    }
  }

  function renderProgress() {
    var n = SCANS.length;
    $('progtext').textContent = counts.rated + ' / ' + n + ' rated';
    $('barfill').style.width = (n ? (counts.rated / n * 100) : 0) + '%';
    var mins = (Date.now() - sessionStart) / 60000;
    var rate = mins > 0.05 ? (sessionRated / mins) : 0;
    $('progsub').textContent = counts.partial + ' partial \u00b7 ' + (n - counts.rated - counts.partial) +
      ' untouched \u00b7 ' + fmtDuration(Date.now() - sessionStart) +
      ' \u00b7 ' + rate.toFixed(1) + ' scans/min';
    renderDone(n);
  }

  function renderDone(n) {
    var box = $('donebar');
    if (!box) return;
    var done = n > 0 && counts.rated === n;
    box.hidden = !done;
    if (done) $('donetext').textContent = 'All ' + n + ' scans rated.';
  }

  function setFocus(which) {
    focus = which;
    var col = which === 'mri' ? 'var(--mri)' : 'var(--seg)';
    document.documentElement.style.setProperty('--focus-col', col);
    var label = which === 'mri' ? 'MRI quality' : 'Seg quality';
    $('fb-text').textContent = 'next digit \u2192 ' + label;
    $('fb-sub').textContent = which === 'mri'
      ? 'grade the raw image'
      : 'grade the segmentation' + (settings.autoAdvance ? ' \u2014 auto-advance on' : '');
    renderRatings();
  }

  /* ------------------------------------------------------------ navigation */

  function cancelAdvance() {
    if (advanceTimer !== null) { clearTimeout(advanceTimer); advanceTimer = null; }
  }

  function go(i) {
    if (!SCANS.length) return;
    cancelAdvance();
    flushNotes();
    toothStep = null;               // navigation always dismisses the follow-ups
    i = Math.max(0, Math.min(SCANS.length - 1, i));
    idx = i;
    setFocus('mri');
    renderScan();
    preload();
    $('viewer').scrollTop = 0;
  }

  function nextUnrated() {
    if (!SCANS.length) return;
    for (var k = 1; k <= SCANS.length; k++) {
      var i = (idx + k) % SCANS.length;
      if (classOf(records.get(SCANS[i].scan_id)) !== 'rated') { go(i); return; }
    }
    setStatus('ok', 'Every scan in this build is rated.');
  }

  function preload() {
    [1, 2, -1].forEach(function (d) {
      var i = idx + d;
      if (i < 0 || i >= SCANS.length) return;
      var s = SCANS[i];
      var kind = ovKind(s);
      PLANES.forEach(function (p) {
        [urlFor(s, p, 'bg'), urlFor(s, p, kind)].forEach(function (u) {
          var im = new Image();
          im.decoding = 'async';
          im.src = u;
          preloadRing.push(im);
        });
      });
    });
    while (preloadRing.length > 36) preloadRing.shift();
  }

  /* ---------------------------------------------------------- rating input */

  function applyDigit(v) {
    var s = cur();
    if (!s) return;
    cancelAdvance();
    var rec = recOf(s.scan_id);
    if (focus === 'mri') {
      rec.mri = v;
      stamp(rec);
      writeRec(s.scan_id, rec);
      setFocus('seg');
    } else {
      rec.seg = v;
      stamp(rec);
      writeRec(s.scan_id, rec);
      /* Tooth-flagged scans ask the follow-ups before advancing. */
      if (rec.tooth && rec.mri !== null) toothStep = 'vol';
      renderRatings();
      if (!rec.tooth && settings.autoAdvance && rec.mri !== null) {
        advanceTimer = setTimeout(function () { advanceTimer = null; go(idx + 1); }, ADVANCE_MS);
      }
    }
    renderRatings();
    renderProgress();
    nagRater();
  }

  function nagRater() {
    var f = $('rater');
    if (!f) return;
    if (raterName()) { f.classList.remove('needed'); f.style.outline = ''; return; }
    f.classList.add('needed');
    f.style.outline = '2px solid var(--err, #e2564a)';
    f.style.outlineOffset = '1px';
    setStatus('err', 'Enter your name in the rater field — the CSV needs it to merge.');
  }

  /* Backspace clears the current scan only; on an empty scan it just steps back. */
  function undo() {
    var s = cur();
    if (!s) return;
    cancelAdvance();
    var rec = recOf(s.scan_id);
    /* Unwind order: shape -> volume -> tooth flag -> seg -> mri -> previous scan. */
    if (rec.tooth && rec.toothShape !== null) {
      rec.toothShape = null;
      stamp(rec);
      writeRec(s.scan_id, rec);
      if (toothOpen()) toothStep = 'shape';
    } else if (rec.tooth && rec.toothVol !== null) {
      rec.toothVol = null;
      stamp(rec);
      writeRec(s.scan_id, rec);
      if (toothOpen()) toothStep = 'vol';
    } else if (rec.tooth) {
      rec.tooth = false;
      rec.toothVol = null;
      rec.toothShape = null;
      stamp(rec);
      writeRec(s.scan_id, rec);
      toothStep = null;
    } else if (rec.seg !== null) {
      rec.seg = null;
      stamp(rec);
      writeRec(s.scan_id, rec);
      setFocus('seg');
    } else if (rec.mri !== null) {
      rec.mri = null;
      stamp(rec);
      writeRec(s.scan_id, rec);
      setFocus('mri');
    } else if (idx > 0) {
      var now = Date.now();
      if (now - lastUndoNavAt < UNDO_NAV_MS) return;   // never a fast walk-back
      lastUndoNavAt = now;
      go(idx - 1);
      return;
    }
    renderRatings();
    renderProgress();
  }

  /* ---------------------------------------------------------------- notes */

  function flushNotes() {
    if (notesTimer !== null) { clearTimeout(notesTimer); notesTimer = null; }
    if (!notesScanId) return;
    var val = $('notes').value;
    var rec = recOf(notesScanId);
    if (rec.notes === val) return;
    rec.notes = val;
    stamp(rec);
    writeRec(notesScanId, rec);
    renderProgress();
  }

  /* ------------------------------------------------------------- display */

  function applyDisplay() {
    document.documentElement.style.setProperty('--ov-op', String(settings.opacity / 100));
    $('opacityv').textContent = settings.opacity + '%';
    $('opacity').value = settings.opacity;
  }

  function syncOverlayButton() {
    $('ovmode').textContent = 'overlay: ' + settings.overlayMode;
    $('ovmode').classList.toggle('on', settings.overlayMode === 'contour');
  }

  function setOverlayMode(mode) {
    if (mode === 'contour' && contourBroken) {
      setStatus('warn', 'This build has no contour strips.');
      return;
    }
    settings.overlayMode = mode;
    saveSettings();
    syncOverlayButton();
    renderScan();
    preload();
  }

  function bumpOpacity(d) {
    settings.opacity = Math.max(0, Math.min(100, settings.opacity + d));
    saveSettings();
    applyDisplay();
  }

  function toggleQC() {
    settings.qcOpen = !settings.qcOpen;
    saveSettings();
    $('qctoggle').classList.toggle('on', settings.qcOpen);
    var s = cur();
    if (s) renderQC(s);
  }

  function toggleMetrics() {
    settings.metricsOpen = !settings.metricsOpen;
    saveSettings();
    $('mtoggle').classList.toggle('on', settings.metricsOpen);
    var s = cur();
    if (s) renderMetrics(s);
  }

  function toggleAuto() {
    settings.autoAdvance = !settings.autoAdvance;
    saveSettings();
    $('autoadv').checked = settings.autoAdvance;
    setFocus(focus);
  }

  function openLightbox(src) {
    if (!src) return;
    $('lbimg').src = src;
    $('lightbox').hidden = false;
  }

  function closeLightbox() {
    $('lightbox').hidden = true;
    $('lbimg').removeAttribute('src');
  }

  /* ------------------------------------------------------------------ CSV */

  function q(v) {
    return '"' + String(v === null || v === undefined ? '' : v).replace(/"/g, '""') + '"';
  }

  function raterName() { return $('rater').value.trim(); }

  function buildCSV() {
    var rater = raterName();
    var out = [CSV_HEADER.map(q).join(',')];
    for (var i = 0; i < SCANS.length; i++) {
      var s = SCANS[i];
      var r = records.get(s.scan_id);
      var mri = r && r.mri !== null ? String(r.mri) : '';
      var seg = r && r.seg !== null ? String(r.seg) : '';
      var notes = r && r.notes ? String(r.notes).replace(/[\r\n]+/g, ' ') : '';
      var tooth = !!(r && r.tooth);
      out.push([
        SCHEMA,
        s.scan_id,
        s.sub || '',
        s.ses || '',
        s.space || MANIFEST.space || '',
        mri,
        seg,
        dispositionOf('mri', r && r.mri !== null ? r.mri : null),
        dispositionOf('seg', r && r.seg !== null ? r.seg : null),
        (mri !== '' && seg !== '') ? '1' : '0',
        notes,
        tooth ? '1' : '0',
        tooth && r.toothVol ? r.toothVol : '',
        tooth && r.toothShape ? r.toothShape : '',
        rater,
        (r && r.rated_at) || '',
        s.order === undefined ? i : s.order
      ].map(q).join(','));
    }
    return out.join('\n');
  }

  /* RFC4180: quoted fields, doubled quotes, embedded commas/newlines, CRLF. */
  function parseCSV(text) {
    var rows = [], row = [], field = '', inQ = false, i = 0;
    if (text.charCodeAt(0) === 0xFEFF) text = text.slice(1);
    while (i < text.length) {
      var c = text.charAt(i);
      if (inQ) {
        if (c === '"') {
          if (text.charAt(i + 1) === '"') { field += '"'; i += 2; }
          else { inQ = false; i++; }
        } else { field += c; i++; }
      } else if (c === '"') {
        inQ = true; i++;
      } else if (c === ',') {
        row.push(field); field = ''; i++;
      } else if (c === '\r' || c === '\n') {
        if (c === '\r' && text.charAt(i + 1) === '\n') i++;
        row.push(field); field = ''; rows.push(row); row = []; i++;
      } else {
        field += c; i++;
      }
    }
    if (field !== '' || row.length) { row.push(field); rows.push(row); }
    return rows;
  }

  function importCSV(text, opts) {
    opts = opts || {};
    var rows = parseCSV(text);
    if (!rows.length) return { applied: 0, unknown: 0, skipped: 0, conflicts: 0 };
    var head = rows[0].map(function (h) { return h.trim().toLowerCase(); });
    var col = {};
    CSV_HEADER.forEach(function (name) { col[name] = head.indexOf(name); });
    if (col.scan_id < 0) {
      setStatus('err', 'CSV has no scan_id column \u2014 nothing imported.');
      return { applied: 0, unknown: 0, skipped: 0, conflicts: 0 };
    }

    var pending = [], unknown = 0, conflicts = 0;
    for (var i = 1; i < rows.length; i++) {
      var r = rows[i];
      if (!r.length || (r.length === 1 && r[0] === '')) continue;
      var id = (r[col.scan_id] || '').trim();
      if (!INDEX_BY_ID.has(id)) { unknown++; continue; }
      /* Columns are matched by header name; a missing column reads as empty. */
      var tflag = col.tooth_artefact >= 0 ? boolOf(r[col.tooth_artefact]) : false;
      var inc = normRec({
        mri: col.mri_quality >= 0 ? r[col.mri_quality] : '',
        seg: col.seg_quality >= 0 ? r[col.seg_quality] : '',
        notes: col.notes >= 0 ? (r[col.notes] || '') : '',
        tooth: tflag,
        toothVol: col.tooth_affects_volume >= 0 ? r[col.tooth_affects_volume] : '',
        toothShape: col.tooth_affects_shape >= 0 ? r[col.tooth_affects_shape] : '',
        rated_at: col.rated_at >= 0 ? (r[col.rated_at] || '') : ''
      });
      if (isBlank(inc)) continue;
      var existing = records.get(id);
      var clash = !!existing && (existing.mri !== null || existing.seg !== null || existing.tooth) &&
        (existing.mri !== inc.mri || existing.seg !== inc.seg || existing.tooth !== inc.tooth);
      if (clash) conflicts++;
      pending.push({ id: id, rec: inc, clash: clash });
    }

    var overwrite = true;
    if (conflicts && !opts.force) {
      overwrite = window.confirm(conflicts + ' scan(s) already have ratings that differ from this CSV.\n\n' +
        'OK = overwrite them.  Cancel = keep what is on this machine and import only the rest.');
    }

    var applied = 0, skipped = 0;
    var wasSuspended = undoSuspended;
    undoSuspended = true;             // a bulk import must not flood the stack
    try {
      pending.forEach(function (p) {
        if (p.clash && !overwrite) { skipped++; return; }
        var rec = normRec(p.rec);
        if (!rec.rated_at) rec.rated_at = nowISO();
        writeRec(p.id, rec);
        applied++;
      });
    } finally { undoSuspended = wasSuspended; }
    recount();
    renderScan();
    return { applied: applied, unknown: unknown, skipped: skipped, conflicts: conflicts };
  }

  function downloadCSV(csv, name) {
    var blob = new Blob([csv], { type: 'text/csv;charset=utf-8' });
    var url = URL.createObjectURL(blob);
    var a = document.createElement('a');
    a.href = url;
    a.download = name || 'tongue_qc_ratings.csv';
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    setTimeout(function () { URL.revokeObjectURL(url); }, 4000);
  }

  /* -------------------------------------------------------- checkpointing */

  function setStatus(kind, msg) {
    var box = $('ckstatus');
    box.className = kind;
    $('ckmsg').textContent = msg;
  }

  function setTierLabel() {
    var t = $('cktier');
    if (tier === 'server') t.textContent = 'tier 1 \u00b7 server \u2192 disk';
    else if (tier === 'fs') t.textContent = 'tier 2 \u00b7 file handle \u2192 disk';
    else t.textContent = 'tier 3 \u00b7 download fallback';
  }

  function chooseTier() {
    var proto = location.protocol;
    if (proto === 'http:' || proto === 'https:') tier = 'server';
    else if (fsHandle) tier = 'fs';
    else tier = 'download';
    setTierLabel();
  }

  function degrade(why) {
    if (tier === 'server') {
      tier = fsHandle ? 'fs' : 'download';
    } else if (tier === 'fs') {
      tier = 'download';
      fsHandle = null;
    }
    setTierLabel();
    setStatus('err', why + ' \u2014 dropped to ' + (tier === 'fs' ? 'file handle' : 'download fallback') + '.');
  }

  function scheduleCheckpoint() {
    if (ckTimer !== null) clearTimeout(ckTimer);
    ckTimer = setTimeout(function () { ckTimer = null; runCheckpoint(false); }, CK_DEBOUNCE_MS);
  }

  function runCheckpoint(manual, keepalive) {
    if (ckBusy) { ckPending = true; return; }
    if (!ckDirty && !manual) return;
    ckBusy = true;
    ckDirty = false;
    var csv = buildCSV();

    var done = function () {
      ckBusy = false;
      if (ckPending) { ckPending = false; runCheckpoint(false); }
    };

    if (tier === 'server') {
      postCSV(csv, keepalive).then(function (bytes) {
        setStatus('ok', 'checkpointing to disk \u00b7 ' + bytes + ' bytes \u00b7 ' + timeNow());
        done();
      }).catch(function (err) {
        ckDirty = true;
        degrade('server save failed (' + err.message + ')');
        done();
      });
      return;
    }

    if (tier === 'fs' && fsHandle) {
      writeHandle(csv).then(function () {
        setStatus('ok', 'written through to your CSV \u00b7 ' + csv.length + ' bytes \u00b7 ' + timeNow());
        done();
      }).catch(function (err) {
        ckDirty = true;
        degrade('file write failed (' + err.message + ')');
        done();
      });
      return;
    }

    var every = Math.max(1, settings.checkpointEvery | 0);
    if (sinceDownload >= every) {
      downloadCSV(csv);
      sinceDownload = 0;
      setStatus('ok', 'snapshot downloaded \u00b7 ' + timeNow() + ' \u00b7 next in ' + every + ' ratings');
    } else {
      var left = every - sinceDownload;
      var extra = window.showSaveFilePicker ? ' \u00b7 use Connect CSV\u2026 for continuous writes' : '';
      setStatus('warn', 'kept in this browser only \u00b7 auto-download in ' + left + ' rating' +
        (left === 1 ? '' : 's') + extra);
    }
    done();
  }

  function timeNow() {
    var d = new Date();
    return pad2(d.getHours()) + ':' + pad2(d.getMinutes()) + ':' + pad2(d.getSeconds());
  }

  /* keepalive only for bodies within the browser's keepalive size limit. */
  function postCSV(csv, keepalive) {
    var keep = !!keepalive && new Blob([csv]).size <= 65536;
    var attempt = function (n) {
      if (n >= SAVE_URLS.length) return Promise.reject(new Error('no /api/save endpoint'));
      var url = SAVE_URLS[(saveUrlIdx + n) % SAVE_URLS.length];
      return fetch(url, {
        method: 'POST',
        headers: { 'Content-Type': 'text/csv;charset=utf-8' },
        body: csv,
        keepalive: keep
      }).then(function (resp) {
        if (!resp.ok) throw new Error('HTTP ' + resp.status);
        saveUrlIdx = (saveUrlIdx + n) % SAVE_URLS.length;
        return resp.json().then(function (j) {
          return (j && j.bytes) || csv.length;
        }, function () { return csv.length; });
      }).catch(function (e) {
        if (n + 1 < SAVE_URLS.length) return attempt(n + 1);
        throw e;
      });
    };
    return attempt(0);
  }

  function writeHandle(csv) {
    return fsHandle.createWritable().then(function (w) {
      return w.write(csv).then(function () { return w.close(); });
    });
  }

  function connectCSV() {
    if (!window.showSaveFilePicker) {
      setStatus('warn', 'This browser has no File System Access API \u2014 use Export CSV instead.');
      return;
    }
    window.showSaveFilePicker({
      suggestedName: 'tongue_qc_ratings.csv',
      types: [{ description: 'CSV', accept: { 'text/csv': ['.csv'] } }]
    }).then(function (h) {
      fsHandle = h;
      tier = 'fs';
      setTierLabel();
      ckDirty = true;
      runCheckpoint(true);
    }).catch(function (e) {
      if (e && e.name === 'AbortError') return;
      setStatus('err', 'Could not connect a CSV file: ' + (e && e.message ? e.message : e));
    });
  }

  /* Server mode only: adopt an existing on-disk CSV when this browser is empty. */
  function tryServerLoad() {
    if (tier !== 'server') return;
    if (records.size) { setStatus('idle', 'server mode \u00b7 saving starts with your first rating'); return; }
    var attempt = function (n) {
      if (n >= LOAD_URLS.length) return Promise.reject(new Error('none'));
      return fetch(LOAD_URLS[n]).then(function (r) {
        if (!r.ok) throw new Error('HTTP ' + r.status);
        return r.text();
      }).catch(function () { return attempt(n + 1); });
    };
    attempt(0).then(function (text) {
      if (!text || text.indexOf('scan_id') < 0) return;
      var res = importCSV(text, { force: true });
      resetSessionCounters();
      setStatus('ok', 'resumed ' + res.applied + ' rating(s) from the CSV on disk' +
        (res.unknown ? ' \u00b7 ' + res.unknown + ' unknown scan_id(s) ignored' : ''));
    }).catch(function () {
      setStatus('idle', 'server mode \u00b7 saving starts with your first rating');
    });
  }

  /* Imports must not pollute the session rate or trip an auto-download. */
  function resetSessionCounters() {
    sessionStart = Date.now();
    sessionRated = 0;
    sinceDownload = 0;
    ckDirty = false;
    if (ckTimer !== null) { clearTimeout(ckTimer); ckTimer = null; }
    renderProgress();
  }

  /* ------------------------------------------------------------- keyboard */

  /* Only text-entry fields may swallow the rating keys. */
  var TEXTLIKE_INPUT = {
    text: 1, search: 1, url: 1, email: 1, password: 1, number: 1, tel: 1
  };

  function isTyping(e) {
    var t = e.target;
    if (!t) return false;
    if (t.isContentEditable === true) return true;
    var tag = t.tagName;
    if (tag === 'TEXTAREA') return true;
    if (tag === 'INPUT') {
      var ty = String(t.type || 'text').toLowerCase();
      return TEXTLIKE_INPUT[ty] === 1;
    }
    return false;
  }

  /* Only navigation keys may auto-repeat. */
  var REPEAT_OK = { ArrowRight: 1, ArrowLeft: 1 };

  function onKeyDown(e) {
    if (e.code === 'Escape') {
      if (!$('lightbox').hidden) { closeLightbox(); e.preventDefault(); return; }
      if (toothOpen() && !isTyping(e)) {
        e.preventDefault();
        toothStep = null;
        renderTooth();
        setStatus('warn', 'Tooth follow-ups dismissed — flag kept, answers left empty.');
        return;
      }
      if (isTyping(e)) { e.target.blur(); e.preventDefault(); }
      return;
    }
    if (isTyping(e)) return;

    /* Ctrl/Cmd+Z restores the last cleared or overwritten rating. */
    if ((e.ctrlKey || e.metaKey) && !e.altKey && e.code === 'KeyZ') {
      if (e.repeat) return;
      e.preventDefault();
      undoLastMutation();
      return;
    }
    if (e.ctrlKey || e.metaKey || e.altKey) return;
    if (e.repeat && REPEAT_OK[e.code] !== 1) return;

    var d = digitOf(e.code);
    if (d !== null) {
      e.preventDefault();
      /* Digits are blocked while a tooth question is open. */
      if (toothOpen()) {
        setStatus('warn', 'Answer the tooth question first — y / n, or Esc to dismiss.');
        return;
      }
      if (imagesPending()) { setStatus('warn', 'loading… ignoring that digit until the strips are on screen.'); return; }
      applyDigit(d);
      return;
    }

    switch (e.code) {
      case 'ArrowRight': e.preventDefault(); go(idx + 1); break;
      case 'ArrowLeft': e.preventDefault(); go(idx - 1); break;
      case 'Backspace': e.preventDefault(); undo(); break;
      case 'KeyA': e.preventDefault(); toggleAuto(); break;
      case 'KeyO': e.preventDefault(); setOverlayMode(settings.overlayMode === 'fill' ? 'contour' : 'fill'); break;
      case 'BracketLeft': e.preventDefault(); bumpOpacity(-10); break;
      case 'BracketRight': e.preventDefault(); bumpOpacity(10); break;
      case 'KeyH':
        e.preventDefault();
        document.body.classList.add('ovhidden');
        break;
      case 'KeyQ': e.preventDefault(); toggleQC(); break;
      case 'KeyM': e.preventDefault(); toggleMetrics(); break;
      case 'KeyT': e.preventDefault(); toggleTooth(); break;
      case 'KeyY': e.preventDefault(); if (toothOpen()) answerTooth('y'); break;
      /* `n` answers "no" while a tooth question is open, else focuses notes. */
      case 'KeyN':
        e.preventDefault();
        if (toothOpen()) answerTooth('n'); else $('notes').focus();
        break;
      case 'KeyF': e.preventDefault(); nextUnrated(); break;
      default: break;
    }
  }

  function onKeyUp(e) {
    if (e.code === 'KeyH') document.body.classList.remove('ovhidden');
  }

  /* -------------------------------------------------- build-scoped storage */

  function seedWasApplied() {
    try { return localStorage.getItem(SEED_KEY) === '1'; } catch (e) { return false; }
  }

  function markSeedApplied() {
    try { localStorage.setItem(SEED_KEY, '1'); } catch (e) { /* non-fatal */ }
  }

  /* Ratings for these scan_ids under another build hash; offered, never auto-applied. */
  function foreignRatings() {
    var out = [];
    try {
      for (var i = 0; i < localStorage.length; i++) {
        var k = localStorage.key(i);
        var parts = splitKey(k);
        if (!parts || parts.hash === BUILD_HASH) continue;
        if (!INDEX_BY_ID.has(parts.id)) continue;
        var v = null;
        try { v = JSON.parse(localStorage.getItem(k)); } catch (e) { continue; }
        if (!v || typeof v !== 'object') continue;
        var r = normRec(v);
        if (isBlank(r)) continue;
        out.push({ id: parts.id, rec: r, hash: parts.hash });
      }
    } catch (e) { /* storage unavailable */ }
    return out;
  }

  function importForeign(list) {
    var applied = 0, skipped = 0;
    undoSuspended = true;
    try {
      list.forEach(function (f) {
        var existing = records.get(f.id);
        if (existing && (existing.mri !== null || existing.seg !== null)) { skipped++; return; }
        writeRec(f.id, normRec(f.rec));
        applied++;
      });
    } finally { undoSuspended = false; }
    recount();
    renderScan();
    return { applied: applied, skipped: skipped };
  }

  function clearBuildRatings() {
    var n = records.size;
    if (!window.confirm('Delete all ' + n + ' rating(s) stored in this browser for THIS build?\n\n' +
      'Export the CSV first if you want to keep them. This cannot be undone.')) return;
    var keys = [];
    try {
      for (var i = 0; i < localStorage.length; i++) {
        var k = localStorage.key(i);
        if (k && k.lastIndexOf(LS_PREFIX, 0) === 0) keys.push(k);
      }
      keys.forEach(function (k) { localStorage.removeItem(k); });
    } catch (e) { /* best effort */ }
    records.clear();
    toothStep = null;
    undoStack.length = 0;
    recount();
    resetSessionCounters();
    ckDirty = true;
    scheduleCheckpoint();
    setFocus('mri');
    renderScan();
    setStatus('warn', 'Cleared ' + n + ' rating(s) for this build.');
  }

  /* ------------------------------------------------------------ extra UI */

  /* Creates the loading tag, the clear-ratings row and the completion card. */
  function buildExtraUI() {
    var fb = $('focusbar');
    if (fb && !$('loadingtag')) {
      var lt = el('span', null, 'loading…');
      lt.id = 'loadingtag';
      lt.hidden = true;
      lt.style.marginLeft = '10px';
      lt.style.flex = '0 0 auto';
      lt.style.fontSize = '11px';
      lt.style.letterSpacing = '0.06em';
      lt.style.color = '#ffcf6b';
      fb.appendChild(lt);
    }

    var ck = $('ckstatus') ? $('ckstatus').parentNode : null;
    if (ck && !$('clearbuild')) {
      var row = el('div', 'btnrow');
      row.style.marginTop = '8px';
      var b = el('button', 'btn', "Clear this build's ratings");
      b.id = 'clearbuild';
      b.type = 'button';
      b.title = 'Delete every rating this browser holds for build ' + BUILD_HASH;
      b.addEventListener('click', function (ev) { clearBuildRatings(); ev.currentTarget.blur(); });
      row.appendChild(b);
      var note = el('div', null, 'build ' + BUILD_HASH + ' · Ctrl/⌘+Z undoes the last rating change');
      note.style.fontSize = '11px';
      note.style.color = '#6c7885';
      note.style.marginTop = '4px';
      ck.appendChild(row);
      ck.appendChild(note);
    }

    var side = $('side');
    if (side && !$('donebar')) {
      var done = el('div', 'card');
      done.id = 'donebar';
      done.hidden = true;
      done.style.background = '#123c22';
      done.style.border = '1px solid #2ea55f';
      var dt = el('div', null, '');
      dt.id = 'donetext';
      dt.style.fontWeight = '700';
      dt.style.marginBottom = '6px';
      done.appendChild(dt);
      var db = el('button', 'btn', 'Export CSV');
      db.type = 'button';
      db.addEventListener('click', function (ev) {
        flushNotes();
        downloadCSV(buildCSV());
        ev.currentTarget.blur();
      });
      done.appendChild(db);
      side.insertBefore(done, side.firstChild);
    }
  }

  function maybeOfferImport() {
    var dismissed = false;
    try { dismissed = localStorage.getItem(IMPORT_DISMISS_KEY) === '1'; } catch (e) { dismissed = true; }
    if (dismissed) return;
    var list = foreignRatings();
    if (!list.length) return;

    var side = $('side');
    if (!side) return;
    var bar = el('div', 'card');
    bar.id = 'importbar';
    bar.style.background = '#3a2e10';
    bar.style.border = '1px solid #9a7a22';
    bar.appendChild(el('div', null,
      list.length + ' rating(s) from a previous build of these scans are in this browser. ' +
      'They were NOT applied — the segmentations may have changed.'));
    var row = el('div', 'btnrow');
    row.style.marginTop = '6px';
    var yes = el('button', 'btn', 'Import ' + list.length);
    yes.type = 'button';
    var no = el('button', 'btn', 'Dismiss');
    no.type = 'button';
    var close = function () {
      try { localStorage.setItem(IMPORT_DISMISS_KEY, '1'); } catch (e) { /* ignore */ }
      if (bar.parentNode) bar.parentNode.removeChild(bar);
    };
    yes.addEventListener('click', function () {
      var res = importForeign(list);
      close();
      setStatus(res.applied ? 'ok' : 'warn', 'imported ' + res.applied + ' old rating(s)' +
        (res.skipped ? ' · ' + res.skipped + ' kept' : ''));
    });
    no.addEventListener('click', close);
    row.appendChild(yes);
    row.appendChild(no);
    bar.appendChild(row);
    side.insertBefore(bar, side.firstChild);
  }

  /* ------------------------------------------------------------------ wire */

  function makeRatingButtons(scale) {
    var host = $('btns-' + scale);
    clear(host);
    for (var v = GRADE_MIN; v <= GRADE_MAX; v++) {
      (function (val) {
        var b = el('button', 'rbtn', String(val));
        b.type = 'button';
        b.title = scale === 'mri' ? 'MRI quality ' + val : 'Seg quality ' + val;
        b.addEventListener('click', function () {
          if (focus !== scale) setFocus(scale);
          applyDigit(val);
          b.blur();
        });
        host.appendChild(b);
      })(v);
    }
  }

  function wire() {
    makeRatingButtons('mri');
    makeRatingButtons('seg');

    $('prev').addEventListener('click', function (ev) { go(idx - 1); ev.currentTarget.blur(); });
    $('next').addEventListener('click', function (ev) { go(idx + 1); ev.currentTarget.blur(); });

    /* Blur after a change so the rating keys keep working. */
    $('autoadv').addEventListener('change', function () {
      settings.autoAdvance = $('autoadv').checked;
      saveSettings();
      setFocus(focus);
      $('autoadv').blur();
    });

    var toothBtn = $('toothbtn');
    if (toothBtn) toothBtn.addEventListener('click', function (ev) { toggleTooth(); ev.currentTarget.blur(); });
    var taYes = $('ta-yes');
    if (taYes) taYes.addEventListener('click', function (ev) { answerTooth('y'); ev.currentTarget.blur(); });
    var taNo = $('ta-no');
    if (taNo) taNo.addEventListener('click', function (ev) { answerTooth('n'); ev.currentTarget.blur(); });
    var taSkip = $('ta-skip');
    if (taSkip) taSkip.addEventListener('click', function (ev) {
      toothStep = null;
      renderTooth();
      ev.currentTarget.blur();
    });

    $('notes').addEventListener('input', function () {
      if (notesTimer !== null) clearTimeout(notesTimer);
      notesTimer = setTimeout(function () { notesTimer = null; flushNotes(); }, NOTES_DEBOUNCE_MS);
    });
    $('notes').addEventListener('blur', flushNotes);

    $('opacity').addEventListener('input', function () {
      settings.opacity = parseInt($('opacity').value, 10);
      applyDisplay();
    });
    $('opacity').addEventListener('change', function () { saveSettings(); $('opacity').blur(); });

    $('ovmode').addEventListener('click', function (ev) {
      setOverlayMode(settings.overlayMode === 'fill' ? 'contour' : 'fill');
      ev.currentTarget.blur();
    });
    $('qctoggle').addEventListener('click', function (ev) { toggleQC(); ev.currentTarget.blur(); });
    $('mtoggle').addEventListener('click', function (ev) { toggleMetrics(); ev.currentTarget.blur(); });

    $('rater').addEventListener('change', function () {
      settings.rater = raterName();
      saveSettings();
      ckDirty = true;
      scheduleCheckpoint();
    });
    $('rater').addEventListener('input', function () {
      if (raterName()) {
        $('rater').classList.remove('needed');
        $('rater').style.outline = '';
      }
    });

    $('ckevery').addEventListener('change', function () {
      var v = parseInt($('ckevery').value, 10);
      settings.checkpointEvery = (isNaN(v) || v < 1) ? 100 : v;
      $('ckevery').value = settings.checkpointEvery;
      saveSettings();
    });

    $('connectcsv').addEventListener('click', function (ev) { connectCSV(); ev.currentTarget.blur(); });
    $('savenow').addEventListener('click', function (ev) {
      flushNotes();
      ckDirty = true;
      if (tier === 'download') sinceDownload = Math.max(sinceDownload, settings.checkpointEvery);
      runCheckpoint(true);
      ev.currentTarget.blur();
    });
    $('export').addEventListener('click', function (ev) {
      flushNotes();
      downloadCSV(buildCSV());
      ev.currentTarget.blur();
    });

    $('loadbtn').addEventListener('click', function (ev) { $('loadfile').click(); ev.currentTarget.blur(); });
    $('loadfile').addEventListener('change', function () {
      var f = $('loadfile').files && $('loadfile').files[0];
      if (!f) return;
      var fr = new FileReader();
      fr.onload = function () {
        var res = importCSV(String(fr.result));
        setStatus(res.applied ? 'ok' : 'warn',
          'loaded ' + res.applied + ' rating(s)' +
          (res.skipped ? ' \u00b7 ' + res.skipped + ' kept local' : '') +
          (res.unknown ? ' \u00b7 ' + res.unknown + ' unknown scan_id(s) ignored' : ''));
      };
      fr.onerror = function () { setStatus('err', 'Could not read that file.'); };
      fr.readAsText(f);
      $('loadfile').value = '';
    });

    $('fillcsv').addEventListener('click', function (ev) {
      flushNotes();
      $('csvbox').value = buildCSV();
      ev.currentTarget.blur();
    });
    $('copycsv').addEventListener('click', function (ev) {
      flushNotes();
      var box = $('csvbox');
      box.value = buildCSV();
      box.focus();
      box.select();
      var ok = false;
      try { ok = document.execCommand('copy'); } catch (e) { ok = false; }
      if (!ok && navigator.clipboard) {
        navigator.clipboard.writeText(box.value).then(function () {
          setStatus('ok', 'CSV copied to the clipboard.');
        }, function () {
          setStatus('warn', 'Copy blocked \u2014 select the text and copy manually.');
        });
      } else {
        setStatus(ok ? 'ok' : 'warn', ok ? 'CSV copied to the clipboard.' : 'Copy blocked \u2014 select and copy manually.');
      }
      box.blur();
      ev.currentTarget.blur();
    });

    $('qcimg').addEventListener('click', function () { openLightbox($('qcimg').src); });
    $('lightbox').addEventListener('click', closeLightbox);

    document.addEventListener('keydown', onKeyDown);
    document.addEventListener('keyup', onKeyUp);
    window.addEventListener('blur', function () { document.body.classList.remove('ovhidden'); });
    window.addEventListener('beforeunload', function () {
      flushNotes();
      if (ckTimer !== null) { clearTimeout(ckTimer); ckTimer = null; runCheckpoint(false, true); }
    });
  }

  /* ------------------------------------------------------------------ init */

  function init() {
    loadSettings();
    buildPlanes();
    buildExtraUI();
    wire();

    $('buildmeta').textContent = SCANS.length + ' scans \u00b7 ' +
      (MANIFEST.space || '?') + ' \u00b7 built ' + String(MANIFEST.built_utc || '').slice(0, 16);

    $('autoadv').checked = settings.autoAdvance;
    $('ckevery').value = settings.checkpointEvery;
    $('rater').value = settings.rater || '';
    $('qctoggle').classList.toggle('on', settings.qcOpen);
    $('mtoggle').classList.toggle('on', settings.metricsOpen);
    syncOverlayButton();
    applyDisplay();

    hydrate();

    /* The seed CSV is applied once per build. */
    if (SEED_RATINGS && !seedWasApplied()) {
      undoSuspended = true;
      var res;
      try { res = importCSV(SEED_RATINGS, { force: false }); } finally { undoSuspended = false; }
      markSeedApplied();
      resetSessionCounters();
      if (res.applied) setStatus('idle', 'pre-loaded ' + res.applied + ' rating(s) from the build.');
    }

    maybeOfferImport();

    if (!SCANS.length) {
      $('viewer-empty').classList.remove('hidden');
      $('planes').classList.add('hidden');
      setStatus('err', 'This build contains no scans.');
      return;
    }

    /* Open on the first unrated scan. */
    idx = 0;
    for (var i = 0; i < SCANS.length; i++) {
      if (classOf(records.get(SCANS[i].scan_id)) !== 'rated') { idx = i; break; }
    }

    setFocus('mri');
    renderScan();
    preload();

    chooseTier();
    if (tier === 'server') {
      tryServerLoad();
    } else if (window.showSaveFilePicker) {
      setStatus('warn', 'no server \u2014 click Connect CSV\u2026 to write straight to a file on disk');
    } else {
      setStatus('warn', 'no server \u2014 snapshots auto-download every ' + settings.checkpointEvery + ' ratings');
    }

    setInterval(renderProgress, 1000);
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();
