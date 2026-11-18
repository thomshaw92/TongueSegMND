import json
import sys
from pathlib import Path

import nibabel as nib
import numpy as np
import pytest

from lats.config import LatsConfig, Timepoint
from lats.flags import template_labels, write_flags
from lats.nodes.autofix import _autofix, _fix_session, autofix, clip_suplong, dark_cut, drop_pieces, genio_rules, shave_side, largest_piece, order_inflong, sl_cap, smooth


def tongue():
    lab = np.zeros((24, 24, 22), np.int16)
    lab[4:20, 4:20, 4:16] = 1
    lab[4:20, 4:20, 14:16] = 2
    lab[10:14, 6:14, 5:12] = 4
    lab[5:8, 6:12, 5:9] = 3
    lab[16:19, 6:12, 5:9] = 3
    return lab


def test_pieces_go_to_what_surrounds_them():
    lab = tongue()
    lab[11:13, 9:11, 16:19] = 4
    lab[15, 15, 10] = 4
    out = drop_pieces(lab, (1, 1, 1))
    assert not out[11:13, 9:11, 16:19].any(), "blob outside the tongue removed"
    assert out[15, 15, 10] == 1, "speck inside the tongue joins its neighbour"
    thin = tongue()
    thin[16:19, 6:14, 5:9] = 1
    thin[17:19, 6:14, 5:9] = 3
    assert (drop_pieces(thin, (1, 1, 1)) == thin).all(), "thin side kept"


def test_one_tongue_and_clip():
    lab = tongue()
    lab[1:3, 1:3, 1:3] = 1
    assert not largest_piece(lab)[1:3, 1:3, 1:3].any()
    lab = tongue()
    lab[8:12, 8:12, 3] = 2
    out = clip_suplong(lab)
    assert not (out[:, :, :4] == 2).any() and (out[8:12, 8:12, 3] > 0).all()


def test_cap_fills_gaps_in_superior_longitudinal_not_its_edge():
    lab = np.zeros((22, 26, 20), np.int16)
    lab[3:19, 4:22, 4:17] = 1
    lab[4:19, 4:22, 14:17] = 2
    lab[4:19, 11:13, 14:17] = 1
    lab[8:10, 16:18, 14:17] = 1
    out = sl_cap(lab)
    assert (out[4:19, 11:13, 14:17] == 2).all() and (out[8:10, 16:18, 14:17] == 2).all()
    assert (out[3, 4:22, 14:17] == 1).all(), "vertical outside the cap's footprint stays"
    assert (out[:, :, :14] == lab[:, :, :14]).all(), "only the cap's thickness changes"


def test_trim_removes_dark_surface_and_keeps_the_layers():
    lab = tongue()
    lab[4:20, 4:20, 10:16] = 2
    img = np.where(lab > 0, 100.0, 0.0)
    img[:, :, 15] = 10.0
    out, changed = autofix(lab, img, (1, 1, 1))
    assert not out[:, :, 15].any() and changed["trim"] > 0 and changed["smooth"] >= 0
    assert (out[8:16, 8:16, 14] == 2).all(), "superior longitudinal still caps the dorsum"


def test_inferior_longitudinal_trim_uses_its_own_band():
    lab = tongue()
    lab[4:9, 6:14, 5:10] = 3
    img = np.where(lab == 3, 60.0, np.where(lab > 0, 100.0, 0.0))
    img[4, 6:14, 5:10] = 30.0
    out, changed = autofix(lab, img, (1, 1, 1))
    assert not (out[4, 6:14, 5:10] == 3).any(), "dark outer surface trimmed"
    assert (out[6, 8:12, 6:9] == 3).all(), "inferior longitudinal itself kept, though darker than the core"
    assert changed["trim_il"] > 0


def test_node_works_in_any_voxel_order(tmp_path, monkeypatch):
    monkeypatch.setattr("lats.nodes.autofix._tongue_n4", lambda path, labels, d: np.asanyarray(nib.load(path).dataobj).astype(np.float32))
    lab = tongue()
    lab[11:13, 9:11, 16:19] = 4
    img = np.where(lab > 0, 100.0, 0.0)
    img[:, :, 15] = 10.0
    ras = np.diag([1.0, 1.0, 1.0, 1.0])
    sir = np.array([[0, 1.0, 0, 0], [0, 0, 1, 0], [1, 0, 0, 0], [0, 0, 0, 1]])
    outs = []
    for name, aff, flip, back in (("ras", ras, lambda a: a, lambda a: a),
                                  ("sir", sir, lambda a: a.transpose(2, 0, 1), lambda a: a.transpose(1, 2, 0))):
        d = tmp_path / name
        d.mkdir()
        nib.save(nib.Nifti1Image(flip(lab).astype(np.uint8), aff), str(d / "l.nii.gz"))
        nib.save(nib.Nifti1Image(flip(img).astype(np.float32), aff), str(d / "i.nii.gz"))
        outs.append(back(np.asanyarray(nib.load(_autofix(str(d / "l.nii.gz"), str(d / "i.nii.gz"), str(d))).dataobj)))
        assert json.loads((d / "autofix.json").read_text())["changed"]["pieces"] > 0
    assert (outs[0] == outs[1]).all()


def test_flags(tmp_path):
    cfg = LatsConfig(output_prefix=tmp_path / "out", timepoints=[Timepoint(index=0, image=tmp_path / "a.nii.gz")], prior_refine=True)
    sst, stats = cfg.output_prefix / "sst", cfg.output_prefix / "stats"
    sst.mkdir(parents=True)
    stats.mkdir()
    (sst / "prior_refine.json").write_text(json.dumps({"dl_foreground_voxels": 1000, "refined_foreground_voxels": 880}))
    lab = tongue()
    lab[5:8, 6:12, 5:9] = 1
    nib.save(nib.Nifti1Image(lab.astype(np.uint8), np.eye(4)), str(sst / "sst_seg_autofix.nii.gz"))
    (stats / "volumes.csv").write_text("timepoint,label_id,seg_jac_pct_diff\n0,0,\n1,0,1.5\n")
    (stats / "longitudinal.csv").write_text("timepoint,label_id,jac_pct_change\n0,0,0.0\n1,0,12.0\n")
    r = write_flags(cfg)
    assert r["flags"] == ["prior_removed", "il_sides", "tongue_change_tp01"]
    assert r["values"]["cap_coverage_pct"] == 100.0 and r["values"]["crop_edge_voxels"] == 0
    assert json.loads((cfg.output_prefix / "qc/flags.json").read_text())["flags"] == r["flags"]
    qa = LatsConfig(output_prefix=cfg.output_prefix, timepoints=cfg.timepoints, prior_refine=True, qa_labels=sst / "sst_seg_autofix.nii.gz")
    (cfg.output_prefix / "pre_qa").mkdir()
    for used in (False, True):
        (cfg.output_prefix / "pre_qa/lats_run.json").write_text(json.dumps({"prior_refine": used}))
        assert ("prior_removed" in write_flags(qa)["flags"]) is used, "a re-apply follows the original run's --prior-refine"


def test_asymmetric_flag(tmp_path):
    cfg = LatsConfig(output_prefix=tmp_path / "out", timepoints=[Timepoint(index=0, image=tmp_path / "a.nii.gz")])
    sst = cfg.output_prefix / "sst"
    sst.mkdir(parents=True)
    (sst / "midline.json").write_text(json.dumps({"point_mm": [15.5, 0, 0], "normal": [1, 0, 0]}))
    lab = np.pad(tongue(), 4)
    for extra, flagged in ((False, False), (True, True)):
        if extra:
            lab[24:28, 10:22, 8:20] = 1
        nib.save(nib.Nifti1Image(lab.astype(np.uint8), np.eye(4)), str(sst / "sst_seg_autofix.nii.gz"))
        r = write_flags(cfg)
        assert ("asymmetric" in r["flags"]) is flagged
    assert r["values"]["unpaired_pct"] == round(100 * 144 / (lab > 0).sum(), 2)


def test_smoothing_fills_a_ragged_surface():
    g = np.indices((32, 32, 32)) - 15.5
    ball = (g ** 2).sum(0) <= 12 ** 2
    lab = np.where(ball, np.where(g[2] > 8, 2, 1), 0).astype(np.int16)
    rough = lab.copy()
    top = ball & ~np.roll(ball, -1, axis=2)
    i = np.indices(ball.shape)
    pits = top & (g[2] > 8) & (i[0] % 3 == 0) & (i[1] % 3 == 0)
    rough[pits] = 0
    out = smooth(rough, (1, 1, 1))
    assert (out[pits] == 2).mean() > 0.7, "most pits filled with superior longitudinal"
    assert abs(int((out > 0).sum()) - int(ball.sum())) < 0.02 * ball.sum()


def test_session_fix_keeps_left_right_and_muscles_consistent(tmp_path):
    lab = tongue()
    lat = np.where(lab > 0, np.where(np.arange(24)[:, None, None] < 12, 100, 200) + lab, 0)
    rng = np.random.default_rng(1)
    edge = (lab == 2) & (rng.random(lab.shape) < 0.3)
    lat[edge] -= 1
    faces = lambda a: sum(int((np.diff(a, axis=k) != 0).sum()) for k in range(3))
    for name, arr in (("l", lat % 100), ("lat", lat)):
        nib.save(nib.Nifti1Image(arr.astype(np.uint8), np.eye(4)), str(tmp_path / f"{name}.nii.gz"))
    main, side = _fix_session(str(tmp_path / "lat.nii.gz"), True, 1, str(tmp_path / "labels"), str(tmp_path / "lateral"))
    m, s = (np.asanyarray(nib.load(p).dataobj).astype(int) for p in (main, side))
    assert main.endswith("labels/tp01.nii.gz") and side.endswith("lateral/tp01.nii.gz")
    assert (m == s % 100).all(), "muscle labels follow the left-right labels"
    assert faces(s) < faces(lat), "smoother boundaries"


def test_no_inferior_longitudinal_above_superior_longitudinal():
    lab = tongue()
    lab[5:8, 14:18, 15] = 3
    out = order_inflong(lab)
    assert (out[5:8, 14:18, 15] == 2).all() and (out[5:8, 6:12, 5:9] == 3).all()


def test_confidence_cut_drops_only_unsure_tongue(tmp_path, monkeypatch):
    lab = tongue()
    lab[8:16, 8:16, 16:19] = 2
    img = np.where(lab > 0, 100.0, 0.0)
    tp = np.where(lab > 0, 1.0, 0.0)
    tp[8:16, 8:16, 16:19] = 0.5
    kept, _ = autofix(lab, img, (1, 1, 1), tongue_prob=tp)
    cut, changed = autofix(lab, img, (1, 1, 1), tongue_prob=tp, prob_cut=0.99)
    assert kept[10:14, 10:14, 18].all() and not cut[8:16, 8:16, 18].any(), "lump kept without the cut, gone with it"
    assert changed["prob_cut"] > 0 and (cut[:, :, :16] == kept[:, :, :16]).all(), "confident tongue untouched"
    monkeypatch.setattr("lats.nodes.autofix._tongue_n4", lambda path, labels, d: np.asanyarray(nib.load(path).dataobj).astype(np.float32))
    nib.save(nib.Nifti1Image(lab.astype(np.uint8), np.eye(4)), str(tmp_path / "l.nii.gz"))
    nib.save(nib.Nifti1Image(img.astype(np.float32), np.eye(4)), str(tmp_path / "i.nii.gz"))
    with pytest.raises(ValueError, match="probabilities"):
        _autofix(str(tmp_path / "l.nii.gz"), str(tmp_path / "i.nii.gz"), str(tmp_path), prob_cut=0.99)


def test_genioglossus_rules_remove_it_from_the_side_and_top_only():
    lab = tongue()
    good = genio_rules(lab, (1, 1, 1))
    assert (good == lab).all(), "genioglossus in its place is untouched"
    lab[1:4, 8:12, 5:12] = 4
    lab[11:13, 9:11, 14] = 4
    lab[6:8, 9:11, 16:18] = 4
    out = genio_rules(lab, (1, 1, 1))
    assert not out[1:4, 8:12, 5:12].any() and not out[6:8, 9:11, 16:18].any(), "side column and lump on top removed"
    assert (out[11:13, 9:11, 14] == 2).all(), "enclosed voxels go to their neighbours"
    assert (out[10:14, 6:14, 5:12] == 4).all(), "genioglossus core kept"


def test_dark_cut_removes_only_thick_dark_tongue_joined_to_the_outside():
    g = np.indices((40, 40, 40))
    lab = np.where(((g - 20) ** 2).sum(0) <= 14 ** 2, 1, 0).astype(np.int16)
    rng = np.random.default_rng(0)
    img = np.where(lab > 0, 100.0, 5.0) + rng.normal(0, 5, lab.shape)
    lab[20:28, 14:26, 30:38] = 2
    img[20:28, 14:26, 31:38] = 20.0
    img[(lab == 1) & (((g - 20) ** 2).sum(0) > 13 ** 2)] = 40.0
    img[18:22, 18:22, 18:22] = 20.0
    out = dark_cut(lab, img)
    assert (out[21:27, 15:25, 32:37] > 0).mean() < 0.05, "thick dark lump removed (the opening leaves a few corners)"
    assert (out[18:22, 18:22, 18:22] == 1).all(), "enclosed dark spot kept"
    assert (out[lab == 1] == 1).mean() > 0.99, "thin dark surface layer kept"


def test_side_shave_takes_the_outer_side_of_vertical_and_superior_longitudinal_only():
    lab = tongue()
    lab[4, 6:12, 5:9] = 3
    out = shave_side(lab)
    assert not out[4, 14:20, 10:16].any() and not out[19, 14:20, 10:16].any(), "outer side of vertical and SL removed"
    assert (out[4, 6:12, 5:9] == 3).all(), "inferior longitudinal on the side kept"
    assert (out[5:19, 4:20, 15] == 2).all(), "top of superior longitudinal kept"
    assert (out[5:19] == lab[5:19]).all(), "nothing inside the side layer changes"


def test_autofix_equals_the_qa_app_reference():
    """LATS autofix and the QA app's Python twin give the same labels at the default settings."""
    ref = Path(__file__).resolve().parents[1] / "postproc-qa" / "reference"
    sys.path.insert(0, str(ref))
    import postproc_ref as R
    steps = {"inferior_clip": True, "sl_cap": True, "sl_trim": 2, "il_trim": 3, "trim_sd": 2.0, "follow_dorsum": True,
             "order": True, "dark_sd": 3.0, "prob_cut": 0.99, "smooth": True, "sigma": 1.0, "genio": True,
             "shave": True, "clean": True}
    zooms, rng = (1.0, 0.8, 1.2), np.random.default_rng(0)
    for lab in (tongue(), tongue()[:, :, 3:]):
        img = 0.3 + 0.7 * (lab > 0) + 0.05 * rng.standard_normal(lab.shape)
        prob = np.where(lab > 0, 0.999, 0.1)
        prob[4:9, 4:9, :] = 0.5
        mine, _ = autofix(lab, img, zooms, tongue_prob=prob, prob_cut=0.99)
        theirs = R.run_pipeline(lab.astype(np.int32), img, zooms, steps, tongue_prob=prob)
        assert int((mine != theirs).sum()) == 0


def test_flags_read_the_labels_this_run_made(tmp_path):
    out = tmp_path / "out"
    (out / "sst").mkdir(parents=True)
    for n in ("sst_seg", "sst_seg_autofix"):
        (out / "sst" / f"{n}.nii.gz").write_bytes(b"x")
    cfg = LatsConfig(output_prefix=out, timepoints=[Timepoint(index=0, image=tmp_path / "a.nii.gz")])
    import dataclasses
    assert template_labels(cfg).name == "sst_seg_autofix.nii.gz"
    assert template_labels(dataclasses.replace(cfg, autofix=False)).name == "sst_seg.nii.gz", "not a stale autofix file"
