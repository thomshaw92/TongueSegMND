"""Structural tests: no ANTs, no container, no data."""
from __future__ import annotations

import dataclasses
import json
from pathlib import Path

import pytest

from lats.cli import _build_parser, _config_from_args, _write_manifest
from lats.config import LatsConfig, Timepoint
from lats.utils.labels import TONGUE_LABELS, read_labels_file, write_labels_file
from lats.utils.paths import manifest_path
from lats.workflow import build_workflow


def _config(tmp_path: Path, n_tp: int = 3, **kw) -> LatsConfig:
    images = []
    for i in range(n_tp):
        p = tmp_path / f"ses{i}_T1w.nii.gz"
        p.write_bytes(b"")
        images.append(p)
    kw.setdefault("container", "tongue-seg:amd64")
    kw.setdefault("runtime", "docker")
    return LatsConfig(
        output_prefix=tmp_path / "out",
        timepoints=[Timepoint(index=i, image=p) for i, p in enumerate(images)],
        **kw,
    )


def test_labels_roundtrip(tmp_path):
    path = write_labels_file(tmp_path / "labels.txt")
    pairs = read_labels_file(path)
    assert dict(pairs) == TONGUE_LABELS
    assert pairs[0] == (1, "vertical")


def test_workflow_has_expected_stages(tmp_path):
    wf = build_workflow(_config(tmp_path))
    names = set(wf.list_node_names())
    for expected in ("preprocess", "prep_crop", "sst", "publish_sst",
                     "segment_sst", "register", "warp_labels", "jacobian",
                     "stats", "aggregate", "qc"):
        assert expected in names, f"missing {expected} in {sorted(names)}"


def test_optional_stages_can_be_disabled(tmp_path):
    wf = build_workflow(_config(tmp_path, jacobian=False, qc=False))
    names = set(wf.list_node_names())
    assert "jacobian" not in names
    assert "qc" not in names


def test_sst_is_single_modality_two_iterations(tmp_path):
    cfg = _config(tmp_path)
    wf = build_workflow(cfg)
    sst = wf.get_node("sst")
    # single modality (-k 1), two iterations; images are wired from prep_crop
    assert sst.inputs.n_modalities == 1
    assert sst.inputs.iterations == 2


def test_single_session_uses_copy_sst_and_no_jacobian(tmp_path):
    wf = build_workflow(_config(tmp_path, n_tp=1))
    names = set(wf.list_node_names())
    assert "sst" in names          # the copy-through SST node
    assert "segment_sst" in names
    assert "warp_labels" in names
    assert "jacobian" not in names  # nothing longitudinal to measure
    assert "shape" in names         # shape still runs on the one session


def test_mapnode_iterfields_match_timepoint_count(tmp_path):
    cfg = _config(tmp_path, n_tp=4)
    wf = build_workflow(cfg)
    for node_name in ("register", "warp_labels", "jacobian", "stats", "qc"):
        node = wf.get_node(node_name)
        assert len(node.inputs.timepoint_idx) == 4, node_name


def test_runtime_resolution(tmp_path):
    # a .sif always implies apptainer, whatever else is on PATH
    cfg = _config(tmp_path, runtime="auto", container="x.sif")
    assert cfg.resolved_runtime() == "apptainer"

    # no container but a pipeline script -> native
    cfg2 = _config(tmp_path, runtime="auto", container=None,
                   pipeline=Path("run_tongue_pipeline.py"))
    assert cfg2.resolved_runtime() == "native"

    # neither -> refuse to guess
    cfg3 = _config(tmp_path, runtime="auto", container=None, pipeline=None)
    with pytest.raises(ValueError):
        cfg3.resolved_runtime()

    # explicit runtime always wins
    cfg4 = _config(tmp_path, runtime="docker", container="x.sif")
    assert cfg4.resolved_runtime() == "docker"


def test_deps_allows_single_session(tmp_path):
    # single-session is a supported mode (cross-sectional, same layout).
    from lats.deps import check_dependencies

    cfg = _config(tmp_path, n_tp=1)
    problems = check_dependencies(cfg)
    assert not any("session" in p for p in problems)


def test_manifest_records_every_config_field(tmp_path):
    cfg = _config(tmp_path)
    cfg.output_prefix.mkdir()
    _write_manifest(cfg)
    written = json.loads(manifest_path(cfg.output_prefix).read_text())
    assert {f.name for f in dataclasses.fields(LatsConfig)} <= set(written)
    assert written["timepoints"][0] == {
        "index": 0, "subject_id": "tp00", "image": str(cfg.timepoints[0].image)}


def test_relative_atlas_dir_is_made_absolute(tmp_path, monkeypatch):
    monkeypatch.chdir(tmp_path)
    args = _build_parser().parse_args(
        ["-o", "out", "--atlas-dir", "atlas", "ses0_T1w.nii.gz"])
    assert _config_from_args(args).atlas_dir == (tmp_path / "atlas").resolve()


def _finished_run(tmp_path, n_tp=2):
    import nibabel as nib
    import numpy as np
    cfg = _config(tmp_path, n_tp=n_tp)
    out = cfg.output_prefix
    for rel in ["sst/T_template0.nii.gz", "sst/midline.json", "stats/volumes.csv"] + [
            f"intermediate/reg/tp{i:02d}_{s}" for i in range(n_tp) for s in ("0GenericAffine.mat", "1Warp.nii.gz", "1InverseWarp.nii.gz")] + [
            f"intermediate/prep_crop/tp{i:02d}/native2template_0GenericAffine.mat" for i in range(n_tp)]:
        (out / rel).parent.mkdir(parents=True, exist_ok=True)
        (out / rel).write_bytes(b"x")
    nib.save(nib.Nifti1Image(np.zeros((4, 4, 4), np.float32), np.eye(4)), out / "sst/T_template0.nii.gz")
    (out / "lats_run.json").write_text(json.dumps({"timepoints": [{"image": str(tp.image)} for tp in cfg.timepoints]}))
    return cfg


def _qa_labels(tmp_path, record):
    import nibabel as nib
    import numpy as np
    qa = tmp_path / "sub-01_space-sst_desc-qa_dseg.nii.gz"
    nib.save(nib.Nifti1Image(np.ones((4, 4, 4), np.uint8), np.eye(4)), qa)
    (tmp_path / "sub-01_space-sst_desc-qa_dseg.json").write_text(json.dumps(record))
    return qa


def test_reapply_reuses_the_run_and_keeps_its_outputs(tmp_path):
    import nibabel as nib
    import numpy as np
    from lats.reapply import prepare_reapply, write_applied
    cfg = _finished_run(tmp_path)
    qa = _qa_labels(tmp_path, {"status": "done", "decision": "include", "midline": {"point_mm": [1, 2, 3], "normal": [1, 0, 0]}})
    cfg = dataclasses.replace(cfg, qa_labels=qa)
    prepare_reapply(cfg)
    out = cfg.output_prefix
    assert (out / "pre_qa/stats/volumes.csv").is_file() and (out / "pre_qa/lats_run.json").is_file()
    assert (out / "pre_qa/midline.json").is_file() and not (out / "sst/midline.json").exists()
    assert not (out / "stats").exists() and not (out / "qa_applied.json").exists()
    write_applied(cfg)
    applied = json.loads((out / "qa_applied.json").read_text())
    assert applied["midline_from"].endswith("_dseg.json") and len(applied["sidecar_sha256"]) == 64
    names = set(build_workflow(dataclasses.replace(cfg, shape=True)).list_node_names())
    assert "reuse" in names and "warp_labels" in names and "lateralise" in names
    assert not names & {"preprocess", "prep_crop", "sst", "segment_sst", "register"}


def test_reapply_rejects_labels_off_the_template_grid(tmp_path):
    import nibabel as nib
    import numpy as np
    from lats.reapply import prepare_reapply
    cfg = _finished_run(tmp_path)
    qa = tmp_path / "wrong_grid.nii.gz"
    nib.save(nib.Nifti1Image(np.ones((5, 4, 4), np.uint8), np.eye(4)), qa)
    with pytest.raises(ValueError, match="template grid"):
        prepare_reapply(dataclasses.replace(cfg, qa_labels=qa))
    (cfg.output_prefix / "intermediate/reg/tp01_1Warp.nii.gz").unlink()
    with pytest.raises(FileNotFoundError, match="tp01_1Warp"):
        prepare_reapply(dataclasses.replace(cfg, qa_labels=qa))


def test_reapply_refuses_unfinished_records_and_other_sessions(tmp_path):
    from lats.reapply import prepare_reapply
    cfg = _finished_run(tmp_path)
    for record in ({"status": "in progress", "decision": None}, {"status": "done", "decision": "exclude"}):
        with pytest.raises(ValueError, match="not a finished include"):
            prepare_reapply(dataclasses.replace(cfg, qa_labels=_qa_labels(tmp_path, record)))
    qa = _qa_labels(tmp_path, {"status": "done", "decision": "include"})
    swapped = dataclasses.replace(cfg, qa_labels=qa, timepoints=cfg.timepoints[::-1])
    with pytest.raises(ValueError, match="sessions differ"):
        prepare_reapply(swapped)
    assert (cfg.output_prefix / "stats/volumes.csv").is_file(), "nothing moved before the checks pass"


def test_reapply_after_a_crash_keeps_the_original_outputs(tmp_path):
    from lats.reapply import prepare_reapply
    cfg = _finished_run(tmp_path)
    out = cfg.output_prefix
    (out / "pre_qa").mkdir()
    (out / "qa_applied.json").write_text("{}")
    (out / "sst/midline.json").unlink()
    prepare_reapply(dataclasses.replace(cfg, qa_labels=_qa_labels(tmp_path, {"status": "done", "decision": "include"})))
    assert (out / "pre_qa/stats/volumes.csv").read_bytes() == b"x"
    assert not (out / "qa_applied.json").exists()
    for rel in ("stats/volumes.csv", "sst/midline.json"):
        (out / rel).parent.mkdir(exist_ok=True)
        (out / rel).write_text("from a re-apply")
    prepare_reapply(dataclasses.replace(cfg, qa_labels=_qa_labels(tmp_path, {"status": "done", "decision": "include"})))
    assert not (out / "stats").exists() and (out / "pre_qa/stats/volumes.csv").read_bytes() == b"x"
    assert not (out / "sst/midline.json").exists() and not (out / "pre_qa/midline.json").exists(), "a re-apply's output is never kept as original"
