"""Structural tests — no ANTs, no container, no data."""
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
