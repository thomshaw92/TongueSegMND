"""Tests for :mod:`tongue_qc.discovery`."""

from __future__ import annotations

from pathlib import Path
from typing import Callable

import pytest

from tongue_qc.discovery import Scan, find_scans, parse_ids, read_metrics, read_postprocess

from .conftest import build_scan_dir

# --------------------------------------------------------------------------- #
# parse_ids
# --------------------------------------------------------------------------- #

PARSE_ID_CASES: list[tuple[str, tuple[str, str]]] = [
    # Rule 2: numeric suffix.
    ("ABC_1", ("ABC", "ses-01")),
    ("ABC123_2", ("ABC123", "ses-02")),
    ("ABC_12", ("ABC", "ses-12")),
    ("ABC_1_2", ("ABC_1", "ses-02")),
    # Rule 1: explicit session token, case-insensitive, optional hyphen.
    ("sub-004_ses-01", ("sub-004", "ses-01")),
    ("sub-004_ses-2", ("sub-004", "ses-02")),
    ("sub-004_SES-3", ("sub-004", "ses-03")),
    ("sub-004_ses02", ("sub-004", "ses-02")),
    # Rule 1 wins over rule 2.
    ("ABC_2_ses-03", ("ABC_2", "ses-03")),
    # Rule 3: no match at all.
    ("foo", ("foo", "ses-01")),
    ("ABC_123", ("ABC_123", "ses-01")),
    ("SYD_2023", ("SYD_2023", "ses-01")),
    # The false positive that must NOT be split: no separator, so no session.
    ("SYD2023", ("SYD2023", "ses-01")),
    ("20200101_3T-siteA_T1w-UNI", ("20200101_3T-siteA_T1w-UNI", "ses-01")),
    # Extensions are stripped first.
    ("x.nii.gz", ("x", "ses-01")),
    ("ABC_2.nii.gz", ("ABC", "ses-02")),
    ("ABC_2.nii", ("ABC", "ses-02")),
]


@pytest.mark.parametrize(("name", "expected"), PARSE_ID_CASES, ids=[c[0] for c in PARSE_ID_CASES])
def test_parse_ids_table(name: str, expected: tuple[str, str]) -> None:
    assert parse_ids(name) == expected


def test_parse_ids_never_returns_empty_sub() -> None:
    for name in ("foo", "SYD2023", "sub-004_ses-01", "ABC_1"):
        sub, ses = parse_ids(name)
        assert sub, f"empty sub for {name!r}"
        assert ses.startswith("ses-")


# --------------------------------------------------------------------------- #
# find_scans: layouts
# --------------------------------------------------------------------------- #


def test_find_scans_flat_layout(flat_root: Path) -> None:
    """No derivatives/ dir: fall back to the root and walk it."""
    scans, skipped = find_scans(flat_root)

    assert sorted(s.scan_id for s in scans) == ["AAA111_1", "AAA111_2", "BBB222_1"]
    assert all(s.path.parent == flat_root for s in scans)
    # A non-scan sibling directory is silently ignored, not "skipped".
    assert "logs" not in {name for name, _ in skipped}


def test_find_scans_prefers_derivatives_dir(deriv_root: Path) -> None:
    """root/derivatives/<deriv_name> wins; scan dirs directly under root are not walked."""
    scans, _ = find_scans(deriv_root)

    assert sorted(s.scan_id for s in scans) == ["CCC333_1", "DDD444_2"]
    assert "DECOY_1" not in {s.scan_id for s in scans}


def test_find_scans_honours_deriv_name(tmp_path: Path) -> None:
    root = tmp_path / "s"
    build_scan_dir(root / "derivatives" / "tongue-seg-v2" / "EEE555_1")
    scans, _ = find_scans(root, deriv_name="tongue-seg-v2")
    assert [s.scan_id for s in scans] == ["EEE555_1"]


def test_find_scans_nested_bids_layout(bids_root: Path) -> None:
    """<sub>/<ses>/ is two levels below the derivatives dir and must be reached."""
    scans, _ = find_scans(bids_root)

    deriv = bids_root / "derivatives" / "tongue-seg"
    assert {s.path for s in scans} == {
        deriv / "sub-001" / "sub-001_ses-01",
        deriv / "sub-002" / "sub-002_ses-01",
    }
    assert len({s.scan_id for s in scans}) == 2, "scan_id must be unique per scan"
    for scan in scans:
        assert scan.sub
        assert scan.ses.startswith("ses-")


def test_find_scans_mixes_depths(tmp_path: Path) -> None:
    root = tmp_path / "mixed"
    build_scan_dir(root / "FLAT01_1")
    build_scan_dir(root / "sub-009" / "sub-009_ses-01")
    scans, _ = find_scans(root)
    assert {s.path.name for s in scans} == {"FLAT01_1", "sub-009_ses-01"}


def test_find_scans_does_not_recurse_past_two_levels(tmp_path: Path) -> None:
    root = tmp_path / "deep"
    build_scan_dir(root / "a" / "b" / "TOO_DEEP_1")
    scans, _ = find_scans(root)
    assert scans == []


def test_find_scans_single_scan_dir(single_scan_root: Path) -> None:
    """Pointing --root straight at one scan dir returns exactly that scan."""
    scans, skipped = find_scans(single_scan_root)

    assert len(scans) == 1
    assert scans[0].path == single_scan_root
    assert scans[0].scan_id == single_scan_root.name
    assert skipped == []


def test_find_scans_ignores_the_work_subdir(single_scan_root: Path) -> None:
    """work/ holds *_raw / *_cropped NIfTIs but is not a scan dir."""
    assert (single_scan_root / "work").is_dir()
    scans, _ = find_scans(single_scan_root)
    assert all(s.path.name != "work" for s in scans)


def test_find_scans_empty_root_returns_empty(empty_root: Path) -> None:
    scans, skipped = find_scans(empty_root)
    assert scans == []
    assert skipped == []


# --------------------------------------------------------------------------- #
# find_scans: per-scan record
# --------------------------------------------------------------------------- #


def test_find_scans_populates_every_field(single_scan_root: Path) -> None:
    (scan,), _ = find_scans(single_scan_root)

    assert isinstance(scan, Scan)
    assert scan.image == single_scan_root / "image_T1w.nii.gz"
    assert scan.seg_native == single_scan_root / "seg_native.nii.gz"
    assert scan.seg_template == single_scan_root / "seg_template.nii.gz"
    assert scan.reg_warped == single_scan_root / "reg_Warped.nii.gz"
    assert scan.metrics_csv == single_scan_root / "tongue_metrics.csv"
    assert scan.postprocess_json == single_scan_root / "postprocess.json"
    assert scan.pipeline_qc == single_scan_root / "qc.png"


def test_optional_files_are_none_when_absent(scan_dir_factory: Callable[..., Path]) -> None:
    d = scan_dir_factory(
        "BARE001_1", with_metrics=False, with_postprocess=False, with_qc=False,
        with_reg_warped=False,
    )
    (scan,), _ = find_scans(d)
    assert scan.metrics_csv is None
    assert scan.postprocess_json is None
    assert scan.pipeline_qc is None
    assert scan.reg_warped is None


def test_scan_is_frozen(single_scan_root: Path) -> None:
    (scan,), _ = find_scans(single_scan_root)
    with pytest.raises(Exception):
        scan.scan_id = "mutated"  # type: ignore[misc]


# --------------------------------------------------------------------------- #
# find_scans: source-image disambiguation
# --------------------------------------------------------------------------- #


def test_source_image_excludes_pipeline_outputs(scan_dir_factory: Callable[..., Path]) -> None:
    """seg_*/reg_Warped/*_cropped are never candidates; the one leftover wins."""
    d = scan_dir_factory("ONE001_1")
    for junk in ("image_cropped.nii.gz", "label_cropped.nii.gz"):
        (d / junk).write_bytes((d / "seg_native.nii.gz").read_bytes())

    (scan,), skipped = find_scans(d)
    assert skipped == []
    assert scan.image.name == "image_T1w.nii.gz"


def test_two_candidate_images_are_skipped(two_candidate_dir: Path) -> None:
    scans, skipped = find_scans(two_candidate_dir)
    assert scans == []
    assert [name for name, _ in skipped] == [two_candidate_dir.name]
    reason = skipped[0][1]
    assert isinstance(reason, str) and reason.strip()


def test_zero_candidate_images_are_skipped(zero_candidate_dir: Path) -> None:
    scans, skipped = find_scans(zero_candidate_dir)
    assert scans == []
    assert [name for name, _ in skipped] == [zero_candidate_dir.name]
    assert skipped[0][1].strip()


def test_a_bad_scan_does_not_hide_the_good_ones(tmp_path: Path) -> None:
    root = tmp_path / "mix"
    build_scan_dir(root / "GOOD001_1")
    build_scan_dir(root / "BAD001_1", source_names=())
    scans, skipped = find_scans(root)
    assert [s.scan_id for s in scans] == ["GOOD001_1"]
    assert [name for name, _ in skipped] == ["BAD001_1"]


# --------------------------------------------------------------------------- #
# find_scans: space selection
# --------------------------------------------------------------------------- #


def test_space_auto_prefers_native(single_scan_root: Path) -> None:
    (scan,), _ = find_scans(single_scan_root, space="auto")
    assert scan.space == "native"


def test_space_auto_falls_back_to_template(scan_dir_factory: Callable[..., Path]) -> None:
    d = scan_dir_factory("TPLONLY_1", with_seg_native=False)
    (scan,), _ = find_scans(d, space="auto")
    assert scan.space == "template"
    assert scan.seg_native is None
    assert scan.seg_template is not None


def test_space_can_be_forced(single_scan_root: Path) -> None:
    (scan,), _ = find_scans(single_scan_root, space="template")
    assert scan.space == "template"


# --------------------------------------------------------------------------- #
# select
# --------------------------------------------------------------------------- #


def test_select_default_is_sorted_and_complete(fake_scans: Callable[..., list[Scan]]) -> None:
    from tongue_qc.discovery import select

    scans = fake_scans(20)
    out = select(scans)
    assert [s.scan_id for s in out] == sorted(s.scan_id for s in scans)


def test_select_never_mutates_its_input(fake_scans: Callable[..., list[Scan]]) -> None:
    from tongue_qc.discovery import select

    scans = fake_scans(30)
    before = list(scans)
    out = select(scans, sample=7, seed=42, shuffle_seed=1234)
    assert scans == before, "input list was reordered or filtered in place"
    assert out is not scans


def test_select_is_deterministic(fake_scans: Callable[..., list[Scan]]) -> None:
    from tongue_qc.discovery import select

    scans = fake_scans(50)
    a = select(scans, sample=11, seed=42, shuffle_seed=1234)
    b = select(scans, sample=11, seed=42, shuffle_seed=1234)
    assert [s.scan_id for s in a] == [s.scan_id for s in b]
    assert len(a) == 11


def test_select_sample_larger_than_population_is_clamped(
    fake_scans: Callable[..., list[Scan]],
) -> None:
    from tongue_qc.discovery import select

    scans = fake_scans(8)
    out = select(scans, sample=999, seed=42)
    assert {s.scan_id for s in out} == {s.scan_id for s in scans}


def test_sampling_seed_does_not_perturb_ordering(fake_scans: Callable[..., list[Scan]]) -> None:
    """With nothing sampled, the sampling seed must be inert."""
    from tongue_qc.discovery import select

    scans = fake_scans(40)
    a = select(scans, seed=1, shuffle_seed=7)
    b = select(scans, seed=999_999, shuffle_seed=7)
    assert [s.scan_id for s in a] == [s.scan_id for s in b]

    c = select(scans, seed=1)
    d = select(scans, seed=999_999)
    assert [s.scan_id for s in c] == [s.scan_id for s in d]


def test_ordering_seed_does_not_perturb_the_sample(fake_scans: Callable[..., list[Scan]]) -> None:
    from tongue_qc.discovery import select

    scans = fake_scans(60)
    drawn = {
        shuffle_seed: {s.scan_id for s in select(scans, sample=15, seed=5,
                                                 shuffle_seed=shuffle_seed)}
        for shuffle_seed in (None, 1, 2, 3, 99)
    }
    reference = drawn[1]
    for shuffle_seed, ids in drawn.items():
        assert ids == reference, f"shuffle_seed={shuffle_seed} changed which scans were drawn"


def test_shuffle_seed_actually_shuffles(fake_scans: Callable[..., list[Scan]]) -> None:
    from tongue_qc.discovery import select

    scans = fake_scans(40)
    ordered = [s.scan_id for s in select(scans)]
    shuffled = {
        tuple(s.scan_id for s in select(scans, shuffle_seed=seed)) for seed in (1, 2, 3, 1234)
    }
    assert len(shuffled) > 1, "different shuffle seeds produced identical orders"
    assert any(list(order) != ordered for order in shuffled)
    for order in shuffled:
        assert sorted(order) == ordered


def test_select_filters_to_subjects(fake_scans: Callable[..., list[Scan]]) -> None:
    from tongue_qc.discovery import select

    scans = fake_scans(20)
    wanted = ["SCAN003_1", "SCAN011_1"]
    out = select(scans, subjects=wanted)
    assert [s.scan_id for s in out] == sorted(wanted)


def test_select_subjects_filter_runs_before_sampling(
    fake_scans: Callable[..., list[Scan]],
) -> None:
    from tongue_qc.discovery import select

    scans = fake_scans(20)
    wanted = ["SCAN001_1", "SCAN002_1", "SCAN003_1"]
    out = select(scans, sample=2, seed=42, subjects=wanted)
    assert len(out) == 2
    assert set(s.scan_id for s in out) <= set(wanted)


def test_select_returns_a_list(fake_scans: Callable[..., list[Scan]]) -> None:
    from tongue_qc.discovery import select

    out = select(fake_scans(5))
    assert isinstance(out, list)


# --------------------------------------------------------------------------- #
# read_metrics / read_postprocess: must never raise
# --------------------------------------------------------------------------- #


def test_read_metrics_returns_the_first_data_row(single_scan_root: Path) -> None:
    (scan,), _ = find_scans(single_scan_root)
    metrics = read_metrics(scan)

    assert metrics["tongue_volume_mm3"] == "60000.0"
    assert metrics["space"] == "native"
    assert all(isinstance(k, str) and isinstance(v, str) for k, v in metrics.items())


def test_read_metrics_missing_file_returns_empty(scan_dir_factory: Callable[..., Path]) -> None:
    d = scan_dir_factory("NOMET01_1", with_metrics=False)
    (scan,), _ = find_scans(d)
    assert read_metrics(scan) == {}


def test_read_metrics_on_garbage_returns_empty(scan_dir_factory: Callable[..., Path]) -> None:
    d = scan_dir_factory("BADMET1_1")
    (d / "tongue_metrics.csv").write_bytes(b"\x00\x01\x02 not a csv")
    (scan,), _ = find_scans(d)
    assert read_metrics(scan) == {}


def test_read_metrics_header_only_returns_empty(scan_dir_factory: Callable[..., Path]) -> None:
    d = scan_dir_factory("HDRMET1_1")
    (d / "tongue_metrics.csv").write_text("a,b,c\n", encoding="utf-8")
    (scan,), _ = find_scans(d)
    assert read_metrics(scan) == {}


def test_read_postprocess_parses_json(single_scan_root: Path) -> None:
    (scan,), _ = find_scans(single_scan_root)
    pp = read_postprocess(scan)
    assert pp["connected_components"]["removed_voxels"] == 30000
    assert pp["connected_components"]["n_components"] == 10


def test_read_postprocess_on_malformed_json_returns_empty(
    scan_dir_factory: Callable[..., Path],
) -> None:
    d = scan_dir_factory("BADJSON_1")
    (d / "postprocess.json").write_text("{not json", encoding="utf-8")
    (scan,), _ = find_scans(d)
    assert read_postprocess(scan) == {}


def test_read_postprocess_missing_file_returns_empty(
    scan_dir_factory: Callable[..., Path],
) -> None:
    d = scan_dir_factory("NOJSON1_1", with_postprocess=False)
    (scan,), _ = find_scans(d)
    assert read_postprocess(scan) == {}
