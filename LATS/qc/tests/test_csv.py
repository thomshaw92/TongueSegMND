"""Tests that pin the ratings CSV header and the disposition map."""

from __future__ import annotations

import re
from pathlib import Path

FIELDS: tuple[str, ...] = (
    "schema_version",
    "scan_id",
    "sub",
    "ses",
    "space",
    "mri_quality",
    "seg_quality",
    "mri_disposition",
    "seg_disposition",
    "touched",
    "notes",
    "tooth_artefact",
    "tooth_affects_volume",
    "tooth_affects_shape",
    "rater",
    "rated_at",
    "order",
)

#: Byte-exact header line the app writes.
CSV_HEADER = (
    '"schema_version","scan_id","sub","ses","space","mri_quality","seg_quality",'
    '"mri_disposition","seg_disposition","touched","notes",'
    '"tooth_artefact","tooth_affects_volume","tooth_affects_shape",'
    '"rater","rated_at","order"'
)

#: The three tooth columns.
TOOTH_FIELDS: tuple[str, ...] = (
    "tooth_artefact", "tooth_affects_volume", "tooth_affects_shape",
)

#: Pass/fail per grade, from Shaw 2025 (CIBM) Appendix One, Table One; mirrors app.js and build.py.
DISPOSITION: dict[str, dict[int, str]] = {
    "mri": {1: "fail", 2: "fail-or-check", 3: "pass-check", 4: "pass", 5: "pass"},
    "seg": {1: "fail", 2: "fail", 3: "pass", 4: "pass", 5: "pass"},
}

_APP_JS = Path(__file__).resolve().parents[1] / "tongue_qc" / "assets" / "app.js"


def _js_csv_header(js: str) -> list[str]:
    """The column names of ``var CSV_HEADER = [...]`` in app.js, in order."""
    m = re.search(r"\bCSV_HEADER\s*=\s*\[(.*?)\]", js, re.DOTALL)
    assert m, "no CSV_HEADER array in app.js"
    return re.findall(r"""['"]([^'"]*)['"]""", m.group(1))


def _js_disposition(js: str) -> dict[str, dict[int, str]]:
    """``var DISPOSITION = {mri: {1: '...', ...}, seg: {...}}`` in app.js, as a dict."""
    m = re.search(r"\bDISPOSITION\s*=\s*\{(.*?)\n\s*\};", js, re.DOTALL)
    assert m, "no DISPOSITION object in app.js"
    return {
        scale: {int(g): d for g, d in re.findall(r"""(\d+)\s*:\s*['"]([^'"]*)['"]""", body)}
        for scale, body in re.findall(r"(\w+)\s*:\s*\{([^}]*)\}", m.group(1))
    }


def test_field_order_has_schema_first_and_order_last() -> None:
    assert FIELDS.index("mri_quality") < FIELDS.index("seg_quality") < FIELDS.index("touched")
    assert FIELDS[0] == "schema_version"
    assert FIELDS[-1] == "order"


def test_tooth_columns_sit_immediately_after_notes() -> None:
    """The analysis-relevant grading columns stay contiguous."""
    start = FIELDS.index("notes") + 1
    assert FIELDS[start:start + 3] == TOOTH_FIELDS
    assert FIELDS[start + 3] == "rater"


def test_builder_csv_columns_match_this_header() -> None:
    """``ratings_template.csv`` is written from ``build.CSV_COLUMNS``."""
    from tongue_qc.build import CSV_COLUMNS

    assert tuple(CSV_COLUMNS) == FIELDS
    assert ",".join(f'"{c}"' for c in CSV_COLUMNS) == CSV_HEADER


def test_app_js_header_and_disposition_match_the_builder_and_this_file() -> None:
    """The app writes this header and derives the same dispositions as the builder."""
    from tongue_qc.build import CSV_COLUMNS
    from tongue_qc.build import DISPOSITION as BUILD_DISPOSITION

    js = _APP_JS.read_text(encoding="utf-8")

    header = _js_csv_header(js)
    assert header == list(FIELDS) == list(CSV_COLUMNS)
    assert ",".join(f'"{c}"' for c in header) == CSV_HEADER

    assert _js_disposition(js) == DISPOSITION == BUILD_DISPOSITION
