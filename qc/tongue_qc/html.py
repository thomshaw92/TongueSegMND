"""Assemble the single-file Tongue-QC rating app."""

from __future__ import annotations

import json
import re
from pathlib import Path
from typing import Any, Final

ASSET_DIR: Final[Path] = Path(__file__).parent / "assets"

#: Template placeholders, in substitution order; the manifest must stay last.
PLACEHOLDERS: Final[tuple[str, ...]] = (
    "{{CSS}}",
    "{{JS}}",
    "{{RATER}}",
    "{{SEED_RATINGS}}",
    "{{SCALE}}",
    "{{MANIFEST}}",
)

_CLOSE_SCRIPT: Final[re.Pattern[str]] = re.compile(r"</(?=\s*script)", re.IGNORECASE)
_CLOSE_STYLE: Final[re.Pattern[str]] = re.compile(r"</(?=\s*style)", re.IGNORECASE)


def _read_asset(name: str) -> str:
    """Read one file from ``tongue_qc/assets``."""
    path = ASSET_DIR / name
    if path.is_file():
        return path.read_text(encoding="utf-8")
    from importlib import resources

    return resources.files(__package__).joinpath("assets", name).read_text(encoding="utf-8")


def _js_literal(value: Any) -> str:
    """Encode ``value`` as a JSON literal that is safe inside a ``<script>``."""
    text = json.dumps(value, ensure_ascii=True, separators=(",", ":"))
    return text.replace("</", "<\\/")


def _js_string(text: str) -> str:
    """Encode a free-text value as a JS string literal with no ``{`` in it."""
    return _js_literal(text).replace("{", "\\u007b")


def render_html(manifest: dict, *, rater: str = "", seed_ratings: str = "",
                scale_html: str = "") -> str:
    """Return the complete, self-contained Tongue-QC HTML document."""
    css = _CLOSE_STYLE.sub(r"<\\/", _read_asset("app.css"))
    js = _CLOSE_SCRIPT.sub(r"<\\/", _read_asset("app.js"))
    for name, body in (("app.css", css), ("app.js", js)):
        stray = [p for p in PLACEHOLDERS if p in body]
        if stray:
            raise ValueError(f"{name} contains template placeholders: {', '.join(stray)}")

    doc = _read_asset("index.html")
    doc = doc.replace("{{CSS}}", css)
    doc = doc.replace("{{JS}}", js)
    doc = doc.replace("{{RATER}}", _js_string(str(rater or "")))
    doc = doc.replace("{{SEED_RATINGS}}", _js_string(str(seed_ratings or "")))
    # Escape "{" so the scale HTML cannot reintroduce a placeholder.
    doc = doc.replace("{{SCALE}}", str(scale_html or "").replace("{", "&#123;"))
    doc = doc.replace("{{MANIFEST}}", _js_literal(manifest))

    leftover = [p for p in PLACEHOLDERS if p in doc]
    if leftover:
        raise ValueError(f"unsubstituted placeholders in index.html: {', '.join(leftover)}")
    return doc
