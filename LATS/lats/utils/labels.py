"""Tongue label map, matching the segmenter's label ids."""
from __future__ import annotations

from pathlib import Path

TONGUE_LABELS: dict[int, str] = {
    1: "vertical",
    2: "superior longitudinal",
    3: "inferior longitudinal",
    4: "genioglossus",
}

# Writes "<id> <name>" lines, the format the stats and jacobian nodes parse.
def write_labels_file(path: Path, labels: dict[int, str] | None = None) -> Path:
    labels = labels or TONGUE_LABELS
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("\n".join(f"{i} {n}" for i, n in sorted(labels.items())) + "\n")
    return path


def read_labels_file(path: str | Path) -> list[tuple[int, str]]:
    pairs: list[tuple[int, str]] = []
    for line in Path(path).read_text().splitlines():
        parts = line.split(None, 1)
        if len(parts) == 2:
            try:
                pairs.append((int(parts[0]), parts[1].strip()))
            except ValueError:
                pass
    return pairs
