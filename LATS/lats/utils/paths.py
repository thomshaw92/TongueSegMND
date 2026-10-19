"""Output-directory layout for a LATS run."""
from __future__ import annotations

from pathlib import Path

# ---- top-level files ------------------------------------------------------

def manifest_path(output_prefix: Path) -> Path:
    return output_prefix / "lats_run.json"


def labels_path(output_prefix: Path) -> Path:
    return output_prefix / "labels.txt"


# ---- user-facing output subdirs -------------------------------------------

def sst_dir(output_prefix: Path) -> Path:
    return output_prefix / "sst"


def sst_segmenter_dir(output_prefix: Path) -> Path:
    return sst_dir(output_prefix) / "segmenter"


def labels_dir(output_prefix: Path) -> Path:
    return output_prefix / "labels"


def jacobian_dir(output_prefix: Path) -> Path:
    return output_prefix / "jacobian"


def stats_dir(output_prefix: Path) -> Path:
    return output_prefix / "stats"


def per_timepoint_stats_dir(output_prefix: Path) -> Path:
    return stats_dir(output_prefix) / "per_timepoint"


def qc_dir(output_prefix: Path) -> Path:
    return output_prefix / "qc"


def shape_dir(output_prefix: Path) -> Path:
    return output_prefix / "shape"


def lateral_labels_dir(output_prefix: Path) -> Path:
    return output_prefix / "labels_lateral"


# ---- intermediate / working dirs ------------------------------------------

def intermediate_dir(output_prefix: Path) -> Path:
    return output_prefix / "intermediate"


def sst_work_dir(output_prefix: Path) -> Path:
    return intermediate_dir(output_prefix) / "sst"


def reg_dir(output_prefix: Path) -> Path:
    return intermediate_dir(output_prefix) / "reg"


# ---- nipype cache (sibling of output_prefix) ------------------------------

def nipype_base_dir(output_prefix: Path) -> Path:
    return output_prefix.parent / f"{output_prefix.name}_nipype"
