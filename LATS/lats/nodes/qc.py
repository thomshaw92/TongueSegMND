"""Per-timepoint QC montage with the labels overlaid."""
from __future__ import annotations

from nipype.interfaces.utility import Function
from nipype.pipeline import engine as pe

from ..config import LatsConfig
from ..utils.paths import qc_dir


def _qc_for_timepoint(
    timepoint_idx: int,
    image: str,
    warped_labels: str,
    output_dir: str,
) -> str:
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    import nibabel as nib
    import numpy as np
    from pathlib import Path as _P

    colors = {
        1: (0.20, 0.60, 1.00),
        2: (0.95, 0.30, 0.30),
        3: (0.30, 0.80, 0.40),
        4: (1.00, 0.75, 0.10),
    }

    od = _P(output_dir)
    od.mkdir(parents=True, exist_ok=True)
    out = od / f"tp{timepoint_idx:02d}.png"

    img = nib.as_closest_canonical(nib.load(image))
    seg = nib.as_closest_canonical(nib.load(warped_labels))
    vol = np.asarray(img.get_fdata(), dtype=float)
    lab = np.rint(np.asarray(seg.get_fdata())).astype(int)

    # Centre the montage on the segmentation, not the image.
    idx = np.argwhere(lab > 0)
    if idx.size:
        cx, cy, cz = (int(v) for v in idx.mean(axis=0).round())
    else:
        cx, cy, cz = (s // 2 for s in vol.shape[:3])

    lo, hi = np.percentile(vol, [1, 99])
    denom = (hi - lo) or 1.0

    def _overlay(ax, img2d, seg2d, title):
        base = np.clip((img2d - lo) / denom, 0, 1)
        rgb = np.dstack([base] * 3)
        for lid, col in colors.items():
            m = seg2d == lid
            if m.any():
                for c in range(3):
                    rgb[..., c][m] = 0.45 * base[m] + 0.55 * col[c]
        ax.imshow(np.rot90(rgb), interpolation="nearest")
        ax.set_title(title, fontsize=9)
        ax.axis("off")

    fig, axes = plt.subplots(1, 3, figsize=(11, 4.2))
    _overlay(axes[0], vol[cx, :, :], lab[cx, :, :], "sagittal")
    _overlay(axes[1], vol[:, cy, :], lab[:, cy, :], "coronal")
    _overlay(axes[2], vol[:, :, cz], lab[:, :, cz], "axial")
    n_vox = int((lab > 0).sum())
    fig.suptitle(
        f"tp{timepoint_idx:02d}  —  {_P(image).name}  —  {n_vox} labelled voxels",
        fontsize=10,
    )
    fig.tight_layout()
    fig.savefig(out, dpi=110, bbox_inches="tight")
    plt.close(fig)
    return str(out)


def build_qc(config: LatsConfig) -> pe.MapNode:
    """MapNode over timepoints. Caller wires ``warped_labels``."""
    node = pe.MapNode(
        Function(
            input_names=["timepoint_idx", "image", "warped_labels", "output_dir"],
            output_names=["qc_png"],
            function=_qc_for_timepoint,
        ),
        name="qc",
        iterfield=["timepoint_idx", "image", "warped_labels"],
    )
    node.inputs.timepoint_idx = list(range(config.n_timepoints))
    # With qc_preproc, ``image`` is wired from native_qc in the workflow.
    if not config.qc_preproc:
        node.inputs.image = [str(tp.image) for tp in config.timepoints]
    node.inputs.output_dir = str(qc_dir(config.output_prefix))
    return node
