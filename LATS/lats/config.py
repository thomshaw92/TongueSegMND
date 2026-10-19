"""Run configuration for LATS."""
from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

MODALITIES = ("T1w", "T2w", "Spine-T2w")
RUNTIMES = ("auto", "docker", "apptainer", "native")


@dataclass(frozen=True)
class Timepoint:
    index: int
    image: Path

    @property
    def subject_id(self) -> str:
        return f"tp{self.index:02d}"


@dataclass
class LatsConfig:
    output_prefix: Path
    timepoints: list[Timepoint]

    # --- segmenter -----------------------------------------------------------
    modality: str = "T1w"
    runtime: str = "auto"
    container: str | None = None        # docker tag or path to .sif
    pipeline: Path | None = None        # run_tongue_pipeline.py, for runtime=native
    assets: Path | None = None          # override the segmenter's asset dir
    container_args: str = ""            # extra args spliced into docker/apptainer
    postproc: bool = True               # segmenter's anatomical clean-up

    # --- template ------------------------------------------------------------
    template_iterations: int = 2
    template_metric: str = "CC"
    template_transform: str = "SyN"
    template_gradient_step: float = 0.25
    template_pairwise_iters: str = "100x70x30x3"
    n4: bool = False
    denoise: bool = False            # DenoiseImage(rician) after N4, per session

    # --- tongue-local crop + QC enhancement ---------------------------------
    crop_n4: bool = True
    crop_denoise: bool = True
    isotropic_mm: float = 1.0        # resample crop + QC image to this iso mm (0 = off)
    qc_preproc: bool = True          # build the re-preprocessed native QC image

    # --- SST -> timepoint registration --------------------------------------
    reg_transform: str = "s"            # antsRegistrationSyN.sh -t

    # --- execution -----------------------------------------------------------
    plugin: str = "Linear"
    n_procs: int = 2
    keep_tmp: bool = False

    # --- optional stages -----------------------------------------------------
    jacobian: bool = True
    qc: bool = True
    shape: bool = True
    meshes: bool = True

    # --- lateralisation ------------------------------------------------------
    # Midline passes through this label's centroid (4 = genioglossus).
    midline_label: int = 4
    midline_mode: str = "centroid"   # 'centroid' | 'pca'

    # --- atlas-prior refinement (FreeSurfer-aseg style) ---------------------
    prior_refine: bool = False       # opt-in: refine the SST seg with the atlas
    atlas_dir: Path | None = None    # dir with atlas_foreground + atlas_prob_{1..4}
    atlas_reg_transform: str = "s"   # antsRegistrationSyN -t for atlas->tongue
    atlas_reg_downsample_mm: float = 1.0
    # Keep prior_dilate_mm at 0 so the prior never adds foreground.
    prior_weight: float = 0.3
    debris_prior_threshold: float = 0.05
    prior_likelihood_smooth_mm: float = 1.5
    prior_dilate_mm: float = 0.0
    # Softmax likelihood; needs a segmenter that supports --save-probs.
    prior_use_probs: bool = False

    @property
    def n_timepoints(self) -> int:
        return len(self.timepoints)

    def resolved_runtime(self) -> str:
        """Resolve ``auto`` to a concrete runtime."""
        import shutil

        if self.runtime != "auto":
            return self.runtime
        if self.container and str(self.container).endswith(".sif"):
            return "apptainer"
        if self.container:
            if shutil.which("docker"):
                return "docker"
            if shutil.which("apptainer"):
                return "apptainer"
        if self.pipeline:
            return "native"
        raise ValueError(
            "cannot resolve --runtime auto: pass --container (docker tag or .sif) "
            "or --pipeline /path/to/run_tongue_pipeline.py"
        )
