"""Top-level Nipype workflow."""
from __future__ import annotations

from nipype.pipeline import engine as pe

from .config import LatsConfig
from .nodes.enhance import build_enhance_crop, build_native_qc
from .nodes.jacobian import build_jacobian
from .nodes.lateralise import build_lateralise
from .nodes.preprocess import build_preprocess
from .nodes.prior import build_refine, build_register_atlas, build_warp_priors
from .nodes.qc import build_qc
from .nodes.register import build_register, build_warp_labels
from .nodes.segment import build_prep_crop, build_segment
from .nodes.shape import build_aggregate_shape, build_shape, build_sst_meshes
from .nodes.sst import build_publish_sst, build_single_session_sst, build_sst
from .nodes.stats import build_aggregate, build_stats
from .utils.labels import write_labels_file
from .utils.paths import labels_path, lateral_labels_dir, nipype_base_dir


def build_workflow(config: LatsConfig) -> pe.Workflow:
    n_tp = config.n_timepoints
    longitudinal = n_tp > 1

    wf = pe.Workflow(name="lats", base_dir=str(nipype_base_dir(config.output_prefix)))
    wf.config["execution"]["crashfile_format"] = "txt"
    wf.config["execution"]["crashdump_dir"] = str(nipype_base_dir(config.output_prefix))

    labels_file = str(write_labels_file(labels_path(config.output_prefix)))

    # --- preprocess (N4 -> denoise) ----------------------------------------
    preprocess = build_preprocess(config)

    # --- register each session to the template + crop ----------------------
    prep_crop = build_prep_crop(config)
    wf.connect(preprocess, "preprocessed", prep_crop, "image")

    # --- tongue-local N4 + denoise + isotropic on the crop -----------------
    enhance = build_enhance_crop(config)
    wf.connect(prep_crop, "cropped", enhance, "cropped")

    # --- single-subject template from the enhanced crops -------------------
    if longitudinal:
        sst = build_sst(config)
        wf.connect(enhance, "enhanced", sst, "images")
    else:
        sst = build_single_session_sst(config)
        wf.connect(enhance, "enhanced", sst, "images")
    publish = build_publish_sst(config)
    wf.connect(sst, "template0", publish, "template")
    wf.connect(enhance, "enhanced", publish, "crop_reference")

    # --- segment the cropped SST once --------------------------------------
    segment = build_segment(config)
    wf.connect(publish, "template", segment, "template")

    # --- atlas-prior refinement (opt-in) -----------------------------------
    if config.prior_refine:
        reg_atlas = build_register_atlas(config)
        warp_priors = build_warp_priors(config)
        refine = build_refine(config)
        wf.connect(segment, "sst_segmentation", reg_atlas, "dl_seg")
        wf.connect(segment, "sst_segmentation", warp_priors, "sst_seg")
        wf.connect(reg_atlas, "reg_prefix", warp_priors, "reg_prefix")
        wf.connect(segment, "sst_segmentation", refine, "dl_seg")
        wf.connect(warp_priors, "warped_priors", refine, "warped_priors")
        wf.connect(segment, "sst_probs", refine, "dl_probs")
        sst_labels_source = (refine, "refined_seg")
    else:
        sst_labels_source = (segment, "sst_segmentation")

    # --- SST -> each cropped session ---------------------------------------
    register = build_register(config)
    wf.connect(publish, "template", register, "sst")
    wf.connect(enhance, "enhanced", register, "timepoint_image")

    # --- propagate SST labels to native ------------------------------------
    warp_labels = build_warp_labels(config)
    wf.connect(sst_labels_source[0], sst_labels_source[1], warp_labels, "sst_labels")
    wf.connect(register, "affine", warp_labels, "affine")
    wf.connect(register, "inverse_warp", warp_labels, "inverse_warp")
    wf.connect(prep_crop, "affine_mat", warp_labels, "native_affine")

    # --- log-Jacobian volumes (multi-session only) -------------------------
    jacobian = None
    if config.jacobian and longitudinal:
        jacobian = build_jacobian(config)
        jacobian.inputs.labels_file = labels_file
        wf.connect(publish, "template", jacobian, "sst")
        wf.connect(register, "affine", jacobian, "affine")
        wf.connect(register, "warp", jacobian, "warp")
        wf.connect(sst_labels_source[0], sst_labels_source[1], jacobian, "sst_labels")
        wf.connect(prep_crop, "affine_mat", jacobian, "native_affine")

    # --- stats --------------------------------------------------------------
    stats = build_stats(config)
    stats.inputs.labels_file = labels_file
    wf.connect(warp_labels, "warped_labels", stats, "warped_labels")

    aggregate = build_aggregate(config)
    aggregate.inputs.labels_file = labels_file
    wf.connect(stats, "stats_file", aggregate, "stats_files")
    wf.connect(sst_labels_source[0], sst_labels_source[1], aggregate, "sst_labels")
    if jacobian is not None:
        wf.connect(jacobian, "volumes_file", aggregate, "jacobian_files")

    # --- shape descriptors + lateralisation --------------------------------
    if config.shape:
        lateralise = build_lateralise(config)
        wf.connect(sst_labels_source[0], sst_labels_source[1], lateralise, "sst_labels")

        warp_lateral = build_warp_labels(
            config, name="warp_lateral_labels",
            output_dir=str(lateral_labels_dir(config.output_prefix)),
        )
        wf.connect(lateralise, "lateral_labels", warp_lateral, "sst_labels")
        wf.connect(register, "affine", warp_lateral, "affine")
        wf.connect(register, "inverse_warp", warp_lateral, "inverse_warp")
        wf.connect(prep_crop, "affine_mat", warp_lateral, "native_affine")

        shape = build_shape(config)
        shape.inputs.labels_file = labels_file
        wf.connect(warp_labels, "warped_labels", shape, "warped_labels")
        wf.connect(warp_lateral, "warped_labels", shape, "warped_lateral_labels")

        aggregate_shape = build_aggregate_shape(config)
        wf.connect(shape, "descriptors_tsv", aggregate_shape, "shape_files")

        if config.meshes:
            meshes = build_sst_meshes(config)
            meshes.inputs.labels_file = labels_file
            wf.connect(sst_labels_source[0], sst_labels_source[1], meshes, "sst_labels")

    # --- QC -----------------------------------------------------------------
    if config.qc:
        qc = build_qc(config)
        if config.qc_preproc:
            # QC on the re-preprocessed native image and its resampled labels.
            native_qc = build_native_qc(config)
            wf.connect(warp_labels, "warped_labels", native_qc, "warped_labels")
            wf.connect(native_qc, "qc_image", qc, "image")
            wf.connect(native_qc, "qc_labels", qc, "warped_labels")
        else:
            wf.connect(warp_labels, "warped_labels", qc, "warped_labels")

    return wf
