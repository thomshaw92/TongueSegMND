"""LATS command-line interface."""
from __future__ import annotations

import argparse
import dataclasses
import json
import logging
import os
import sys
from pathlib import Path

from .config import MODALITIES, RUNTIMES, LatsConfig, Timepoint
from .deps import check_dependencies
from .utils.paths import manifest_path, nipype_base_dir, sst_work_dir
from .workflow import build_workflow

LOG = logging.getLogger("lats")


def _build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        prog="lats",
        description="Longitudinal Automatic Tongue Segmentation: single-subject "
                    "template, segment once, propagate labels, log-Jacobian volumes.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    p.add_argument("images", nargs="*", type=Path,
                   help="one image per timepoint, in acquisition order")
    p.add_argument("-o", "--output-prefix", type=Path, required=False,
                   help="output directory")

    seg = p.add_argument_group("segmenter")
    seg.add_argument("-M", "--modality", default="T1w", choices=MODALITIES,
                     help="input modality (selects the segmenter's template)")
    seg.add_argument("-R", "--runtime", default="auto", choices=RUNTIMES,
                     help="how to invoke the tongue segmenter")
    seg.add_argument("--container", default=None,
                     help="docker image tag or path to an apptainer .sif")
    seg.add_argument("--pipeline", type=Path,
                     default=(Path(os.environ["LATS_PIPELINE"])
                              if os.environ.get("LATS_PIPELINE") else None),
                     help="run_tongue_pipeline.py, for --runtime native "
                          "(defaults to $LATS_PIPELINE, set inside the container)")
    seg.add_argument("--assets", type=Path, default=None,
                     help="override the segmenter's asset directory")
    seg.add_argument("--container-args", default="",
                     help="extra args spliced into the docker/apptainer command, "
                          "e.g. '--platform linux/amd64'")
    seg.add_argument("--no-postproc", action="store_true",
                     help="skip the segmenter's anatomical clean-up")

    tmpl = p.add_argument_group("template")
    tmpl.add_argument("-i", "--template-iterations", type=int, default=2,
                      help="antsMultivariateTemplateConstruction2 -i")
    tmpl.add_argument("--template-metric", default="CC", help="AMTC2 -m")
    tmpl.add_argument("--template-transform", default="SyN", help="AMTC2 -t")
    tmpl.add_argument("--template-gradient-step", type=float, default=0.25,
                      help="AMTC2 -g")
    tmpl.add_argument("--template-pairwise-iters", default="100x70x30x3",
                      help="AMTC2 -q")
    pre = p.add_argument_group("preprocessing")
    pre.add_argument("-n", "--n4", action="store_true",
                     help="N4 bias-field correction on each session before "
                          "registration to the template")
    pre.add_argument("--denoise", action="store_true",
                     help="DenoiseImage (Rician) on each session, after N4")
    pre.add_argument("--no-crop-n4", action="store_true",
                     help="disable the second, tongue-local N4 on the crop "
                          "(on by default)")
    pre.add_argument("--no-crop-denoise", action="store_true",
                     help="disable denoising the crop (on by default)")
    pre.add_argument("--isotropic-mm", type=float, default=1.0,
                     help="resample the crop and QC image to this isotropic mm "
                          "(default 1.0; 0 disables)")
    pre.add_argument("--no-qc-preproc", action="store_true",
                     help="QC/native image stays raw instead of being re-preprocessed "
                          "(whole+local N4, denoise, isotropic; on by default)")

    reg = p.add_argument_group("registration")
    reg.add_argument("--reg-transform", default="s",
                     help="antsRegistrationSyN.sh -t for SST->timepoint")

    pri = p.add_argument_group("atlas-prior refinement (FreeSurfer-aseg style)")
    pri.add_argument("--prior-refine", action="store_true",
                     help="refine the SST segmentation with the probabilistic "
                          "tongue atlas (DL = likelihood, atlas = spatial prior)")
    pri.add_argument("--atlas-dir", type=Path,
                     default=(Path(os.environ["LATS_ATLAS_DIR"])
                              if os.environ.get("LATS_ATLAS_DIR") else None),
                     help="directory with atlas_foreground.nii.gz and "
                          "atlas_prob_{1..4}.nii.gz (defaults to $LATS_ATLAS_DIR)")
    pri.add_argument("--atlas-reg-transform", default="s",
                     help="antsRegistrationSyN.sh -t for atlas->tongue "
                          "('s' SyN recommended; affine alone collapses)")
    pri.add_argument("--atlas-reg-downsample-mm", type=float, default=1.0,
                     help="resolution for the atlas->tongue registration "
                          "(a spatial prior needs no sub-mm precision)")
    pri.add_argument("--prior-weight", type=float, default=0.3,
                     help="weight on log-prior vs the DL likelihood; small values "
                          "only bite at boundaries (MND-safe default)")
    pri.add_argument("--prior-likelihood-smooth-mm", type=float, default=1.5,
                     help="Gaussian smoothing (mm) of the DL likelihood before "
                          "combining; larger gives the atlas prior more say near "
                          "boundaries")
    pri.add_argument("--debris-prior-threshold", type=float, default=0.05,
                     help="DL-foreground voxels with total prior below this are "
                          "removed as out-of-anatomy debris")
    pri.add_argument("--prior-dilate-mm", type=float, default=0.0,
                     help="grow the DL-foreground candidate region before "
                          "refinement. 0 (default) forbids the prior from adding "
                          "tissue the DL did not find -- keeps atrophy visible")
    pri.add_argument("--prior-probs", action="store_true",
                     help="use the network's softmax probabilities as the "
                          "likelihood (a proper Bayesian posterior) instead of a "
                          "smoothed one-hot of the hard labels. Requires a segmenter "
                          "that supports --save-probs")

    lat = p.add_argument_group("lateralisation")
    lat.add_argument("--midline-label", type=int, default=4,
                     help="label whose centroid defines the midline "
                          "(4 = genioglossus, the bottom-most)")
    lat.add_argument("--midline-mode", default="centroid",
                     choices=("centroid", "pca"),
                     help="'centroid' uses the canonical Right axis as the plane "
                          "normal; 'pca' takes it from the tongue's least-elongated "
                          "principal axis (tolerates head tilt, fails if the LR axis "
                          "is not the smallest)")

    exe = p.add_argument_group("execution")
    exe.add_argument("-c", "--plugin", default="Linear",
                     help="nipype plugin (Linear, MultiProc, SLURM, SGE, PBS)")
    exe.add_argument("-j", "--n-procs", type=int, default=2,
                     help="threads for ANTs and the segmenter")
    exe.add_argument("-b", "--keep-tmp", action="store_true",
                     help="keep the nipype working tree")
    exe.add_argument("--no-jacobian", action="store_true",
                     help="skip log-Jacobian volume estimation")
    exe.add_argument("--no-shape", action="store_true",
                     help="skip shape descriptors and lateralisation")
    exe.add_argument("--no-meshes", action="store_true",
                     help="skip per-label STL export of the SST")
    exe.add_argument("--no-qc", action="store_true",
                     help="skip per-timepoint QC montages")
    exe.add_argument("--check-deps", action="store_true",
                     help="report missing dependencies and exit")
    exe.add_argument("--dry-run", action="store_true",
                     help="build the workflow and print the stage graph, then exit")
    exe.add_argument("-v", "--verbose", action="store_true", help="INFO log level")
    exe.add_argument("-z", "--debug", action="store_true", help="DEBUG log level")
    return p


def _config_from_args(args) -> LatsConfig:
    timepoints = [Timepoint(index=i, image=img.resolve())
                  for i, img in enumerate(args.images)]
    return LatsConfig(
        # None only in a standalone `--check-deps` probe (which never uses it).
        output_prefix=(args.output_prefix.resolve() if args.output_prefix else Path.cwd()),
        timepoints=timepoints,
        modality=args.modality,
        runtime=args.runtime,
        container=args.container,
        pipeline=args.pipeline,
        assets=args.assets,
        container_args=args.container_args,
        postproc=not args.no_postproc,
        template_iterations=args.template_iterations,
        template_metric=args.template_metric,
        template_transform=args.template_transform,
        template_gradient_step=args.template_gradient_step,
        template_pairwise_iters=args.template_pairwise_iters,
        n4=args.n4,
        denoise=args.denoise,
        crop_n4=not args.no_crop_n4,
        crop_denoise=not args.no_crop_denoise,
        isotropic_mm=args.isotropic_mm,
        qc_preproc=not args.no_qc_preproc,
        reg_transform=args.reg_transform,
        plugin=args.plugin,
        n_procs=args.n_procs,
        keep_tmp=args.keep_tmp,
        jacobian=not args.no_jacobian,
        qc=not args.no_qc,
        shape=not args.no_shape,
        meshes=not args.no_meshes,
        midline_label=args.midline_label,
        midline_mode=args.midline_mode,
        prior_refine=args.prior_refine,
        # absolute, because Nipype runs each node in its own working directory
        atlas_dir=args.atlas_dir.resolve() if args.atlas_dir else None,
        atlas_reg_transform=args.atlas_reg_transform,
        atlas_reg_downsample_mm=args.atlas_reg_downsample_mm,
        prior_weight=args.prior_weight,
        prior_likelihood_smooth_mm=args.prior_likelihood_smooth_mm,
        debris_prior_threshold=args.debris_prior_threshold,
        prior_dilate_mm=args.prior_dilate_mm,
        prior_use_probs=args.prior_probs,
    )


def _write_manifest(config: LatsConfig) -> None:
    """Write the full run config to ``lats_run.json``."""
    payload = {
        **dataclasses.asdict(config),
        "runtime": config.resolved_runtime(),
        "timepoints": [
            {"index": tp.index, "subject_id": tp.subject_id, "image": str(tp.image)}
            for tp in config.timepoints
        ],
    }
    manifest_path(config.output_prefix).write_text(
        json.dumps(payload, indent=2, default=str) + "\n")


def _print_recent_crashes(config: LatsConfig, limit: int = 3) -> None:
    """Nipype writes one crash file per failed node; surface the newest."""
    base = nipype_base_dir(config.output_prefix)
    crashes = sorted(base.glob("crash-*.txt"), key=lambda p: p.stat().st_mtime,
                     reverse=True)[:limit]
    if not crashes:
        return
    print("\n--- most recent nipype crash files ---", file=sys.stderr)
    for c in crashes:
        print(f"\n=== {c} ===", file=sys.stderr)
        tail = c.read_text().splitlines()[-40:]
        print("\n".join(tail), file=sys.stderr)


def main(argv: list[str] | None = None) -> int:
    args = _build_parser().parse_args(argv)

    logging.basicConfig(
        level=logging.DEBUG if args.debug else (logging.INFO if args.verbose else logging.WARNING),
        format="%(levelname)s %(name)s: %(message)s",
    )

    if not args.output_prefix and not args.check_deps:
        print("error: -o/--output-prefix is required", file=sys.stderr)
        return 2
    if not args.images and not args.check_deps:
        print("error: at least one input image is required", file=sys.stderr)
        return 2

    config = _config_from_args(args)

    problems = check_dependencies(config)
    if problems:
        print("error: unmet dependencies:", file=sys.stderr)
        for p in problems:
            print(f"  - {p}", file=sys.stderr)
        return 1
    if args.check_deps:
        print("All dependencies satisfied.")
        return 0

    # Input sanity checks; hard failures raise CheckError.
    from .utils.checks import CheckError, preflight
    try:
        warnings = preflight([tp.image for tp in config.timepoints])
    except CheckError as exc:
        print(f"error: input check failed: {exc}", file=sys.stderr)
        return 1
    for w in warnings:
        print(f"warning: {w}", file=sys.stderr)

    config.output_prefix.mkdir(parents=True, exist_ok=True)
    _write_manifest(config)

    wf = build_workflow(config)

    if args.dry_run:
        print(f"LATS workflow: {len(wf.list_node_names())} nodes")
        for name in sorted(wf.list_node_names()):
            print(f"  {name}")
        print(f"\n{config.n_timepoints} timepoints, runtime={config.resolved_runtime()}")
        print(f"template: AMTC2 -k 1 -i {config.template_iterations} "
              f"-t {config.template_transform} -m {config.template_metric}")
        return 0

    plugin_args = {"n_procs": config.n_procs} if config.plugin == "MultiProc" else None

    try:
        wf.run(plugin=config.plugin, plugin_args=plugin_args)
    except Exception as exc:  # nipype re-raises node failures here
        LOG.error("workflow failed: %s", exc)
        _print_recent_crashes(config)
        return 1

    if not config.keep_tmp:
        import shutil
        shutil.rmtree(nipype_base_dir(config.output_prefix), ignore_errors=True)
        # AMTC2 scratch only; the SST is already in sst/ and reg/ is kept.
        shutil.rmtree(sst_work_dir(config.output_prefix), ignore_errors=True)

    print(f"Done. Outputs in {config.output_prefix}")
    print(f"  volumes:      {config.output_prefix / 'stats' / 'volumes.csv'}")
    print(f"  longitudinal: {config.output_prefix / 'stats' / 'longitudinal.csv'}")
    return 0
