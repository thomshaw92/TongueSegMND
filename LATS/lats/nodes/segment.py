"""Run the tongue segmenter in prep-crop and segment-cropped modes."""
from __future__ import annotations

import shlex

from nipype.interfaces.utility import Function
from nipype.pipeline import engine as pe

from ..config import LatsConfig
from ..utils.paths import intermediate_dir, sst_dir, sst_segmenter_dir


def _segmenter_cmd(runtime, container, pipeline, assets, container_args,
                   input_path, out_dir, pipeline_args):
    """Build the argv for one segmenter invocation. Pure string assembly."""
    from pathlib import Path as _P

    inp = _P(input_path).resolve()
    out = _P(out_dir).resolve()
    if runtime == "native":
        cmd = ["python", str(pipeline), "--input", str(inp),
               "--output", str(out), *pipeline_args]
        if assets:
            cmd += ["--assets", str(assets)]
        return cmd
    mount = "-v" if runtime == "docker" else "--bind"
    launch = ["docker", "run", "--rm"] if runtime == "docker" else ["apptainer", "run"]
    cmd = [*launch, mount, f"{inp.parent}:/in:ro", mount, f"{out}:/out"]
    if assets:
        cmd += [mount, f"{_P(assets).resolve()}:/assets:ro"]
    cmd += shlex.split(container_args or "")
    cmd += [container, "--input", f"/in/{inp.name}", "--output", "/out", *pipeline_args]
    if assets:
        cmd += ["--assets", "/assets"]
    return cmd


def _run(cmd, runtime, label):
    import subprocess

    proc = subprocess.run(cmd, capture_output=True, text=True)
    if proc.returncode != 0:
        tail = "\n".join((proc.stderr or "").splitlines()[-40:]) or "<empty>"
        out_tail = "\n".join((proc.stdout or "").splitlines()[-20:]) or "<empty>"
        raise RuntimeError(
            f"{label} failed (exit {proc.returncode})\n  runtime={runtime}\n"
            f"  cmd={' '.join(cmd)}\n--- stdout ---\n{out_tail}\n--- stderr ---\n{tail}"
        )
    return proc


# ---------------------------------------------------------------------------
# prep-crop: register each session to the template and crop
# ---------------------------------------------------------------------------

def _prep_crop_one(
    image, timepoint_idx, runtime, container, pipeline, modality, assets,
    container_args, threads, output_dir,
) -> tuple[str, str, str]:
    """Register+crop one session. Returns (cropped, affine_mat, crop_json)."""
    import json
    import shutil
    from pathlib import Path as _P

    from lats.nodes.segment import _run, _segmenter_cmd
    from lats.utils.checks import check_registration_affine, load_checked

    od = _P(output_dir).resolve() / f"tp{timepoint_idx:02d}"
    od.mkdir(parents=True, exist_ok=True)

    args = ["--modality", modality, "--threads", str(threads),
            "--prep-only", "--keep-work", "--input-space", "raw"]
    cmd = _segmenter_cmd(runtime, container, pipeline, assets, container_args,
                         image, od, args)
    _run(cmd, runtime, f"prep-crop tp{timepoint_idx:02d}")

    cropped = od / "image_cropped.nii.gz"
    affine = od / "work" / "reg_0GenericAffine.mat"
    crop_json = od / "work" / "crop.json"
    for f, what in [(cropped, "cropped image"), (affine, "registration affine"),
                    (crop_json, "crop.json")]:
        if not f.is_file():
            produced = sorted(p.name for p in od.rglob("*"))
            raise RuntimeError(f"prep-crop produced no {what} ({f.name}) for "
                               f"tp{timepoint_idx:02d}; found: {produced}")

    # Guard: stop on a collapsed registration affine.
    det = check_registration_affine(str(affine), f"tp{timepoint_idx:02d} native->template")

    load_checked(cropped)
    box = json.loads(crop_json.read_text())

    # World affine from the template's, never the crop's own; crop box order is [z,x,y].
    import nibabel as nib
    import numpy as np
    warped = od / "reg_Warped.nii.gz"
    if not warped.is_file():
        warped = od / "work" / "reg_Warped.nii.gz"
    tmpl_affine = np.array(nib.load(str(warped)).affine, dtype=float)
    cimg = nib.load(str(cropped))
    start = np.array([box["z_min"], box["x_min"], box["y_min"], 1.0])
    new_aff = np.array(cimg.affine, dtype=float)
    new_aff[:3, :3] = tmpl_affine[:3, :3]
    new_aff[:3, 3] = (tmpl_affine @ start)[:3]
    fixed = od / "image_cropped_worldfix.nii.gz"
    nib.save(nib.Nifti1Image(np.asarray(cimg.dataobj), new_aff, cimg.header), fixed)

    shutil.copy(affine, od / "native2template_0GenericAffine.mat")
    box["affine_det"] = det
    (od / "crop.json").write_text(json.dumps(box, indent=2) + "\n")
    return (str(fixed), str(od / "native2template_0GenericAffine.mat"),
            str(od / "crop.json"))


def build_prep_crop(config: LatsConfig) -> pe.MapNode:
    """MapNode over sessions. Caller wires ``image`` (preprocessed)."""
    runtime = config.resolved_runtime()
    node = pe.MapNode(
        Function(
            input_names=["image", "timepoint_idx", "runtime", "container",
                         "pipeline", "modality", "assets", "container_args",
                         "threads", "output_dir"],
            output_names=["cropped", "affine_mat", "crop_json"],
            function=_prep_crop_one,
        ),
        name="prep_crop",
        iterfield=["image", "timepoint_idx"],
    )
    node.inputs.timepoint_idx = list(range(config.n_timepoints))
    node.inputs.runtime = runtime
    node.inputs.container = str(config.container or "")
    node.inputs.pipeline = str(config.pipeline or "")
    node.inputs.modality = config.modality
    node.inputs.assets = str(config.assets) if config.assets else ""
    node.inputs.container_args = config.container_args
    node.inputs.threads = config.n_procs
    node.inputs.output_dir = str(intermediate_dir(config.output_prefix) / "prep_crop")
    return node


# ---------------------------------------------------------------------------
# segment-cropped: predict on the already-cropped SST
# ---------------------------------------------------------------------------

def _segment_cropped(
    template, runtime, container, pipeline, modality, assets, container_args,
    postproc, threads, segmenter_dir, sst_out_dir, save_probs=False,
) -> tuple[str, str, str]:
    """Segment the cropped SST. Returns (sst_segmentation, segmenter_dir, sst_probs)."""
    from pathlib import Path as _P

    import nibabel as nib
    import numpy as np

    from lats.nodes.segment import _run, _segmenter_cmd
    from lats.utils.checks import check_labels, warn_tongue_volume

    out_dir = _P(segmenter_dir).resolve()
    out_dir.mkdir(parents=True, exist_ok=True)

    # --no-qc: LATS makes its own QC.
    args = ["--modality", modality, "--threads", str(threads),
            "--input-space", "cropped", "--no-qc"]
    if save_probs:
        args.append("--save-probs")
    if not postproc:
        args.append("--no-postproc")
    cmd = _segmenter_cmd(runtime, container, pipeline, assets, container_args,
                         template, out_dir, args)
    _run(cmd, runtime, "segment-cropped SST")

    # Cropped mode writes seg_template.nii.gz; resample it onto the SST's crop grid.
    seg_template = out_dir / "seg_template.nii.gz"
    if not seg_template.is_file():
        produced = sorted(p.name for p in out_dir.glob("*"))
        raise RuntimeError("segment-cropped produced no seg_template.nii.gz in "
                           f"{out_dir}; found: {produced}")

    sd = _P(sst_out_dir)
    sd.mkdir(parents=True, exist_ok=True)
    dest = sd / "sst_seg.nii.gz"
    _run(["antsApplyTransforms", "-d", "3", "-i", str(seg_template),
          "-r", str(template), "-o", str(dest), "-n", "GenericLabel"],
         runtime, "resample SST seg onto crop grid")

    # Checks on the SST segmentation before everything downstream depends on it.
    seg_img = nib.load(str(dest))
    data = np.asarray(seg_img.get_fdata())
    check_labels(data, "SST segmentation")
    warns = warn_tongue_volume(data, seg_img.header.get_zooms()[:3], "SST segmentation")
    dup = out_dir / _P(template).name
    if dup.is_file():
        dup.unlink()
    if warns:
        (sd / "sst_seg_warnings.txt").write_text("\n".join(warns) + "\n")

    # 4D softmax: Linear (never GenericLabel), -e 3 resamples all channels together.
    probs_out = ""
    seg_probs = out_dir / "seg_template_probs.nii.gz"
    if save_probs and seg_probs.is_file():
        probs_dest = sd / "sst_seg_probs.nii.gz"
        _run(["antsApplyTransforms", "-d", "3", "-e", "3", "-i", str(seg_probs),
              "-r", str(template), "-o", str(probs_dest), "-n", "Linear"],
             runtime, "resample SST probs onto crop grid")
        probs_out = str(probs_dest)
    return str(dest), str(out_dir), probs_out


def build_segment(config: LatsConfig) -> pe.Node:
    """Return the SST segmentation node. Caller wires ``template`` (cropped)."""
    runtime = config.resolved_runtime()
    node = pe.Node(
        Function(
            input_names=["template", "runtime", "container", "pipeline",
                         "modality", "assets", "container_args", "postproc",
                         "threads", "segmenter_dir", "sst_out_dir", "save_probs"],
            output_names=["sst_segmentation", "segmenter_dir", "sst_probs"],
            function=_segment_cropped,
        ),
        name="segment_sst",
    )
    node.inputs.runtime = runtime
    node.inputs.container = str(config.container or "")
    node.inputs.pipeline = str(config.pipeline or "")
    node.inputs.modality = config.modality
    node.inputs.assets = str(config.assets) if config.assets else ""
    node.inputs.container_args = config.container_args
    node.inputs.postproc = config.postproc
    node.inputs.threads = config.n_procs
    node.inputs.segmenter_dir = str(sst_segmenter_dir(config.output_prefix))
    node.inputs.sst_out_dir = str(sst_dir(config.output_prefix))
    node.inputs.save_probs = config.prior_use_probs
    return node
