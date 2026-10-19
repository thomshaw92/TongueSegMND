"""Single-subject template (SST) via ``antsMultivariateTemplateConstruction2.sh``."""
from __future__ import annotations

from pathlib import Path

from nipype.interfaces.base import (
    CommandLine,
    CommandLineInputSpec,
    File,
    InputMultiPath,
    TraitedSpec,
    traits,
)
from nipype.pipeline import engine as pe

from ..config import LatsConfig
from ..utils.paths import sst_work_dir


class _AMTCInputSpec(CommandLineInputSpec):
    """Inputs for ``antsMultivariateTemplateConstruction2.sh``."""
    output_prefix = traits.Str(
        mandatory=True, argstr="-o %s", position=0,
        desc="output prefix (e.g. .../T_)",
    )
    dimension = traits.Int(3, usedefault=True, argstr="-d %d")
    n_modalities = traits.Int(1, usedefault=True, argstr="-k %d")
    iterations = traits.Int(2, usedefault=True, argstr="-i %d",
                            desc="template-construction iterations")
    gradient_step = traits.Float(0.25, usedefault=True, argstr="-g %f")
    pairwise_iters = traits.Str(
        "100x70x30x3", usedefault=True, argstr="-q %s",
        desc="max iterations per pairwise registration",
    )
    metric = traits.Str("CC", usedefault=True, argstr="-m %s")
    transform = traits.Str("SyN", usedefault=True, argstr="-t %s")
    update_full_affine = traits.Int(1, usedefault=True, argstr="-y %d")
    rigid_init = traits.Int(1, usedefault=True, argstr="-r %d")
    parallel_control = traits.Int(0, usedefault=True, argstr="-c %d")
    n_cores = traits.Int(2, usedefault=True, argstr="-j %d")
    n4_bias = traits.Int(0, usedefault=True, argstr="-n %d")
    backup_iter = traits.Int(0, usedefault=True, argstr="-b %d")
    images = InputMultiPath(
        File(exists=True), mandatory=True, argstr="%s", position=-1,
        desc="one anatomical image per timepoint",
    )


class _AMTCOutputSpec(TraitedSpec):
    template0 = File(desc="T_template0.nii.gz")
    output_dir = traits.Str()


class _AntsMultivariateTemplateConstruction2(CommandLine):
    _cmd = "antsMultivariateTemplateConstruction2.sh"
    input_spec = _AMTCInputSpec
    output_spec = _AMTCOutputSpec

    def _list_outputs(self):
        prefix = Path(self.inputs.output_prefix)
        out_dir = prefix.parent
        out = self.output_spec().get()
        out["output_dir"] = str(out_dir)
        out["template0"] = str(out_dir / f"{prefix.name}template0.nii.gz")
        return out


def amtc_plugin_code(plugin: str) -> int:
    """Map a Nipype plugin to AMTC2 ``-c``; never a cluster-submission code."""
    return {"Linear": 0}.get(plugin, 2)


def build_sst(config: LatsConfig) -> pe.Node:
    """Return the SST node. Caller wires ``images`` (the cropped sessions)."""
    out_dir = sst_work_dir(config.output_prefix)
    out_dir.mkdir(parents=True, exist_ok=True)

    sst = pe.Node(_AntsMultivariateTemplateConstruction2(), name="sst")
    sst.inputs.output_prefix = str(out_dir / "T_")
    sst.inputs.n_modalities = 1
    sst.inputs.iterations = config.template_iterations
    sst.inputs.metric = config.template_metric
    sst.inputs.transform = config.template_transform
    sst.inputs.gradient_step = config.template_gradient_step
    sst.inputs.pairwise_iters = config.template_pairwise_iters
    sst.inputs.parallel_control = amtc_plugin_code(config.plugin)
    sst.inputs.n_cores = config.n_procs
    sst.inputs.n4_bias = 0
    # Cap ITK threads for AMTC2's child antsRegistration processes.
    sst.inputs.environ = {"ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS": str(config.n_procs)}
    return sst


def build_single_session_sst(config: LatsConfig) -> pe.Node:
    """1-session run: publish the one cropped image as ``T_template0``."""
    from nipype.interfaces.utility import Function

    node = pe.Node(
        Function(
            input_names=["images", "output_dir"],
            output_names=["template0"],
            function=_single_session_template,
        ),
        name="sst",
    )
    node.inputs.output_dir = str(sst_work_dir(config.output_prefix))
    return node


def _single_session_template(images, output_dir: str) -> str:
    import shutil
    from pathlib import Path as _P

    od = _P(output_dir)
    od.mkdir(parents=True, exist_ok=True)
    src = images[0] if isinstance(images, (list, tuple)) else images
    dest = od / "T_template0.nii.gz"
    shutil.copy(src, dest)
    return str(dest)


def build_publish_sst(config: LatsConfig) -> pe.Node:
    """Publish the SST onto the exact crop grid, in ``sst/``."""
    from nipype.interfaces.utility import Function

    node = pe.Node(
        Function(
            input_names=["template", "crop_reference", "output_dir"],
            output_names=["template"],
            function=_publish_template,
        ),
        name="publish_sst",
    )
    from ..utils.paths import sst_dir
    node.inputs.output_dir = str(sst_dir(config.output_prefix))
    return node


def _publish_template(template: str, crop_reference, output_dir: str) -> str:
    import subprocess
    from pathlib import Path as _P

    import nibabel as nib
    import numpy as np

    from lats.utils.checks import check_finite_nonempty

    od = _P(output_dir)
    od.mkdir(parents=True, exist_ok=True)
    dest = od / "T_template0.nii.gz"

    ref = crop_reference[0] if isinstance(crop_reference, (list, tuple)) else crop_reference
    # Identity reslice onto the crop grid (shared world coords -> lossless).
    proc = subprocess.run(
        ["antsApplyTransforms", "-d", "3", "-i", str(template), "-r", str(ref),
         "-o", str(dest), "-n", "Linear"],
        capture_output=True, text=True,
    )
    if proc.returncode != 0:
        tail = "\n".join((proc.stderr or "").splitlines()[-20:]) or "<empty>"
        raise RuntimeError(f"publishing SST onto crop grid failed:\n{tail}")

    check_finite_nonempty(np.asarray(nib.load(str(dest)).get_fdata()), "SST template")
    return str(dest)
