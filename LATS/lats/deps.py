"""Check ANTs, the segmenter runtime, input images and atlas assets."""
from __future__ import annotations

import shutil
import subprocess
from pathlib import Path

# ANTs tools LATS invokes.
ANTS_PROGRAMS = (
    "antsApplyTransforms",
    "CreateJacobianDeterminantImage",
    "N4BiasFieldCorrection",
    "DenoiseImage",
    "ResampleImage",
)
ANTS_SCRIPTS = (
    "antsMultivariateTemplateConstruction2.sh",
    "antsRegistrationSyN.sh",
)


def check_dependencies(config) -> list[str]:
    """Return a list of human-readable problems. Empty list = all good."""
    problems: list[str] = []

    for prog in (*ANTS_PROGRAMS, *ANTS_SCRIPTS):
        if shutil.which(prog) is None:
            problems.append(f"missing on PATH: {prog} (check ANTSPATH)")

    try:
        runtime = config.resolved_runtime()
    except ValueError as exc:
        problems.append(str(exc))
        return problems

    if runtime == "native":
        if not config.pipeline:
            problems.append("runtime=native needs --pipeline /path/to/run_tongue_pipeline.py")
        elif not Path(config.pipeline).is_file():
            problems.append(f"--pipeline not found: {config.pipeline}")
    else:
        if shutil.which(runtime) is None:
            problems.append(f"runtime={runtime} but '{runtime}' is not on PATH")
        if not config.container:
            problems.append(f"runtime={runtime} needs --container")
        elif runtime == "apptainer" and not Path(config.container).is_file():
            problems.append(f"--container .sif not found: {config.container}")
        elif runtime == "docker":
            try:
                subprocess.run(["docker", "info"], check=True,
                               capture_output=True, timeout=30)
            except (subprocess.CalledProcessError, subprocess.TimeoutExpired):
                problems.append("docker is on PATH but the daemon is not responding")
            except FileNotFoundError:
                problems.append("docker not found on PATH")

    # No session-count check: `lats --check-deps` runs with no images.
    for tp in config.timepoints:
        if not Path(tp.image).is_file():
            problems.append(f"input image not found: {tp.image}")

    if config.prior_refine:
        if not config.atlas_dir:
            problems.append("--prior-refine needs --atlas-dir")
        else:
            ad = Path(config.atlas_dir)
            needed = ["atlas_foreground.nii.gz"] + \
                     [f"atlas_prob_{k}.nii.gz" for k in (1, 2, 3, 4)]
            for f in needed:
                if not (ad / f).is_file():
                    problems.append(f"missing in --atlas-dir: {f}")

    return problems
