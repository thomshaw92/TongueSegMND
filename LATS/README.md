# LATS: Longitudinal Automatic Tongue Segmentation

LATS is a Nipype pipeline that builds a single-subject template (SST) from a subject's MRI sessions, segments the tongue muscles on it once and propagates the labels back to every session to measure volume and shape over time.
The tongue segmentation model is from Shaw et al., Computers in Biology and Medicine 2025, https://doi.org/10.1016/j.compbiomed.2025.110824.
The atlas prior is built from the probabilistic tongue atlas at https://osf.io/wt9fc/.

## Pipeline

1. `preprocess`: optional whole-image N4 bias correction and Rician denoising (`-n`, `--denoise`).
2. `prep_crop`: register each session to the segmenter's template and crop to the tongue.
3. `enhance_crop`: tongue-local N4, denoising and isotropic resampling of each crop.
4. `sst`: build the SST from the crops; a single session uses its own crop.
5. `segment_sst`: segment the SST with the tongue model; `--prior-refine` adds atlas-prior refinement.
6. `register`, `warp_labels`: register the SST to each crop and warp its labels to native space.
7. `jacobian`: per-label log-Jacobian volumes (two or more sessions).
8. `stats`, `shape`: label volumes, change from the first session, left/right shape descriptors and meshes.
9. `native_qc`, `qc`: re-preprocessed native QC image and labels, and a QC montage per session.

## Install

```bash
pip install -e .
```

ANTs must be on `PATH` (`antsMultivariateTemplateConstruction2.sh`, `antsRegistrationSyN.sh`, `antsApplyTransforms`, `CreateJacobianDeterminantImage`, `N4BiasFieldCorrection`, `DenoiseImage`, `ResampleImage`). LATS also needs the tongue segmenter, as a Docker or Apptainer container or as its `run_tongue_pipeline.py` script.

## Usage

```bash
lats -o out/ --container tongue-seg:amd64 ses-01_T1w.nii.gz ses-02_T1w.nii.gz
lats -o out/ --runtime native --pipeline /path/to/run_tongue_pipeline.py --prior-refine --atlas-dir atlas-assets ses-01_T1w.nii.gz ses-02_T1w.nii.gz
```

Give one image per session, in acquisition order. Main flags: `--container`, `--pipeline`, `-R/--runtime`, `-M/--modality`, `-n/--n4`, `--denoise`, `--prior-refine`, `--atlas-dir`, `-c/--plugin`, `-j/--n-procs`, `--no-jacobian`, `--no-shape`, `--no-qc`, `-b/--keep-tmp`, `--dry-run`, `--check-deps`.

## Outputs

- `lats_run.json` (run configuration) and `labels.txt` (label ids and names).
- `sst/` (template and its segmentation), `labels/` and `labels_lateral/` (labels in each session's native space).
- `jacobian/` (Jacobian and log-Jacobian images), `shape/` (descriptors and SST meshes) and `stats/` (`volumes.csv`, `longitudinal.csv`, `shape.csv`, `asymmetry.csv`).
- `qc/` (montages, QC images and labels) and `intermediate/` (working files, including the SST-to-session registrations).

## Tests

Run `pip install -e ".[dev]"`, then `pytest`.
