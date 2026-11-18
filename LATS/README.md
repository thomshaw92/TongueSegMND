# LATS: Longitudinal Automatic Tongue Segmentation

LATS is a Nipype pipeline that builds a single-subject template (SST) from a subject's MRI sessions, segments the tongue muscles on it once and propagates the labels back to every session to measure volume and shape over time.
The tongue segmentation model is from Shaw et al., Computers in Biology and Medicine 2025, https://doi.org/10.1016/j.compbiomed.2025.110824.
The atlas prior is built from the probabilistic tongue atlas at https://osf.io/wt9fc/.

## Pipeline

1. `preprocess`: optional whole-image N4 bias correction (`-n`) and Rician denoising (`--denoise`). Denoising is off by
   default here and in `enhance_crop`, as the segmenter was trained on images that were not denoised.
2. `prep_crop`: register each session to the segmenter's template and crop to the tongue.
3. `enhance_crop`: tongue-local N4 and isotropic resampling of each crop (denoising with `--crop-denoise`).
4. `sst`: build the SST from the crops; a single session uses its own crop.
5. `segment_sst`: segment the SST with the tongue model; `--prior-refine` adds atlas-prior refinement.
   `autofix` then bias-corrects the SST within the tongue (N4), clips superior longitudinal below vertical, keeps
   superior longitudinal as a continuous cap over the dorsum, trims superior and inferior longitudinal surface voxels
   outside each muscle's own intensity band (`--sl-trim-layers`, `--il-trim-layers`, `--trim-sd`), moves inferior
   longitudinal lying above superior longitudinal, removes thick dark tongue joined to the outside (`--dark-sd`), drops
   tongue below the segmenter probability set by `--prob-cut` (0.99; 0 turns it off), smooths (1 mm), removes genioglossus at the side or top of
   the tongue or far in front of vertical or superior longitudinal, gives stray pieces to what surrounds them and keeps
   one connected tongue. The outermost side voxel of vertical and superior longitudinal is removed (`--no-side-shave`). Each session's warped labels are smoothed and cleaned the same way. `--no-autofix` skips both.
6. `register`, `warp_labels`: register the SST to each crop and warp its labels to native space.
7. `jacobian`: per-label log-Jacobian volumes (two or more sessions).
8. `stats`, `shape`: label volumes, Jacobian change from the first session, left/right shape descriptors and meshes.
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

Give one image per session, in acquisition order. Main flags: `--container`, `--pipeline`, `-R/--runtime`, `-M/--modality`, `-n/--n4`, `--denoise`, `--prior-refine`, `--no-autofix`, `--sl-trim-layers`, `--il-trim-layers`, `--trim-sd`, `--prob-cut`, `--dark-sd`, `--no-side-shave`, `--atlas-dir`, `-c/--plugin`, `-j/--n-procs`, `--no-jacobian`, `--no-shape`, `--no-qc`, `-b/--keep-tmp`, `--overwrite`, `--dry-run`, `--check-deps`.

## Outputs

- `lats_run.json` (run configuration) and `labels.txt` (label ids and names).
- `sst/` (template, its tongue bias-corrected copy, its segmentation and the fixed `sst_seg_autofix.nii.gz`), `labels/`
  and `labels_lateral/` (labels in each session's native space; the unfixed warps are in `intermediate/`). Left and right are split at the template's mirror-symmetry plane, recorded in `sst/midline.json`.
- After QA, `lats -o <run> --qa-labels <corrected template labels> <images>` re-applies the corrections to every session, reusing the run's template and registrations; the original run's outputs move to `pre_qa/`, and a later re-apply replaces the previous re-apply's outputs. A full run refuses a folder that has been through QA; give it a new `-o`. A full run also refuses a folder that holds a finished run unless given `--overwrite`, which first deletes that run's outputs.
- `jacobian/` (Jacobian and log-Jacobian images), `shape/` (descriptors and SST meshes) and `stats/` (`volumes.csv`, `longitudinal.csv`, `shape.csv`, `asymmetry.csv`).
- `qc/` (montages, QC images and labels, and `flags.json` with provisional checks of the run) and `intermediate/` (working files, including the SST-to-session registrations).

## Tests

Run `pip install -e ".[dev]"`, then `pytest`.
