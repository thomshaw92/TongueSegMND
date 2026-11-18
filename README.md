# TongueSegMND

Exploring tongue segmentation in MND patients. Segmentation, data wrangling, analysis.

- [`LATS/`](LATS/README.md): longitudinal tongue-muscle segmentation. Builds a single-subject
  template from a subject's MRI sessions, segments it once, and measures change by the
  deformation between sessions. With it:
  - [`LATS/postproc-qa/`](LATS/postproc-qa/README.md): an in-browser tool to review and apply
    post-processing to tongue-muscle segmentations.
  - [`LATS/qc/`](LATS/qc/README.md): a keyboard-driven app for rating MRI quality and segmentation
    quality on the published 1 to 5 scales, two keystrokes per scan.
- `SciData-paper-code/`, `2025-CIBM-paper-code/`, `stats/`: analysis code for the papers, with the
  scripts each used (joint label fusion in `SciData-paper-code/JLF/`; FastSurfer and the volume and
  curvature extraction in `2025-CIBM-paper-code/`). The analysis scripts read their data from
  `$TONGUE_ROOT` and write tables and figures to `$TONGUE_OUT`; the data are not in this repository.
- `utils/`: helper scripts.
