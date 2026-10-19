# TongueSegMND

Exploring tongue segmentation in MND patients. Segmentation, data wrangling, analysis.

- [`LATS/`](LATS/README.md): longitudinal tongue-muscle segmentation. Builds a single-subject
  template from a subject's MRI sessions, segments it once, and measures change by the
  deformation between sessions.
- [`qc/`](qc/README.md): a keyboard-driven app for rating MRI quality and segmentation quality
  on the published 1 to 5 scales, two keystrokes per scan.
- [`postproc-qa/`](postproc-qa/README.md): an in-browser tool to review and apply
  post-processing to tongue-muscle segmentations.
- `stats/`, `SciData-paper-code/`: analysis code for the papers. The scripts read their data
  from `$TONGUE_ROOT` and write tables and figures to `$TONGUE_OUT`; the data are not in this
  repository.
- `utils/`, `volume_curv_stats_extraction/`, `JLF/`, `fastsurfer/`: helper scripts.
