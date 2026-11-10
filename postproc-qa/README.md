# Tongue segmentation post-processing QA

A client-side web tool for quality control of LATS segmentations, in Chrome or Edge. Images are
processed in the browser and never leave your machine.

## Run

```
npm run serve
```

Then open http://localhost:8000, enter your initials and open a BIDS folder whose `derivatives/lats`
holds LATS outputs. The app lists every template (one per run) and every session, and takes each
through seven steps: rate the scan, rate the labels, correct the surface, follow the dorsum and smooth,
check the midline (templates only), rate the corrected labels, and decide (include, touch up in
ITK-SNAP, or exclude). Each step has an information button.

Labels, a JSON record per item (ratings, settings, midline, decision, rater and time) and `qa.tsv`
are written to `derivatives/lats-qa`, named after the image (`*_desc-qa_dseg.nii.gz`). Work is saved
as you go; reopening the folder resumes at the first unfinished item.

## Test

```
npm test
```

This regenerates the test fixtures first and needs Python 3 with numpy and nibabel.
