# Tongue segmentation post-processing QA

A client-side web tool for quality control of LATS segmentations, in Chrome or Edge. Images are
processed in the browser and never leave your machine.

## Run

```
npm run serve
```

Then open http://localhost:8000, enter your initials and open a BIDS folder whose `derivatives/lats`
holds LATS outputs. The app has two streams, done in order.

1. **Templates**, one per run, in five steps: rate the template and each session's scan (no labels shown),
   correct the surface (moving or turning the whole segmentation first if it sits off), follow the dorsum and smooth, check the midline, then rate the corrected labels and
   decide (include, edit by hand in the app or in ITK-SNAP, or exclude). When the templates are done, re-apply
   them with LATS (`--qa-labels`, or `lats_submit.sh --qa` on a cluster). LATS re-applies only templates marked
   include, and uses the midline saved with each one.
2. **Sessions**, which open once LATS has re-applied a template: rate how well the re-applied labels fit each
   session's scan, then include or exclude it. Every session must be checked; labels are not edited here. If a
   template is changed after LATS re-applied it, its sessions close until LATS re-applies it again, and then
   need checking again.

Each step has an information button. Labels, a JSON record per item (ratings, settings, midline, decision,
rater and time) and `qa.tsv` (including the options changed from the defaults) are written to
`derivatives/lats-qa`, named after the image (`*_desc-qa_dseg.nii.gz`). Work is saved as you go; reopening the
folder resumes at the first unfinished item.

## Test

```
npm test
```

This regenerates the test fixtures first and needs Python 3 with numpy and nibabel.
