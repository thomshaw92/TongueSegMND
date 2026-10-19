# Tongue segmentation post-processing QA

A client-side web tool to apply post-processing to tongue-muscle segmentations and view the
result in NiiVue. Images are processed in the browser and never leave your machine.

Steps: superior longitudinal thinning, inferior clip, surface trim, clean, smooth.

## Run

```
npm run serve
```

Then open http://localhost:8000, load an image and its segmentation (same grid, RAS
orientation), toggle the steps and download the corrected labels.

## Test

```
npm test
```

This regenerates the test fixtures first and needs Python 3 with numpy and nibabel.
