# Tongue-QC

Visual quality control for tongue segmentation pipeline outputs. It renders montage strips for each
scan into a self-contained HTML app, in which a rater grades MRI and segmentation quality by keyboard.

The two 1 to 5 scales are those published in Shaw et al. 2025 (Computers in Biology and Medicine), reproduced in [`TONGUE_QC_SCALE.md`](TONGUE_QC_SCALE.md).

## Install

```bash
pip install -r requirements.txt
```

## Build

```bash
python build_tongue_qc.py --root <study> --out <build>
```

`--root` accepts a study root, a derivatives folder, a folder of scan folders or a single scan folder.

## Rate

```bash
cd <build>
python qc_serve.py
```

Ratings are saved to `tongue_qc_ratings.csv` as you go; the keys are listed in the app's keys card.

## Output

`tongue_qc_ratings.csv` has one row per scan, with these columns:

```text
schema_version, scan_id, sub, ses, space, mri_quality, seg_quality, mri_disposition, seg_disposition,
touched, notes, tooth_artefact, tooth_affects_volume, tooth_affects_shape, rater, rated_at, order
```

## Tests

```bash
pytest
```
