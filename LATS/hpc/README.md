# Running LATS on a Slurm cluster

Three scripts run LATS over a BIDS dataset with the LATS Apptainer image, one Slurm array task per
run. Each script prints its options with `--help`.

1. `lats_make_lists.sh` finds one T1w image per session and writes the run lists:
   `longitudinal.tsv` for subjects with two or more sessions, `cross-sectional.tsv` for
   single-session runs, plus `excluded.tsv` and `summary.txt`.
   ```bash
   ./lats_make_lists.sh -b /path/to/bids -o lists
   ```
2. `lats_submit.sh` submits one list as an array job. Run it once per list.
   ```bash
   ./lats_submit.sh -l lists/longitudinal.tsv -i lats-tongue.sif
   ./lats_submit.sh -l lists/cross-sectional.tsv -i lats-tongue.sif -J lats-cross
   ```
3. `lats_task.sh` is the array task that `lats_submit.sh` submits; it is not run by hand.

Outputs go to `BIDS/derivatives/lats/longitudinal/sub-X/` and
`BIDS/derivatives/lats/cross-sectional/sub-X_ses-Y/`. A run counts as done when its
`stats/volumes.csv` exists, so resubmitting a list redoes only the runs that are missing, and
`lats_make_lists.sh --skip-done` lists only those.
