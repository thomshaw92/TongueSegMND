#!/bin/bash
# Usage: fastsurfer_script.sh SUBJECT DATASET
# Needs in the environment: DATA_DIR (holding DATASET/SUBJECT_ses-02_T1w.nii.gz), OUT_DIR,
# LICENSE_DIR (holding license.txt) and FASTSURFER_SIF (the FastSurfer GPU Singularity image).
ml cuda
nvidia-smi
subjName=$1
dataset=$2

singularity exec --nv \
                 -B "${DATA_DIR:?set DATA_DIR}":/data \
                 -B "${OUT_DIR:?set OUT_DIR}":/output \
                 -B "${LICENSE_DIR:?set LICENSE_DIR}":/fs_license \
                 "${FASTSURFER_SIF:?set FASTSURFER_SIF}" \
                 /fastsurfer/run_fastsurfer.sh \
		--parallel --sd /output --sid ${subjName}_ses-02 --T1 /data/${dataset}/${subjName}_ses-02_T1w.nii.gz \
		--fs_license /fs_license/license.txt
