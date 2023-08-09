#!/bin/bash
ml cuda
nvidia-smi

singularity exec --nv \
                 -B /scratch/user/uqtshaw/data/:/data \
                 -B /scratch/user/uqtshaw/data/FastSurferOutput:/output \
                 -B /scratch/user/uqtshaw/data:/fs_license \
                 /scratch/user/uqtshaw/sif_containers/fastsurfer-gpu.sif \
                 /fastsurfer/run_fastsurfer.sh \
		--parallel --sd /output --sid sub-019_somatomnd --vox_size '0.75' --T1 /data/sub-019_ses-01_acq-UNIDEN_run-1_T1w.nii.gz \
		--fs_license /fs_license/license.txt
