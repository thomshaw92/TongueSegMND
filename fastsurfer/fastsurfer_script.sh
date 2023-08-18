#!/bin/bash
ml cuda
nvidia-smi
subjName=$1
dataset=$2

singularity exec --nv \
                 -B /scratch/user/uqtshaw/data/:/data \
                 -B /scratch/user/uqtshaw/data/FastSurferOutput:/output \
                 -B /scratch/user/uqtshaw/data:/fs_license \
                 /scratch/user/uqtshaw/sif_containers/fastsurfer-gpu.sif \
                 /fastsurfer/run_fastsurfer.sh \
		--parallel --sd /output --sid ${subjName}_ses-02 --T1 /data/${dataset}/${subjName}_ses-02_T1w.nii.gz \
		--fs_license /fs_license/license.txt

#cd /scratch/user/uqtshaw/data/FastSurferOutput/${subjName}
#zip -r extra_fastsurfer_output.zip tmp trash touch scripts 
#rm -r tmp trash touch scripts
#mv /scratch/user/uqtshaw/data/FastSurferOutput/${subjName} /QRISdata/Q5346/data/fastsurfer_output/

        #--vox_size '0.75'
