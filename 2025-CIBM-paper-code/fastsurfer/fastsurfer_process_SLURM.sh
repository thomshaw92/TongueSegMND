#!/bin/bash --login
# Usage, from this folder: sbatch -p PARTITION -A ACCOUNT --array=1-N fastsurfer_process_SLURM.sh SUBJECT_LIST DATASET
# SUBJECT_LIST has one subject name per line (N lines, indexed from 1).

#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=20
#SBATCH --mem=32G
#SBATCH --job-name=fastsurfer
#SBATCH --time=4:00:00
#SBATCH --gres=gpu:a100:1
#SBATCH --output=fastsurfer_%A_%a.out
#SBATCH --error=fastsurfer_%A_%a.err

list=$1
dataset=$2
# Extract subjName based on the array index
subjName=$(sed -n "${SLURM_ARRAY_TASK_ID}p" "$list")

module load cuda/11.7.0

srun bash "$SLURM_SUBMIT_DIR/fastsurfer_script.sh" "${subjName}" "${dataset}"
