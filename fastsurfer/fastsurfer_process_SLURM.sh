#!/bin/bash --login

#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=20
#SBATCH --mem=32G
#SBATCH --job-name=fastsurfer
#SBATCH --time=4:00:00
#SBATCH --partition=ai_collab
#SBATCH --account=a_barth
#SBATCH --gres=gpu:a100:1
#SBATCH --output=/scratch/user/uqtshaw/data/FastSurferOutput/belong2fastsurfer_%A_%a.out 
#SBATCH --error=/scratch/user/uqtshaw/data/FastSurferOutput/belong2fastsurfer_%A_%a.err 
#SBATCH --array=1-40 ##this is the number of subjects in $dataset indexed from 1

dataset="BeLong" #dataset name
# Extract subjName based on the array index 
subjName=$(sed -n "${SLURM_ARRAY_TASK_ID}p" /scratch/user/uqtshaw/code/TongueSegMND/fastsurfer/${dataset}_subjnames.csv) #path to subjnames
 
# load necessary modules
module load cuda/11.7.0

# Running the script that runs a single subject through fastsurfer
srun bash /scratch/user/uqtshaw/code/TongueSegMND/fastsurfer/fastsurfer_script.sh ${subjName} ${dataset}


