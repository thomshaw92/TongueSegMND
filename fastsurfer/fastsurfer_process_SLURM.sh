#!/bin/bash --login
#SBATCH --partition=ai
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=10
#SBATCH --mem=24G
#SBATCH --job-name=fastsurfer
#SBATCH --time=8:00:00
#SBATCH --account=a_barth
#SBATCH --gres=gpu:a100:1
#SBATCH -o slurm.output
#SBATCH -e slurm.error

# load necessary modules
ml cuda/11.7.0

# Running the script that runs a single subject through fastsurfer
srun fastsurfer_script.sh


