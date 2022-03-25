#!/usr/bin/bash
#SBATCH --job-name=multi-modal
#SBATCH --output=log/%j.out
#SBATCH --error=log/%j.err
#SBATCH --time=2:00
#SBATCH -p normal
#SBATCH -c 1
#SBATCH --mem=8GB
#SBATCH --export=NONE

PATH="$HOME/miniconda3/bin:$PATH"

conda activate r_env

Rscript dev-predict.R --num_it ${SLURM_ARRAY_TASK_ID} --cognitive_measure {COG_MEASURE} 


