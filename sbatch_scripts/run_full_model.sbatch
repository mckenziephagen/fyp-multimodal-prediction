#!/usr/bin/bash
#SBATCH --job-name=multi-modal
#SBATCH --output=log/%j.out
#SBATCH --error=log/%j.err
#SBATCH --time=2:00
#SBATCH -p normal
#SBATCH -c 1
#SBATCH --mem=8GB
#SBATCH --array 1-100

conda activate r-env

Rscript dev-predict.R --num_its SLURM_ARRAY_JOB_ID --cognitive_measure {COG_MEASURE}


