#!/usr/bin/bash
#SBATCH --job-name=multi-modal
#SBATCH --output=log/%j.out
#SBATCH --error=log/%j.err
#SBATCH --time=1:00:00
#SBATCH -p normal,jyeatman
#SBATCH --mem=50G
#SBATCH --export=NONE

source /home/users/mphagen/miniconda3/etc/profile.d/conda.sh

PATH="$HOME/miniconda3/bin:$PATH"

conda activate r-env

cd ../r_scripts

Rscript dev-predict.R --num_it ${SLURM_ARRAY_TASK_ID} --cognitive_measure {COG_MEASURE} -subject_subset {SUBJECT_SUBSET}


