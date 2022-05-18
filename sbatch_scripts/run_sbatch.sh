#!/usr/bin/bash

#ex run command: bash run_sbatch.sh 5 "full" 5 FALSE
num_it=$1
subject_subset=$2
k=$3
shuffle=$4
#add reading to this list 

for cog in PMAT24_A_CR CogCrystalComp_Unadj DDisc_AUC_200 #IWRD_TOT VSPLOT_TC CogTotalComp_Unadj #SCPT_SEN removed because it kept failing        
          do sed -e "s/{COG_MEASURE}/$cog/g"\
           -e "s/{SUBJECT_SUBSET}/$subject_subset/g"\
           -e "s/{NUM_K}/$k/g"\
           -e "s/{SHUFFLE}/$shuffle/g" run_full_model.sbatch | sbatch --array 1-${num_it} --output=log/${cog}_%a.out --error=log/${cog}_%a.err
 done 

