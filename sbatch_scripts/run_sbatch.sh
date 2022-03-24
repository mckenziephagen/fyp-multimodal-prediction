#!/usr/bin/bash

for cog in CogTotalComp_Unadj CogFluidComp_Unadj CogCrystalComp_Unadj SCPT_SEN DDisc_AUC_200 IWRD_TOT VSPLOT_TC
   do 
    sed -e "s/{COG_MEASURE}/$cog/g" run_full_model.sbatch | sbatch 
  done 
    