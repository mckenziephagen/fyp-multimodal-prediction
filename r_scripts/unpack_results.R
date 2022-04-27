# ---
# jupyter:
#   jupytext:
#     formats: ipynb,R:light
#     text_representation:
#       extension: .R
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.13.7
#   kernelspec:
#     display_name: R [conda env:r-env]
#     language: R
#     name: conda-env-r-env-r
# ---

library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)
library(psych)

result_dir='/scratch/users/mphagen/fyp_results'

git_hash = system("git rev-parse HEAD", intern=TRUE)

cognitive_measures <- c('CogTotalComp_Unadj', 
                        'CogFluidComp_Unadj', 
                        'CogCrystalComp_Unadj', 
                        'SCPT_SEN', 
                        'DDisc_AUC_200', 
                        'IWRD_TOT', 
                        'VSPLOT_TC')

cog <- 'CogTotalComp_Unadj'

# +
#for (cog in cognitive_measures)  
oos_csv_paths <- Sys.glob(file.path(result_dir, git_hash, 'full', 
                       cog, 'it*', 'oos_rsq_df.csv'))

combined_df <- data.frame()
for (csv in oos_csv_paths) { 
    temp_df <- read.csv(csv)
    combined_df<-rbind(combined_df, temp_df)
    }

plot_df <- melt(combined_df[-1])
    
ggplot(data = plot_df, mapping = aes(x= ordered(variable,
                                    levels = c("connectome", "surface", 
                                                "thickness", "volume", 
                                                "local_connectome")), 
                                     y = value )) +
    geom_boxplot() + 
    geom_jitter() 
    

# -

full_csv_paths <- Sys.glob(file.path(result_dir, git_hash, 'full', 
                       cog, 'it*', 'full*.csv'))

read.csv(full_csv_paths[1])

full_rsq_df <- data.frame()

for (csv in full_csv_paths) { 
    temp_rsq <- read.csv(csv)
    full_rsq_df <- rbind(full_rsq_df, temp_rsq)
}

describe(combined_df)

describe(full_rsq_df[-1])

rds_paths <- Sys.glob(file.path(result_dir, git_hash, 'full', 
                       cog, 'it*', '*.RDS'))

readRDS(rds_paths[1])

rds_paths[1]

