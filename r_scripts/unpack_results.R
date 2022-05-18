# ---
# jupyter:
#   jupytext:
#     formats: ipynb,R:light
#     text_representation:
#       extension: .R
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.13.8
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

cog <- 'CogTotalComp_Unadj' 

# +
#git_hash = system("git rev-parse HEAD", intern=TRUE)
# -

git_hash <- '0fc58275891f2b7fb543a83bef3808412071c6cb' 

k <- 5

# +
#for (cog in cognitive_measures)  
corr_df <- data.frame()


corr_csvs <- Sys.glob(file.path(result_dir, git_hash, 'Q2', k,
                       cog, 'it*', 'correlation.csv'))


corr_df <- data.frame()
for (csv in corr_csvs) { 
    temp_df <- read.csv(csv)
    corr_df<-rbind(corr_df, temp_df)
    }

plot_df <- reshape2::melt(corr_df[-1])
psych::describe(corr_df)
# +
#for (cog in cognitive_measures)  
rsq_csvs <- Sys.glob(file.path(result_dir, git_hash, 'Q2', k,
                       cog, 'it*', 'single_rsq.csv'))

rsq_df <- data.frame()
for (csv in rsq_csvs) { 
    temp_df <- read.csv(csv)
    rsq_df<-rbind(rsq_df, temp_df)
    }

plot_df <- reshape2::melt(rsq_df[-1])
 
psych::describe(rsq_df)
ggplot(data = plot_df, mapping = aes(x= ordered(variable,
                                    levels = c("connectome", "surface", 
                                                "thickness", "volume", 
                                                "local_connectome")), 
                                     y = value )) +
    geom_boxplot() + 
    geom_jitter() 
    

# -

full_csv_paths <- Sys.glob(file.path(result_dir, git_hash, 'Q2', 
                       cog, 'it*', 'single*.csv'))

full_rsq_df <- data.frame()

for (csv in full_csv_paths) { 
    temp_rsq <- read.csv(csv)
    full_rsq_df <- rbind(full_rsq_df, temp_rsq)
}

describe(full_rsq_df)

full_rsq_df


