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

#renv hack
setwd('/home/users/mphagen/fyp-multimodal-prediction/r_scripts')
source('.Rprofile')

library(rhdf5 , quietly=TRUE) 
library(hash, quietly=TRUE)

data_dir <- ('/home/users/mphagen/fyp-multimodal-prediction/data')

rasero_data_path <- file.path(data_dir, 'final_data_R1.hdf5')
features = H5Fopen(rasero_data_path)
feature_names <- h5ls(features)['name']
rasero_subjects <- features$subjects


# +
predictors <- hash()

cognition <- data.frame(t(features$YY_domain_cognition) )

predictors$connectome <- t(features$connectome_features) 
predictors$volume <- t(features$sub_vols_features)
predictors$local_connectome <- t(features$loc_conn_features)
predictors$surface <- t(features$surface_features)
predictors$thickness <- t(features$thickness_features)

H5close()

# +
rownames(cognition) <- rasero_subjects

colnames(cognition) <- c('CogTotalComp_Unadj', 
                        'CogFluidComp_Unadj', 
                        'CogCrystalComp_Unadj', 
                        'SCPT_SEN', 
                        'DDisc_AUC_200', 
                        'IWRD_TOT', 
                        'VSPLOT_TC')

pred_list <- keys(predictors)
for (i in keys(predictors)) {
    rownames(predictors[[i]]) <- rasero_subjects
}
# -

unrestricted_data <- read.csv('../data/unrestricted_mphagen_1_27_2022_20_50_7.csv')
rownames(unrestricted_data) <- unrestricted_data$Subject
restricted_data <- read.csv('../data/RESTRICTED_arokem_1_31_2022_23_26_45.csv')

H5Fopen(rasero_data_path)

h5createGroup(rasero_data_path,"fyp_data")


h5closeAll()

h5ls(rasero_data_path)
