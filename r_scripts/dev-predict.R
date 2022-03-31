# ---
# jupyter:
#   jupytext:
#     formats: ipynb,R:percent
#     text_representation:
#       extension: .R
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.13.7
#   kernelspec:
#     display_name: R [conda env:test-R]
#     language: R
#     name: conda-env-test-R-r
# ---

# %%
source('.Rprofile')

# %% tags=[]
library(rhdf5 , quietly=TRUE) #https://www.bioconductor.org/packages/release/bioc/html/rhdf5.html
library(glmnet, quietly=TRUE)
library(caret, quietly=TRUE)
library(hash, quietly=TRUE)
library(dplyr, quietly=TRUE)
library(broom) 
library(R.utils)

# %%
source('functions.R')

# %%
default_args = list(cognitive_measure='CogTotalComp_Unadj', num_it=1) 

args <- commandArgs(defaults=default_args, asValues=TRUE)
subject_subset <- args$subject_subset
cog <- args$cognitive_measure
num_it <- args$num_it

# %%
data_dir <- ('../data')

# %%
rasero_data_path <- file.path(data_dir, 'final_data_R1.hdf5')
features = H5Fopen(rasero_data_path)
feature_names <- h5ls(features)['name']
rasero_subjects <- features$subjects

# %%
predictors <- hash()

cognition <- data.frame(t(features$YY_domain_cognition) )

predictors$connectome <- t(features$connectome_features) 
predictors$volume <- t(features$sub_vols_features)
predictors$local_connectome <- t(features$loc_conn_features)
predictors$surface <- t(features$surface_features)
predictors$thickness <- t(features$thickness_features)

H5close()

# %%
rownames(cognition) <- rasero_subjects

colnames(cognition) <- c('CogTotalComp_Unadj', 
                        'CogFluidComp_Unadj', 
                        'CogCrystalComp_Unadj', 
                        'SCPT_SEN', 
                        'DDisc_AUC_200', 
                        'IWRD_TOT', 
                        'VSPLOT_TC')

for (i in keys(predictors)) {
    rownames(predictors[[i]]) <- rasero_subjects
}

# %%
unrestricted_data <- read.csv('../data/unrestricted_mphagen_1_27_2022_20_50_7.csv')
rownames(unrestricted_data) <- unrestricted_data$Subject
restricted_data <- read.csv('../data/RESTRICTED_arokem_1_31_2022_23_26_45.csv')


# %%
#reproducible things
git_hash = system("git rev-parse HEAD", intern=TRUE)

# %%
#check to make sure that my download of the hcp data == rasero's 
stopifnot(identical(subset(unrestricted_data, Subject %in% rasero_subjects)[[cog]], cognition[[cog]]))

# %%
if (subject_subset == 'Q2') { 
    q2 <- c(subset(unrestricted_data, 
                        (Release == 'Q2' | Release == 'Q1') 
                        & ('3T_Full_MR_Compl' = TRUE))$'Subject')
    subjects <- intersect(rasero_subjects, q2)     
} else {
    subjects = rasero_subjects } 
 
#cull data to specific subset of subjects 
restricted_data <- restricted_data %>% filter(
  Subject %in% subjects
  )

# %%
# I literally cannot remember what's going on here
if (length(subjects) != dim(cognition)[1]) {
cognition <- subset(cognition, rownames(cognition) %in% subjects)
    for (i in pred_list) {
        predictors[[i]] <- subset(predictors[[i]], rownames(predictors[[i]]) %in% subjects)
    }
}

# %%
it_dir_name <- paste('iteration', num_it, sep='_')
result_path <- file.path('../results', git_hash, subject_subset, cog, it_dir_name)
dir.create(result_path,recursive = TRUE)

# %%
single_rsq_df <- data.frame(matrix(ncol = length(pred_list), nrow=0))
colnames(single_rsq_df) <- pred_list

stacked_rsq_df <- data.frame(matrix(ncol = 2, nrow=0))
colnames(stacked_rsq_df) <- c('stacked', 'full')

# %% tags=[]
set.seed(num_it)
folds <- groupKFold(restricted_data$Family_ID, k=5) #write test for this
test_prediction_df <- data.frame(row.names=subjects) 
final_yhat_df <- data.frame(row.names=subjects) 

for (num_fold in 1:length(folds)) {    
    indices <- CreateIndices(folds, num_curr_fold=num_fold) #fix here 

    train_index <- indices$train_index
    test_index <- indices$test_index 

    split_y_data <- SplitYData(cog, train_index, test_index, cognition)

    y_train_data <- split_y_data$'y_train'
    y_test_data <- split_y_data$'y_test'

    x_train_data <- list()
    x_test_data <- list()

    for (pred in pred_list) { 

        split_x_data <- SplitXData(pred, train_index, test_index)
        x_train_data[[pred]] <- split_x_data$'x_train'
        x_test_data[[pred]] <- split_x_data$'x_test'
    } 

    start_time <- Sys.time()

    single_models <- RunSingleModels(pred_list, x_train_data, y_train_data)
    single_rsq_df <- rbind(single_rsq_df, single_models$rsq)
    
    stacked_model <- RunStackedModel(as.matrix(single_models$predictions), cognition[train_index, cog])
    
    #turn into function
     for (pred in pred_list) { 
         curr_model <- single_models$models[pred]
         test_prediction_df[test_index, pred] <- predict(curr_model, newx=x_test_data[[pred]], s='lambda.1se')  
     } 

     final_yhat_df[test_index,'predictions'] <- predict(stacked_model$model, newx=as.matrix(test_prediction_df[test_index,]), 
                        s='lambda.1se', type = 'link') #double check why newy was in here
    
    rds_name <- file.path(result_path, paste('fold', num_fold, '.RDS', sep=''))
    save(single_models, stacked_model, num_it, indices, file = rds_name)
} 

full_rsq <- CalcRsq(final_yhat_df[['predictions']], cognition[[cog]])
write.csv(single_rsq_df, file = file.path(result_path, paste('single_rsq_dfs', '.csv', sep='')))
write.csv(stacked_rsq_df, file = file.path(result_path, paste('stacked_rsq_dfs', '.csv', sep='')))
