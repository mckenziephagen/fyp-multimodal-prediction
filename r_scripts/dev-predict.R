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
#     display_name: R [conda env:r-env]
#     language: R
#     name: conda-env-r-env-r
# ---

# %% tags=[]
setwd('/home/users/mphagen/fyp-multimodal-prediction/r_scripts')

source('.Rprofile')

library(rhdf5) #https://www.bioconductor.org/packages/release/bioc/html/rhdf5.html
library(glmnet, quietly=TRUE)
library(caret, quietly=TRUE)
library(hash, quietly=TRUE)
library(dplyr, quietly=TRUE)
library(broom) 
library(R.utils)

# %%
source('functions.R')

# %%
default_args = list(subject_subset='Q2', cognitive_measure= 'CogTotalComp_Unadj', 
                    num_it=1, output_dir='/scratch/users/mphagen', 
                    lambda='lambda.1se', num_k = 10, stacked = FALSE) 

args <- R.utils::commandArgs(defaults=default_args, asValues=TRUE)
subject_subset <- args$subject_subset
cog <- args$cognitive_measure
num_it <- args$num_it
lambda <- args$lambda
output_dir <- args$output_dir
num_k <- args$num_k
stacked <- args$stacked
h5disableFileLocking()


# %%
data_dir <- ('/home/users/mphagen/fyp-multimodal-prediction/data')

# %%
rasero_data_path <- file.path(data_dir, 'final_data_R1.hdf5')
temp_data <- LoadData(rasero_data_path)
predictors <- temp_data$predictors
cognition <- temp_data$outcomes
subjects <- temp_data$subjects

pred_list <- names(predictors)

# %%
unrestricted_data <- read.csv('../data/unrestricted_mphagen_1_27_2022_20_50_7.csv')
rownames(unrestricted_data) <- unrestricted_data$Subject
restricted_data <- read.csv('../data/RESTRICTED_arokem_1_31_2022_23_26_45.csv')


# %%
#reproducible things
git_hash = system("git rev-parse HEAD", intern=TRUE)

# %%
#check to make sure that my download of the hcp data == rasero's 
#stopifnot(identical(subset(unrestricted_data, Subject %in% rasero_subjects)[[cog]], cognition[[cog]]))

# %%
if (subject_subset == 'Q2') { 
    q2 <- c(subset(unrestricted_data, 
                        (Release == 'Q2' | Release == 'Q1') 
                        & ('3T_Full_MR_Compl' = TRUE))$'Subject')
    subjects <- intersect(subjects, q2) #find intersection for qc 
    pred_list = 'connectome'
} else {
    subjects <- subjects 
    pred_list <- names(predictors)} 
 
#cull data to specific subset of subjects 
restricted_data <- restricted_data %>% filter(
  Subject %in% subjects
  )

# %%
#subset cognition to subjects present 

if (length(subjects) != dim(cognition)[1]) {
cognition <- subset(cognition, rownames(cognition) %in% subjects)
for (i in pred_list) {
        predictors[[i]] <- subset(predictors[[i]], rownames(predictors[[i]]) %in% subjects)
    }
}

# %%
it_dir_name <- paste('iteration', num_it, sep='_')
result_path <- file.path(output_dir, 'fyp_results', git_hash, subject_subset, cog, it_dir_name)
dir.create(result_path,recursive = TRUE)

# %%
#setup empty dataframes 
cv_single_rsq_df <- data.frame(matrix(ncol = length(pred_list), nrow=0))
colnames(cv_single_rsq_df) <- pred_list

oos_single_rsq_df <- data.frame(matrix(ncol = length(pred_list), nrow=0))
colnames(oos_single_rsq_df) <- pred_list

oos_correlation_df <- data.frame(matrix(ncol = length(pred_list), nrow=0))
colnames(oos_single_rsq_df) <- pred_list

stacked_rsq_df <- data.frame(matrix(ncol = 1, nrow=0))
colnames(stacked_rsq_df) <- c('stacked')

full_rsq_df <- data.frame(matrix(ncol = 1, nrow=0))
colnames(full_rsq_df) <- c('full')

test_prediction_df <- data.frame(row.names=subjects) 
final_yhat_df <- data.frame(row.names=subjects) 

# %% tags=[]
set.seed(num_it)
folds <- groupKFold(restricted_data$Family_ID, k=num_k) #write test for this

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
    print('running single models')
    
    single_models <- RunSingleModels(pred_list,
                                     x_train_data, 
                                     y_train_data)
    
    cv_single_rsq_df <- rbind(cv_single_rsq_df, single_models$rsq) 

    oos_single_rsq <- list()   
    oos_correlation <- list()

    #turn into function
    for (pred in pred_list) { 
         curr_model <- single_models$models[pred]
         test_prediction_df[test_index, pred] <- predict(curr_model,
                                                         newx=x_test_data[[pred]], 
                                                         s='lambda.1se')  
         oos_single_rsq[pred] <- CalcRsq(test_prediction_df[test_index, pred],
                                         y_test_data[[cog]])
         oos_correlation[pred] <- cor(test_prediction_df[test_index, pred], 
                                      y_test_data[[cog]]) 
     } 
    
     oos_single_rsq_df <- rbind(oos_single_rsq_df, oos_single_rsq)
     oos_correlation_df <- rbind(oos_correlation_df, oos_correlation)
    
    #save lambdas 
    print('running stacked models')
     
    stacked_model = NULL

    if (stacked != FALSE) { 
         
        stacked_model <- RunStackedModel(as.matrix(single_models$predictions), 
                                     cognition[train_index, cog])
        stacked_rsq_df <- rbind(stacked_rsq_df, stacked_model$rsq)
         
        final_yhat_df[test_index,'predictions'] <- predict(stacked_model$model, 
                                                    newdata=as.matrix(test_prediction_df[test_index,])) 
         #double check why newy was in here

        write.csv(stacked_rsq_df, file = file.path(result_path, 
                                               'stacked_rsq.csv'))
        full_rsq <- CalcRsq(final_yhat_df[['predictions']], cognition[[cog]])
        
        write.csv(full_rsq, 
              file = file.path(result_path, 'out_of_sample_full_rsq.csv'))

    } 
    rds_name <- file.path(result_path, paste('fold', num_fold, '.RDS', sep=''))
    save(single_models, stacked_model, num_it, indices, file = rds_name)

}
write.csv(final_yhat_df, 
          file = file.path(result_path, 'final_prediction.csv'))

write.csv(cv_single_rsq_df, 
          file = file.path(result_path, 'in_sample_single_rsq.csv'))

write.csv(oos_single_rsq_df, 
          file = file.path(result_path, 'out_of_sample_single_rsq.csv'))

write.csv(oos_correlation_df, 
          file = file.path(result_path, 'out_of_sample_correlation.csv'))
