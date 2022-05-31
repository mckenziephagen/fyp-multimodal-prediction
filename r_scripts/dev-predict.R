# ---
# jupyter:
#   jupytext:
#     formats: ipynb,R:percent
#     text_representation:
#       extension: .R
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.13.8
#   kernelspec:
#     display_name: R [conda env:r-env]
#     language: R
#     name: conda-env-r-env-r
# ---

# %% tags=[]
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
default_args = list(subject_subset='Q2', cognitive_measure= 'CogCrystalComp_Unadj', 
                    num_it=1, output_dir='/scratch/users/mphagen', 
                    lambda='lambda.1se', num_k = 5, stacked = TRUE, 
                    pred_group = 'rasero') 

args <- R.utils::commandArgs(defaults=default_args, asValues=TRUE)
subject_subset <- args$subject_subset
cog <- args$cognitive_measure
num_it <- args$num_it
lambda <- args$lambda
output_dir <- args$output_dir
num_k <- args$num_k
stacked <- args$stacked
pred_group <- args$pred_group
h5disableFileLocking()


# %%
data_dir <- file.path( '../data')

# %%
if (pred_group == 'rasero') { 
    rasero_data_path <- file.path(data_dir, 'final_data_R1.hdf5')
    temp_data <- LoadData(rasero_data_path)
    predictors <- temp_data$predictors
    cognition <- temp_data$outcomes
    subjects <- temp_data$subjects
    
} else if (pred_group == 'finn') { 
    rasero_data_path <- file.path(data_dir, 'final_data_R1.hdf5')
    temp_data <- LoadData(rasero_data_path)

    predictors <- temp_data$predictors['connectome']
    #add in pos and neg connectomes later
    } 

pred_list <- names(predictors)

# %%
git_hash = system("git rev-parse HEAD", intern=TRUE)

# %%
git_hash

# %%
#check to make sure that my download of the hcp data == rasero's 
#stopifnot(identical(subset(unrestricted_data, Subject %in% rasero_subjects)[[cog]], cognition[[cog]]))

# %%
unrestricted_data <- read.csv('../data/unrestricted_mphagen_1_27_2022_20_50_7.csv')
rownames(unrestricted_data) <- unrestricted_data$subjects


restricted_data <- read.csv('../data/RESTRICTED_arokem_1_31_2022_23_26_45.csv')
rownames(restricted_data) <- restricted_data$subjects

# %%
#cull down subjects

if (subject_subset == 'Q2') {  #ugh fix this
    q2 <- c(subset(unrestricted_data, 
            (Release == 'Q2' | Release == 'Q1') 
            & ('3T_Full_MR_Compl' = TRUE))$'Subject')
    subjects <- intersect(subjects, q2) #find intersection for qc 
} else {
    #this is kinda dumb but I'll get confused if it's not here. 
    subjects <- subjects #defined by predictors up top 
    }
 
cognition <- subset(cognition, rownames(cognition) %in% subjects)
if (cog == 'PMAT24_A_CR') { 
    cognition <- cbind(cognition, unrestricted_data['PMAT24_A_CR'])
}

for (pred in pred_list) {
    predictors[[pred]] <- subset(predictors[[pred]], 
                        rownames(predictors[[pred]]) %in% subjects)
}

unrestricted_data <- filter(unrestricted_data, 
                    Subject %in% subjects)
restricted_data <- filter(restricted_data, 
                    Subject %in% subjects)

# %%
it_dir_name <- paste('iteration', num_it, sep='_')
k_dir_name <- paste('k', num_k, sep='_')

result_path <- file.path(output_dir, 'fyp_results', 
                         git_hash, subject_subset, 
                         num_k, cog, it_dir_name)

dir.create(result_path,recursive = TRUE)

# %%
#setup empty dataframes 
stacked_rsq_df <- data.frame(matrix(ncol = 1, nrow=0))
colnames(stacked_rsq_df) <- c('stacked')

test_single <- data.frame(row.names=subjects) 

stacked_pred_df <- data.frame(row.names=subjects) 
stacked_perf <- list()

stacked_perf_df <- data.frame(matrix(ncol = 2, nrow=0))
colnames(stacked_perf_df) <- c("rsq", "corr")


# %%
source('functions.R')

# %% tags=[]
set.seed(num_it)
folds <- groupKFold(restricted_data$Family_ID, k=num_k) #write test for this
start_time <- Sys.time()

for (num_fold in 1:length(folds)) { 
    print(num_fold)
    indices <- CreateIndices(folds, num_curr_fold=num_fold) #fix here 

    train_index <- indices$train_index
    test_index <- indices$test_index 
    
    train_subjects <- subjects[train_index] 
    test_subjects <- subjects[test_index] 

    split_y_data <- SplitYData(cog, train_index, test_index, cognition)

    y_train_data <- split_y_data$'y_train'
    y_test_data <- split_y_data$'y_test'
    
    train_single <- data.frame(y_train_data,
                                row.names=train_subjects) 
    
    colnames(train_single) <- 'cog'

    x_train_data <- list()
    x_test_data <- list()

    for (pred in pred_list) { 
        split_x_data <- SplitXData(pred, train_index, test_index)
        x_train_data[[pred]] <- as.matrix(split_x_data$'x_train')
        x_test_data[[pred]] <- as.matrix(split_x_data$'x_test')
    } 

    print('running single models')
    
    single_models <- TrainSingleModels(pred_list,
                                     x_train_data, 
                                     y_train_data)
    
    
    #turn into function - PredictSingleModels
    single_perf <- list()
    for (pred in pred_list) {  
        
        curr_model <- single_models[[pred]]

        train_single[pred] <- predict(curr_model, 
                                       newx=x_train_data[[pred]])

        test_single[test_index, pred] <- predict(curr_model,
                                        newx=x_test_data[[pred]])
        
        single_perf[[pred]] <- AssessModel(test_single[test_index, pred],
                                  y_test_data)     
        
       
    } 
     csv_name <- file.path(result_path, paste('fold', num_fold, 
                      'single_training_predictions.csv', sep=''))
     write.csv(train_single, csv_name)
    
     csv_name <- file.path(result_path, paste('fold', num_fold, 
                      'single_test_predictions.csv', sep=''))
    
     write.csv(test_single, csv_name)

    
    if (length(pred_list) > 1 & stacked == TRUE) { 
         
        print('running stacked models')

        stacked_model <- TrainStackedModel(train_single)
     
        #add dim test_single check  
        stacked_pred_df[test_index, 'predictions'] <- predict(stacked_model, 
                            newdata=test_single[test_index,])
        
        stacked_perf <- AssessModel(stacked_pred_df[test_index, 'predictions'], 
                                   y_test_data)
        
        stacked_perf_df <- rbind(stacked_perf_df, stacked_perf)

    } else {stacked_model = NULL} 
      
    rds_name <- file.path(result_path, 
                          paste('fold', num_fold, '.rds', sep=''))
    
    save(single_models, git_hash, stacked_model, num_it, test_subjects, test_index, train_index,
         train_subjects, single_perf, stacked_perf_df,
         num_fold, file = rds_name)

}
end_time <- Sys.time()
run_time <- (end_time - start_time)
print(run_time)

# %% tags=[]
#add in stacked corr 
write.csv(data.frame(stacked_perf_df), file.path(result_path, 'stacked_rsq.csv'))

write.csv(stacked_pred_df, file.path(result_path, 
                                           'stacked_prediction.csv'))
