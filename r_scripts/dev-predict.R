# ---
# jupyter:
#   jupytext:
#     formats: ipynb,Rmd,R:light
#     text_representation:
#       extension: .R
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.13.1
#   kernelspec:
#     display_name: R
#     language: R
#     name: ir
# ---

# + tags=[]
library(rhdf5, quietly=TRUE)
library(glmnet, quietly=TRUE)
library(caret, quietly=TRUE)
library(hash, quietly=TRUE)
library(dplyr, quietly=TRUE)
library(broom) 
library(assertthat)
# -

subject_subset = NULL

rasero_data_path <- file.path('..', 'data', 'final_data_R1.hdf5')
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

for (i in keys(predictors)) {
    rownames(predictors[[i]]) <- rasero_subjects
}

#to do: save into csvs maybe? 
# -

cog = 'CogTotalComp_Unadj' 

unrestricted_data <- read.csv('../data/unrestricted_mphagen_1_27_2022_20_50_7.csv')
rownames(unrestricted_data) <- unrestricted_data$Subject


#check to make sure that my download of the hcp data == rasero's 
stopifnot(identical(subset(unrestricted_data, Subject %in% rasero_subjects)[[cog]], cognition[[cog]]))

subject_subset = 'Q2'

# +
if (subject_subset == 'Q2') { 
    q2 <- c(subset(unrestricted_data, 
                        (Release == 'Q2' | Release == 'Q1') 
                        & ('3T_Full_MR_Compl' = TRUE))$'Subject')
    subjects <- intersect(subjects, q2)     
} else {
    subjects = rasero_subjects } 

 
#cull data to specific subset of subjects 
restricted_data <- restricted_data %>% filter(
  Subject %in% subjects
  )


# -

if (length(subjects) != dim(cognition)[1]) {
cognition <- subset(cognition, rownames(cognition) %in% subjects)
    
    for (i in keys(predictors)) {
        print(i)
        print(typeof(i))
        predictors[[i]] <- subset(predictors[[i]], rownames(predictors[[i]]) %in% subjects)
        }
}


CreateIndices <- function(folds, num_curr_fold) { 
    
    index <- (1: length(subjects))
    train_index <- folds[[num_curr_fold]]
    test_index <- index[!(index %in% train_index)] 
    
   return(list("train_index" = train_index, "test_index" = test_index))
}

SplitYData <- function(cog, train_index, test_index) { 
        

    y_train <- cognition[train_index,][cog]
    
    
    y_test <- cognition[test_index,][cog]

    return(list("y_train" = y_train, 
                "y_test" = y_test))
}

SplitXData <- function(pred, train_index, test_index) { 
        
    x_train <- predictors[[pred]][train_index,] 
    x_test <- predictors[[pred]][test_index,]
    
    return(list("x_train" = x_train,  
               "x_test" = x_test))
}

RunSingleChannel <- function(x_train, y_train) { 
    
    cv_fit <- cv.glmnet(x_train, y_train)  
    y_hat_train <- predict(cv_fit, newx=x_train, s='lambda.1se', type = 'link') #same training data, but we cv above
    
   return(list("yhattrain" = y_hat_train, "model" = cv_fit))
}

CalcRsq <- function(y_hat, y_train) {

    y_mean <- mean(y_train[[1]])

    sse <- sum((y_train - y_hat)^2)
    ssr <- sum((y_hat - y_mean)^2) 
    sst <- sum((y_train - y_mean)^2)  
    return(1 - (sse / sst)) 
}

#compare lm and lasso 
RunStackedModel <- function(y_hat_train, y_train) { 
   stacked_model <- cv.glmnet(y_hat_train, y_train)
   temp_predictions <- predict(stacked_model, newx=y_hat_train) #in sample cv prediction
   return(list("model" = stacked_model, "predictions" = temp_predictions))
}

folds <- groupKFold(restricted_data$Family_ID, k=5) #write test for this
fold_num = 1

# +
indices <- CreateIndices(folds, num_curr_fold=fold_num)

train_index <- indices$train_index
test_index <- indices$test_index 

split_y_data <- SplitYData(cog, train_index, test_index)

y_train_data <- split_y_data$'y_train'
y_test_data <- split_y_data$'y_test'

x_train_data <- list()
x_test_data <- list()
for (pred in keys(predictors)) { 
    
    split_x_data <- SplitXData(pred, train_index, test_index)
    x_train_data[[pred]] <- split_x_data$'x_train'
    x_test_data[[pred]] <- split_x_data$'x_test'
} 
# -

RunSingleModel <- function(predictors, subjects, x_train_data, y_train_data) {
    
    models <- list()
    train_prediction_df <- data.frame(row.names=rownames(y_train_data)   )

    for (pred in predictors) { 
        
        print(pred)

        temp_predictions <- RunSingleChannel(as.matrix(x_train_data[[pred]]), as.matrix(y_train_data))

        train_prediction_df[pred] <- temp_predictions[['yhattrain']] 

        models[pred] <-  temp_predictions['model'] 
        
        
    }
    return(list("models" = models, "predictions" = train_prediction_df))
} 

# +
start_time <- Sys.time()

single_models <- RunSingleModel(keys(predictors), subjects, x_train_data, y_train_data) 

end_time <- Sys.time()

start_time - end_time #total time

# + tags=[]
stacked_model <- RunStackedModel(as.matrix(single_models$predictions), cognition[train_index, cog])# summary(stacked_model) 

# +
test_prediction_df <- data.frame(row.names=subjects[test_index]) 
for (pred in keys(predictors)) { 
    curr_model <- single_models$models[pred]
    temp_y_predict <- predict(curr_model, newx=x_test_data[[pred]]) 
    test_prediction_df[pred] <- temp_y_predict 
    } 

final_yhat <- predict(stacked_model$model, newx=as.matrix(test_prediction_df), 
                      newy=split_y_data[['y_test']][[cog]], s='lambda.1se', type = 'link')
# -

CalcRsq(final_yhat, split_y_data[['y_test']][[cog]])
