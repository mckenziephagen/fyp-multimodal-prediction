LoadData <- function(data_path) { 
    data = H5Fopen(rasero_data_path)
    data_names <- h5ls(data)['name']
    subjects <- data$subjects
    
    predictors <- list()

    outcomes <- data.frame(t(data$YY_domain_cognition) )
    
    predictors$connectome <- t(data$connectome_features) 
    predictors$volume <- t(data$sub_vols_features)
    predictors$local_connectome <- t(data$loc_conn_features)
    predictors$surface <- t(data$surface_features)
    predictors$thickness <- t(data$thickness_features)

    H5close()
    
    rownames(outcomes) <- subjects

    colnames(outcomes) <- c('CogTotalComp_Unadj', 
                        'CogFluidComp_Unadj', 
                        'CogCrystalComp_Unadj', 
                        'SCPT_SEN', 
                        'DDisc_AUC_200', 
                        'IWRD_TOT', 
                        'VSPLOT_TC')

    pred_list <- names(predictors)
    for (i in names(predictors)) {
        rownames(predictors[[i]]) <- subjects
}
    
    return(list("predictors" = predictors, "outcomes" = outcomes, "subjects" = subjects))
} 

CreateIndices <- function(folds, num_curr_fold) { #passing folds and num_curr_fold is redundant but oh well. 
    
    index <- (1: length(subjects))
    train_index <- folds[[num_curr_fold]]
    test_index <- index[!(index %in% train_index)] 
    
   return(list("train_index" = train_index, "test_index" = test_index))
}


SplitYData <- function(cog, train_index, test_index, cognition) { 
    
    y_train <- cognition[train_index,cog]
    y_test <- cognition[test_index,cog]

    return(list("y_train" = y_train, 
                "y_test" = y_test))
}

AssessModel <- function(y_test_predictions, y_test) { 
    mse <- as.numeric(assess.glmnet(y_test_predictions, 
              newy=y_test)$mse)
    rsq <- as.numeric(1 - (mse / var(y_test)))
    corr <- cor(y_test_predictions, y_test) 
    return(list('rsq'=rsq, 'corr' = corr))
    } 

# PredictSingleModels <- function() { 
    
#     } 


TrainSingleModels <- function(predictors, x_train_data, y_train_data) {
    
    single_models <- list()
    y_train_mat <- as.matrix(y_train_data)

    for (pred in predictors) { 
        
        single_x_train <- as.matrix(x_train_data[[pred]]) #glmnet needs matrix
        single_models[[pred]] <- cv.glmnet(single_x_train, 
                                           y_train_mat, family = "gaussian")
    }
    
    return(single_models)
} 

TrainStackedModel <- function(train_predictions) { 
   stacked_model <- lm(cog ~ ., data = train_predictions)
   return(stacked_model)
}

CalcRsq <- function(y_hat, y_actual) { #write test 

    y_mean <- mean(y_actual)
    sse <- sum((y_actual - y_hat)^2)
    ssr <- sum((y_hat - y_mean)^2) 
    sst <- sum((y_actual - y_mean)^2)
    
    return(1 - (sse / sst)) 
}


SplitXData <- function(pred, train_index, test_index) { 
        
    x_train <- predictors[[pred]][train_index,] 
    x_test <- predictors[[pred]][test_index,]
    
    return(list("x_train" = x_train,  
               "x_test" = x_test))
}