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
    
    y_train <- cognition[train_index,][cog]
    y_test <- cognition[test_index,][cog]

    return(list("y_train" = y_train, 
                "y_test" = y_test))
}


RunSingleModels <- function(predictors, x_train_data, y_train_data) {
    
    models <- list()
    rsq_list <- list()
    train_prediction_df <- data.frame(row.names=rownames(y_train_data))
    
    for (pred in predictors) { 
        temp_predictions <- RunSingleChannel(as.matrix(x_train_data[[pred]]), as.matrix(y_train_data))
        train_prediction_df[pred] <- temp_predictions[['yhattrain']] 
        models[pred] <-  temp_predictions['model']   
        rsq_list[pred] <- temp_predictions[['rsq']]
    }
    
    return(list("models" = models, "predictions" = train_prediction_df, 
                "train" = rownames(x_train_data[[pred]]), "rsq" = rsq_list))
} 

RunStackedModel <- function(y_hat_train, y_train) {  
   stacked_model <- lm(y_train ~ y_hat_train)
   predictions <- predict(stacked_model, newdata=data.frame(y_hat_train)) 
   rsq <- CalcRsq(predictions, y_train) 
    
   return(list("model" = stacked_model, "predictions" = predictions, "rsq" = rsq))
}

CalcRsq <- function(y_hat, y_actual) { #write test 

    y_mean <- mean(y_actual)
    sse <- sum((y_actual - y_hat)^2)
    ssr <- sum((y_hat - y_mean)^2) 
    sst <- sum((y_actual - y_mean)^2)
    
    return(1 - (sse / sst)) 
}

RunSingleChannel <- function(x_train, y_train) { 
    
    cv_fit <- cv.glmnet(x_train, y_train)  
    y_hat_train <- predict(cv_fit, newx=x_train, s='lambda.1se', type = 'link')
    rsq <- CalcRsq(y_hat_train, y_train)
    
   return(list("yhattrain" = y_hat_train, "model" = cv_fit, "rsq" = rsq))
}

SplitXData <- function(pred, train_index, test_index) { 
        
    x_train <- predictors[[pred]][train_index,] 
    x_test <- predictors[[pred]][test_index,]
    
    return(list("x_train" = x_train,  
               "x_test" = x_test))
}