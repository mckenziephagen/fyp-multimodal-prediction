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

# + tags=[]
library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)
library(psych)
library(rhdf5)
# -

source('functions.R')

result_dir='/scratch/users/mphagen/fyp_results'
cog <- 'CogCrystalComp_Unadj' 
git_hash <- '378919164eef649225006a8babc9c26fe515dab2' 
k <- 5
subject_subset <- 'full' 

# +
data_dir <- file.path( '../data')
rasero_data_path <- file.path(data_dir, 'final_data_R1.hdf5')

temp_data <- LoadData(rasero_data_path)
cognition <- temp_data$outcomes
# -

plot(stacked_rsq_df[-1])

stacked_df

# +
#for (cog in cognitive_measures)  
stacked_acc <- Sys.glob(file.path(result_dir,git_hash, 'full', k,
                       cog, 'iteration_*',  'stacked_rsq.csv'))

stacked_rsq_df <- data.frame()
for (csv in stacked_acc) { 
    temp_df <- read.csv(csv)
    stacked_rsq_df <-rbind(stacked_rsq_df, temp_df)
}

plot_df <- reshape2::melt(stacked_rsq_df[-1])  
psych::describe(stacked_rsq_df[-1])
ggplot(plot_df, mapping = aes(x= ordered(variable),y = value )) + 
 geom_boxplot() 
# -


plot(x=cognition[[cog]], y=stacked_predictions[['predictions']], 
    xlim=c(80, 150), ylim=c(80, 150), ylab='Predicted', xlab='Actual', 
    )
abline(a=0, b=1)

full_df<-read.csv(Sys.glob(file.path(result_dir, git_hash, subject_subset, k,
                       cog, 'iteration_1', 'fold5*single_test_predictions.csv')))

rownames(full_df) <- full_df[['X']]

full_df <- cbind(full_df, cognition['CogTotalComp_Unadj'])

head(full_df)

cor(full_df[-1])

head(full_df) 

#do cv here 
model <- lm(CogTotalComp_Unadj ~ ., data=full_df)

pred_df <- data.frame(predict(model, newx=full_df[['CogTotalComp_Unadj']]) ) 

plot(full_df[['CogTotalComp_Unadj']], pred_df[['predict.model..newx...full_df...CogTotalComp_Unadj....']],
    
     xlim=c(80, 150), ylim=c(80, 150), ylab='Predicted', xlab='Actual', )
abline(a=0, b=1)

library(glmnet)

AssessModel(pred_df, full_df[['CogTotalComp_Unadj']]) 

psych::describe(stacked_rsq_df)
ggplot(data = plot_df, mapping = aes(x= ordered(variable,
                                    levels = c("connectome", "surface", 
                                                "thickness", "volume", 
                                                "local_connectome", "stacked")), 
                                     y = value )) +
    geom_boxplot() + 
    geom_jitter() 
