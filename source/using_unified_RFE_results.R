## ---- imports, echo=FALSE, include=FALSE---------------------------------------------------------------------------------------------------------------------
library(smotefamily)
library(plyr)
library(dplyr)
library(ROSE)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(ModelMetrics)
library(caret)
library(themis)
library(ranger)
library(randomForest)
library(mice)
library(sos)
library(kernlab)
library(ramify)
library(Matrix)
library(MLmetrics)
library(pROC)
library(hash)
library(numbers)
library(ggplot2)
library(DALEX)


## ---- load_code_and_data, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------

source("models_config.R")
source("models_aux.R")

load(data_path)
cat(paste(preprocessing_comments, collapse="\n\n"))


## ---- split, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------------

# Initializations
final_results = list()
final_results$dt_reduced <- list()
final_results$rf_reduced <- list()
final_results$svm_reduced <- list()
final_results$adaboost_reduced <- list()
final_results$xgboost_reduced <- list()
final_results$voting_reduced <- list()
final_results$visualization_reduced <- list()

# total_rfe_variables <- c()
# total_rfe_frequencies <- c()
# 
# for (j in 1:as.integer(config_list["number_of_imputed_datasets"])) {
#   data <- final_features[[j]]
#   
#   # Split the dataset into train/test (technically the split has happened during the preprocessing, we are just grouping together)
#   train_features <- data[train_indices, ]
#   test_features <- data[-train_indices, ]
#   
#   train_labels <- new_final_label[train_indices]
#   test_labels <- new_final_label[-train_indices]
#   
#   myFuncs <- rfFuncs
#   myFuncs$summary <- many_stats_summary
#   myFuncs$selectSize <- myPickSizeTolerance
#   myFuncs$fit <- myFit
#   
#   rfe_control <- rfeControl(
#     functions = myFuncs,
#     method = "repeatedcv",
#     number = rfe_k_fold,
#     repeats = rfe_repeats,
#     verbose = FALSE,
#     returnResamp = "all"
#   )
#   rfe <- rfe(
#     train_features, train_labels,
#     sizes = rfe_sizes,
#     rfeControl = rfe_control,
#     metric = metric,
#     ntree = rfe_trees
#   )
#   
#   total_rfe_variables <- union(total_rfe_variables, rfe$optVariables)
#   for (var in rfe$optVariables) {
#     if (var %in% names(total_rfe_frequencies)) {
#       total_rfe_frequencies[var] <- total_rfe_frequencies[var] + 1
#     } else {
#       total_rfe_frequencies[var] <- 1
#     }
#   }
# }
# 
# final_results$total_rfe_variables <- total_rfe_variables
# final_results$total_rfe_frequencies <- total_rfe_frequencies

load(r"{..\results\RFE_results.RData}")
total_rfe_variables <- rfe_opt_variables

# Find the optimal model for every imputed dataset
for (j in 1:as.integer(config_list["number_of_imputed_datasets"])) {
  data <- final_features[[j]]
  
  # Split the dataset into train/test (technically the split has happened during the preprocessing, we are just grouping together)
  train_features <- data[train_indices, ]
  test_features <- data[-train_indices, ]
  
  train_labels <- new_final_label[train_indices]
  test_labels <- new_final_label[-train_indices]
  
  train_reduced <- train_features[, total_rfe_variables]
  test_reduced <- test_features[, total_rfe_variables]

  
  ## One-hot encode the rfe features (mostly numeric features are chosen so this is mostly placebo
  ## and so that it doesn't crash in cases when categorical features rarely get selected)
  dummy_one_hot <- dummyVars("~.", data = rbind(train_reduced, test_reduced))
  one_hot_train_reduced <- data.frame(predict(dummy_one_hot, newdata = train_reduced))
  one_hot_test_reduced <- data.frame(predict(dummy_one_hot, newdata = test_reduced))
  cat("Done computing one-hot features\n")
  
  
  ## ---- train_control, echo=TRUE-------------------------------------------------------------------------------------------------------------------------------
  
  # Use repeated k-fold cross validation to evaluate the models
  # and the appropriate summary function for the metric (f2/auc)
  
  ctrl <- trainControl(
    method = "repeatedcv",
    number = k_fold,
    repeats = repeats,
    summaryFunction = f2_summary,
    sampling = sampling_method,
    verboseIter = FALSE
  )
  
  
  ## ---- dt, echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------
  
  gs <- data.frame(cp = c(0.001, 0.005, 0.01, 0.05, 0.1))

  # Reduced features
  dt_reduced <- run_model(gs, train_reduced, train_labels, test_reduced, test_labels, method="rpart", ctrl)
  final_results$dt_reduced[[j]] <- dt_reduced
  
  
  ## ---- rf, echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------
  
  # Appropriate upper limit
  default <- round(sqrt(ncol(train_reduced)))
  gs <- data.frame(mtry = c(default, default + 2, default + 4, default + 6))
  
  # Reduced features
  rf_reduced <- run_model(gs, train_reduced, train_labels, test_reduced, test_labels, method="rf", ctrl)
  final_results$rf_reduced[[j]] <- rf_reduced
  
  
  ## ---- SVM, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------
  
  gs <- expand.grid(C = c(0.1, 0.5, 1, 2, 5, 10),
                    sigma = c(0.1, 0.5, 1, 2, 5)
                    # Weight = seq(0.5, 2, by=0.5)
  )
  
  # One-hot reduced features
  svm_reduced <- run_model(gs, one_hot_train_reduced, train_labels, one_hot_test_reduced, test_labels, method="svmRadial", ctrl, preprocess=TRUE)
  final_results$svm_reduced[[j]] <- svm_reduced
  
  
  ## ---- adaboost, echo=TRUE------------------------------------------------------------------------------------------------------------------------------------
  
  gs <- expand.grid(mfinal = c(25, 50, 100),
                    maxdepth = c(1, 3, 5),
                    coeflearn = c("Breiman"))
  
  # All features reduced
  adaboost_reduced <- run_model(gs, train_reduced, train_labels, test_reduced, test_labels, method="AdaBoost.M1", ctrl)
  final_results$adaboost_reduced[[j]] <- adaboost_reduced
  
  
  ## ---- gradient_boost, echo=TRUE------------------------------------------------------------------------------------------------------------------------------
  
  gs <- expand.grid(nrounds = c(25, 50, 100),
                    eta = c(0.05, 0.1, 0.3),
                    gamma = 0,
                    max_depth = c(4, 6, 8),
                    min_child_weight = 1,
                    subsample = c(0.8, 1),
                    colsample_bytree = c(0.8, 1)
  )
  
  # One_hot reduced features
  xgboost_reduced <- run_model(gs, one_hot_train_reduced, train_labels, one_hot_test_reduced, test_labels, method="xgbTree", ctrl)
  final_results$xgboost_reduced[[j]] <- xgboost_reduced
  
  
  ## ---- voting_classifier, echo=TRUE---------------------------------------------------------------------------------------------------------------------------
  
  ## Using reduced features
  voting_reduced <- voting_classifier(dt_reduced, rf_reduced, svm_reduced, adaboost_reduced, xgboost_reduced)
  final_results$voting_reduced[[j]] <- voting_reduced
  
  
  ## ---- grouped_results----------------------------------------------------------------------------------------------------------------------------------------
  
  # Using reduced features
  auc_scores_reduced <- c(dt_reduced$auc, rf_reduced$auc, svm_reduced$auc, adaboost_reduced$auc, xgboost_reduced$auc, voting_reduced$auc)
  f2_scores_reduced <- c(dt_reduced$f2, rf_reduced$f2, svm_reduced$f2, adaboost_reduced$f2, xgboost_reduced$f2, voting_reduced$f2)
  visualization_results_reduced <- visualize(auc_scores_reduced, f2_scores_reduced, 'Performance using reduced features')

  final_results$visualization_reduced[[j]] <- visualization_results_reduced
  
  cat(paste(j, 'imputed dataset(s) finished\n', sep=" "))
}

processed_results <- process_final_results_reduced_only(final_results)

## Save the results
save(final_results, processed_results, training_config_list, data_file, file=save_results_path)
