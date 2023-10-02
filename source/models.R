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
final_results$dt <- list()
final_results$dt_reduced <- list()
final_results$rf <- list()
final_results$rf_reduced <- list()
final_results$svm <- list()
final_results$svm_reduced <- list()
final_results$adaboost <- list()
final_results$adaboost_reduced <- list()
final_results$xgboost <- list()
final_results$xgboost_reduced <- list()
final_results$voting <- list()
final_results$voting_reduced <- list()
final_results$rfe <- list()
final_results$visualization <- list()
final_results$visualization_reduced <- list()

# Find the optimal model for every imputed dataset
for (j in 1:as.integer(config_list["number_of_imputed_datasets"])) {
# for (j in 1:1) {
  data <- final_features[[j]]
  
  # Split the dataset into train/test (technically the split has happened during the preprocessing, we are just grouping together)
  train_features <- data[train_indices, ]
  test_features <- data[-train_indices, ]
  
  train_labels <- new_final_label[train_indices]
  test_labels <- new_final_label[-train_indices]
  
  # One-hot-encode our features for the models that can't handle factors
  dummy <- dummyVars("~.", data = data)
  one_hot_train_features <- data.frame(predict(dummy, newdata = train_features))
  one_hot_test_features <- data.frame(predict(dummy, newdata = test_features))
  
  
  
  ## ---- rfe, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------
  
  start_rfe <- Sys.time()
  
  myFuncs <- rfFuncs
  myFuncs$summary <- many_stats_summary
  myFuncs$selectSize <- myPickSizeTolerance
  myFuncs$fit <- myFit
  
  rfe_control <- rfeControl(
    functions = myFuncs,
    method = "repeatedcv",
    number = rfe_k_fold,
    repeats = rfe_repeats,
    verbose = FALSE,
    returnResamp = "all"
  )
  rfe <- rfe(
    train_features, train_labels,
    sizes = rfe_sizes,
    rfeControl = rfe_control,
    metric = metric,
    ntree = rfe_trees
  )
  
  train_reduced <- train_features[, rfe$optVariables]
  test_reduced <- test_features[, rfe$optVariables]
  final_results$rfe[[j]] <- rfe
  
  end_rfe <- Sys.time()
  # cat(paste(sprintf("RFE decided that the optimal number of features is %i:", rfe$optsize), paste(rfe$optVariables, collapse=", "), sep="\n"))
  cat(sprintf("RFE took %.2f seconds\n", end_rfe-start_rfe))
  
  
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

  start_dt <- Sys.time()

  gs <- data.frame(cp = c(0.001, 0.005, 0.01, 0.05, 0.1))

  # All features
  dt <- run_model(gs, train_features, train_labels, test_features, test_labels, method="rpart", ctrl)
  final_results$dt[[j]] <- dt

  # Reduced features
  dt_reduced <- run_model(gs, train_reduced, train_labels, test_reduced, test_labels, method="rpart", ctrl)
  final_results$dt_reduced[[j]] <- dt_reduced

  end_dt <- Sys.time()
  cat(sprintf("Decision Tree took %.2f seconds\n", end_dt-start_dt))


  ## ---- rf, echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------

  start_rf <- Sys.time()

  default <- round(sqrt(ncol(train_features)))
  gs <- data.frame(mtry = c(default - 10, default - 5, default, default + 5, default + 10, default + 20))

  # All features
  rf <- run_model(gs, train_features, train_labels, test_features, test_labels, method="rf", ctrl)

  # Appropriate upper limit
  default <- round(sqrt(ncol(train_reduced)))
  gs <- data.frame(mtry = c(default, default + 2, default + 4, default + 6))
  final_results$rf[[j]] <- rf

  # Reduced features
  rf_reduced <- run_model(gs, train_reduced, train_labels, test_reduced, test_labels, method="rf", ctrl)
  final_results$rf_reduced[[j]] <- rf_reduced

  end_rf <- Sys.time()
  cat(sprintf("Random Forest took %.2f seconds\n", end_rf-start_rf))


  ## ---- SVM, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------

  start_svm <- Sys.time()

  # One-hot encoded features
  gs <- expand.grid(C = c(0.1, 0.5, 1, 2, 5, 10),
                    sigma = c(0.1, 0.5, 1, 2, 5)
                    # Weight = seq(0.5, 2, by=0.5)
                    )
  svm <- run_model(gs, one_hot_train_features, train_labels, one_hot_test_features, test_labels, method="svmRadial", ctrl, preprocess=TRUE)
  final_results$svm[[j]] <- svm

  # One-hot reduced features
  gs <- expand.grid(C = c(1),
                    sigma = c(1 / ncol(one_hot_train_reduced))
                    # Weight = seq(0.5, 2, by=0.5)
                    )
  svm_reduced <- run_model(gs, one_hot_train_reduced, train_labels, one_hot_test_reduced, test_labels, method="svmRadial", ctrl, preprocess=TRUE)
  final_results$svm_reduced[[j]] <- svm_reduced

  end_svm <- Sys.time()
  cat(sprintf("SVM took %.2f seconds\n", end_svm-start_svm))


  ## ---- adaboost, echo=TRUE------------------------------------------------------------------------------------------------------------------------------------

  start_adaboost <- Sys.time()

  gs <- expand.grid(mfinal = c(25, 50, 100),
                    maxdepth = c(1, 3, 5),
                    coeflearn = c("Breiman"))

  # All features
  adaboost <- run_model(gs, train_features, train_labels, test_features, test_labels, method="AdaBoost.M1", ctrl)
  final_results$adaboost[[j]] <- adaboost

  # All features reduced
  adaboost_reduced <- run_model(gs, train_reduced, train_labels, test_reduced, test_labels, method="AdaBoost.M1", ctrl)
  final_results$adaboost_reduced[[j]] <- adaboost_reduced

  end_adaboost <- Sys.time()
  cat(sprintf("Adaboost took %.2f seconds\n", end_adaboost-start_adaboost))
  
  
  ## ---- gradient_boost, echo=TRUE------------------------------------------------------------------------------------------------------------------------------
  
  start_xgboost <- Sys.time()
  
  gs <- expand.grid(nrounds = c(25, 50, 100),
                    eta = c(0.05, 0.1, 0.3),
                    gamma = 0,
                    max_depth = c(4, 6, 8),
                    min_child_weight = 1,
                    subsample = c(0.8, 1),
                    colsample_bytree = c(0.8, 1)
                    )
  
  # One_hot features
  xgboost <- run_model(gs, one_hot_train_features, train_labels, one_hot_test_features, test_labels, method="xgbTree", ctrl)
  final_results$xgboost[[j]] <- xgboost
  
  # One_hot reduced features
  xgboost_reduced <- run_model(gs, one_hot_train_reduced, train_labels, one_hot_test_reduced, test_labels, method="xgbTree", ctrl)
  final_results$xgboost_reduced[[j]] <- xgboost_reduced
  
  end_xgboost <- Sys.time()
  cat(sprintf("XGBoost took %.2f seconds\n", end_xgboost-start_xgboost)) 
  
  
  ## ---- voting_classifier, echo=TRUE---------------------------------------------------------------------------------------------------------------------------
  
  start_voting <- Sys.time()
  
  ## Using all features
  voting <- voting_classifier(dt, rf, svm, adaboost, xgboost)
  final_results$voting[[j]] <- voting
  
  ## Using reduced features
  voting_reduced <- voting_classifier(dt_reduced, rf_reduced, svm_reduced, adaboost_reduced, xgboost_reduced)
  final_results$voting_reduced[[j]] <- voting_reduced
  
  end_voting <- Sys.time()
  cat(sprintf("Voting took %.2f seconds\n", end_voting-start_voting)) 
  
  
  ## ---- grouped_results----------------------------------------------------------------------------------------------------------------------------------------
  
  # Using all the features
  auc_scores <- c(dt$auc, rf$auc, svm$auc, adaboost$auc, xgboost$auc, voting$auc)
  f2_scores <- c(dt$f2, rf$f2, svm$f2, adaboost$f2, xgboost$f2, voting$f2)
  visualization_results <- visualize(auc_scores, f2_scores, 'Performance using all features')
  
  # Using reduced features
  auc_scores_reduced <- c(dt_reduced$auc, rf_reduced$auc, svm_reduced$auc, adaboost_reduced$auc, xgboost_reduced$auc, voting_reduced$auc)
  f2_scores_reduced <- c(dt_reduced$f2, rf_reduced$f2, svm_reduced$f2, adaboost_reduced$f2, xgboost_reduced$f2, voting_reduced$f2)
  visualization_results_reduced <- visualize(auc_scores_reduced, f2_scores_reduced, 'Performance using reduced features')
  
  final_results$visualization[[j]] <- visualization_results
  final_results$visualization_reduced[[j]] <- visualization_results_reduced
  
  cat(paste(j, 'imputed dataset(s) finished', sep=" "))
  cat("\n")
  
}

processed_results <- process_final_results(final_results)
# processed_results$visualization$plot
# processed_results$visualization_reduced$plot
# processed_results$f2_scores
# processed_results$grouped_results$svm_reduced$f2

## Save the results
save(final_results, processed_results, training_config_list, data_file, file=save_results_path)
