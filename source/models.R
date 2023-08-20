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


## ---- load_data, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------

# Absolute path
my_path <- r"{D:\Κωνσταντίνος Data\Σχολής\Διπλωματική Εργασία\Main\source}"
setwd(my_path)

# Relative path
clean_data_folder <- r"{..\dataset\preprocessed_results}"
data_file <- r"{M6_4.RData}"
data_path <- paste(clean_data_folder, data_file, sep=r"{\}")

load(data_path)

# Print the preprocessing comments
cat(paste(preprocessing_comments, collapse="\n\n"))



## ---- load_code, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------

source("models_config.R")
source("models_aux.R")



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
final_results$visualization <- list()
final_results$visualization_reduced <- list()

# Find the optimal model for every imputed dataset
for (j in 1:as.integer(config_list["number_of_imputed_datasets"])) {
  data <- final_features[[j]]
  
  # Split the dataset into train/test (technically the split has happened during the preprocessing,
  # we are just grouping together)
  train_features <- new_data[train_indices, ]
  test_features <- new_data[-train_indices, ]
  
  train_labels <- new_final_label[train_indices]
  test_labels <- new_final_label[-train_indices]
  
  # One-hot-encode our features for the models that can't handle factors
  dummy <- dummyVars("~.", data = new_data)
  one_hot_train_features <- data.frame(predict(dummy, newdata = train_features))
  one_hot_test_features <- data.frame(predict(dummy, newdata = test_features))
  
  
  
  ## ---- rfe, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------
  
  myFuncs <- rfFuncs
  myFuncs$summary <- many_stats_summary
  myFuncs$selectSize <- myPickSizeTolerance
  if (rfe_sampling_method == "down") {
    myFuncs$fit <- rf_fit_down
  }
  rfe_control <- rfeControl(
    functions = myFuncs,
    method = "repeatedcv",
    number = rfe_k_fold,
    repeats = rfe_repeats,
    verbose = FALSE,
    returnResamp = "all"
  )
  
  
  ## Rfe on initial features
  
  rfe_results <- list()
  features_dict <- hash()
  for (i in 1:rfe_iterations) {
    rfe <- rfe(train_features, train_labels,
                           sizes = rfe_sizes,
                           rfeControl = rfe_control,
                           metric = metric,
                           ntree = rfe_trees)
    rfe_results[[i]] <- rfe
    
    # Loop through the features chosen
    for (feature in rfe$optVariables) {
      if (has.key(feature, features_dict)) {
        # Feature has been seen at least once
        features_dict[[feature]] <- features_dict[[feature]] + 1
      } else {
        # Newly considered feature
        features_dict[[feature]] <- 1
      }
    }
  }
  
  chosen_features <- c()
  for (feature in keys(features_dict)) {
    if (features_dict[[feature]] >= rfe_cutoff) {
      chosen_features <- append(chosen_features, feature)
    }
  }
  train_reduced <- train_features[, chosen_features]
  test_reduced <- test_features[, chosen_features]
  
  
  ## Rfe on one_hot_encoded features
  
  rfe_one_hot_results <- list()
  one_hot_features_dict <- hash()
  for (i in 1:rfe_iterations) {
    rfe <- rfe(one_hot_train_features, train_labels,
                           sizes = rfe_sizes,
                           rfeControl = rfe_control,
                           metric = metric,
                           ntree = rfe_trees)
    rfe_one_hot_results[[i]] <- rfe
    
    # Loop through the features chosen
    for (feature in rfe$optVariables) {
      if (has.key(feature, one_hot_features_dict)) {
        # Feature has been seen at least once
        one_hot_features_dict[[feature]] <- one_hot_features_dict[[feature]] + 1
      } else {
        # Newly considered feature
        one_hot_features_dict[[feature]] <- 1
      }
    }
  }
  
  one_hot_chosen_features <- c()
  for (feature in keys(one_hot_features_dict)) {
    if (one_hot_features_dict[[feature]] >= rfe_cutoff) {
      one_hot_chosen_features <- append(one_hot_chosen_features, feature)
    }
  }
  one_hot_train_reduced <- one_hot_train_features[, one_hot_chosen_features]
  one_hot_test_reduced <- one_hot_test_features[, one_hot_chosen_features]
  
  
  
  ## ---- rfe_results, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
  
  features_list <- list()
  for (key in keys(features_dict)) {
    features_list[[key]] <- features_dict[[key]]
  }
  sorted_indices <- c(order(unlist(features_list), decreasing=TRUE))
  sorted_features <- features_list[sorted_indices]
  sorted_chosen <- sorted_features[sorted_features >= rfe_cutoff]
  sorted_left <- sorted_features[sorted_features < rfe_cutoff & sorted_features > 2]
  
  cat(paste(names(sorted_chosen), sorted_chosen, sep = ": ", collapse = "\n"))
  cat('\n')
  cat('------------------------------------------------------------------------------')
  cat('\n')
  cat(paste(names(sorted_left), sorted_left, sep = ": ", collapse = "\n"))
  
  cat('\n\n\n')
  cat('------------------------------------------------------------------------------')
  cat('\n\n\n')
  
  one_hot_features_list <- list()
  for (key in keys(one_hot_features_dict)) {
    one_hot_features_list[[key]] <- one_hot_features_dict[[key]] 
  }
  one_hot_sorted_indices <- c(order(unlist(one_hot_features_list), decreasing=TRUE))
  one_hot_sorted_features <- one_hot_features_list[one_hot_sorted_indices]
  one_hot_sorted_chosen <- one_hot_sorted_features[one_hot_sorted_features >= rfe_cutoff]
  one_hot_sorted_left <- one_hot_sorted_features[one_hot_sorted_features < rfe_cutoff & one_hot_sorted_features > 2]
  
  cat(paste(names(one_hot_sorted_chosen), one_hot_sorted_chosen, sep = ": ", collapse = "\n"))
  cat('\n')
  cat('------------------------------------------------------------------------------')
  cat('\n')
  cat(paste(names(one_hot_sorted_left), one_hot_sorted_left, sep = ": ", collapse = "\n"))
  
  
  
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
  
  # All features
  set.seed(1)
  dt <- run_model(gs, train_features, train_labels, test_features, test_labels, method="rpart", ctrl)
  final_results$dt[[j]] <- dt
  
  # Reduced features
  set.seed(1)
  dt_reduced <- run_model(gs, train_reduced, train_labels, test_reduced, test_labels, method="rpart", ctrl)
  final_results$dt_reduced[[j]] <- dt_reduced

  
  
  
  ## ---- rf, echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------
  
  default <- round(sqrt(ncol(train_features)))
  gs <- data.frame(mtry = c(default - 10, default - 5, default, default + 5, default + 10, default + 20))
  
  # All features
  set.seed(1)
  rf <- run_model(gs, train_features, train_labels, test_features, test_labels, method="rf", ctrl)
  
  # Appropriate upper limit
  default <- round(sqrt(ncol(train_reduced)))
  gs <- data.frame(mtry = c(default, default + 2, default + 4, default + 6))
  final_results$rf[[j]] <- rf
  
  # Reduced features
  set.seed(1)
  rf_reduced <- run_model(gs, train_reduced, train_labels, test_reduced, test_labels, method="rf", ctrl)
  final_results$rf_reduced[[j]] <- rf_reduced
  
    
  
  ## ---- SVM, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------
  
  # One-hot encoded features
  gs <- expand.grid(C = c(1),
                    sigma = c(1 / ncol(one_hot_train_features)),
                    Weight = seq(0.5, 2, by=0.5))
  set.seed(1)
  svm <- run_model(gs, one_hot_train_features, train_labels, one_hot_test_features, test_labels, method="svmRadialWeights", ctrl)
  final_results$svm[[j]] <- svm
  
  # One-hot reduced features
  gs <- expand.grid(C = c(1),
                    sigma = c(1 / ncol(one_hot_train_reduced)),
                    Weight = seq(0.5, 2, by=0.5))
  set.seed(1)
  svm_reduced <- run_model(gs, one_hot_train_reduced, train_labels, one_hot_test_reduced, test_labels, method="svmRadialWeights", ctrl)
  final_results$svm_reduced[[j]] <- svm_reduced
  
  
  
  ## ---- adaboost, echo=TRUE------------------------------------------------------------------------------------------------------------------------------------
  
  gs <- expand.grid(mfinal = c(10, 20, 50, 100, 150),
                    maxdepth = c(1, 2),
                    coeflearn = c("Breiman"))
  
  # All features
  set.seed(1)
  adaboost <- run_model(gs, train_features, train_labels, test_features, test_labels, method="AdaBoost.M1", ctrl)
  final_results$adaboost[[j]] <- adaboost
  
  # All features reduced
  set.seed(1)
  adaboost_reduced <- run_model(gs, train_reduced, train_labels, test_reduced, test_labels, method="AdaBoost.M1", ctrl)
  final_results$adaboost_reduced[[j]] <- adaboost_reduced
  
  
  
  ## ---- gradient_boost, echo=TRUE------------------------------------------------------------------------------------------------------------------------------
  
  gs <- expand.grid(nrounds = c(10, 20, 50, 100),
                    max_depth = 6,
                    eta = 0.3,
                    gamma = 0,
                    colsample_bytree = 1,
                    min_child_weight = 1,
                    subsample = 1
                    )
  
  # One_hot features
  set.seed(1)
  xgboost <- run_model(gs, one_hot_train_features, train_labels, one_hot_test_features, test_labels, method="xgbTree", ctrl)
  final_results$xgboost[[j]] <- xgboost
  
  # One_hot reduced features
  set.seed(1)
  xgboost_reduced <- run_model(gs, one_hot_train_reduced, train_labels, one_hot_test_reduced, test_labels, method="xgbTree", ctrl)
  final_results$xgboost_reduced[[j]] <- xgboost_reduced
  
  
  
  ## ---- voting_classifier, echo=TRUE---------------------------------------------------------------------------------------------------------------------------
  
  ## Using all features
  voting <- voting_classifier(dt, rf, svm, adaboost, xgboost)
  final_results$voting[[j]] <- voting
  
  ## Using reduced features
  voting_reduced <- voting_classifier(dt_reduced, rf_reduced, svm_reduced, adaboost_reduced, xgboost_reduced)
  final_results$voting_reduced[[j]] <- voting_reduced
  
  
  
  ## ---- grouped_results----------------------------------------------------------------------------------------------------------------------------------------
  
  # Using all the features
  auc_scores <- c(dt$auc, rf$auc, svm$auc, adaboost$auc, xgboost$auc, voting$auc)
  f2_scores <- c(dt$f2, rf$f2, svm$f2, adaboost$f2, xgboost$f2, voting$f2)
  visualization_results <- visualize(auc_scores, f2_scores, 'Performance using all features')
  
  # Using reduced features
  auc_scores_reduced <- c(dt_reduced$auc, rf_reduced$auc, svm_reduced$auc, adaboost_reduced$auc, xgboost_reduced$auc, voting_reduced$auc)
  f2_scores_reduced <- c(dt_reduced$f2, rf_reduced$f2, svm_reduced$f2, adaboost_reduced$f2, xgboost_reduced$f2, voting_reduced$f2)
  visualization_results_reduced <- visualize(auc_scores_reduced, f2_scores_reduced, 'Performance using reduced features')
  
  final_results$visualization[[j]] <- visualization_results$plot
  final_results$visualization_reduced[[j]] <- visualization_results_reduced$plot
  
  cat(paste(j, 'imputed dataset finished', sep=" "))
  cat("\n")
  
  ## ---- visualization------------------------------------------------------------------------------------------------------------------------------------------
  
  # results_plot
  # results_reduced_plot

}

setwd("D:/Κωνσταντίνος Data/Σχολής/Διπλωματική Εργασία/Main/results")
save(final_results, training_config_list, data_file, file=save_results)

classifiers <- c(
  'dt', 'rf', 'svm', 'adaboost', 'xgboost', 'voting',
  'dt_reduced', 'rf_reduced', 'svm_reduced', 'adaboost_reduced', 'xgboost_reduced', 'voting_reduced'
)
metrics <- c('auc', 'f2')

grouped_results <- list()
for (clf in classifiers) {
  grouped_results[[clf]] <- list()
  
  for (metric in metrics) {
    grouped_results[[clf]][[metric]] <- c()
    
    for (k in 1:2) {
      grouped_results[[clf]][[metric]] <- append(grouped_results[[clf]][[metric]], final_results[[clf]][[k]][[metric]])
    }
  }
}

aggregate_results <- list()

for (metric in metrics) {
  aggregate_results[[metric]] <- c()
  
  for (clf in classifiers) {
    aggregate_results[[metric]] <- append(aggregate_results[[metric]], sum(grouped_results[[clf]][[metric]]) / length(grouped_results[[clf]][[metric]]))
  }
}

auc_scores <- aggregate_results$auc[1:6]
f2_scores <- aggregate_results$f2[1:6]
aggregate_visualization_results <- visualize(auc_scores, f2_scores, 'Performance using all features')
aggregate_visualization_results$plot

auc_scores_reduced <- aggregate_results$auc[7:12]
f2_scores_reduced <- aggregate_results$f2[7:12]
aggregate_visualization_results_reduced <- visualize(auc_scores_reduced, f2_scores_reduced, 'Performance using reduced features')
aggregate_visualization_results_reduced$plot

