#!/usr/bin/env Rscript

####  Installations and imports ####

# install.packages("DMwR",
#                  repos = "http://cran.us.r-project.org")

# install.packages("smotefamily")
# install.packages("dplyr")
# install.packages("ROSE")
# install.packages("caret")
# install.packages("themis")
# install.packages("randomForest")
# install.packages("sos")
# install.packages("e1071")
# install.packages("kernlab")

library(smotefamily)
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
# library(corrplot)

####  Initializing and loading the preprocessed data  ####

# Absolute path
my_path <- r"{D:\Κωνσταντίνος Data\Σχολής\Διπλωματική Εργασία\Main\source}"
setwd(my_path)
# Relative path
clean_data_folder <- r"{..\dataset\preprocessed_results}"
data_file <- r"{\M6_1.RData}"
# label_name <- "M6_ptsd"
data_path <- paste(clean_data_folder, data_file, sep="")

# Load the clean dataset which includes
# 1) final_train_features:    list of m datasets created during preprocessing (containing only the features)
# 2) train_labels:            the labels of the train datasets (same for all of them)
# 3) test_features:           dataframe with the features to be used for testing
# 4) test_labels:             the labels to be used for evaluating the models
# 5) preprocessing_comments:  detailed comments about the preprocessing procedure
#                             Use function print_comments to pretty print them
# 6) config_list:             named list containing the parameters used to preprocess the data-set
load(data_path)

source("models_config.R")
source("models_aux.R")

# Print the preprocessing comments
cat(paste(preprocessing_comments, collapse="\n\n"))


####  Find the optimal model for every imputed dataset  ####

# for (i in 1:as.integer(config_list["number_of_imputed_datasets"]) {

  data <- final_features[[1]]

  # Split the dataset into train/test (technically the split has happened during the preprocessing,
  # we are just grouping together)
  train_features <- data[train_indices, ]
  test_features <- data[-train_indices, ]
  
  train_labels <- new_final_label[train_indices]
  test_labels <- new_final_label[-train_indices]
  
  # train_labels <- data.frame(train_labels)
  # names(train_labels) <- label_name
  
  # Use recursive feature elimination to reduce the number of our features even more
  myFuncs <- rfFuncs
  myFuncs$summary <- f2_summary
  rfe_control <- rfeControl(functions = myFuncs,
                            method = "repeatedcv",
                            repeats = 5,
                            verbose = FALSE)
  
  rfe_profile <- rfe(train_features, train_labels, 
                     sizes = c(seq(10, 50, 10), 100, 150, ncol(data)),
                     rfeControl = rfe_control,
                     metric = "f2")
  
  train_reduced <- train_features[, rfe_profile$optVariables]
  test_reduced <- test_features[, rfe_profile$optVariables]
  
  ####  Models  ####
  
  # Run the wanted model using all the combinations of the hyperparameters in the
  # respective grid, find the optimal model and evaluate it on the test set.
  # We try both the full-featured dataset and the reduced one

  ## Decision tree

  gs <- data.frame(cp = c(0.001, 0.005, 0.01, 0.05, 0.1))
  
  # Use k-fold cross validation to evaluate the models
  # and the appropriate summary function for the metric (f2)
  ctrl <- trainControl(method = "cv", number = k_fold,
                       summaryFunction = f2_summary,
                       # summaryFunction = defaultSummary,
                       # sampling = sampling_method,
                       sampling="down"
                       )
  
  # All features
  set.seed(1)
  dt_results <- run_model(gs, train_features, train_labels, test_features, test_labels, method="rpart", ctrl)
  dt_results$conf_matrix
  dt_results$f2
  
  # Reduced features
  set.seed(1)
  dt_reduced <- run_model(gs, train_reduced, train_labels, test_reduced, test_labels, method="rpart", ctrl)
  dt_reduced$conf_matrix
  dt_reduced$f2
  
  
  ## Random forest
  
  default <- round(sqrt(ncol(train_features)))
  gs <- data.frame(mtry = c(default - 10, default - 5, default, default + 5, default + 10, default + 20))
  
  # All features
  set.seed(1)
  rf_results <- run_model(gs, train_features, train_labels, test_features, test_labels, method="rf", ctrl)
  rf_results$conf_matrix
  rf_results$f2
  
  # Reduced features
  set.seed(1)
  rf_reduced <- run_model(gs, train_reduced, train_labels, test_reduced, test_labels, method="rf", ctrl)
  rf_reduced$conf_matrix
  rf_reduced$f2
  
  
  ## Support vector machines
            
  gs <- expand.grid(C = c(1),
                    sigma = c(1 / ncol(train_features)),
                    Weight = seq(0.5, 2, by=0.5))
  
  
  numeric_train <- train_features %>% select_if(is.numeric)
  numeric_test <- test_features %>% select_if(is.numeric)
  
  # Use recursive feature elimination to reduce the number of our features even more
  rfe_control <- rfeControl(functions = myFuncs,
                            method = "repeatedcv",
                            repeats = 5,
                            verbose = FALSE)
  
  rfe_profile <- rfe(numeric_train, train_labels, 
                     sizes = c(seq(10, 50, 10), 100, 150, ncol(data)),
                     rfeControl = rfe_control,
                     metric = "f2")
  
  numeric_train_reduced <- numeric_train[, rfe_profile$optVariables]
  numeric_test_reduced <- numeric_test[, rfe_profile$optVariables]
  
  # All features
  set.seed(1)
  svm_results <- run_model(gs, numeric_train, train_labels, numeric_test, test_labels, method="svmRadialWeights", ctrl)
  svm_results$conf_matrix
  svm_results$f2
  
  # Reduced features
  set.seed(1)
  svm_reduced <- run_model(gs, numeric_train_reduced, train_labels, numeric_test_reduced, test_labels, method="svmRadialWeights", ctrl)
  svm_reduced$conf_matrix
  svm_reduced$f2
  
  