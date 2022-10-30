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
# library(corrplot)

####  Initializing and loading the preprocessed data  ####

# Absolute path
my_path <- r"{D:\Κωνσταντίνος Data\Σχολής\Διπλωματική Εργασία\Main\source}"
setwd(my_path)
# Relative path
clean_data_folder <- r"{..\dataset\preprocessed_results}"
data_file <- r"{\M6_1.RData}"
label_name <- "M6_ptsd"
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

print_comments(preprocessing_comments)

train_labels <- data.frame(train_labels)
names(train_labels) <- label_name

# Create an imputed version of the test features to use it in models that don't
# support missing values

# for (i in 1:nrow(test_features)) {
#   ext_train_features <- 
# }

####  Find the optimal model for every imputed dataset  ####

# for (i in 1:as.integer(config_list["number_of_imputed_datasets"]) {

  train <- cbind(final_train_features[[1]], train_labels)

  ####  Models  ####
  
  # Run the wanted model using all the combinations of the hyperparameters in the
  # respective grid, find the optimal model and evaluate it on the test set.

  ## Decision tree

  gs <- data.frame(cp = c(0.001, 0.005, 0.01, 0.05, 0.1))
  
  set.seed(1)
  dt_results <- run_model(gs, final_train_features[[1]], train_labels, test_features, test_labels, method="rpart")
  
  dt_results$conf_matrix
  dt_results$f2
  
  ## Random forest
  
  default <- round(sqrt(ncol(final_train_features[[1]])))
  gs <- list(mtry = c(default - 10, default - 5, default, default + 5, default + 10, default + 20),
                   splitrule = c("gini", "extratrees","hellinger"),
                   min.node.size = c(1, 3, 5, 7)) %>% cross_df()
  
  gs <- data.frame(mtry = c(default - 10, default - 5, default, default + 5, default + 10, default + 20))
  
  set.seed(1)
  rf_results <- run_model(gs, final_train_features[[1]], train_labels, test_features, test_labels, method="rf")
  