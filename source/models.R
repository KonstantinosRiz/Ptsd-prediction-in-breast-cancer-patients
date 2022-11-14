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

  # Split the dataset into train/test
  train_features <- final_features[[1]][train_indices, ]
  test_features <- final_features[[1]][-train_indices, ]
  
  train_labels <- new_final_label[train_indices]
  test_labels <- new_final_label[-train_indices]
  
  # train_labels <- data.frame(train_labels)
  # names(train_labels) <- label_name


  ####  Models  ####
  
  # Run the wanted model using all the combinations of the hyperparameters in the
  # respective grid, find the optimal model and evaluate it on the test set.

  ## Decision tree

  gs <- data.frame(cp = c(0.001, 0.005, 0.01, 0.05, 0.1))
  
  # Use k-fold cross validation to evaluate the models
  # and the appropriate summary function for the metric (f2)
  ctrl <- trainControl(method = "cv", number = k_fold,
                       summaryFunction = f2_summary,
                       # summaryFunction = defaultSummary,
                       # sampling = sampling_method,
                       sampling="smote"
                       )
  
  set.seed(1)
  dt_results <- run_model(gs, train_features, train_labels, test_features, test_labels, method="rpart", ctrl)
  
  dt_results$conf_matrix
  dt_results$grid
  
  ## Random forest
  
  default <- round(sqrt(ncol(train_features)))
  gs <- data.frame(mtry = c(default - 10, default - 5, default, default + 5, default + 10, default + 20))
  
  set.seed(1)
  rf_results <- run_model(gs, train_features, train_labels, test_features, test_labels, method="rf", ctrl)
  
  # gs <- list(mtry = c(default - 10, default - 5, default, default + 5, default + 10, default + 20),
  #            splitrule = c("gini", "extratrees","hellinger"),
  #            min.node.size = c(1, 3, 5, 7)) %>% cross_df()
  # set.seed(1)
  # rf_results <- run_model(gs, train_features, train_labels, test_features, test_labels, method="ranger", ctrl)
  
  rf_results$conf_matrix
  rf_results$grid
  
  train_data <- train_features
  train_data[, new_label_name] <- train_labels
  
  label_formula <- as.formula(paste(new_label_name, "~.", sep=""))
  rf_fit <- ranger(label_formula, train_data)
  pred <- predict(rf_fit, test_features)
  table(pred$predictions, test_labels)
  
  
  ## Support vector machines
            
  gs <- data.frame(sigma = c(1 / ncol(train_features)),
                   C = c(1),
                   Weight = I(list( c("0"= 0.2, "1"=0.8), c("0"=0.1, "1"=0.9), c("0"=0.3, "1"=0.7) ))
                   )
  
  gs <- expand.grid(C = c(1),
                    sigma = c(1 / ncol(train_features)),
                    Weight = seq(0.05, 0.15, by=0.01))
  
  # numeric_train <- train_features %>% select_if(is.numeric)
  # numeric_test <- test_features %>% select_if(is.numeric)
  
  ## Taken code
  my_pca <- prcomp(numeric_train, scale = TRUE,
                   center = TRUE, retx = T)
  
  my_pca$rotation
  dim(my_pca$x)
  biplot(my_pca, main = "Biplot", scale = 0)
  my_pca$sdev
  my_pca.var <- my_pca$sdev ^ 2
  propve <- my_pca.var / sum(my_pca.var)
  plot(propve, xlab = "principal component",
       ylab = "Proportion of Variance Explained",
       ylim = c(0, 1), type = "b",
       main = "Scree Plot")
  plot(cumsum(propve),
       xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       ylim = c(0, 1), type = "b")
  which(cumsum(propve) >= 0.9)[1]
  
  my_components <- as_tibble(my_pca$x) %>% select(PC1:PC72)
  my_test <- as_tibble(predict(my_pca, newdata = numeric_test)) %>% select(PC1:PC72)
  ##
  
  set.seed(1)
  svm_results <- run_model(gs, my_components, train_labels, my_test, test_labels, method="svmRadialWeights", ctrl)
  # svm_results <- run_model(gs, numeric_train, train_labels, numeric_test, test_labels, method="svmRadialWeights", ctrl))
  
  svm_results$conf_matrix
  svm_results$grid
  
  