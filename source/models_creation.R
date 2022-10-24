#!/usr/bin/env Rscript

####  Installations and imports ####

# install.packages("DMwR",
#                  repos = "http://cran.us.r-project.org")

# install.packages("smotefamily")
# install.packages("dplyr")
# install.packages("ROSE")
# install.packages("caret")

library(smotefamily)
library(dplyr)
library(ROSE)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(ModelMetrics)
library(caret)
# library(corrplot)

####  Initializing and loading the preprocessed data  ####

# Absolute path
my_path <- r"{D:\Κωνσταντίνος Data\Σχολής\Διπλωματική Εργασία\Main}"
setwd(my_path)
# Relative path
clean_data_folder <- r"{Dataset\Preprocessed_results}"
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

####  Find the optimal model for every imputed dataset  ####

# for (i in 1:as.integer(config_list["number_of_imputed_datasets"]) {

  train <- cbind(final_train_features[[1]], train_labels)

  ####  Balance the training datasets  ####

  # table(train_labels)
  # 
  # positives <- sum(as.integer(levels(train[, label_name]))[train[, label_name]])
  # negatives <- nrow(train) - positives
  # # str1 <- sprintf("The number of people with PTSD is %i", positives)
  # # str2 <- sprintf("The number of people without PTSD is %i", negatives)
  # # str3 <- sprintf("The positives ratio is %.2f", positives/nrow(train))
  # # cat(paste(str1, str2, str3, sep="\n"))
  # 
  # # numeric_dataset <- train %>%
  # #   select_if(is.numeric)
  # 
  # # smoted <- SMOTE(numeric_dataset, numeric_dataset$M6_ptsd, K=5)
  # # a <- smoted$data
  # 
  # # I oversample the minority class('1') until I reach the same sample size
  # label_formula <- as.formula(paste(label_name, "~.", sep=""))
  # oversampling <- ovun.sample(label_formula, data=train,
  #                             N=2*negatives, seed=2, method="over")
  # balanced_train <- oversampling$data
  # 
  # balanced_train_features <- balanced_train
  # balanced_train_features[, label_name] <- NULL
  # balanced_train_labels <- balanced_train[, label_name]
  # 
  # table(balanced_train_labels)

  ####  Models  ####
  
  ## Decision tree
  
  # options(repr.plot.width = 6, repr.plot.height = 5)
  
  # dt_results <- decision_tree(cp=0.01, minsplit=ceiling(0.03 * nrow(balanced_train)), maxdepth=10)
  # dt_results$confusion_matrix
  
  # Create gridsearch
  gs <- list(cp = c(0.001, 0.005, 0.01, 0.05, 0.1),
             minsplit = ceiling(c(0.01, 0.03, 0.05, 0.07, 0.1) * nrow(train)),
             maxdepth = c(5, 10, 20, 30)
             ) %>%
    cross_df()
  # gs <- data.frame(cp = c(0.0001, 0.0005, 0.001, 0.0025, 0.005, 0.01, 0.05, 0.1))
  
  # Run the wanted model using all the combinations of the hyperparameters in the
  # gridsearch. This returns...
  
  mySummary <- function(data, lev=NULL, model=NULL) {
    out <- fScore(actual=as.integer(data$obs) - 1, predicted=as.integer(data$pred) - 1, beta=2)
    # out <- fScore(actual=data$obs, predicted=data$pred, beta=2)
    names(out) <- "f2"
    out
  }
  
  #### fiveStats summary
  ## For accuracy, Kappa, the area under the ROC curve, sensitivity and specificity:
  # fiveStats <- function(...) c(twoClassSummary(...),defaultSummary(...))
  
  # Smote parameters perc.over = 100,perc.under = 200
  
  ctrl <- trainControl(method = "cv", number = 5,
                       summaryFunction = mySummary
                       )
  
  set.seed(1)
  fit <- train(balanced_train_features, balanced_train_labels, method = "rpart",
               trControl = ctrl,
               # preProcess = c("center", "scale"),
               # tuneGrid=gs,
               metric="f2",
               control=rpart.control(maxdepth=2)
               )
  
  # set.seed(1)
  # fit2 <- train(M6_ptsd~. , data = balanced_train, method = "rpart",
  #              trControl = ctrl,
  #              # preProcess = c("center", "scale"),
  #              tuneGrid=data.frame(cp = c(0.001, 0.005, 0.01, 0.05, 0.1)),
  #              # metric="f2"
  # )
  
  # fit$bestTune
  fit$results
  rpart.plot(fit$finalModel)
  
  preds <- predict(fit, test_features)
  acc <- compute_accuracy(preds, test_labels)
  table(preds, test_labels)
