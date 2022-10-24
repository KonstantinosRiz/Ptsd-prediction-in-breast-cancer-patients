#!/usr/bin/env Rscript

####  Installations and imports ####

# install.packages("dplyr", "corrplot",
#                  repos = "http://cran.us.r-project.org")
# 
# install.packages("caret")
# 
# install.packages("devtools")
# devtools::install_github(repo = "amices/mice")

library(mice)
library(dplyr)
library(corrplot)
library(caret)

####  Argument parsing ####

# args = commandArgs(trailingOnly=TRUE)
# acceptable_args = list("M6", "M12", "M18")
# 
# if (length(args) != 1 || !(args[1] %in% acceptable_args)) {
#   stop('You must provide exactly one of ["M6", "M12", "M18"]')
# } else {
#   month_label <- args[1]
# }

####  Data loading and variable renaming  ####

# Set the hyperparameters
source("config.R")

setwd(my_path)
load(dataset_path)

# Rename just for ease
data <- dataPCL

########## Split the dataset into potential features and potential labels

potential_features <- subset(dataPCL, select = c(Depression_HADS.0 : M3_PTGI_appreciation_of_life,
                                       M3_MAC_helpless : Sex_Enjoy_BR23.3))

potential_labels <- subset(dataPCL, select = M6_ptsd_PCL5 : M18_DSMdiagnosis_PCL)

##########  Choose my label

label_name <- paste(month_label, "_ptsd_PCL5", sep="")
label <- potential_labels[, label_name]

# Gather the preprocessing comments to have a general idea of what happened
# during every step
preprocessing_comments <- list()

####  1 Eliminate samples with no label  ####

# This has to happen before step 2 because I don't wanna count as missing
# values of a feature the values of a sample that has no label and is therefore
# useless.

features_1 <- potential_features[!is.na(label), ]
label_1 <- label[!is.na(label)]

# Comment handling
comment_1 <- sprintf("Step 1: Eliminated %i samples with no %s label",
                     nrow(potential_features) - nrow(features_1), label_name)
preprocessing_comments <- append(preprocessing_comments, comment_1)

####  2 Eliminate features with too many missing values ####

missing_values_per_column <- colSums(is.na(features_1))
hist(missing_values_per_column)

percent_missing = missing_values_per_column / nrow(features_1)
feature_selection <- percent_missing < missing_samples_threshold
features_2 <- features_1[, feature_selection]
label_2 <- label_1

# Comment handling
temp <- names(feature_selection[which(feature_selection == FALSE)])

if (length(temp) != 0) {
  comment_2_1 <- sprintf("Step 2: Eliminated following features with missing values exceeding threshold %.2f", missing_samples_threshold)
  comment_2_2 <- paste(temp, collapse="\n")
  comment_2 <- paste(comment_2_1, comment_2_2, sep="\n")
} else {
  comment_2 <- sprintf("Step 2: No features had enough missing values to be eliminated using threshold %.2f", missing_samples_threshold)
}
preprocessing_comments <- append(preprocessing_comments, comment_2)

####  3 Eliminate samples with too many missing values  ####

missing_values_per_row <- rowSums(is.na(features_2))
hist(missing_values_per_row)

# The histogram showed that the ideal points to cut samples with more missing
# features are 50 and 90. Due to having few samples comparing to our features
# we decide to keep the higher threshold.

sample_selection <- missing_values_per_row < missing_features_threshold
features_3 <- features_2[sample_selection, ]
final_label <- label_2[missing_values_per_row < missing_features_threshold]

# Comment handling
temp <- data[names(sample_selection[which(sample_selection == FALSE)]), ]$mrn

if (length(temp) != 0) {
  comment_3_1 <- sprintf("Step 3: Eliminated following samples with missing values exceeding threshold %i", missing_features_threshold)
  comment_3_2 <- paste(temp, collapse="\n")
  comment_3 <- paste(comment_3_1, comment_3_2, sep="\n")
} else {
  comment_3 <- sprintf("Step 2: No samples had enough missing values to be eliminated using threshold %i missing features", missing_features_threshold)
}
preprocessing_comments <- append(preprocessing_comments, comment_3)


####  4 Split the dataset into train / validation (for the voting classifier of
#       the differently imputed datasets) / test (outer loop) and threshold the label  ####

# Threshold the label
new_final_label <- as.factor(as.integer(final_label > ptsd_threshold))
# new_label_name <- paste(month_label, "_ptsd", sep="")

set.seed(1)

# Train/test split !! 1st !!

# train_sample_size <- floor(train_size * nrow(features_3))
# train_indices <- sample(seq_len(nrow(features_3)), size=train_sample_size)
train_indices <- createDataPartition(new_final_label, p=train_size, list=FALSE)

temp_train_features <- features_3[train_indices, ]
test_features <- features_3[-train_indices, ]
temp_train_labels <- new_final_label[train_indices]
test_labels <- new_final_label[-train_indices]

# Train/validation split !! 2nd !!
if (val_size != 0) {
  # val_sample_size <- floor(val_size * nrow(temp_train_features))
  # val_indices <- sample(seq_len(nrow(temp_train_features)), size=val_sample_size)
  val_indices <- createDataPartition(temp_train_labels, p=val_size, list=FALSE)
  
  train_features <- temp_train_features[-val_indices, ]
  val_features <- temp_train_features[val_indices, ]
  train_labels <- temp_train_labels[-val_indices]
  val_labels <- temp_train_labels[val_indices]
} else {
  train_features <- temp_train_features
  train_labels <- temp_train_labels
}

# Comment handling
comment_4 <- sprintf("Step 4: Split the dataset into train/test with a %.2f ratio\nand then into train/validation with a %.2f ratio", train_size, 1-val_size)
preprocessing_comments <- append(preprocessing_comments, comment_4)

####  5 Impute training (only inner training) set missing values ####

# I choose to impute the missing values before eliminating low variance features
# because the features I might eliminate (despite the low variance) can impact
# the imputation.

imp_data <- mice(train_features, m=number_of_imputed_datasets, method=imputation_method)
# imp_data$imp

features_5 = list()
for (i in 1:number_of_imputed_datasets) {
  features_5[[i]] <- complete(imp_data, i)
}

# Comment handling
comment_5 <- sprintf("Step 5: Used Multiple Imputation Chained Equations (MICE) to impute %i different datasets", number_of_imputed_datasets)
preprocessing_comments <- append(preprocessing_comments, comment_5)

# We plan to use all 5 different imputed datasets to predict using our model and
# afterwards use a voting classifier to ensemble the 5 different predictions.

####  6 Imputed datasets' preprocessing ####

features_6_1 = list()
final_train_features = list()
comments_6_1 = list()
comments_6_2 = list()

for (i in 1:number_of_imputed_datasets) {

  ####  6.1 Eliminate features with very low variance  ####

  # zero_var_features <- apply(features_5[[i]], 2, function(x) var(x) == 0)
  # features_6_1[[i]] <- features_5[[i]][, !zero_var_features]
  
  near_zero_var_indices <- nearZeroVar(features_5[[i]], freqCut=freqCut)
  features_6_1[[i]] <- features_5[[i]][, -near_zero_var_indices]
  
  # Comment handling
  # temp <- names(zero_var_features[which(zero_var_features == TRUE)])
  temp <- names(features_5[[i]])[near_zero_var_indices]
  if (length(temp) != 0) {
    comment_6_1 <- "Step 6.1: Eliminated following features with near zero variance"
    comment_6_2 <- paste(temp, collapse="\n")
    comment_6 <- paste(comment_6_1, comment_6_2, sep="\n")
  } else {
    comment_6 <- "Step 6.1: No features with zero variance were found"
  }
  comments_6_1[[i]] <- comment_6

  ####  6.2 Eliminate highly correlated features  ####

  # Use only numeric features
  numeric_features <- features_6_1[[i]] %>%
    select_if(is.numeric)
  
  correlation <- cor(numeric_features, method=c(correlation_method))
  
  # Visualization of a small part of the correlation plot
  # corrplot(correlation[1:4, 1:4])
  
  # Find which features need to be eliminated due to high correlation with others
  correlation[!lower.tri(correlation)] <- 0
  numeric_features_selection <- apply(correlation, 1,
                                         function(row) any(abs(row) > cor_threshold, na.rm = TRUE))
  to_eliminate_numeric_features <- numeric_features_selection[which(numeric_features_selection == TRUE)]
  final_train_features[[i]] <- features_6_1[[i]][, -which(names(features_6_1[[i]]) %in%
                                                          names(to_eliminate_numeric_features))]
  
  # Comment handling
  temp <- names(to_eliminate_numeric_features)
  if (length(temp) != 0) {
    
    # For every feature we want to eliminate we find the correlated with it features
    temp2 <- list()
    for (j in temp) {
      corr_row <- correlation[j, ]
      correlated_features <- names(corr_row[abs(corr_row) > cor_threshold])
      temp2 <- append(temp2, paste(correlated_features, collapse=", "))
    }
    
    comment_7_1 <- "Step 6.2: Eliminated following features due to high correlation with another feature"
    comment_7_2 <- paste(temp, temp2, sep=" is highly correlated with ", collapse="\n")
    comment_7 <- paste(comment_7_1, comment_7_2, sep="\n\n")
  } else {
    comment_7 <- sprintf("Step 6.2: No highly correlated features were observed using threshold %s", cor_threshold)
  }
  comments_6_2[[i]] <- comment_7
}

preprocessing_comments[[length(preprocessing_comments)+1]] <- comments_6_1
preprocessing_comments[[length(preprocessing_comments)+1]] <- comments_6_2

####  Final save file which will be used to train and evaluate the models ####

if (val_size != 0) {
  save(final_train_features, train_labels, val_features, val_labels, test_features, test_labels, preprocessing_comments, config_list, file=save_file_name)
} else {
  save(final_train_features, train_labels, test_features, test_labels, preprocessing_comments, config_list, file=save_file_name)
}
