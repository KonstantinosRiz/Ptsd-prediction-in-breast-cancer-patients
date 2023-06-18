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
library(purrr)


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
source("preprocessing_config.R")

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

percent_missing <- missing_values_per_column / nrow(features_1)
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


####  4 Split the dataset into train/test ####

# Threshold the label
new_final_label <- as.factor(as.integer(final_label >= ptsd_threshold))
new_label_name <- paste(month_label, "_ptsd", sep="")

train_indices <- createDataPartition(new_final_label, p=train_size, list=FALSE)

# Unimputed test features/labels to be used on models that can handle it
# !!!!!!!!!!!!!!!! Not being used right now !!!!!!!!!!!!!!!!
raw_test_features <- features_3[-train_indices, ]
raw_test_labels <- new_final_label[-train_indices]

# Comment handling
comment_4 <- sprintf("Step 4: Split the dataset into train/test with a %.2f ratio", train_size)
preprocessing_comments <- append(preprocessing_comments, comment_4)

####  5 Impute missing values ####

# I choose to impute the missing values before eliminating low variance features
# because the features I might eliminate (despite the low variance) can impact
# the imputation.

# I impute using only the train samples but apply the imputation process to the
# test set as well.
ignore_vector <- rep(TRUE, nrow(features_3))
ignore_vector[train_indices] <- FALSE

imp_data <- mice(features_3, m=number_of_imputed_datasets, method=imputation_method, ignore=ignore_vector)
# imp_data$imp

features_5 = list()
for (i in 1 : (number_of_imputed_datasets + 1)) {
  features_5[[i]] <- complete(imp_data, i)
}

# Comment handling
comment_5 <- sprintf("Step 5: Used Multiple Imputation Chained Equations (MICE) to impute %i different datasets", number_of_imputed_datasets)
preprocessing_comments <- append(preprocessing_comments, comment_5)

# We plan to use all 5 different imputed datasets to predict using our model and
# afterwards use a voting classifier to ensemble the 5 different predictions.


####  6 Imputed datasets' preprocessing ####

final_features = list()
total_near_zero_var_indices <- c()
total_highly_correlated_indices <- c()
to_eliminate_indices <- c()

for (i in 1:number_of_imputed_datasets) {
  
  ####  6.1 Eliminate features with very low variance  ####
  
  # Find the low variance indices
  near_zero_var_indices <- nearZeroVar(features_5[[i]], freqCut=freqCut)
  # Add them to the union that will be eliminated from all datasets
  total_near_zero_var_indices <- union(total_near_zero_var_indices, near_zero_var_indices)
  
  ####  6.2 Eliminate highly correlated features  ####
  
  # Use only numeric features
  numeric_features <- features_5[[i]] %>% select_if(is.numeric)
  correlation <- cor(numeric_features, method=c(correlation_method))
  
  # Visualization of a small part of the correlation plot
  # corrplot(correlation[1:4, 1:4])
  
  # Find which features need to be eliminated due to high correlation with others
  correlation[!lower.tri(correlation)] <- 0
  numeric_features_selection <- apply(correlation, 1,
                                      function(row) any(abs(row) > cor_threshold, na.rm = TRUE))
  highly_correlated_numeric_features <- numeric_features_selection[which(numeric_features_selection == TRUE)]
  
  # Find the indices of these features
  highly_correlated_indices <- map_int( names(highly_correlated_numeric_features), grep, names(features_5[[i]]) )
  # Add them to the union that will be eliminated from all datasets
  total_highly_correlated_indices <- union(total_highly_correlated_indices, highly_correlated_indices)
  
  # Comment handling
  temp <- names(highly_correlated_numeric_features)
  temp_comments <- c()
  if (length(temp) != 0) {
    # For every feature we want to eliminate we find the correlated with it features
    for (j in temp) {
      corr_row <- correlation[j, ]
      correlated_features <- names(corr_row[abs(corr_row) > cor_threshold])
      my_comment <- sprintf("%s is highly correlated with %s", j, correlated_features[1])
      temp_comments <- union(temp_comments, my_comment)
    }
  }
}

for (i in 1:number_of_imputed_datasets) {
  to_eliminate_indices <- union(total_near_zero_var_indices, total_highly_correlated_indices)
  final_features[[i]] <- features_5[[i]][, -to_eliminate_indices]
}

# Comment handling
comment_6_1 <- "Step 6: Eliminated features due to low variance or high correlation\n"
if (identical(total_near_zero_var_indices, integer(0))) {
  comment_6_2 <- "Step 6.1: No low variance features detected\n"
} else {
  comment_6_2_1 <- "Step 6.1: Low variance features:"
  comment_6_2_2 <- paste(names(features_5[[1]][total_near_zero_var_indices]), collapse=", ")
  comment_6_2 <- paste(comment_6_2_1, comment_6_2_2, sep="\n")
}
if (is.null(temp_comments)) {
  comment_6_3 <- "Step 6.2: No highly correlated feature pairs detected\n"
} else {
  comment_6_3_1 <- "Step 6.2: Highly correlated features:\n"
  comment_6_3_2 <- paste(temp_comments, collapse="\n")
  comment_6_3 <- paste(comment_6_3_1, comment_6_3_2, sep="\n")
}

comment_6 <- paste(comment_6_1, comment_6_2, comment_6_3, sep="--------------------------------------------\n")
preprocessing_comments <- append(preprocessing_comments, comment_6)

####  Final save file which will be used to train and evaluate the models ####

if (val_size != 0) {
  # Create the validation set indices
  val_indices <- createDataPartition(new_final_label[train_indices], p=val_size, list=FALSE)
  
  save(final_features, new_final_label, new_label_name, train_indices, val_indices, preprocessing_comments, config_list, file=save_file_name)
} else {
  save(final_features, new_final_label, new_label_name, train_indices, preprocessing_comments, config_list, file=save_file_name)
}
