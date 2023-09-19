# !!!!!!!!!!!!!!!!!! Choose label
## Choose label from ["M6", "M12", "M18"]
month_label <- "M6"

dataset_path <- r"{..\dataset\DataForPCL_20June22.Rdata}"


## Preprocessing

# Features to be ignored (hinted by medics)
ignore <- FALSE
ignored_features <- c('Emot_Fun_QLQ30.0', 'Emot_Fun_QLQ30.3', 'nccn_distress_thermometer.0', 'nccn_distress_thermometer.3')

missing_samples_threshold <- 0.25
missing_features_threshold <- 0.5
number_of_imputed_datasets <- 5
imputation_method <- "rf"
# Default values for thresholding near zero variance features
freqCut <- 95/5
uniqueCut <- 10
correlation_method <- "spearman"
cor_threshold <- 0.8

# One of ['random', 'IEO', 'HUS', 'HUJI', 'CHA']
split_method <- 'CHA'
train_size <- 0.7
# Not using a validation set for now
val_size <- 0

# Threshold for the label (given by research)
ptsd_threshold <- 33


config_list <- list(
  "ignore" = ignore,
  "ignored_features" = ignored_features,
  "missing_samples_threshold" = missing_samples_threshold,
  "missing_features_threshold" = missing_features_threshold,
  "number_of_imputed_datasets" = number_of_imputed_datasets,
  "imputation_method" = imputation_method,
  "freqCut" = freqCut,
  "uniqueCut" = uniqueCut,
  "correlation_method" = correlation_method,
  "cor_threshold" = cor_threshold,
  "split_method" = split_method,
  "train_size" = train_size,
  "val_size" = val_size,
  "ptsd_threshold" = ptsd_threshold
)


## Save file
save_folder <- r"{..\dataset\preprocessed_results}"
if (ignore) {
  save_file_name <- paste(paste(month_label, split_method, 'ignored', sep="_"), "RData", sep=".")
} else {
  save_file_name <- paste(paste(month_label, split_method, sep="_"), "RData", sep=".")
}
save_path <- paste(save_folder, save_file_name, sep=r"{\}")

