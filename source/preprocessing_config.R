# Hyperparameters

## Working directory (should be changed to run file in different device)
# Absolute path
my_path <- r"{D:\Κωνσταντίνος Data\Σχολής\Διπλωματική Εργασία\Main}"
# Relative path
dataset_path <- r"{Dataset\DataForPCL_20June22.Rdata}"

## Choose label from ["M6", "M12", "M18"]
month_label = "M6"

## Preprocessing

missing_samples_threshold = 0.25
missing_features_threshold = 90
number_of_imputed_datasets = 5
imputation_method ="rf"
# Default value for thresholding near zero variance features
freqCut = 95/5
correlation_method = "spearman"
cor_threshold = 0.8

train_size = 0.7
# Not using a validation set for now
val_size = 0

# For thresholding the label
ptsd_threshold = 33

config_list = list("missing_samples_threshold"=missing_samples_threshold,
                   "missing_features_threshold"=missing_features_threshold,
                   "number_of_imputed_datasets"=number_of_imputed_datasets,
                   "imputation_method"=imputation_method,
                   "freqCut"=freqCut,
                   "correlation_method"=correlation_method,
                   "cor_threshold"=cor_threshold,
                   "train_size"=train_size,
                   "val_size"=val_size,
                   "ptsd_threshold"=ptsd_threshold,
                   )

## Save file
# Increment this to add a new entry and not override existing files
file_id <- 2
file_name <- paste(paste(month_label, file_id, sep="_"), "RData", sep=".")
save_file_name <- paste(r"{Dataset\Preprocessed_results\}", file_name, sep="")
