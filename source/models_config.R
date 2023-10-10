## Rfe parameters

rfe_sizes <- c(2, 5, seq(10, 20, 2), 30)
# One of ["none", "down", "up"]
rfe_sampling_method <- "down"
rfe_trees <- 2000
rfe_tol <- 3.5
rfe_k_fold <- 5
rfe_repeats <- 10


## Training parameters

k_fold <- 5
repeats <- 5
# One of [NULL, "down", "up", "rose"]
sampling_method <- "down"
# One of ["ROC", "f2"]
metric <- "f2"

training_config_list <- list(
  "rfe_sizes" = rfe_sizes,
  "rfe_sampling_method" = rfe_sampling_method,
  "rfe_trees" = rfe_trees,
  "rfe_tol" = rfe_tol,
  "rfe_k_fold" = rfe_k_fold,
  "rfe_repeats" = rfe_repeats,
  "k_fold" = k_fold,
  "repeats" = repeats,
  "sampling_method" = sampling_method,
  "metric" = metric
)


## Choose the correct data file to load

clean_data_folder <- r"{..\dataset\preprocessed_results}"
data_file <- r"{M6_IEO_ignored.RData}"
data_path <- paste(clean_data_folder, data_file, sep=r"{\}")


## Save file

# This does not override the data file because I change the working directory before saving
save_results_folder <- r"{..\results}"
save_results_file <- r"{M6_IEO.RData}"
save_results_path <- paste(save_results_folder, save_results_file, sep=r"{\}")
