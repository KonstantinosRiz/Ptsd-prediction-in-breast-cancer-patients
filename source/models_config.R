
## Rfe parameters

rfe_sizes <- c(2, 5, seq(10, 20, 2), 30)
# One of ["none", "down"]
rfe_sampling_method = "down"
rfe_trees <- 2000
rfe_tol <- 3.5
# previous value 10
rfe_iterations <- 10
rfe_cutoff <- div(rfe_iterations, 2)
# Usually 5, 5
rfe_k_fold <- 5
rfe_repeats <- 5

k_fold <- 5
repeats <- 5


# One of ["none", "down", "up", "smote", "rose"]
sampling_method <- "down"

# One of ["ROC", "f2"]
metric <- "ROC"

training_config_list <- list(
  "rfe_sizes" = rfe_sizes,
  "rfe_sampling_method" = rfe_sampling_method,
  "rfe_trees" = rfe_trees,
  "rfe_tol" = rfe_tol,
  "rfe_iterations" = rfe_iterations,
  "rfe_cutoff" = rfe_cutoff,
  "rfe_k_fold" = rfe_k_fold,
  "rfe_repeats" = rfe_repeats,
  "k_fold" = k_fold,
  "repeats" = repeats,
  "sampling_method" = sampling_method,
  "metric" = metric
)

## Save file
# Increment this to add a new entry and not override existing files
file_id <- 1
save_results <- paste(file_id, "RData", sep=".")
