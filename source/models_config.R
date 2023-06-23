# Features to be ignored (hinted by medics)
ignore = FALSE
ignored_features = c('Emot_Fun_QLQ30.0', 'Emot_Fun_QLQ30.3', 'nccn_distress_thermometer.0', 'nccn_distress_thermometer.3')


## Rfe parameters

rfe_sizes <- c(2, 5, seq(10, 20, 2), 30)
# One of ["none", "down"]
rfe_sampling_method = "down"
rfe_trees <- 2000
rfe_tol <- 3.5
rfe_iterations <- 10
rfe_cutoff <- div(rfe_iterations, 2)

k_fold = 5


# One of ["none", "down", "up", "smote", "rose"]
sampling_method = "down"

# One of ["ROC", "f2"]
metric = "ROC"
