k_fold = 5

## Rfe parameters

rfe_sizes <- c(2, 5, seq(10, 20, 2), 30)
# One of ["none", "down"]
rfe_sampling_method = "down"
rfe_trees <- 2000
rfe_tol <- 2.5
rfe_iterations <- 10
rfe_cutoff <- div(rfe_iterations, 2)



# One of ["none", "down", "up", "smote", "rose"]
sampling_method = "down"

# One of ["ROC", "f2"]
metric = "ROC"
