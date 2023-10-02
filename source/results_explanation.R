#### Choose file
results_folder <- r"{..\results}"
results_file <- r"{M6_random_ignored.RData}"
# results_file <- r"{M6_random_ignored.RData}"
results_path <- paste(results_folder, results_file, sep=r"{\}")
load(results_path)


# Visualize rfe
# plot(final_results$rfe[[1]], type=c("g", "o"))


processed_results$visualization




# #### Explainable AI usage
# library(DALEX)
# 
# exp <- explain(
#   rf_reduced$best_fit,
#   data = train_reduced,
#   y = train_labels
# )
# 
# # These basically create the same plot
# xai <- variable_effect(exp2, colnames(train_reduced))
# plot(xai)
# 
# xai_model_profile <- model_profile(exp2, colnames(train_reduced))
# plot(xai_model_profile)