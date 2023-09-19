f2_summary <- function(data, lev=NULL, model=NULL) {
  out <- fScore(actual=as.integer(data$obs) - 1, predicted=as.integer(data$pred) - 1, beta=2)
  names(out) <- "f2"
  out
}

many_stats_summary <-function(data, lev = levels(data$obs), model = NULL) {
    c(
      f2_summary(data = data, lev = levels(data$obs), model),
      twoClassSummary(data = data, lev = levels(data$obs), model),
      prSummary(data = data, lev = levels(data$obs), model),
      mnLogLoss(data = data, lev = levels(data$obs), model),
      defaultSummary(data = data, lev = levels(data$obs), model)
    )
}

rf_fit_down <- function(x, y, first, last, ...){
  # Function that downsamples the data before running rfe
  loadNamespace("randomForest")
  
  df_down <- caret::downSample(x, y)
  
  randomForest::randomForest(
    dplyr::select(df_down, -Class),
    df_down$Class,
    importance = (first | last),
    ...)
}

myPickSizeTolerance <- function(x, metric, tol=1.5, maximize=TRUE) {
  # This uses as a metric the one we chose at our config file
  pickSizeTolerance(x, metric, tol = rfe_tol, maximize = maximize)
}

run_model <- function(gs, train_features, train_labels, test_features, test_labels, method, control, metric="f2", preprocess=NULL) {
  # This functions runs all the models, according to the inputs

  # Create the models
  results <- train(train_features, train_labels, method = method,
        trControl = control,
        # preProcess = c("center", "scale"),
        tuneGrid=gs,
        metric=metric
  )

  optimal_model <- results$finalModel
  preds <- predict(results, test_features)
  factor_preds <- factor(preds, ordered=TRUE)
  # preds <- as.factor(argmax(preds, rows=TRUE) - 1)
  
  acc <- compute_accuracy(preds, test_labels)
  f2 <- compute_f2(preds, test_labels)
  roc <- roc(response = test_labels, predictor = factor_preds, levels=c(0,1), direction="<")
  auc <- auc(test_labels, factor_preds, levels=c(0,1), direction="<")
  
  grid_results <- results$results
  grid_results <- grid_results %>% arrange(desc(f2))

  conf_matrix <- caret::confusionMatrix(data=preds, reference=test_labels, positive="1")

  list(
    "best_fit" = optimal_model,
     "preds" = preds,
     "grid" = grid_results,
     "acc" = round(acc, digits=2),
     "f2" = round(f2, digits=2),
     "roc" = roc,
     "auc" = round(auc, digits=2),
     "conf_matrix" = conf_matrix
  )
}

voting_classifier <- function(...) {
  classifiers <- list(...)
  num_classifiers <- length(classifiers)
  
  total_preds <- 0
  for (classifier in classifiers) {
    total_preds <- total_preds + as.integer(classifier$preds)
  }
  # This is because the $preds we receive are in (1, 2) class format and we want to handle like (0, 1)
  total_preds <- total_preds - num_classifiers
  # Equal in case we have even amount of classifiers (which isn't optimal)
  total_preds <- as.integer(total_preds >= (num_classifiers / 2))
  factor_preds <- factor(total_preds, ordered=TRUE)
  
  acc <- compute_accuracy(total_preds, test_labels)
  f2 <- fScore(actual=as.integer(test_labels) - 1, predicted=total_preds, beta=2)
  roc <- roc(response = test_labels, predictor = factor_preds, , levels=c(0,1), direction="<")
  auc <- auc(test_labels, factor_preds)
  conf_matrix <- caret::confusionMatrix(data=factor_preds, reference=test_labels, positive="1")
  
  list(
    "preds" = total_preds,
    "acc" = round(acc, digits=2),
    "f2" = round(f2, digits=2),
    "roc" = roc,
    "auc" = round(auc, digits=2),
    "conf_matrix" = conf_matrix
  )
}

visualize <- function(auc_scores, f2_scores, title) {
  # Assumptions:
  # auc_scores and f2_scores have length 6 for our 6 classifiers:
  # dt, rf, svm, adaboost, xgboost, voting
  
  original_classifier <- c('dt', 'rf', 'svm', 'adaboost', 'xgboost', 'voting')
  classifier <- rep(original_classifier, 2)
  no_class <- length(original_classifier)
  metric_name <- c(rep('auc', no_class), rep('f2', no_class))
  
  score <- c(auc_scores, f2_scores)
  
  # Pretty results df
  results_df_pretty <- data.frame(original_classifier, auc_scores, f2_scores)
  
  # Df for visualization
  results_df <- data.frame(classifier, metric_name, score)
  results_df$classifier <- factor(results_df$classifier, levels=original_classifier)
  
  results_plot <- ggplot(
    data = results_df,
    mapping = aes(x = classifier, y = score, fill = metric_name)
  ) +
    geom_col(position = 'dodge') +
    scale_fill_manual(values = c('#417FBA', '#F2555E')) +
    ggtitle(title)
  
  list(
    'plot' = results_plot,
    'df' = results_df_pretty
  )
}

process_final_results <- function(final_results_list) {
  # This argument-list has been created from running the models.R file has and been saved (along with some other stuff)
  
  classifiers <- c(
    'dt', 'rf', 'svm', 'adaboost', 'xgboost', 'voting',
    'dt_reduced', 'rf_reduced', 'svm_reduced', 'adaboost_reduced', 'xgboost_reduced', 'voting_reduced'
  )
  metrics <- c('auc', 'f2')
  
  grouped_results <- list()
  for (clf in classifiers) {
    grouped_results[[clf]] <- list()
    
    for (metric in metrics) {
      grouped_results[[clf]][[metric]] <- c()
      
      for (k in 1:as.integer(config_list["number_of_imputed_datasets"])) {
        grouped_results[[clf]][[metric]] <- append(grouped_results[[clf]][[metric]], final_results_list[[clf]][[k]][[metric]])
      }
    }
  }
  
  aggregate_results <- list()
  
  for (metric in metrics) {
    aggregate_results[[metric]] <- c()
    
    for (clf in classifiers) {
      aggregate_results[[metric]] <- append(aggregate_results[[metric]], sum(grouped_results[[clf]][[metric]]) / length(grouped_results[[clf]][[metric]]))
    }
    names(aggregate_results[[metric]]) <- classifiers
  }
  
  auc_scores <- aggregate_results$auc[1:6]
  f2_scores <- aggregate_results$f2[1:6]
  aggregate_visualization_results <- visualize(auc_scores, f2_scores, 'Aggregate performance using all features')
  
  auc_scores_reduced <- aggregate_results$auc[7:12]
  f2_scores_reduced <- aggregate_results$f2[7:12]
  aggregate_visualization_results_reduced <- visualize(auc_scores_reduced, f2_scores_reduced, 'Aggregate performance using reduced features')

  list(
    "grouped_results" = grouped_results,
    "f2_scores" = f2_scores,
    "auc_scores" = auc_scores,
    "f2_scores_reduced" = f2_scores_reduced,
    "auc_scores_reduced" = auc_scores_reduced,
    "visualization" = aggregate_visualization_results,
    "visualization_reduced" = aggregate_visualization_results_reduced
  )
}

compute_accuracy <- function(preds, test_labels) {
  mean(preds == test_labels)
}

compute_f2 <- function(preds, test_labels) {
  fScore(actual=as.integer(test_labels) - 1, predicted=as.integer(preds) - 1, beta=2)
}
