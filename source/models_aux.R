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
  pickSizeTolerance(x, metric, tol = rfe_tol, maximize = maximize)
}

run_model <- function(gs, train_features, train_labels, test_features, test_labels, method, control, metric="f2") {
  # This functions runs all the models, according to the inputs

  # Create the models
  results <- train(train_features, train_labels, method = method,
        trControl = control,
        # preProcess = c("center", "scale", "pca"),
        tuneGrid=gs,
        metric=metric
  )

  optimal_model <- results$finalModel
  preds <- predict(results, test_features)
  factor_preds <- factor(preds, ordered=TRUE)
  # preds <- as.factor(argmax(preds, rows=TRUE) - 1)
  
  acc <- compute_accuracy(preds, test_labels)
  f2 <- compute_f2(preds, test_labels)
  roc <- roc(response = test_labels, predictor = factor_preds)
  auc <- auc(test_labels, factor_preds)
  
  grid_results <- results$results
  grid_results <- grid_results %>% arrange(desc(f2))

  conf_matrix <- caret::confusionMatrix(data=preds, reference=test_labels, positive="1")

  # preds
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
  roc <- roc(response = test_labels, predictor = factor_preds)
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

compute_accuracy <- function(preds, test_labels) {
  mean(preds == test_labels)
}

compute_f2 <- function(preds, test_labels) {
  fScore(actual=as.integer(test_labels) - 1, predicted=as.integer(preds) - 1, beta=2)
}
