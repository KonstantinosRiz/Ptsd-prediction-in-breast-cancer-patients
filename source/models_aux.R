decision_tree <- function(cp=0.01, minsplit=20, maxdepth=30) {
  
  # Create the fitted model
  fit <- rpart(
    M6_ptsd~. , method="class", parms=list(split = "gini"),
    data = balanced_train,
    control=rpart.control(
      cp=cp,
      minsplit=minsplit,
      maxdepth=maxdepth 
    )
  )
  rpart.plot(fit, cex=.5, extra=4)
  
  # summary(fit)
  # plotcp(fit)
  # printcp(fit)
  
  preds <- predict(fit, test, type="class")
  conf_matrix <- table(test$M6_ptsd, preds)
  
  results <- list("fit" = fit, "preds" = preds,
                  "confusion_matrix" = conf_matrix)
  
  return (results)
  
  # # fit$cptable[which.min(fit$cptable[,”xerror”]),”CP”]
  # short_tree <- prune(fit, cp=0.011765)
  # 
  # rpart.plot(short_tree, cex=.5, extra=4)
  # plotcp(short_tree)
  # printcp(short_tree)
  # 
  # short_tree$variable.importance
  # summary(short_tree)
}

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
  # preds <- predict(optimal_model, test_features)
  # preds <- as.factor(argmax(preds, rows=TRUE) - 1)
  
  acc <- compute_accuracy(preds, test_labels)
  f2 <- fScore(actual=as.integer(test_labels) - 1, predicted=as.integer(preds) - 1, beta=2)
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

create_decision_tree <- function(cp, minsplit, maxdepth, train_features, train_labels, ctrl) {
  
  train(train_features, train_labels[, 1], method = "rpart",
         trControl = ctrl,
         # preProcess = c("center", "scale"),
         # tuneGrid=gs,
         metric="f2",
         # method="class",
          parms=list(split="gini"),
         control=rpart.control(cp=cp, minsplit=minsplit, maxdepth=maxdepth)
  )$finalModel
}

compute_preds <- function(fit, test_features) {
  predict(fit, test_features, type = "class")
}

compute_accuracy <- function(preds, test_labels) {
  mean(preds == test_labels)
}

#### are you really gonna lets this through?!
compute_f2 <- function(preds, test_labels) {
  fScore(actual=as.integer(test_labels) - 1, predicted=as.integer(preds) - 1, beta=2)
}

create_conf_matrix <- function(fit, test_features, test_labels) {
  predicted <- predict(fit, test_features, type = "class")
  table(predicted, test_labels)
}


