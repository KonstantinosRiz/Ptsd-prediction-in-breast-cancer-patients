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
  # out <- fScore(actual=data$obs, predicted=data$pred, beta=2)
  names(out) <- "f2"
  out
}

run_model <- function(gs, train_features, train_labels, test_features, test_labels, method, control, metric="f2") {
  
  # Create the models
  results <- train(train_features, train_labels, method = method,
        trControl = control,
        # preProcess = c("center", "scale"),
        tuneGrid=gs,
        metric=metric
  )
  optimal_model <- results$finalModel
  # print("Finished model, now predicting")
  
  preds <- predict(results, test_features)
  
  acc <- compute_accuracy(preds, test_labels)
  grid_results <- results$results
  grid_results <- grid_results %>% arrange(desc(f2))
  f2 <- grid_results$f2[1]
  conf_matrix <- caret::confusionMatrix(data=preds, reference=test_labels, positive="1")
  
  # preds
  list("best_fit" = optimal_model,
       "preds" = preds,
       "grid" = grid_results,
       "acc" = acc,
       "f2" = f2,
       "conf_matrix" = conf_matrix)
}

f2_summary <- function(data, lev=NULL, model=NULL) {
  out <- fScore(actual=as.integer(data$obs) - 1, predicted=as.integer(data$pred) - 1, beta=2)
  # out <- fScore(actual=data$obs, predicted=data$pred, beta=2)
  names(out) <- "f2"
  out
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


