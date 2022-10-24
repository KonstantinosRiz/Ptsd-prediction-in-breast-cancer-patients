print_comments <- function(comments) {
  str1 <- paste(comments[1:5], collapse="\n-----------------------------------------------------------------------------------\n")
  str2 <- "Step 6: Preprocessing all the imputed datasets separately"
  str3 <- paste(str1, str2, sep="\n-----------------------------------------------------------------------------------\n")
  
  starting <- TRUE
  for (i in 1:as.integer(config_list["number_of_imputed_datasets"])) {
    str4_1 <- sprintf("Dataset #%i\n", i)
    str4_2 <- paste(comments[[6]][[i]], comments[[7]][[i]], sep="\n-----------------------------------------------------------------------------------\n")
    
    str4_3 <- paste(str4_1, str4_2, sep="\n\n")
    
    if (starting) {
      str4 <- str4_3
      starting <- FALSE
    } else {
      str4 <- paste(str4, str4_3, sep="\n\n\n")
    }
  }
  cat(paste(str3, str4, sep="\n\n\n"))
}

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

run_model <- function(gs, create_fit, train_features, train_labels, test_features, test_labels) {
  # Create the models
  gs <- gs %>% mutate(fit = pmap(gs, create_fit))
  
  # # Create the predictions
  # gs <- gs %>%
  #   mutate(preds = map(fit, compute_preds, test_features))
  # 
  # # Compute the models' accuracy
  # gs <- gs %>%
  #   mutate(test_acc = map_dbl(preds, compute_accuracy, test_labels))
  # 
  # # Compute the models f2 score
  # gs <- gs %>%
  #   mutate(f2_score = map_dbl(preds, compute_f2, test_labels))
  
  # Cross-validation with accuracy metric
  
  
  
  # Include the models confusion matrix
  ######## check the conf matrix existing function
  gs <- gs %>%
    mutate(conf_matrix = map(fit, create_conf_matrix,
                             test_features, test_labels))
  
  # Arrange by optimal metrics
  gs <- gs %>% arrange(desc(f2_score), desc(test_acc))
  
  list("best_model" = gs[1, ],
               "all_models" = gs)
}

create_decision_tree <- function(cp, minsplit, maxdepth, train_features, train_labels) {
  ########## this is where i have to do better, just like clarke
  
  train <- 
  
  rpart(
    M6_ptsd~. , method="class", parms=list(split = "gini"),
    data = balanced_train,
    control=rpart.control(
      cp=cp,
      minsplit=minsplit,
      maxdepth=maxdepth 
    )
  )
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


