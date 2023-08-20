#### Choose file
result_file <- '1.RData'


setwd("D:/Κωνσταντίνος Data/Σχολής/Διπλωματική Εργασία/Main/results")
load(result_file)

# Process, group and visualize results
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
    
    for (k in 1:2) {
      grouped_results[[clf]][[metric]] <- append(grouped_results[[clf]][[metric]], final_results[[clf]][[k]][[metric]])
    }
  }
}

aggregate_results <- list()

for (metric in metrics) {
  aggregate_results[[metric]] <- c()
  
  for (clf in classifiers) {
    aggregate_results[[metric]] <- append(aggregate_results[[metric]], sum(grouped_results[[clf]][[metric]]) / length(grouped_results[[clf]][[metric]]))
  }
}

auc_scores <- aggregate_results$auc[1:6]
f2_scores <- aggregate_results$f2[1:6]
aggregate_visualization_results <- visualize(auc_scores, f2_scores, 'Performance using all features')
aggregate_visualization_results$plot

auc_scores_reduced <- aggregate_results$auc[7:12]
f2_scores_reduced <- aggregate_results$f2[7:12]
aggregate_visualization_results_reduced <- visualize(auc_scores_reduced, f2_scores_reduced, 'Performance using reduced features')
aggregate_visualization_results_reduced$plot