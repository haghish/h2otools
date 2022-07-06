# R define h2o.performance2 function
# ============================================================
h2o.performance2 <- function(perf, conf, print = TRUE) {
  auc <- perf@metrics$AUC                                # auc
  accuracy <- conf$overall[1]                            # accuracy
  prauc <- perf@metrics$pr_auc                           # aucpr
  f2 <- perf@metrics$max_criteria_and_metric_scores[2,]  # f2
  mcc <- perf@metrics$max_criteria_and_metric_scores[8,] # mcc
  names <- c("aucpr_eval", "f2_eval", "mcc_eval", "kappa", "specificity", "sensitivity","auc","accuracy")
  vals <- unlist(c(prauc, f2[3], mcc[3]))
  vals <- c(vals, conf$overall[2], conf$byClass[2], conf$byClass[1], auc, accuracy)
  names(vals) <- names
  if (print) print(vals)
  return(vals)
}