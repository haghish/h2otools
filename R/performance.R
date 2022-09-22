#' @title provides performance measures using objects from h2o and caret package
#' @description takes h2o performance object of class "H2OBinomialMetrics"
#'              alongside caret confusion matrix and provides different
#'              model performance measures supported by h2o and caret
#' @param perf h2o performance object of class "H2OBinomialMetrics"
#' @param conf caret confusion matrix object.
#' @param print logical. if TRUE, results are printed in the console
#' @return numeric vector
#' @author E. F. Haghish
#' @export

# # R define h2o.performance2 function
# # ============================================================
# performance <- function(perf, conf, print = TRUE) {
#   auc <- perf@metrics$AUC                                # auc
#   accuracy <- conf$overall[1]                            # accuracy
#   prauc <- perf@metrics$pr_auc                           # aucpr
#   f2 <- perf@metrics$max_criteria_and_metric_scores[2,]  # f2
#   mcc <- perf@metrics$max_criteria_and_metric_scores[8,] # mcc
#   names <- c("aucpr_eval", "f2_eval", "mcc_eval", "kappa", "specificity", "sensitivity","auc","accuracy")
#   vals <- unlist(c(prauc, f2[3], mcc[3]))
#   vals <- c(vals, conf$overall[2], conf$byClass[2], conf$byClass[1], auc, accuracy)
#   names(vals) <- names
#   if (print) cat(vals, "\n")
#   return(vals)
# }

performance <- function(perf) {
  auc <- perf@metrics$AUC                                # auc
  mean_per_class_error <- perf@metrics$mean_per_class_error                            # accuracy
  prauc <- perf@metrics$pr_auc                           # aucpr
  f2 <- perf@metrics$max_criteria_and_metric_scores[2,]  # f2
  mcc <- perf@metrics$max_criteria_and_metric_scores[8,] # mcc
  names <- c("aucpr_eval", "f2_eval", "mcc_eval", "auc","mean_per_class_error")
  vals <- unlist(c(prauc, f2[3], mcc[3]))
  vals <- c(vals, auc, mean_per_class_error)
  names(vals) <- names
  return(vals)
}
