#' @title provides performance measures using objects from h2o
#' @description takes h2o performance object of class "H2OBinomialMetrics"
#'              alongside caret confusion matrix and provides different
#'              model performance measures supported by h2o and caret
#' @param perf h2o performance object of class "H2OBinomialMetrics"
#' @return numeric vector
#' @author E. F. Haghish
#' @examples
#'
#' \dontrun{
#' library(h2o)
#' h2o.init(ignore_config = TRUE, nthreads = 2, bind_to_localhost = FALSE, insecure = TRUE)
#' prostate_path <- system.file("extdata", "prostate.csv", package = "h2o")
#' prostate <- h2o.importFile(path = prostate_path, header = TRUE)
#' y <- "CAPSULE"
#' prostate[,y] <- as.factor(prostate[,y])  #convert to factor for classification
#' aml <- h2o.automl(y = y, training_frame = prostate, max_runtime_secs = 30)
#'
#' # evaluate the model performance
#' perf <- h2o.performance(aml@leader, xval = TRUE)
#'
#' # compute more performance measures
#' performance(perf)
#'
#' }
#' @export


performance <- function(perf) {
  auc <- perf@metrics$AUC                                # auc
  mean_per_class_error <- perf@metrics$mean_per_class_error # mpce
  prauc <- perf@metrics$pr_auc                           # aucpr
  f2 <- perf@metrics$max_criteria_and_metric_scores[2,]  # f2
  mcc <- perf@metrics$max_criteria_and_metric_scores[8,] # mcc
  kappa <- kappa(perf = perf, max=TRUE)
  names <- c("aucpr", "f2", "mcc", "kappa","auc","mean_per_class_error")
  vals <- unlist(c(prauc, f2[3], mcc[3]))
  vals <- c(vals, kappa, auc, mean_per_class_error)
  names(vals) <- names
  return(vals)
}




# # R define h2o.performance2 function (old version)
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
