#' @title summarizePerformance
#' @description summarizes performance of a model to a vector
#' @author E. F. Haghish
#' @return numeric vector
#' @keywords Internal
#' @noRd

summarizePerformance <- function(perf) {
  # get the performance metrics
  perf <- perf@metrics

  results <- NULL
  varname <- NULL

  if (!is.null(perf$MSE)) {
    MSE <- perf$MSE
    varname <- c(varname, "MSE")
    results <- c(results, MSE)
  }
  if (!is.null(perf$RMSE)) {
    RMSE <- perf$RMSE
    varname <- c(varname, "RMSE")
    results <- c(results, RMSE)
  }
  if (!is.null(perf$r2)) {
    r2 <- perf$r2
    varname <- c(varname, "r2")
    results <- c(results, r2)
  }
  if (!is.null(perf$logloss)) {
    logloss <- perf$logloss
    varname <- c(varname, "logloss")
    results <- c(results, logloss)
  }
  if (!is.null(perf$AUC)) {
    AUC <- perf$AUC
    varname <- c(varname, "AUC")
    results <- c(results, AUC)
  }
  if (!is.null(perf$pr_auc)) {
    AUCPR <- perf$pr_auc
    varname <- c(varname, "AUCPR")
    results <- c(results, AUCPR)
  }
  if (!is.null(perf$Gini)) {
    Gini <- perf$Gini
    varname <- c(varname, "Gini")
    results <- c(results, Gini)
  }
  if (!is.null(perf$mean_per_class_error)) {
    mean_per_class_error <- perf$mean_per_class_error
    varname <- c(varname, "mean_per_class_error")
    results <- c(results, mean_per_class_error)
  }

  names(results) <- varname
  return(results)
}



