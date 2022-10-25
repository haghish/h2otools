#' @title kappa
#' @description Calculates kappa for all thresholds
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom h2o h2o.precision h2o.recall
#' @importFrom curl curl
#' @param perf a h2o object of class \code{"H2OBinomialMetrics"} which is provided
#'             by 'h2o.performance' function.
#' @param max logical. default is FALSE. if TRUE, instead of providing the F-Measure
#'            for all the thresholds, the highest F-Measure is reported.
#' @return a matrix of F-Measures for different thresholds or the highest F-Measure value
#' @author E. F. Haghish
#'
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
#' # evaluate F-Measure for a Beta = 3
#' kappa(perf, max = TRUE)
#' }
#' @export

kappa <- function(perf, max = FALSE) {

  kp <- function(TP, FP, TN, FN) {
    total <- TP + FN + FP + TN
    true <- ((TP + FN) * (TP + FP)) / total
    false <- ((FP + TN) * (FN + TN)) / total
    expected     <- (true + false) / total
    observed     <- (TP + TN) / total
    res <- matrix((observed - expected) / (1 - expected), ncol = 1)
    colnames(res) <- "Kappa"
    return(res)
  }

  matrix <- as.data.frame(perf@metrics$thresholds_and_metric_scores)
  kappa <- kp(TP=matrix$tps, FP=matrix$fps, TN=matrix$tns, FN=matrix$fns)
  threshold <- matrix$threshold

  if (max) {
    return(max(kappa, na.rm = TRUE))
  }
  else {
    result <- cbind(threshold, kappa)
    colnames(result) <- c("threshold", "kappa")
    return(result)
  }
}
