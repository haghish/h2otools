#' @title getPerfMatrix
#' @description retrieve performance matrix for all thresholds
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom h2o h2o.precision h2o.recall
#' @importFrom curl curl
#' @param perf a h2o object of class \code{"H2OBinomialMetrics"} which is provided
#'             by 'h2o.performance' function.
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
#' # get the performance matrix for all thresholds
#' getPerfMatrix(perf)
#' }
#' @export

getPerfMatrix <- function(perf) {
  return(perf@metrics$thresholds_and_metric_scores)
}
