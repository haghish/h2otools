#' @title F-Measure
#' @description Calculates F-Measure for any given value of Beta
#' @importFrom utils setTxtProgressBar txtProgressBar capture.output packageVersion
#' @importFrom h2o h2o.precision h2o.recall
#' @importFrom curl curl
#' @param perf a h2o object of class \code{"H2OBinomialMetrics"} which is provided
#'             by 'h2o.performance' function.
#' @param beta numeric, specifying beta value, which must be higher than zero
#' @param max logical. default is FALSE. if TRUE, instead of providing the F-Measure
#'            for all the thresholds, the highest F-Measure is reported.
#' @return a matrix of F-Measures for different thresholds or the highest F-Measure value
#' @author E. F. Haghish
#'
#' @examples
#'
#' \donttest{
#' library(h2o)
#' h2o.init()
#' prostate_path <- system.file("extdata", "prostate.csv", package = "h2o")
#' prostate <- h2o.importFile(path = prostate_path, header = TRUE)
#' y <- "CAPSULE"
#' prostate[,y] <- as.factor(prostate[,y])  #convert to factor for classification
#' aml <- h2o.automl(y = y, training_frame = prostate, max_runtime_secs = 30)
#'
#' # evaluate the model performance
#' perf <- h2o.performance(aml@leader)
#'
#' # evaluate F-Measure for a Beta = 3
#' Fmeasure(perf, beta = 3, max = TRUE)
#'
#' # evaluate F-Measure for a Beta = 1.5
#' Fmeasure(perf, beta = 1.5, max = TRUE)
#'
#' }
#' @export

Fmeasure <- function(perf, beta = 1, max = FALSE) {
  threshold <- h2o.precision(perf)[,1]
  precision <- h2o.precision(perf)[,2]
  recall    <- h2o.recall(perf)[,2]
  f <- ((1+beta^2)*(precision*recall)) / ((beta^2*precision)+recall)
  if (max) {
    return(max(f, na.rm = TRUE))
  }
  else {
    result <- cbind(threshold, f)
    colnames(result) <- c("threshold", paste0("F",beta))
    return(result)
  }
}
