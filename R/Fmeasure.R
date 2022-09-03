


Fmeasure <- function(perf, beta, max = FALSE) {
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
