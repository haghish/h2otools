#' @title Bootstrap Variable Importance And Averaged Grid Variable Importance
#' @description Evaluates variable importance as well as bootstrapped variable
#'              importance for a single model or a grid of models
#' @importFrom h2o as.h2o h2o.performance h2o.getFrame
#' @importFrom boot boot boot.ci
#' @importFrom curl curl
#' @importFrom stats sd
#' @param df dataset for testing the model. if "n" is bigger than 1, this dataset
#'           will be used for drawing bootstrap samples. otherwise (default), the
#'           entire dataset will be used for evaluating the model
#' @param model a model or a model grid of models trained by h2o machine learning software
#' @param metric character. model evaluation metric to be passed to boot R package.
#'               this could be, for example "AUC", "AUCPR", RMSE", etc., depending
#'               of the model you have trained. all evaluation metrics provided
#'               for your H2O models can be specified here.
#' @param n number of bootstraps
#' @return list of mean perforance of the specified metric and other bootstrap results
#' @author E. F. Haghish
#'
#' @examples
#'
#' \dontrun{
#' library(h2o)
#' h2o.init(ignore_config = TRUE, nthreads = 2, bind_to_localhost = FALSE, insecure = TRUE)
#' prostate_path <- system.file("extdata", "prostate.csv", package = "h2o")
#' df <- read.csv(prostate_path)
#'
#' # prepare the dataset for analysis before converting it to h2o frame.
#' df$CAPSULE <- as.factor(df$CAPSULE)
#'
#' # convert the dataframe to H2OFrame and run the analysis
#' prostate.hex <- as.h2o(df)
#' aml <- h2o.automl(y = "CAPSULE", training_frame = prostate.hex, max_runtime_secs = 30)
#'
#' # evaluate the model performance
#' perf <- h2o.performance(aml@leader, xval = TRUE)
#'
#' # evaluate bootstrap performance for the training dataset
#' #    NOTE that the raw data is given not the 'H2OFrame'
#' perf <- bootPerformance(model = aml@leader, df = df, metric = "RMSE", n = 500)
#' }
#' @export

bootImportance <- function(model, df, metric, n = 100) {
  # the first argument to statistic must be the data
  # Any further arguments can be passed to statistic through the ... argument
  # Do not forget to implement the indices!
  statistic <- function(df = df, metric = metric, indices, ...) {
    newdata <- h2o::as.h2o(df[indices, ], #subsample from the bootstrap data
                           destination_frame = "bootPerformance_df")
    Sys.sleep(.1)
    perf <- h2o::h2o.performance(model = model, newdata = h2o::h2o.getFrame("bootPerformance_df"))
    perf <- summarizePerformance(perf)
    return(perf[metric])
  }
  results <- boot::boot(df, statistic = statistic, R = n, #stype = "i", sim = "ordinary",
                        model = model, metric = metric # ... argument
  )

  mean <- mean(results$t)
  ci   <- boot::boot.ci(results, type = "norm")
  names(ci$normal) <- c("ci", "low", "high")
  cat("\nmean (CI) =", mean, "(", ci$normal[1,2:3], ")\n\n")

  return(list(mean = mean,
              ci = ci$normal,
              sd = sd(results$boot$t),
              boot = results))
}

#res <- bootPerformance(model = aml@leader, df = df, metric = "AUC", n = 100)
