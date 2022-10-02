#' @title check input data.frame
#' @description checks the class of the input data.frame, makes sure that
#'              the specified 'df' is indeed a data.frame and more over,
#'              there is no column with class 'character' or 'ordered' in the
#'              data.frame. this function helps you ensure that your data is
#'              compatible with h2o R package.
#' @param df data.frame object to evaluate
#' @param ignore character vector of column names that should be ignored, if any.
#' @param is.df logical. if TRUE, it examines if the 'df' is 'data.frame'
#' @param no.char logical. if TRUE, it examines if the 'df' has any columns of class 'character'
#' @param no.ordered logical. if TRUE, it examines if the 'df' has any columns of class 'ordered' factors
#' @return nothing
#' @author E. F. Haghish
#'
#' @examples
#' data(cars)
#'
#' # no error is expected because 'cars' dataset does not
#' # have 'ordered' or 'character' columns
#' checkFrame(cars)
#' @export

checkFrame <- function(df,
                       ignore = NULL,
                       is.df=TRUE,
                       no.char=TRUE,
                       no.ordered=TRUE) {

  # Do not take ignored columns into account
  if (!is.null(ignore)) df <- df[, !ignore]

  if (is.df & !inherits(df, "data.frame")) {
    stop("'df' is not of class 'data.frame'")
  }

  if (no.char) {
    cols <- sapply(df, class)
    if("character" %in% cols) stop("'df' includes 'character' column(s)")
  }

  if (no.ordered) {
    for (i in 1:ncol(df)) {
      if (inherits(df[, i], "ordered")) {
        stop("'df' includes 'ordered factor' column(s)")
      }
    }
  }
}


