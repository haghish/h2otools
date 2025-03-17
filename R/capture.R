#' Capture Evaluation Side Effects
#'
#' This function evaluates an R expression while capturing its printed output,
#' messages, warnings, and errors. It returns a list containing the result of
#' the evaluation along with all the captured texts.
#'
#' @param expr An R expression to evaluate. The expression is captured using
#'   \code{substitute()} to retain its code form before evaluation.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{value}{The result of evaluating \code{expr}.}
#'   \item{output}{A character vector with the printed output produced during evaluation.}
#'   \item{messages}{A character vector with any messages generated during evaluation.}
#'   \item{warnings}{A character vector with any warnings produced during evaluation.}
#'   \item{error}{A character string with the error message if an error occurred; otherwise \code{NULL}.}
#' }
#'
#' @details
#' The function uses \code{withCallingHandlers()} and \code{tryCatch()} to capture
#' side effects of evaluating the expression. Printed output is captured using
#' \code{capture.output()}. Warnings and messages are intercepted and their default
#' display is suppressed using \code{invokeRestart("muffleWarning")} and
#' \code{invokeRestart("muffleMessage")}, respectively. If an error occurs, its
#' message is stored and \code{NULL} is returned as the value.
#'
#' @importFrom utils capture.output
# @importFrom base substitute
# @importFrom base withCallingHandlers
# @importFrom base tryCatch
# @importFrom base conditionMessage
# @importFrom base eval
# @importFrom base parent.frame
# @importFrom base invokeRestart
#'
#' @examples
#' \dontrun{
#'   # Example: Capturing output, messages, warnings, and errors
#'   captured <- capture({
#'     print("Hello, world!")
#'     message("This is a message.")
#'     warning("This is a warning.")
#'     42  # Final value returned
#'   })
#'
#'   # Display the captured components
#'   print(captured$output)    # Printed output
#'   print(captured$messages)   # Messages
#'   print(captured$warnings)   # Warnings
#'   print(captured$error)      # Error message (if any)
#'   print(captured$value)      # The evaluated result (42 in this example)
#' }
#'
#' @export

capture <- function(expr) {
  # Capture the expression as code
  expr <- substitute(expr)

  # Initialize storage for console outputs, messages, warnings, and errors
  output_text <- character(0)
  message_text <- character(0)
  warning_text <- character(0)
  error_text <- NULL

  # Evaluate the expression while capturing outputs, messages, and warnings
  result <- withCallingHandlers(
    tryCatch({
      # capture.print suppresses all printed output
      output_text <- capture.output({
        res <- eval(expr, envir = parent.frame())
      })
      res
    },
    error = function(e) {
      error_text <<- conditionMessage(e)
      NULL
    }),
    warning = function(w) {
      warning_text <<- c(warning_text, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      message_text <<- c(message_text, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  # Return a list containing the evaluated value and all captured text
  list(
    value = result,
    output = output_text,
    messages = message_text,
    warnings = warning_text,
    error = error_text
  )
}

# # Example usage:
# captured <- capture({
#   print("Hello, world!")
#   message("This is a message.")
#   warning("This is a warning.")
#   cat(42)  # final value returned
# })

