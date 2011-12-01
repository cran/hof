#' Call a function.
#'
#' Useful when you have a list of functions.
#'
#' @param f function to call
#' @param ... other arguments passed on to \code{f}
#' @export
#' @examples 
#' summaries <- list(mean = mean, sum = sum, sd = sd)
#' lapply(summaries, call_fun, runif(100))
call_fun <- function(f, ...) {
  f(...)
}
