#' Modify function to guard against incorrect inputs.
#'
#' Currently only guards the first argument to the function.
#'
#' @param f function to guard
#' @param guard a function that returns \code{TRUE} when the input is ok
#'   and \code{FALSE} when it is not.
#' @param otherwise If specified, the new function will return this value 
#'   when the guard fails. If missing, the function will return its input
#'   when the guard fails.
#' @export
#' @examples
#' guard(log, is.numeric, NA)("asdf")
#' guard(log, is.numeric)("asdf")
guard <- function(f, guard, otherwise) {
  f <- match.fun(f)
  g <- match.fun(guard)
  other_missing <- missing(otherwise)
  
  function(x, ...) {
    if (guard(x)) {
      f(x, ...)
    } else {
      if (other_missing) {
        x
      } else {
        otherwise        
      }
    }
  }  
}

#' Modify function to return default value on failure.
#'
#' @param default default value to return if \code{f} errors
#' @param f function to call
#' @param quiet if \code{TRUE} suppresses all output
#' @return a function with arguments \code{...}
#' @export
#' @examples
#' failwith(NA, log)("asdf")
#' failwith(NA, log, quiet = TRUE)("asdf")
failwith <- function(default = NULL, f, quiet = FALSE) {
  f <- match.fun(f)
  function(...) {
    result <- default
    try(result <- f(...), silent = quiet)
    result
  }
}
