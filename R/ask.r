#' Apply logical predicate to vector.
#'
#' This vectorises logical predicates so that they return a logical vector
#' when applied to a list.
#'
#' Thanks to Bill Venables for the suggestion of the name "Ask".
#'
#' @param f function to apply
#' @param x vector to test
#' @param ... other arguments passed on to \code{f}
#' @return a logical vector with length equal to \code{length(x)}
#' @export
#' @examples
#' elements <- list(1:10, c(-1, 10), c(TRUE, FALSE), letters)
#' \dontrun{results <- lapply(elements, log)}
#' results <- lapply(elements, function(x) try(log(x)))
#' 
#' is.error <- function(x) inherits(x, "try-error")
#' 
#' # Just to get the successful ones
#' successful <- Filter(Negate(is.error), results)
#' 
#' # Which ones failed?
#' failures <- vapply(results, is.error, logical(1))
#' failures <- Ask(is.error, results)
Ask <- function(f, x, ...) {
  ind <- vapply(x, f, logical(1), ...)
  na.false(ind)
}

na.false <- function(x) { 
  !is.na(x) & x
}

# Can now rewrite Filter
Filter <- function(f, x, ...) {
  x[Ask(f, x, ...)]
}
