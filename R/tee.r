#' Tee: report arguments and results whenever a function is called.
#'
#' Inspired by the tee shell command:
#' \url{http://en.wikipedia.org/wiki/Tee_(command)}
#'
#' @param f function to annotate
#' @param args.do function passed list of input arguments. If \code{NULL}, do
#'   nothing.
#' @param result.do function passed results.  If \code{NULL}, do nothing. 
#' @export
#' @examples
#' f <- function(x) sin(x ^ 2)
#' integrate(f, 0, pi)
#' integrate(tee(f, NULL, print), 0, pi)
#'
#' uniroot(f, c(pi/2, pi))
#' uniroot(tee(f, NULL, print), c(pi/2, pi))
#' uniroot(tee(f, print, NULL), c(pi/2, pi))
#'
#' locs <- remember()
#' vals <- remember()
#' uniroot(tee(f, locs, vals), c(pi/2, pi))
#' plot(sapply(locs, pluck(1)))
#' plot(sapply(locs, pluck(1)), sapply(vals, pluck(1)))
tee <- function(f, args.do = str, result.do = str) {
  function(...) {
    if (!is.null(args.do)) args.do(list(...))
    res <- f(...)
    if (!is.null(result.do)) result.do(res)
    res
  }
}


#' Creates a function that remembers all invocations.
#'
#' This is useful in conjunction with \code{\link{tee}} to capture all
#' invocations of a function.
#'
#' @return a function that accepts all arguments and returns nothing.
#' @export
#' @examples
#' r <- remember()
#' r
#' r(1)
#' r
#' r(2); r("a"); r(TRUE)
#' r
remember <- function() {
  memory <- list()
  f <- function(...) {
    # Should use doubling strategy for efficiency
    memory <<- append(memory, list(...))
    invisible()
  }
  
  structure(f, class = "remember")
}

#' @S3method print remember
print.remember <- function(x, ...) {
  cat("Remembering...\n")
  str(as.list(x))
}
#' @S3method as.list remember
as.list.remember <- function(x, ...) {
  get("memory", environment(x))
}
