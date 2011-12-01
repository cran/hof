\name{Ask}
\alias{Ask}
\title{Apply logical predicate to vector.}
\usage{
  Ask(f, x, ...)
}
\arguments{
  \item{f}{function to apply}

  \item{x}{vector to test}

  \item{...}{other arguments passed on to \code{f}}
}
\value{
  a logical vector with length equal to \code{length(x)}
}
\description{
  This vectorises logical predicates so that they return a
  logical vector when applied to a list.
}
\details{
  Thanks to Bill Venables for the suggestion of the name
  "Ask".
}
\examples{
elements <- list(1:10, c(-1, 10), c(TRUE, FALSE), letters)
\dontrun{results <- lapply(elements, log)}
results <- lapply(elements, function(x) try(log(x)))

is.error <- function(x) inherits(x, "try-error")

# Just to get the successful ones
successful <- Filter(Negate(is.error), results)

# Which ones failed?
failures <- vapply(results, is.error, logical(1))
failures <- Ask(is.error, results)
}

