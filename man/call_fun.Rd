\name{call_fun}
\alias{call_fun}
\title{Call a function.}
\usage{
  call_fun(f, ...)
}
\arguments{
  \item{f}{function to call}

  \item{...}{other arguments passed on to \code{f}}
}
\description{
  Useful when you have a list of functions.
}
\examples{
summaries <- list(mean = mean, sum = sum, sd = sd)
lapply(summaries, call_fun, runif(100))
}

