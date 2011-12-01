\name{failwith}
\alias{failwith}
\title{Modify function to return default value on failure.}
\usage{
  failwith(default = NULL, f, quiet = FALSE)
}
\arguments{
  \item{default}{default value to return if \code{f}
  errors}

  \item{f}{function to call}

  \item{quiet}{if \code{TRUE} suppresses all output}
}
\value{
  a function with arguments \code{...}
}
\description{
  Modify function to return default value on failure.
}
\examples{
failwith(NA, log)("asdf")
failwith(NA, log, quiet = TRUE)("asdf")
}

