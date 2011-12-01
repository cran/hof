\name{guard}
\alias{guard}
\title{Modify function to guard against incorrect inputs.}
\usage{
  guard(f, guard, otherwise)
}
\arguments{
  \item{f}{function to guard}

  \item{guard}{a function that returns \code{TRUE} when the
  input is ok and \code{FALSE} when it is not.}

  \item{otherwise}{If specified, the new function will
  return this value when the guard fails. If missing, the
  function will return its input when the guard fails.}
}
\description{
  Currently only guards the first argument to the function.
}
\examples{
guard(log, is.numeric, NA)("asdf")
guard(log, is.numeric)("asdf")
}

