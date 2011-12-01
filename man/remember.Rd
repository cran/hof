\name{remember}
\alias{remember}
\title{Creates a function that remembers all invocations.}
\usage{
  remember()
}
\value{
  a function that accepts all arguments and returns
  nothing.
}
\description{
  This is useful in conjunction with \code{\link{tee}} to
  capture all invocations of a function.
}
\examples{
r <- remember()
r
r(1)
r
r(2); r("a"); r(TRUE)
r
}

