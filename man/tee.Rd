\name{tee}
\alias{tee}
\title{Tee: report arguments and results whenever a function is called.}
\usage{
  tee(f, args.do = str, result.do = str)
}
\arguments{
  \item{f}{function to annotate}

  \item{args.do}{function passed list of input arguments.
  If \code{NULL}, do nothing.}

  \item{result.do}{function passed results.  If
  \code{NULL}, do nothing.}
}
\description{
  Inspired by the tee shell command:
  \url{http://en.wikipedia.org/wiki/Tee_(command)}
}
\examples{
f <- function(x) sin(x ^ 2)
integrate(f, 0, pi)
integrate(tee(f, NULL, print), 0, pi)

uniroot(f, c(pi/2, pi))
uniroot(tee(f, NULL, print), c(pi/2, pi))
uniroot(tee(f, print, NULL), c(pi/2, pi))

locs <- remember()
vals <- remember()
uniroot(tee(f, locs, vals), c(pi/2, pi))
plot(sapply(locs, pluck(1)))
plot(sapply(locs, pluck(1)), sapply(vals, pluck(1)))
}

