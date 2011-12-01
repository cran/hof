\name{pluck}
\alias{pluck}
\title{Pluck a value out of a list.}
\usage{
  pluck(element)
}
\arguments{
  \item{element}{name, or position, of element to pluck
  out.}
}
\description{
  Useful when you have a list of elements and you want to
  pull one thing out of every element of the list. Inspired
  by
  \url{http://osteele.com/sources/javascript/functional/}.
}
\examples{
cyl <- split(mtcars, mtcars$cyl)
mods <- lapply(cyl, lm, f = mpg ~ disp)
lapply(mods, pluck("coefficients"))
lapply(mods, pluck(2))
}

