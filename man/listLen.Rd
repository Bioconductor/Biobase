\name{listLen}
\alias{listLen}

\title{Lengths of list elements}

\description{
  This functions returns an integer vector with the length
  of the elements of its argument, which is expected to be a list.
}

\usage{
listLen(x)
}

\arguments{
  \item{x}{A list}
}

\details{
  This function returns a vector of the same length as
  the list \code{x} containing the lengths of each element.
}

\author{Jeff Gentry and R. Gentleman}

\seealso{\code{\link{sapply}}}

\examples{
  poo = lapply(1:8, rnorm)
  listLen(poo)
}
\keyword{utilities}
