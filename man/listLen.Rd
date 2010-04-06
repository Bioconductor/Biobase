\name{listLen}
\alias{listLen}

\title{Lengths of list elements}

\description{
  This function returns an integer vector with the length
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

  The current implementation is intended for lists containing vectors
  and the C-level length function is used to determine length.  This
  means no dispatch is done for the elements of the list.  If your
  list contains S4 objects, you should use \code{sapply(x, length)}
  instead.
}

\author{Jeff Gentry and R. Gentleman}

\seealso{\code{\link{sapply}}}

\examples{
  foo = lapply(1:8, rnorm)
  listLen(foo)
}
\keyword{utilities}
