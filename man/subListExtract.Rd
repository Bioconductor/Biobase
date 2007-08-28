\name{subListExtract}
\alias{subListExtract}

\title{Extract the same element from the sublists of a list}
\description{
  Given a list of lists, this function can be used to extract a named
  element from each sublist.
}
\usage{
subListExtract(L, name, simplify = FALSE, keep.names = TRUE)
}

\arguments{
  \item{L}{A list of named lists}
  \item{name}{The name of the element in the sublists that should be
    extracted.  This should be a length one character vector.}
  \item{simplify}{When \code{TRUE}, the return value will be an atomic
  vector.  If any extracted sublist value has length not equal to one
  and \code{simplify=TRUE}, an error will be raised.  When \code{FALSE},
  a list is returned containing the extracted elements.}
  \item{keep.names}{If \code{TRUE} (default), the names of \code{L} will
  be attached to the returned vector.}
}
\details{
  This function is implemented in C and is intended to be faster than
  calling \code{lapply} or \code{sapply}.
}
\value{
  If \code{simplify=FALSE}, a list will be returned having the same
  length as \code{L}, but with each element containing the
  element named \code{name} from the corresponding inner list of
  \code{L}.

  When \code{simplify=TRUE}, an atomic vector will be returned
  containing the extracted elements.  If any of the inner list elements
  do not have length one or cannot be put inside an atomic vector, an
  error will be raised.
}
\author{Seth Falcon}
\examples{
list_size = 500000
innerL = list(foo="foo", bar="bar")
L = rep(list(innerL), list_size)

system.time({j0 = sapply(L, function(x) x$foo)})
system.time({j1 = subListExtract(L, "foo", simplify=TRUE)})
stopifnot(all.equal(j0, j1))

LS = L[1:3]
names(LS) = LETTERS[1:3]
subListExtract(LS, "bar", simplify=TRUE)
subListExtract(LS, "bar", simplify=FALSE)
subListExtract(LS, "bar", simplify=TRUE, keep.names=FALSE)
}

\keyword{manip}

