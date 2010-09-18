\name{matchpt}
\alias{matchpt}
\docType{methods}

\title{Nearest neighbor search.}
\description{
    Find the nearest neighbors of a set of query points in the same
    or another set of points in an
    n-dimensional real vector space, using the Euclidean distance.
}

\usage{
    matchpt(x, y)
}
\arguments{
  \item{x}{A matrix (or vector) of coordinates.
    Each row represents a point in an \code{ncol(x)}-dimensional real
    vector space.}
  \item{y}{Optional, matrix (or vector) with the same number of columns
    as \code{x}.}
}

\details{
  If \code{y} is provided, the function searches
  for each point in \code{x} its nearest neighbor in \code{y}.
  If \code{y} is missing, it searches
  for each point in \code{x} its nearest neighbor in \code{x},
  excluding that point itself.
  In the case of ties, only the neighbor with the smaller index is given.

  The implementation is simple and of complexity \code{nrow(x)} times
  \code{nrow(y)}. For larger problems, please consider one of the many
  more efficient nearest neighbor search algorithms.
 }
 
\value{
  A \code{data.frame} with two columns and \code{nrow(x)} rows.
  The first column is the index of the nearest neighbor,
  the second column the distance to the nearest neighbor.
  If \code{y} was given, the index is a row number in \code{y},
  otherwise, in \code{x}. The row names of the result are those of \code{x}.
}

\author{
    Oleg Sklyar \email{osklyar@ebi.ac.uk}
}

\examples{
    a <- matrix(c(2,2,3,5,1,8,-1,4,5,6), ncol=2L, nrow=5L)
    rownames(a) = LETTERS[seq_len(nrow(a))]
    matchpt(a)
    b <- c(1,2,4,5,6)
    d <- c(5.3, 3.2, 8.9, 1.3, 5.6, -6, 4.45, 3.32)
    matchpt(b, d)
    matchpt(d, b)
}

\keyword{manip}
\keyword{array}

