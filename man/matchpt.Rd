\name{matchpt}

\alias{matchpt}

\docType{function}

\title{Finds Nearest Neighbours for Sets of Points given by NxD Coordinates}
\description{
    Finds nearest neighbours (using Euclidian distance) for sets of points given by coordinates. Can be used to match
    one dataset agains another or to locate nearest neighbours within a single dataset.
}

\usage{
    matchpt(x, y = NULL)
}
\arguments{
    \item{x}{A matrix (or a vector) of coordinates. Rows represent different points in ND space, where N is
    the number of columns, and thus coordinates. If argument y is not given, match will be performed within x-dataset.}
    \item{y}{If given a matrix (or a vector) of the same space (same N), that could however have different number
    of points. If given this argument is used to match x against it.}
}

\value{
    A matrix of 2 columns and n rows, where n is the number of points in argument x. First column - index of the nearest
    neighbour, second column - distance to the nearest neighbour. If y is given, index corresponds to the index in y-dataset, if
    only x is given, then in x.
}

\source{
    The C code for this routine is available in \code{src/matchpt.c} of this package.
}

\references{
    Oleg Sklyar, EBI 2006
}

\author{
    Oleg Sklyar, \email{osklyar@ebi.ac.uk}
}

\examples{
    a <- matrix(c(2,2,3,5,1,8,-1,4,5,6), ncol = 2, nrow = 5)
    matchpt(a)
    b <- c(1,2,4,5,6)
    d <- c(5.3, 3.2, 8.9, 1.3, 5.6, -6, 4.45, 3.32)
    matchpt(b, d)
    matchpt(d, b)
}

\keyword{manip}
\keyword{array}

