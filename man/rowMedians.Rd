\name{rowMedians}
\alias{rowMedians}
\title{ Fast computation of row-wise medians. }
\description{
Fast computation of row-wise medians, but currently
we use only \code{ncol} over 2 regardless.
}
\usage{
rowMedians(imat)
}
\arguments{
  \item{imat}{A matrix containing values of mode \code{double}. }
}
\details{
A call to \code{rPsort} is used.
}
\value{
A vector, of length \code{nrow(imat)} containing the row-wise 
medians.
}
\author{R. Gentleman }

\seealso{ \code{\link{rowSums}}, \code{rowMeans} }
\examples{
  set.seed(123)
  x = matrix(rnorm(25), nr=5)
  rowMedians(x)
  ##compare with
  apply(x, 1, median)
}
\keyword{ manip }
