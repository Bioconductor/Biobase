\name{rowQ}
\alias{rowQ}
\alias{rowQ,matrix,numeric-method}
\alias{rowQ,ExpressionSet,numeric-method}
\alias{rowQ,exprSet,numeric-method}
\alias{rowMin}
\alias{rowMax}
\alias{rowMedians}
\alias{rowMedians,matrix-method}
\alias{rowMedians,ExpressionSet-method}
\alias{rowMedians,exprSet-method}

\title{A function to compute empirical row quantiles.}
\description{
 This function computes the requested quantile for each row of a matrix,
 or of an \code{exprSet}.
}
\usage{
rowQ(imat, which)
rowMax(imat)
rowMin(imat)
rowMedians(imat)
}
\arguments{
  \item{imat}{Either a matrix or an \code{exprSet}. }
  \item{which}{An integer indicating which order statistic should be returned.}
}
\details{
 \code{rowMax}, \code{rowMin} and \code{rowMedians} simply call \code{rowQ}
with the appropriate argument set. 

 The argument \code{which} takes values between 1, for the minimum per row,
 and \code{ncol(imat)}, for the maximum per row.

}
\value{
 A vector of length equal to the number of rows of the input matrix containing
the requested quantiles.
}
\author{ R. Gentleman }

\seealso{ \code{\link{colSums}} }
\examples{
 data(sample.exprSet)
 rowMin(sample.exprSet)
 rowQ(sample.exprSet, 4)
}
\keyword{ manip }
