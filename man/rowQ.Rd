\name{rowQ}
\alias{rowQ}
\alias{rowQ,matrix,numeric-method}
\alias{rowQ,ExpressionSet,numeric-method}
\alias{rowMin}
\alias{rowMax}

\title{A function to compute empirical row quantiles.}
\description{
 This function computes the requested quantile for each row of a matrix,
 or of an \code{ExpressionSet}.
}
\usage{
rowQ(imat, which)
rowMax(imat)
rowMin(imat)
}
\arguments{
  \item{imat}{Either a matrix or an \code{ExpressionSet}.}
  \item{which}{An integer indicating which order statistic should be returned.}
}
\details{
 \code{rowMax} and \code{rowMin} simply call \code{rowQ}
with the appropriate argument set. 

 The argument \code{which} takes values between 1, for the minimum per row,
 and \code{ncol(imat)}, for the maximum per row.

}
\value{
 A vector of length equal to the number of rows of the input matrix containing
the requested quantiles.
}
\author{R. Gentleman}

\seealso{
 \code{\link{rowMedians}}.
 \code{rowMeans()} in \code{\link[base]{colSums}}().
}

\examples{
 data(sample.ExpressionSet)
 rowMin(sample.ExpressionSet)
 rowQ(sample.ExpressionSet, 4)
}
