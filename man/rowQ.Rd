\name{rowQ}
\alias{rowQ}
\alias{rowMin}
\alias{rowMax}
\alias{rowMedians}
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
}
\value{
 A vector of length equal to the number of rows of the input matrix containing
the requested quantiles.
}
\author{ R. Gentleman }

\seealso{ \code{\link{colSums}} }
\examples{
 data(eset)
 rowMin(eset)
 rowQ(eset, 4)
}
\keyword{ manip }
