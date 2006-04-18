\name{assayData}
\alias{assayData}
\alias{assayData<-}

\title{Retrieve assay data from eSets and exprSets.}
\description{
This generic function accesses assay
data stored in an object derived from the \code{\link{eSet}} or \code{\link{exprSet}} class.
}
\usage{
assayData(object)
assayData(object) <- value
}
\arguments{
  \item{object}{Object derived from class eSet}
  \item{value}{Named list or environment containing one or more matrices with identical dimensions}
}
\value{
  \code{assayData} applied to \code{eSet}-derived classes returns a list
  or environment; applied to \code{exprSet}, the method returns a
  matrix. See classes for details
}
\author{Biocore}

\seealso{\code{\link{exprSet}}, \code{\link{eSet-class}}, \code{\link{ExpressionSet-class}}, \code{\link{SnpSet-class}}}

\keyword{manip}
