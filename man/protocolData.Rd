\name{protocolData}
\alias{protocolData}
\alias{protocolData<-}
\alias{protocolData<-,eSet,AnnotatedDataFrame-method}

\title{Protocol Metadata}
\description{
This generic function handles methods for adding and retrieving
protocol metadata for the samples in eSets.
}
\usage{
protocolData(object)
protocolData(object) <- value
}
\arguments{
  \item{object}{Object derived from class eSet}
  \item{value}{Object of class AnnotatedDataFrame}
}
\value{
  \code{protocolData(object)} returns an AnnotatedDataFrame containing the
  protocol metadata for the samples.
}
\author{Biocore}

\seealso{\code{phenoData}, \code{\link{AnnotatedDataFrame-class}},
  \code{\link{eSet-class}}, \code{\link{ExpressionSet-class}},
  \code{\link{SnpSet-class}}}

\keyword{manip}
