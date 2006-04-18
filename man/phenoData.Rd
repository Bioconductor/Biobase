\name{phenoData}
\alias{phenoData}
\alias{phenoData<-}
\alias{varLabels}
\alias{varLabels<-}
\alias{varMetadata}
\alias{pData<-}

\title{Retrieve information on experimental phenotypes recorded in
  eSet and exprSet-derived classes.}
\description{
These generic functions access the phenotypic data (e.g., covariates)
and meta-data (e.g., descriptions of covariates associated with an experiment.
}
\usage{
phenoData(x)
phenoData(x) <- new("AnnotatedDataFrame")
varLabels(x)
varLabels(x) <- character(length(varLabels(x)))
varMetadata(x)
varMetadata(x) <- new("data.frame")
pData(x)
pData(x) <- new("data.frame")
}
\arguments{
  \item{x}{Object, possibly derived from class \code{eSet}}
}
\value{
  \code{phenoData} returns an object containing information on both
  variable values and variable meta-data.  \code{varLabels} returns a
  character vector of measured variables. \code{pData} returns a data
  frame with samples as rows, variables as columns. \code{varMetadata}
  returns a data frame with variable names as rows, description tags
  (e.g., unit of measurement) as columns.
}
\author{Biocore}

\seealso{\code{\link{exprSet}}, \code{\link{ExpressionSet-class}}, \code{\link{SnpSet-class}}}

\keyword{manip}
