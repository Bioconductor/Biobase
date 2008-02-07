\name{phenoData}
\alias{phenoData}
\alias{phenoData<-}
\alias{varLabels}
\alias{varLabels<-}
\alias{varMetadata}
\alias{varMetadata<-}
\alias{pData}
\alias{pData<-}

\title{Retrieve information on experimental phenotypes recorded in
  eSet and ExpressionSet-derived classes.}
\description{
  These generic functions access the phenotypic data (e.g., covariates)
  and meta-data (e.g., descriptions of covariates) associated with an
  experiment. 
}
\usage{
phenoData(object)
phenoData(object) <- value
varLabels(object)
varLabels(object) <- value
varMetadata(object)
varMetadata(object) <- value
pData(object)
pData(object) <- value
}
\arguments{
  \item{object}{Object, possibly derived from \code{\link{eSet-class}}
    or \code{\linkS4class{AnnotatedDataFrame}}.}
  \item{value}{Value to be assigned to corresponding object.}
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

\seealso{\code{\link{eSet-class}}, \code{\link{ExpressionSet-class}}, 
  \code{\link{SnpSet-class}}}

\keyword{manip}
