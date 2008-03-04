\name{featureData}
\alias{featureData}
\alias{featureData<-}
\alias{fData}
\alias{fData<-}
\alias{fvarLabels}
\alias{fvarLabels<-}
\alias{fvarMetadata}
\alias{fvarMetadata<-}

\title{Retrieve information on features recorded in eSet-derived
  classes.}
\description{
  These generic functions access feature data (experiment specific
  information about features) and feature meta-data (e.g., descriptions
  of feature covariates).  
}
\usage{
featureData(object)
featureData(object) <- value
fData(object)
fData(object) <- value
fvarLabels(object)
fvarLabels(object) <- value
fvarMetadata(object)
fvarMetadata(object) <- value
}
\arguments{
  \item{object}{Object, possibly derived from \code{\link{eSet-class}}
    or \code{\link{AnnotatedDataFrame-class}}.}
  \item{value}{Value to be assigned to corresponding object.}
}
\value{
  \code{featureData} returns an object containing information on both
  variable values and variable meta-data.  \code{fvarLabels} returns a
  character vector of measured variable names. \code{fData} returns a data
  frame with features as rows, variables as columns. \code{fvarMetadata}
  returns a data frame with variable names as rows, description tags
  (e.g., unit of measurement) as columns.
}

\author{Biocore}

\seealso{
  \code{\linkS4class{eSet}},
  \code{\linkS4class{ExpressionSet}}
}
\keyword{manip}
