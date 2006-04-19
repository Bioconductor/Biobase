\name{featureNames}
\alias{featureNames}
\alias{featureNames<-}
\alias{sampleNames}
\alias{sampleNames<-}
\alias{geneNames}
\alias{geneNames<-}


\title{Retrieve feature and sample names from eSets and exprSets.}
\description{
These generic functions access the feature names (typically, gene or SNP
identifiers) and sample names stored in an object derived from the \code{\link{eSet-class}} or \code{\link{exprSet-class}}.
}
\usage{
featureNames(object)
featureNames(object) <- value
sampleNames(object)
sampleNames(object) <- value
geneNames(object)
geneNames(object) <- value
}
\arguments{
  \item{object}{Object, possibly derived from class \code{eSet}}
  \item{value}{Character vector containing feature or sample names}
}
\value{
  \code{featureNames} and \code{geneNames} return a (usually long!)
  character vector uniquely identifying each
  feature.\code{sampleNames} returns a (usually shorter) character
  vector identifying samples. 
}
\author{Biocore}

\seealso{\code{\link{exprSet-class}}, \code{\link{ExpressionSet-class}}, \code{\link{SnpSet-class}}}

\keyword{manip}
