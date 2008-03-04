\name{data:geneData}
\alias{data:geneData}
\alias{geneData}
\alias{data:geneCov}
\alias{geneCov}
\alias{data:geneCovariate}
\alias{geneCovariate}
\alias{data:seD}
\alias{seD}
\docType{data}
\title{Sample expression matrix and phenotype data.frames.}
\usage{data(geneData)}
\description{
The \code{geneData} data.frame has 500 rows and 26 columns.  It consists
of a subset of real expression data from an Affymetrix U95v2 chip. The
data are anonymous. The covariate data \code{geneCov} and
\code{geneCovariate} are made up. The standard error data \code{seD} is
also made up.
}
\format{
  A 500 by 26 data frame.
}
\source{
  The J. Ritz Laboratory (S. Chiaretti).
}
\examples{
data(geneData)
data(geneCovariate)
data(seD)
}
\keyword{datasets}
