\name{data:geneData}
\alias{data:geneData}
\alias{geneData}
\alias{geneCovariate}
\alias{seD}
\concept{geneCov}
\concept{geneCovariate}
\concept{seD}
\non_function{}
\title{Sample expression matrix and phenotype data.frames.}
\usage{data(geneData)}
\description{

The \code{geneData} data frame has 500 rows and 26 columns.  It consists
of a subset of real expression data from an Affymetrix U95v2 chip. The
data are anonymous. The covariate data \code{geneCov} and
\code{geneCovariate} are made up; \code{geneCovariate} is used for
creating \code{sample.exprSet}.  The standard error data \code{seD} is
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
