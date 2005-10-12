\name{data:exSet}
\alias{data:exSet}
\alias{exSet}
\non_function{}
\title{Dataset of class 'exprSet'}
\usage{data(exSet)}
\format{
The data for 26 cases, labeled A to Z and 500 genes.
}
\description{
 The data are real but anonymized. The data are from an experiment that used Affymetrix
 U95v2 chips. The data were processed by dChip and then exported to R
 for analysis.

 \code{exSet} and \code{eset} share the same expression data.  The
 phenoData and standard error estimates (se.exprs) are made up.  

 The information in the "description" slot is fake.  
}
\examples{
data(exSet)
}
\keyword{datasets}
