\name{data:sample.ExpressionSet}
\alias{data:sample.ExpressionSet}
\alias{sample.ExpressionSet}
\docType{data}
\title{Dataset of class 'ExpressionSet'}
\usage{data(sample.ExpressionSet)}
\format{
  The data for 26 cases, labeled A to Z and 500 genes.
  Each case has three covariates: sex (male/female); type
  (case/control); and score (testing score). 
}
\description{
The expression data are real but anonymized. The data are from an
experiment that used Affymetrix U95v2 chips. The data were processed by
dChip and then exported to R for analysis.

The data illustrate \code{\link{ExpressionSet-class}}, with
\code{assayData} containing the required matrix element \code{exprs} and
an additional matrix \code{se.exprs}. \code{se.exprs} has the same
dimensions as \code{exprs}.

The phenoData and standard error estimates (se.exprs) are made up.  
The information in the "description" slot is fake.  
}
\examples{
data(sample.ExpressionSet)
}
\keyword{datasets}
