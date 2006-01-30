\name{data:sample.exprSet}
\alias{data:sample.exprSet}
\alias{sample.exprSet}
\non_function{}
\title{Dataset of class 'exprSet'}
\usage{data(sample.exprSet)}
\format{
  The data for 26 cases, labeled A to Z and 500 genes.
  Each case has three covariates: sex (male/female); type
  (case/control); and score (testing score). 
}
\description{
 The expression data are real but anonymized. The data are from an
experiment that used Affymetrix U95v2 chips. The data were processed by
dChip and then exported to R for analysis.

The phenoData and standard error estimates (se.exprs) are made up.  
The information in the "description" slot is fake.  
}
\examples{
data(sample.exprSet)
}
\keyword{datasets}
