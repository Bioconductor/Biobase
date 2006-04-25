\name{data:sample.eSet}
\alias{data:sample.eSet}
\alias{sample.eSet}
\non_function{}
\title{(Deprecated) dataset of class 'eSet'}
\usage{data(sample.eSet)}
\format{
  The data for 4 cases, labeled a to d and 500 genes.
  Each case has five covariates: SlideNumber: number; FileName: name;
  Cy3: genotype labelled Cy3; Cy5: genotype labelled Cy5; Date: date.
}
\description{
This deprecated data set can be updated to a
\code{\link{MultiSet-class}} using \code{\link{updateOldESet}}.

 The expression data are real but anonymized. The data are from an
experiment that used Affymetrix U95v2 chips. The data were processed by
dChip and then exported to R for analysis.

The phenoData and standard error estimates (se.exprs) are made up.  
The information in the "description" slot is fake.  
}
\examples{
data(sample.eSet)
updateOldESet(sample.eSet,"MultiSet")
}
\keyword{datasets}
