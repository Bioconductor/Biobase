\name{data:sample.MultiSet}
\alias{data:sample.MultiSet}
\alias{sample.MultiSet}
\docType{data}
\title{Data set of class 'MultiSet'}
\usage{data(sample.MultiSet)}
\format{
  The data for 4 cases, labeled a to d and 500 genes.
  Each case has five covariates: SlideNumber: number; FileName: name;
  Cy3: genotype labeled Cy3; Cy5: genotype labeled Cy5; Date: date.
}
\description{
 The expression data are real but anonymized. The data are from an
experiment that used Affymetrix U95v2 chips. The data were processed by
dChip and then exported to R for analysis.

The phenoData, standard error estimates, and description data are fake.
}
\examples{
data(sample.MultiSet)
}
\keyword{datasets}
