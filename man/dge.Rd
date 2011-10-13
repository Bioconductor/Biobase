\name{dge}
\alias{conditions}
\alias{conditions<-}
\alias{design}
\alias{design<-}
\alias{counts}
\alias{counts<-}
\alias{dispTable}
\alias{dispTable<-}
\alias{sizeFactors}
\alias{sizeFactors<-}
\alias{estimateSizeFactors}
\alias{estimateDispersions}

\title{Accessors and generic functions used in the context of count datasets}

\description{These generic functions provide basic interfaces to operations on
and data access to count datasets.}

\usage{
counts(cds, normalized=FALSE)
counts(cds) <- value
dispTable(cds)
dispTable(cds)  <- value
sizeFactors(cds)
sizeFactors(cds) <- value
conditions(cds, ...)
conditions(cds) <- value
design(cds, ...)
design(cds) <- value
estimateSizeFactors(cds, ...)
estimateDispersions(cds, ...)
}

\arguments{
  \item{cds}{Object of class \code{CountDataSet} or \code{ExonCountSet}.}
  \item{normalized}{Logical indicating whether the returned count values
  should be divided by the \code{sizeFactors}.}
  \item{value}{For \code{dispTable<-}, an object of class ...,
    For \code{sizeFactors<-}, a numeric vectors whose lengths matches.
    the number of samples in \code{cds},
    For \code{conditions<-}, a matrix or a data frame ...}
  \item{...}{Further arguments.}
}

\details{For the details, please consult the manual pages of the
  methods in the \code{DESeq} and \code{DEXSeq} packages and the package
  vignettes.}

\author{W. Huber, S. Anders}

\keyword{manip}
