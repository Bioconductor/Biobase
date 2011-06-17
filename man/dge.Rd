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

\title{Accessors and generic functions for count datasets}

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
}

\arguments{
  \item{cds}{Object of class \code{CountDataSet} or {ExonCountSet}.}
  \item{normalized}{Logical indicating whether the returned count values
  should be divided by the \code{sizeFactors}.} 
  \item{value}{For \code{dispTable<-}, an object of class ...,
    For \code{sizeFactors<-}, a numeric vectors whose lengths matches.
    the number of samples in \code{cds},
    For \code{conditions<-}, a matrix or a data frame ...}
  \item{...}{Further arguments.}  
}

\details{See DESeq and DEXSeq package vignettes.}
\author{W. Huber, S. Anders}


\keyword{manip}
