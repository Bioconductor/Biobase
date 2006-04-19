\name{AssayData-class}
\docType{class}
\alias{AssayData-class}
\alias{assayData<-,eSet,AssayData-method}
\alias{combine,AssayData,AssayData-method}
\alias{featureNames,AssayData-method}
\alias{featureNames<-,AssayData-method}
\alias{sampleNames,AssayData-method}
\alias{sampleNames<-,AssayData-method}
\alias{storageMode,AssayData-method}
\alias{storageMode<-,AssayData,character-method}

\title{Class "AssayData"}
\description{
  Container class defined as a class union of \code{list} and
  \code{environment}.  Designed to contain one or more matrices of the
  same dimension.
}

\section{Methods}{
  \describe{
    \item{assayData<-}{\code{signature(object = "eSet", value =
        "AssayData")}: Replace the \code{AssayData} slot of a
      \code{eSet} instance.}
    \item{combine}{\code{signature(x = "AssayData", y = "AssayData")}:
      Return a new AssayData instance with corresponding elements
      combined using \code{cbind}.}
    \item{featureNames, featureNames<-}{\code{signature(object =
        "AssayData")}: Return or set the feature names as a character
        vector.  These are the row names of the AssayData elements.}
    \item{sampleNames, sampleNames<-}{\code{signature(object =
        "AssayData")}: Return or set the sample names.  These are the column
      names of the the AssayData elements and the row names of
      \code{phenoData}.}
    \item{storageMode, storageMode<-}{\code{signature(object =
        "AssayData")}: Return or set the storage mode for the instance.
        There are three choices: \code{"lockedEnvironment"},
        \code{"environment"}, and \code{"list"}.  Environments offer a
         mechanism for storing data that avoids some of the copying that
         occurs when using lists.  Locked environment help to ensure
         data integrity.  Note that environments are one of the few R
         objects that are pass-by-reference.  This means that if you
         modify a copy of an environment, you also modify the original.
         For this reason, we recommend using lockedEnvironment whenever
         possible.
    }
  }
}
\author{M.T. Morgan}
\seealso{
  \code{\link{eSet-class}}
  \code{\link{ExpressionSet-class}}
}
\keyword{classes}
