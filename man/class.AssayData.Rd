\name{AssayData-class}
\docType{class}
\alias{AssayData-class}
\alias{AssayData}

\alias{assayData,AssayData-method}
\alias{combine,AssayData,AssayData-method}
\alias{featureNames,AssayData-method}
\alias{featureNames<-,AssayData-method}
\alias{sampleNames,AssayData-method}
\alias{sampleNames<-,AssayData,ANY-method}
\alias{sampleNames<-,AssayData,list-method}
\alias{storageMode,AssayData-method}
\alias{storageMode<-,AssayData,character-method}

\alias{assayDataNew}
\alias{assayDataValidMembers}

\title{Class "AssayData"}
\description{
  Container class defined as a class union of \code{list} and
  \code{environment}.  Designed to contain one or more matrices of the
  same dimension.
}

\section{Methods}{
  \describe{
    \item{combine}{\code{signature(x = "AssayData", y = "AssayData")}:
      This method uses \code{cbind} to create new AssayData elements that contain the
      samples of both arguments \code{x} and \code{y}.

      Both AssayData arguments to combine must have the same collection of
      elements.  The elements must have identical numbers of rows
      (features). The numerical contents of any columns (samples)
      present in the same element of different AssayData must be
      identical.  The storageMode of the AssayData arguments must be
      identical, and the function returns an AssayData with storageMode
      matching the incoming mode. See also \code{\link{combine,eSet,eSet-method}}}
    \item{featureNames}{\code{signature(object = "AssayData")}}
    \item{featureNames<-}{\code{signature(object = "AssayData", value = "ANY")}:
      Return or set the feature names as a character
        vector.  These are the row names of the AssayData
	elements. \code{value} can be a character or numeric vector; all
	entries must be unique.}
      \item{sampleNames}{\code{signature(object = "AssayData")}}
      \item{sampleNames<-}{\code{signature(object =
        "AssayData", value="ANY")}: Return or set the sample names.  These are the column
      names of the the AssayData elements and the row names of
      \code{phenoData}. \code{value} can be a character or numeric
      vector.}
    \item{storageMode}{\code{signature(object = "AssayData")}}
    \item{storageMode<-}{\code{signature(object = "AssayData",
	value="character")}: Return or set the storage mode for the
      instance. \code{value} can be one of three choices:
      \code{"lockedEnvironment"}, \code{"environment"}, and
      \code{"list"}. Environments offer a mechanism for storing data that
      avoids some of the copying that occurs when using lists.  Locked
      environment help to ensure data integrity.  Note that environments are
      one of the few R objects that are pass-by-reference.  This means that if
      you modify a copy of an environment, you also modify the original. For
      this reason, we recommend using lockedEnvironment whenever possible.
    }
  }
  Additional functions operating on \code{AssayData} include:
  \describe{
    \item{assayData[[name]]}{Select element \code{name} from \code{assayData}.}
    \item{assayDataNew(storage.mode = c("lockedEnvironment",
	"environment", "list"), ...)}{Use \code{storage.mode} to create a
      new list or environment containing the named elements in \code{...}}
    \item{assayDataValidMembers(assayData, required)}{Validate
      \code{assayData}, ensuring that the named elements \code{required}
      are present, matrices are of the same dimension, and \code{featureNames} (rownames) are
      consistent (identical or \code{NULL}) across entries.}
    \item{assayDataElement(object, element)}{See
      \code{\link{eSet-class}}} 
    \item{assayDataElementReplace(object, element, value,
      validate=TRUE)}{
      See \code{\link{eSet-class}}}
    \item{assayDataElementNames(object)}{See \code{\link{eSet-class}}}
  }
}
\author{Biocore}
\seealso{
  \code{\link{eSet-class}}
  \code{\link{ExpressionSet-class}}
}
\keyword{classes}
