\name{phenoData-class}
\docType{class}
\alias{phenoData-class}
\alias{$.phenoData}
\alias{phenoData<-,pheno}
\alias{[,phenoData-method}
\alias{pData,phenoData-method}
\alias{pData<-,phenoData-method}
\alias{show,phenoData-method}
\alias{split,phenoData,vector-method}
\alias{varLabels,phenoData-method}
\alias{[[,phenoData-method}
\alias{[[<-,phenoData-method}
\alias{convertVarLabels,phenoData-method}
\alias{getUnits,phenoData,character-method}
\alias{getVarMetadata,phenoData,character,character-method}
\alias{addVarMetadataEntry,phenoData,character,character-method}
\alias{getVarMetadata,phenoData,character,missing-method}
\alias{varMetadata,phenoData-method}
\alias{addVarMetadataEntry}
\alias{convertVarLabels}
\alias{getUnits} 
\alias{getVarMetadata}
\alias{varMetadata}
\title{Class phenoData, a class for patient or experiment level data. }
\description{ The class consists of a \code{data.frame} and some
  accompanying methods suited to handle patient level data for microarrays.}
\section{Creating Objects}{
\code{  new('phenoData',}\cr
\code{    pData     = ...., # Object of class \code{data.frame}}\cr
\code{    varLabels = ...., # Object of class \code{list}}\cr
\code{    varMetadata = ...., # Object of class \code{data.frame}}\cr
\code{  )}}
\section{Slots}{
  \describe{
    \item{\code{pData}:}{Object of class "data.frame". The data. }
    \item{\code{varLabels}:}{Object of class "list". Obtional long
      labels for the different variables. }
    \item{\code{varMetadata}:}{Object of class "data.frame". A
		data frame with a column named varNames and
		arbitrarily many additional columns. }
  }
}

\section{Methods}{
  \describe{
    \item{[}{(phenoData): A subset operator. }
    \item{pData}{(phenoData): An accessor function for \code{pData}.}
    \item{show}{(phenoData): A method to display the data in a nice manner. }
    \item{varLabels}{(phenoData): A method to display the variable
      labels. }
    \item{split}{(phenoData, vector): A method to \code{split}
      \code{pData} and return a list of \code{phenoData} objects. Unlike
      \code{split} this function requires the number of rows of
      \code{pData} to be a multiple of the length of \code{vector}.}
    \item{\$}{An old-style method.
      It is \code{pData(object)[[as.character(val)]]} which
      does not quite have the right semantics but it is close. This
      operator extracts the named component of the \code{pData} slot in
      \code{phenoData}.}
    \item{pData<-}{(phenoData): A replacement method for the \code{pData}
      slot.}
    \item{phenoData<-}{(exprSet): A replacement method for the
      \code{phenoData} slot.}
  }
}
\examples{
data(eset)
pes <- phenoData(eset)
pes <- convertVarLabels(pes)
varMetadata(pes)
pes <- addVarMetadataEntry(pes, "cov1", "units", "inches")
getUnits(pes, "cov1")
}
\keyword{methods}
