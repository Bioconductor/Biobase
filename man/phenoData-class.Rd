\name{phenoData-class}
\docType{class}
\alias{phenoData-class}
\title{Class phenoData, a class for patient or experiment level data. }
\description{ The class consists of a \code{data.frame} and some
  accompanying methods suited to handle patient level data for microarrays.}
\section{Creating Objects}{
\code{  new('phenoData',}\cr
\code{    pData     = ...., # Object of class \code{data.frame}}\cr
\code{    varLabels = ...., # Object of class \code{list}}\cr
\code{  )}}
\section{Slots}{
  \describe{
    \item{\code{pData}:}{Object of class "data.frame". The data. }
    \item{\code{varLabels}:}{Object of class "list". Obtional long
      labels for the different variables. }
  }
}

\section{Methods}{
  \describe{
    \item{[}{(phenoData): A subset operator. }
    \item{pData}{(phenoData): An accessor function for \code{pData}.}
    \item{show}{(phenoData): A method to display the data in a nice manner. }
    \item{varLabels}{(phenoData): A method to display the variable labels. }
  }
}
\keyword{methods}
