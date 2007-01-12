\name{annotatedDataset}
\docType{class}
\alias{class:annotatedDataset}
\alias{annotatedDataset}

\alias{annotatedDataset-class}
\alias{[[<-,annotatedDataset-method}
\alias{[[,annotatedDataset-method}
\alias{$<-,annotatedDataset-method}
\alias{$,annotatedDataset-method}
\alias{pData<-,annotatedDataset,ANY-method}
\alias{pData,annotatedDataset-method}
\alias{phenoData,annotatedDataset-method}
\alias{reporterInfo,annotatedDataset-method}
\alias{reporterInfo<-,annotatedDataset,data.frame-method}
\alias{varLabels,annotatedDataset-method}

\concept{pData}
\concept{phenoData}
\concept{varLabels}

\title{
   Virtual superset for 'exprSet'.
}

\description{
   This virtual class is used to derive the \code{exprSet} class. It defines the \code{phenoData} specific methods
}

\section{Objects from the Class}{A virtual Class: No objects may be created from it.}

\section{Slots}{
   \describe{
      \item{\code{phenoData}:}{This is an instance of class \code{phenoData} containing the patient (or case) level data. The columns of the pData slot of this entity represent variables and the rows represent patients or cases.  }
      \item{\code{reporterInfo}}{class:\code{data.frameOrNULL}}
   }
}

\section{Methods}{
   \describe{
      \item{\code{$(annotatedDataset)} and \code{$(annotatedDataset, value)<-}}{An old-style method. It is \code{pData(sample.eSet)[[as.character(val)]]} which does not quite have the right semantics but it is close. This operator extracts the named component of the \code{pData} slot in \code{phenoData}.}
      \item{\code{[[(index)} and \code{[[(index, value)<-}:}{Not documented}
      \item{\code{phenoData(annotatedDataset)} and \code{phenoData(annotatedDataset, value)<-}}{Methods to access/update \code{phenoData} slot}
      \item{\code{reporterInfo(annotatedDataset)} and \code{reporterInfo(annotatedDataset, value)<-}}{Methods to access/update \code{reporterInfo} slot}
      \item{\code{pData(annotatedDataset)} and \code{pData(exprSet, value)<-}}{Not documented}
      \item{\code{varLabels(annotatedDataset)}}{Not documented}
   }
}
\author{ Alan Katz }

\section{Warning}{
  This is a virtual class and no objects of this class should be
  created from it.  Instead, create exprSet or eSet
  }

\seealso{
   \code{\link{exprSet}}, \code{\link{phenoData}}
}

\keyword{classes}
