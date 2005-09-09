\name{annotatedDataset}
\docType{class}
\alias{class:annotatedDataset}
\alias{annotatedDataset}

\concept{pData}
\concept{phenoData}
\concept{varLabels}

\title{
   Virtual Superset for 'exprSet', 'eSet' etc.
}

\description{
   This virtual class is used to derive classes \code{exprSet}, \code{eSet}. It defines the \code{phenoData} specific methods
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
      \item{\code{$(annotatedDataset)} and \code{$(annotatedDataset, value)<-}}{An old-style method. It is \code{pData(eset)[[as.character(val)]]} which does not quite have the right semantics but it is close. This operator extracts the named component of the \code{pData} slot in \code{phenoData}.}
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
   \code{\link{exprSet}}, \code{\link{eSet}}, \code{\link{phenoData}}
}

\keyword{classes}
