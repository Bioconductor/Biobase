\name{annotatedDataset-class}
\docType{class}
\alias{annotatedDataset-class}
\alias{annotatedDataset}
\alias{[[<-,annotatedDataset-method}
\alias{[[,annotatedDataset-method}
\alias{\$<-,annotatedDataset-method}
\alias{\$,annotatedDataset-method}
\alias{pData<-,annotatedDataset-method}
\alias{pData,annotatedDataset-method}
\alias{phenoData<-,annotatedDataset,phenoData-method}
\alias{phenoData,annotatedDataset-method}
\alias{varLabels,annotatedDataset-method}

\title{Class "annotatedDataset": A Virtual superset for exprSet, eSet,
  and others like them  }
\description{  This virtual class is contained by exprSet, eSet, etc and
now defines the phenoData specific methods}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
  \describe{
    \item{\code{phenoData}:}{Object of class \code{"phenoData"} This is an
      instance of class \code{phenoData} 
    containing the patient (or case) level data. The columns of the
    pData slot of this entity represent
    variables and the rows represent patients or cases.  }
  }
}
\section{Methods}{
  \describe{
    \item{[[<-}{\code{signature(x = "annotatedDataset")}: ... }
    \item{[[}{\code{signature(x = "annotatedDataset")}: ... }
    \item{\$<-}{\code{signature(x = "annotatedDataset")}: ... }
    \item{\$}{\code{signature(x = "annotatedDataset")}: ... }
    \item{pData<-}{\code{signature(object = "annotatedDataset")}: ... }
    \item{pData}{\code{signature(object = "annotatedDataset")}: ... }
    \item{phenoData<-}{\code{signature(object = "annotatedDataset", value = "phenoData")}: ... }
    \item{phenoData}{\code{signature(object = "annotatedDataset")}: ... }
    \item{varLabels}{\code{signature(object = "annotatedDataset")}: ... }
  }
}
\author{ Alan Katz }

\section{Warning}{
  This is a virtual class and no objects of this class should be
  created from it.  Instead, create exprSet or eSet 
  }

\seealso{\code{\link{exprSet-class}}, \code{\link{eSet-class}},
	 \code{\link{phenoData-class}}}

\keyword{classes}
