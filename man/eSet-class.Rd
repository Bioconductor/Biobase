\name{eSet-class}
\docType{class}
\alias{eSet-class}
\alias{[,eSet-method}
\alias{$.eSet}
\alias{getExpData}
\alias{combine,eSet,eSet-method}
\alias{combine,eSet,ANY-method}
\alias{eList}
\alias{eList<-}
\alias{eList,eSet-method}
\alias{eList<-,eSet,environment-method}
\alias{eList<-,eSet,list-method}
\alias{exprs,eSet-method}
\alias{getExpData,eSet,character-method}
\alias{pData,eSet-method}
\alias{pData<-,eSet-method}
\alias{phenoData,eSet-method}
\alias{phenoData<-,eSet,phenoData-method}
\alias{show,eSet-method}

\title{Class "eSet": a new exprSet class }
\description{A second attempt at a universal definition of experimental
  data, combined with phenotypic data and in some cases probe data.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("eSet", ...)}.
These objects are basically structured to provide a unified and
simplified set of methods for handling experimental data. This might be
gene expression data, array CGH data or SELD-TOF/MALDI-TOF data.
}
\section{Slots}{
  \describe{
    \item{\code{eList}:}{Object of class \code{"exprList"} containing
      the experimental data. }
    \item{\code{phenoData}:}{Object of class \code{"phenoData"}
      containing the phenotypic or sample level data.}
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "eSet")}: basic subsetting. It returns
      an object of class \code{eSet}. The first index subsets
      experimental units while the second subsets samples. }
    \item{combine}{\code{signature(x = "eSet", y = "eSet")}: a method
      for merging two \code{eSet}s. Substantial checking is carried out. }
    \item{combine}{\code{signature(x = "eSet", y = "ANY")}: As above. }
    \item{eList}{\code{signature(object = "eSet")}: a container for the
      experimental data. This can accomodate multiple channels etc. }
    \item{eList<-}{\code{signature(object = "eSet", value =
	"exprList")}: An assignment function for \code{exprList}s. }
    \item{eList<-}{\code{signature(object = "eSet", value =
	"environment")}: as above}
    \item{eList<-}{\code{signature(object = "eSet", value = "list")}: as
    above.}
    \item{exprs}{\code{signature(object = "eSet")}: extract the
      experimental data. }
    \item{getExpData}{\code{signature(object = "eSet", name =
	"character")}: extract the experimental data.}
    \item{pData}{\code{signature(object = "eSet")}: get the actual
      \code{data.frame} containing the phenotypic data. }
    \item{pData<-}{\code{signature(object = "eSet")}: replace the actual
    \code{data.frame} containing the phenotypic data.}
    \item{phenoData}{\code{signature(object = "eSet")}: extract the
      phenoData object (data frame plus descriptions of the covariates). }
    \item{phenoData<-}{\code{signature(object = "eSet", value =
	"phenoData")}: a replacement method for the \code{phenoData} object. }
    \item{show}{\code{signature(object = "eSet")}: a print method, to
      control printing vast amounts of data. }
  }
}
\author{Biocore}
\note{The data structures etc. were designed by a consensus view of Biocore. }

\seealso{
\code{\link{exprSet-class}}}
\examples{
##---- Should be DIRECTLY executable !! ----
}
\keyword{classes}
