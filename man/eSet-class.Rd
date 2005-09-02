\name{eSet-class}
\docType{class}
\alias{eSet-class}
\alias{listOrEnv-class}
\alias{listOrEnv}
\alias{[,eSet-method}
\alias{dim,eSet-method}
\alias{exprs,eSet-method}
\alias{assayData,eSet-method}
\alias{show,eSet-method}
\alias{annotation,eSet-method}
\alias{generic annotation<-,eSet-method}
\alias{assayData,exprSet-method}
\alias{assayData<-,eSet,listOrEnv-method}
\alias{combine,eSet,eSet-method}
\alias{combine,eSet,ANY-method}
\alias{description,eSet-method}
\alias{description<-,eSet-method}
\alias{eList,eSet-method}
\alias{eList<-,eSet,listOrEnv-method}
\alias{getExpData,eSet,character-method}
\alias{ncol,eSet-method}
\alias{notes,eSet-method}
\alias{notes<-,eSet-method}
\alias{reporterNames,eSet-method}
\alias{sampleNames,eSet-method}
\alias{annotation<-,eSet-method}
\alias{exprs<-,eSet,listOrEnv-method}
\alias{sampleNames<-,eSet,character-method}
\alias{SW}
\alias{swrep}

\title{Class "eSet" -- general container for high-throughput assays and
experimental metadata }
\description{  general container for high-throughput assays and
experimental metadata }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("eSet", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{exprs}:}{Object of class \code{"listOrEnv"}, can be a list
of matrices or an environment populated by matrices.  The matrices must
have common dimensions, and number of columns must agree with
number of rows of \code{phenoData} \code{pData} slot.  }
    \item{\code{sampleNames}:}{vector of sample names}
    \item{\code{phenoData}:}{Object of class \code{"phenoData"} }
  }
}
\section{Extends}{
Class \code{"annotatedDataset"}, directly.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "eSet")}: conducts subsetting of
matrices and phenoData components }
    \item{dim}{\code{signature(x = "eSet")}: ... }
    \item{ncol}{\code{signature(x = "eSet")}: ... }
    \item{exprs}{\code{signature(object = "eSet")}: ... }
    \item{show}{\code{signature(object = "eSet")}: ... }
    \item{combine}{\code{signature(object = "eSet")}: ... }
    \item{description}{\code{signature(object = "eSet")}: ... }
    \item{annotation}{\code{signature(object = "eSet")}: ... }
    \item{eList}{\code{signature(object = "eSet")}: DEPRECATED }
    \item{sampleNames}{\code{signature(object = "eSet")}: DEPRECATED }
    \item{reporterNames}{\code{signature(object = "eSet")}: DEPRECATED }
  }
}
%\references{ ~put references to the literature/web site here ~ }
\author{ VJ Carey after initial design by Robert Gentleman }
\note{ \code{listOrEnv} is a (virtual) class union. }

% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%  or \code{\link{CLASSNAME-class}} for links to other classes
%}
\examples{
#
# let's get some exprSet
#
data(eset)
pd <- phenoData(eset)
xl <- list(e1=exprs(eset))
gn <- geneNames(eset)
sn <- sampleNames(eset)
#
# build an eSet
#
es2 <- new("eSet", phenoData=pd, assayData=xl,
  reporterNames=gn, sampleNames=sn)
#
# illustrate subset and show for assayData as list
#
es2[1:4,1:4]
#
# build with environment assayData
#
eenv <- new.env()
assign("e1", assayData(eset), eenv)
es3 <- new("eSet", phenoData=pd, assayData=eenv,
  reporterNames=gn, sampleNames=sn)
es3
es3[1:5,1:5]
#
# show some legacy behavior: set list element name to
# exprs and invoke exprs()
#
names(assayData(es2)) <- "exprs"
dim(exprs(es2))
#
# show some validity checking
#
assayData(es3) <- assayData(es2)
es3  # now has list
sampleNames(es2) <- letters # ok
if (interactive()) {
 lk <- try(sampleNames(es2) <- letters[-1]) # not ok
 lk
 lk <- try(assayData(es2) <- assayData(es2[-1,])) # not ok
 class(lk)
 }
#
# work with the swirl data
#
data(SW)
data(swrep)
data(SWPD)
sw <- new("eSet", 
  assayData=list( R=SW$R, G=SW$G, Rb=SW$Rb, Gb=SW$Gb), 
  phenoData=SWPD, reporterInfo=swrep, sampleNames=as.character(1:4),
  reporterNames=as.character(swrep$Name))
}
\keyword{classes}
