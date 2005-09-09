\name{eSet}
\docType{class}
\alias{eSet}
\alias{class:eSet}

\alias{listOrEnv}
\alias{eList}
\alias{SW}
\alias{SWPD}
\alias{swrep}
\alias{golubMergeSub}
\alias{assayData}

\concept{exprs}
\concept{annotation}
\concept{combine}
\concept{description}
\concept{getExpData}
\concept{ncol}
\concept{notes}
\concept{reporterNames}
\concept{sampleNames}

\title{
   General Container for High-Throughput Assays and Experimental Metadata
}

\description{
   General container for high-throughput assays and experimental metadata. \code{eSet} class is derived from \code{\link{annotatedDataset}} class.
}
\section{Extends}{
   Directly extends class \code{\link{annotatedDataset}}.
}
\section{Objects from the Class}{
   Objects can be created by calls of the form \code{new("eSet", ...)}.
}
\section{Slots}{
   Derived from \code{\link{annotatedDataset}}:
   \describe{
      \item{\code{reporterInfo}}{class:\code{data.frameOrNULL}}
      \item{\code{phenoData}}{class:\code{phenoData}}
   }
   Introduced in \code{eSet}:
   \describe{
      \item{\code{assayData}:}{Can be a list of matrices or an environment populated by matrices. The matrices must have common dimensions, and number of columns must agree with number of rows of \code{phenoData} \code{pData} slot; class:\code{listOrEnv}}
      \item{\code{sampleNames}:}{Vector of sample names, class:\code{character}}
      \item{\code{reporterNames}:}{Vector of reporter names, class:\code{character}}
      \item{\code{description}:}{Vector of reporter names, \code{\link{class:characterORMIAME}}}
      \item{\code{notes}:}{Notes, class:\code{character}}
      \item{\code{annotation}:}{Annotation, class:\code{character}}
      \item{\code{history}:}{History, class:\code{character}}
  }
}
\section{Methods}{
   Derived from \code{\link{annotatedDataset}}:
   \describe{
      \item{\code{$(eSet)} and \code{$(eSet, value)<-}}{see \code{\link{annotatedDataset}}}
      \item{\code{phenoData(eSet)} and \code{phenoData(eSet, value)<-}}{see \code{\link{annotatedDataset}}}
      \item{\code{reporterInfo(eSet)} and \code{reporterInfo(eSet, value)<-}}{see \code{\link{annotatedDataset}}}
      \item{\code{pData(eSet)} and \code{pData(eSet, value)<-}}{see \code{\link{annotatedDataset}}}
      \item{\code{[[(index)} and \code{[[(index, value)<-}:}{see \code{\link{annotatedDataset}}}
      \item{\code{varLabels(eSet)}}{see \code{\link{annotatedDataset}}}
   }
   Class-specific methods:
   \describe{
      \item{\code{assayData(eSet)} and \code{assayData(eSet, listOrEnc)<-}:}{Access/set \code{eSet} data.}
      \item{\code{exprs(eSet)} and \code{exprs(eSet)<-}:}{Not documented}
      \item{\code{description(eSet)} and \code{description(eSet, value)<-}:}{Not documented}
      \item{\code{annotation(eSet)} and \code{annotation(eSet, value)<-}:}{Not documented}
      \item{\code{notes(eSet)} and \code{notes(eSet, value)<-}:}{Not documented}
      \item{\code{sampleNames(eSet)} and \code{sampleNames(eSet, value)<-}:}{Not documented}
      \item{\code{reporterNames(eSet)} and \code{reporterNames(eSet, value)<-}:}{Not documented}
      \item{\code{combine(eSet, eSet)}:}{Not documented}
      \item{\code{getExpData(eSet, character)}:}{DEPRECATED}
      \item{\code{eList(eSet)} and \code{eList(eSet, value)<-}:}{DEPRECATED}
  }
  Standard generic methods:
  \describe{
      \item{\code{[(index)}:}{Conducts subsetting of matrices and phenoData components }
      \item{\code{dim(eSet)}:}{Not documented}
      \item{\code{ncol(eSet)}:}{Not documented}
      \item{\code{show(eSet)}:}{Not documented}
  }
}
\author{V.J. Carey, after initial design by R. Gentleman }
\note{
   Class \code{listOrEnv} is a (virtual) class union.

   \code{golubMergeSub} is an \code{exprSet} example that can be coerced to \code{eSet}; see the \code{eSet} vignette.}


\seealso{
   \code{\link{annotatedDataset}}, \code{\link{phenoData}}, \code{\link{exprSet}}
}
\examples{
# eSet 1 is loaded
data(eset)
pd <- phenoData(eset)
xl <- list(e1=exprs(eset))
gn <- geneNames(eset)
sn <- sampleNames(eset)

# eSet 2 is constructed
es2 <- new("eSet", phenoData=pd, assayData=xl,
  reporterNames=gn, sampleNames=sn)

# illustrate subset and show for assayData as list
es2[1:4,1:4]

# build with environment assayData
eenv <- new.env()
assign("e1", assayData(eset), eenv)
es3 <- new("eSet", phenoData=pd, assayData=eenv,
  reporterNames=gn, sampleNames=sn)
es3
es3[1:5,1:5]

# show some legacy behavior: set list element name to
# exprs and invoke exprs()
names(assayData(es2)) <- "exprs"
dim(exprs(es2))

# show some validity checking
assayData(es3) <- assayData(es2)
es3  # now has list
sampleNames(es2) <- letters # ok
if (interactive()) {
 lk <- try(sampleNames(es2) <- letters[-1]) # not ok
 lk
 lk <- try(assayData(es2) <- assayData(es2[-1,])) # not ok
 class(lk)
 }

# work with the swirl data
data(SW)
data(swrep)
data(SWPD)
sw <- new("eSet",
  assayData=list( R=SW$R, G=SW$G, Rb=SW$Rb, Gb=SW$Gb),
  phenoData=SWPD, reporterInfo=swrep, sampleNames=as.character(1:4),
  reporterNames=as.character(swrep$Name))
}
\keyword{classes}
