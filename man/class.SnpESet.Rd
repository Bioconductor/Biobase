\name{SnpESet}
\docType{class}
\alias{class:SnpESet}
\alias{SnpESet}

\alias{SnpESet-class}

\alias{exprs,SnpESet-method}
\alias{exprs<-,SnpESet,matrix-method}

\title{
  Virtual Class to Group Objects Describing High-Throughput SNP Assays.
}

\description{
  General container for high-throughput assays and experimental
  metadata. \code{SnpESet} class is derived from
  \code{\link{eSet}} class, and is implemented in
  \code{\link{SnpSet}}.
}
\section{Extends}{
   Directly extends class \code{\link{eSet}}.
}
\section{Objects from the Class}{
  \code{SnpESet} is a virtual class; \code{SnpESet} objects cannot be created.
  See classes \code{\link{SnpSet}}, for an example of a class derived from \code{SnpESet}.
}
\section{Slots}{
  Inherited from \code{eSet}:
   \describe{
      \item{\code{assayData}:}{See \code{\link{eSet}}}
      \item{\code{phenoData}:}{See \code{\link{eSet}}}
      \item{\code{experimentData}:}{See \code{\link{eSet}}}
      \item{\code{annotation}:}{See \code{\link{eSet}}}
  }
}
\section{Methods}{
  Methods defined in derived class (e.g., \code{\link{SnpSet}}) may override the methods described here.

  Class-specific methods:
  \describe{
     \item{\code{exprs(SnpESet)}, \code{exprs(SnpESet,matrix)<-}}{Access and
       set elements named \code{call}  in the \code{AssayData}
       slot.}
  }

   Derived from \code{\link{eSet}}:
   \describe{
     \item{\code{sampleNames(SnpESet)} and \code{sampleNames(SnpESet)<-}:}{See \code{\link{eSet}}}
     \item{\code{featureNames(SnpESet)}, \code{featureNames(SnpESet, value)<-}:}{See \code{\link{eSet}}}
     \item{\code{dims}:}{See \code{\link{eSet}}}
     \item{\code{phenoData(SnpESet)}, \code{phenoData(SnpESet,value)<-}:}{See \code{\link{eSet}}}
     \item{\code{varLabels(SnpESet)}, \code{varLabels(SnpESet, value)<-}:}{See \code{\link{eSet}}}
     \item{\code{varMetadata(SnpESet)}, \code{varMetadata(SnpESet,value)<-}:}{See \code{\link{eSet}}}
     \item{\code{pData(SnpESet)}, \code{pData(SnpESet,value)<-}:}{See \code{\link{eSet}}}
     \item{\code{varMetadata(SnpESet)}, \code{varMetadata(SnpESet,value)}}{See \code{\link{eSet}}}
     \item{\code{experimentData(SnpESet)},\code{experimentData(SnpESet,value)<-}:}{See \code{\link{eSet}}}
     \item{\code{pubMedIds(SnpESet)}, \code{pubMedIds(SnpESet,value)}}{See \code{\link{eSet}}}
     \item{\code{abstract(SnpESet)}:}{See \code{\link{eSet}}}
     \item{\code{annotation(SnpESet)}, \code{annotation(SnpESet,value)<-}}{See \code{\link{eSet}}}
     \item{\code{combine(SnpESet,SnpESet)}:}{See \code{\link{eSet}}}
     \item{\code{reporterNames(SnpESet)}, \code{reporterNames(SnpESet,value)<-}:}{DEPRECATED}
  }
  Standard generic methods:
  \describe{
    \item{\code{initialize(SnpESet)}:}{No \code{SnpESet}-specific initialization code; see \code{\link{eSet-class}}.}
    \item{\code{validObject(SnpESet)}:}{No \code{SnpESet}-sepcific validation method; see \code{\link{eSet-class}}.}
     \item{\code{show(SnpESet)}}{Informatively display object contents.}
     \item{\code{dim(SnpESet)}, \code{ncol}}{Access the common
       dimensions (\code{dim}) or column numbers (\code{ncol}), of all
       memebers (\code{dims}) of \code{assayData}.}
     \item{\code{SnpESet[(index)}:}{Conducts subsetting of matrices and
       phenoData components }
     \item{\code{SnpESet$}, \code{SnpESet$<-}}{Access and set named columns in \code{phenoData}}
  }
}
\author{V.J. Carey, after initial design by R. Gentleman }

\seealso{
  \code{\link{eSet}}, \code{\link{SnpSet}}
}

\keyword{classes}
