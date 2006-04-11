\name{SnpSet}
\docType{class}
\alias{class:SnpSet}
\alias{SnpSet}

\alias{SnpSet-class}

\alias{initialize,SnpSet-method}

\title{
  Class to Contain Objects Describing High-Throughput SNP Assays.
}

\description{
  Container for high-throughput assays and experimental
  metadata. \code{SnpSet} class is derived from
  \code{\link{eSet}} and \code{\link{SnpESet}} classes, and
  requires matricies \code{call}, \code{callProbability} as assay data members.
}
\section{Extends}{
   Directly extends class \code{\link{SnpESet}}; extends class \code{\link{eSet}}.
}
\section{Creating Objects}{

  \code{new('SnpSet',
    phenoData = [AnnotatedDataFrame],
    experimentData = [MIAME],
    annotation = [character],
    call = [matrix],
    callProbability = [matrix],
    ...)
  }

  \code{SnpSet} instances are usually created through
  \code{new("SnpSet", ...)}. Usually the arguments to \code{new}
  include \code{call} (a matrix of gentoypic calls, with features (SNPs)
  corresponding to rows and samples to columns), \code{phenoData},
  \code{experimentData}, and \code{annotation}. 
  \code{phenoData}, \code{experimentData}, and \code{annotation} can be
  missing, in which case they are assigned default values.
}
\section{Slots}{
  Inherited from \code{eSet}:
   \describe{
      \item{\code{assayData}:}{Contains matrices with equal
	dimensions, and with column number equal to
	\code{nrow(phenoData)}. \code{assayData} must contain a matrix
	\code{call} with rows represening features (e.g., SNPs)
	and columns representing samples, and a matrix
	\code{callProbability} describing the certainty of the
	call. The content of \code{call} and \code{callProbability} are
	not enforced by the class. Additional matrices of
	identical size may also be included in
	\code{assayData}. Class:\code{\link{AssayData}}} 
      \item{\code{phenoData}:}{See \code{\link{eSet}}}
      \item{\code{experimentData}:}{See \code{\link{eSet}}}
      \item{\code{annotation}:}{See \code{\link{eSet}}}
  }
}
\section{Methods}{

  Class-specific methods: none.

  Derived from \code{\link{SnpESet}}:
  \describe{
    \item{\code{exprs(SnpSet)}, \code{exprs(SnpSet,AssayData)<-}}{See \code{\link{SnpESet}}}
  }
  
   Derived from \code{\link{eSet}}:
   \describe{
     \item{\code{sampleNames(SnpSet)} and \code{sampleNames(SnpSet)<-}:}{See \code{\link{eSet}}}
     \item{\code{featureNames(SnpSet)}, \code{featureNames(SnpSet, value)<-}:}{See \code{\link{eSet}}}
     \item{\code{dims(SnpSet)}:}{See \code{\link{eSet}}}
     \item{\code{phenoData(SnpSet)}, \code{phenoData(SnpSet,value)<-}:}{See \code{\link{eSet}}}
     \item{\code{varLabels(SnpSet)}, \code{varLabels(SnpSet, value)<-}:}{See \code{\link{eSet}}}
     \item{\code{varMetadata(SnpSet)}, \code{varMetadata(SnpSet,value)<-}:}{See \code{\link{eSet}}}
     \item{\code{pData(SnpSet)}, \code{pData(SnpSet,value)<-}:}{See \code{\link{eSet}}}
     \item{\code{varMetadata(SnpSet)}, \code{varMetadata(SnpSet,value)}}{See \code{\link{eSet}}}
     \item{\code{experimentData(SnpSet)},\code{experimentData(SnpSet,value)<-}:}{See \code{\link{eSet}}}
     \item{\code{pubMedIds(SnpSet)}, \code{pubMedIds(SnpSet,value)}}{See \code{\link{eSet}}}
     \item{\code{abstract(SnpSet)}:}{See \code{\link{eSet}}}
     \item{\code{annotation(SnpSet)}, \code{annotation(SnpSet,value)<-}}{See \code{\link{eSet}}}
     \item{\code{combine(SnpSet,SnpSet)}:}{See \code{\link{eSet}}}
     \item{\code{storageMode(eSet)}, \code{storageMode(eSet,character)<-}:}{See \code{\link{eSet}}}
     \item{\code{reporterNames(SnpSet)}, \code{reporterNames(SnpSet,value)<-}:}{DEPRECATED}
  }

  Standard generic methods:
  \describe{
    \item{\code{initialize(SnpSet)}:}{Object instantiation, used
      by \code{new}; not to be called directly by the user.}
    \item{\code{validObject(SnpSet)}:}{Validity-checking method, ensuring
      that \code{call} and \code{callProbability} is a member of
      \code{assayData}. \code{checkValidity(SnpSet)} imposes this
      validity check, and the validity checks of \code{eSet}.}
     \item{\code{show(SnpSet)}}{See \code{\link{eSet}}}
     \item{\code{dim(SnpSet)}, \code{ncol}}{See \code{\link{eSet}}}
     \item{\code{SnpSet[(index)}:}{See \code{\link{eSet}}}
     \item{\code{SnpSet$}, \code{SnpSet$<-}}{See \code{\link{eSet}}}
  }
}
\author{V.J. Carey, after initial design by R. Gentleman }

\seealso{
  \code{\link{eSet}}, \code{\link{SnpESet}}
}

\keyword{classes}