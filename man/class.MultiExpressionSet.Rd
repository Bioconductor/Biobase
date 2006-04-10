\name{MultiExpressionSet}
\docType{class}
\alias{class:MultiExpressionSet}
\alias{MultiExpressionSet}

\alias{MultiExpressionSet-class}

\alias{initialize,MultiExpressionSet-method}

\title{
  Class to Contain and Describe High-Throughput Expression Level Assays.
}

\description{
  Container for high-throughput assays and experimental
  metadata. \code{MultiExpressionSet} class is derived from
  \code{\link{eSet}} and \code{\link{ExpressionESet}} classes, and
  can contain one or more identical-sized matricies as \code{assayData}
  members. For example, members might represent channels of a two-color microarray.
}
\section{Extends}{
   Directly extends class \code{\link{ExpressionESet}}; extends class \code{\link{eSet}}.
}
\section{Creating Objects}{

  \code{new('MultiExpressionSet',
    phenoData = [AnnotatedDataFrame],
    experimentData = [MIAME],
    annotation = [character],
    ...)
  }

  \code{updateOldESet(old_eSet)}

  \code{MultiExpressionSet} instances are usually created through
  \code{new("MultiExpressionSet", ...)}. Usually the arguments to \code{new}
  include one or more named matrices (representing expression data, with features
  corresponding to rows and samples to columns), \code{phenoData},
  \code{experimentData}, and \code{annotation}. 
  \code{phenoData}, \code{experimentData}, and \code{annotation} can be
  missing, in which case they are assigned default values.

  Objects created under previous definitions of \code{eSet-class} can be
  coerced to \code{MultiExpressionSet} using \code{updateOldESet}.
}
\section{Slots}{
  Inherited from \code{eSet}:
   \describe{
      \item{\code{assayData}:}{Contains matrices with equal
	dimensions, and with column number equal to
	\code{nrow(phenoData)}. \code{assayData} must contain matricies
	with rows represening features (e.g., reporters)
	and columns representing samples. . Class:\code{\link{AssayData}}}
      \item{\code{phenoData}:}{See \code{\link{eSet}}}
      \item{\code{experimentData}:}{See \code{\link{eSet}}}
      \item{\code{annotation}:}{See \code{\link{eSet}}}
  }
}
\section{Methods}{

  Class-specific methods: none. The function \code{\link{updateOldEset}}
  can be used to coerce previous versions of \code{eSet-class} objects
  to objects of \code{MultiExpressionSet-class}

   Derived from \code{\link{eSet}}:
   \describe{
     \item{\code{sampleNames(MultiExpressionSet)} and \code{sampleNames(MultiExpressionSet)<-}:}{See \code{\link{eSet}}}
     \item{\code{featureNames(MultiExpressionSet)}, \code{featureNames(MultiExpressionSet, value)<-}:}{See \code{\link{eSet}}}
     \item{\code{dims(MultiExpressionSet)}:}{See \code{\link{eSet}}}
     \item{\code{phenoData(MultiExpressionSet)}, \code{phenoData(MultiExpressionSet,value)<-}:}{See \code{\link{eSet}}}
     \item{\code{varLabels(MultiExpressionSet)}, \code{varLabels(MultiExpressionSet, value)<-}:}{See \code{\link{eSet}}}
     \item{\code{varMetadata(MultiExpressionSet)}, \code{varMetadata(MultiExpressionSet,value)<-}:}{See \code{\link{eSet}}}
     \item{\code{pData(MultiExpressionSet)}, \code{pData(MultiExpressionSet,value)<-}:}{See \code{\link{eSet}}}
     \item{\code{varMetadata(MultiExpressionSet)}, \code{varMetadata(MultiExpressionSet,value)}}{See \code{\link{eSet}}}
     \item{\code{experimentData(MultiExpressionSet)},\code{experimentData(MultiExpressionSet,value)<-}:}{See \code{\link{eSet}}}
     \item{\code{pubMedIds(MultiExpressionSet)}, \code{pubMedIds(MultiExpressionSet,value)}}{See \code{\link{eSet}}}
     \item{\code{abstract(MultiExpressionSet)}:}{See \code{\link{eSet}}}
     \item{\code{annotation(MultiExpressionSet)}, \code{annotation(MultiExpressionSet,value)<-}}{See \code{\link{eSet}}}
     \item{\code{combine(MultiExpressionSet,MultiExpressionSet)}:}{See \code{\link{eSet}}}
     \item{\code{storageMode(eSet)}, \code{storageMode(eSet,character)<-}:}{See \code{\link{eSet}}}
     \item{\code{reporterNames(MultiExpressionSet)}, \code{reporterNames(MultiExpressionSet,value)<-}:}{DEPRECATED}
  }

  Standard generic methods:
  \describe{
    \item{\code{initialize(MultiExpressionSet)}:}{Object instantiation, used
      by \code{new}; not to be called directly by the user.}
    \item{\code{validObject(MultiExpressionSet)}:}{No \code{MultiExpressionSet}-sepcific validation method; see \code{\link{eSet-class}}.}
     \item{\code{show(MultiExpressionSet)}}{See \code{\link{eSet}}}
     \item{\code{dim(MultiExpressionSet)}, \code{ncol}}{See \code{\link{eSet}}}
     \item{\code{MultiExpressionSet[(index)}:}{See \code{\link{eSet}}}
     \item{\code{MultiExpressionSet$}, \code{MultiExpressionSet$<-}}{See \code{\link{eSet}}}
  }
}
\author{V.J. Carey, after initial design by R. Gentleman }

\seealso{
  \code{\link{eSet}}, \code{\link{ExpressionESet}}, \code{\link{ExpressionSet}}
}

\keyword{classes}
