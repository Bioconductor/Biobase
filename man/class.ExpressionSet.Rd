\name{ExpressionSet}
\docType{class}
\alias{class:ExpressionSet}
\alias{ExpressionSet}

\alias{ExpressionSet-class}

\alias{exprs,ExpressionSet-method}
\alias{exprs<-,ExpressionSet,matrix-method}
\alias{initialize,ExpressionSet-method}
\alias{coerce,exprSet,ExpressionSet-method}

\title{
  Class to Contain and Describe High-Throughput Expression Level Assays.
}

\description{
  Container for high-throughput assays and experimental
  metadata. \code{ExpressionSet} class is derived from
  \code{\link{eSet}}, and requires matricies named \code{exprs} and
  \code{se.exprs} as an assay data members.
}
\section{Extends}{
   Directly extends class \code{\link{eSet}}.
}
\section{Creating Objects}{

  \code{new('ExpressionSet',
    phenoData = [AnnotatedDataFrame],
    experimentData = [MIAME],
    annotation = [character],
    exprs = [matrix],
    ...)
  }

  \code{as([exprSet],"ExpressionSet")}

  \code{ExpressionSet} instances are usually created through
  \code{new("ExpressionSet", ...)}. Usually the arguments to \code{new}
  include \code{exprs} (a matrix of expression data, with features
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
	\code{exprs} with rows represening features (e.g., reporters)
	and columns representing samples. Additional matrices of
	identical size (e.g., representing measurement errors) may
	also be included in \code{assayData}. Class:\code{\link{AssayData-class}}}
      \item{\code{phenoData}:}{See \code{\link{eSet}}}
      \item{\code{experimentData}:}{See \code{\link{eSet}}}
      \item{\code{annotation}:}{See \code{\link{eSet}}}
  }
}
\section{Methods}{

  Class-specific methods.
  \describe{
    \item{\code{as(exprSet,"ExpressionSet")}}{Coerce objects of
      \code{\link{exprSet-class}} to \texttt{ExpressionSet}}
     \item{\code{exprs(ExpressionSet)}, \code{exprs(ExpressionSet,matrix)<-}}{Access and
       set elements named \code{exprs} in the \code{AssayData-class}
       slot.}
    }

   Derived from \code{\link{eSet}}:
   \describe{
     \item{\code{sampleNames(ExpressionSet)} and \code{sampleNames(ExpressionSet)<-}:}{See \code{\link{eSet}}}
     \item{\code{featureNames(ExpressionSet)}, \code{featureNames(ExpressionSet, value)<-}:}{See \code{\link{eSet}}}
     \item{\code{dims(ExpressionSet)}:}{See \code{\link{eSet}}}
     \item{\code{phenoData(ExpressionSet)}, \code{phenoData(ExpressionSet,value)<-}:}{See \code{\link{eSet}}}
     \item{\code{varLabels(ExpressionSet)}, \code{varLabels(ExpressionSet, value)<-}:}{See \code{\link{eSet}}}
     \item{\code{varMetadata(ExpressionSet)}, \code{varMetadata(ExpressionSet,value)<-}:}{See \code{\link{eSet}}}
     \item{\code{pData(ExpressionSet)}, \code{pData(ExpressionSet,value)<-}:}{See \code{\link{eSet}}}
     \item{\code{varMetadata(ExpressionSet)}, \code{varMetadata(ExpressionSet,value)}}{See \code{\link{eSet}}}
     \item{\code{experimentData(ExpressionSet)},\code{experimentData(ExpressionSet,value)<-}:}{See \code{\link{eSet}}}
     \item{\code{pubMedIds(ExpressionSet)}, \code{pubMedIds(ExpressionSet,value)}}{See \code{\link{eSet}}}
     \item{\code{abstract(ExpressionSet)}:}{See \code{\link{eSet}}}
     \item{\code{annotation(ExpressionSet)}, \code{annotation(ExpressionSet,value)<-}}{See \code{\link{eSet}}}
     \item{\code{combine(ExpressionSet,ExpressionSet)}:}{See \code{\link{eSet}}}
     \item{\code{storageMode(eSet)}, \code{storageMode(eSet,character)<-}:}{See \code{\link{eSet}}}
     \item{\code{reporterNames(ExpressionSet)}, \code{reporterNames(ExpressionSet,value)<-}:}{DEPRECATED}
  }

  Standard generic methods:
  \describe{
    \item{\code{initialize(ExpressionSet)}:}{Object instantiation, used
      by \code{new}; not to be called directly by the user.}
    \item{\code{validObject(ExpressionSet)}:}{Validity-checking method, ensuring
      that \code{exprs} is a member of
      \code{assayData}. \code{checkValidity(ExpressionSet)} imposes this
      validity check, and the validity checks of \code{eSet}.}
     \item{\code{show(ExpressionSet)}}{See \code{\link{eSet}}}
     \item{\code{dim(ExpressionSet)}, \code{ncol}}{See \code{\link{eSet}}}
     \item{\code{ExpressionSet[(index)}:}{See \code{\link{eSet}}}
     \item{\code{ExpressionSet$}, \code{ExpressionSet$<-}}{See \code{\link{eSet}}}
  }
}
\author{V.J. Carey, after initial design by R. Gentleman }

\seealso{
  \code{\link{eSet}}, \code{\link{ExpressionSet}}
}

\keyword{classes}
