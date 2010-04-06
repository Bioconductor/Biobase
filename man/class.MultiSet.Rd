\name{MultiSet}
\docType{class}
\alias{class:MultiSet}
\alias{MultiSet}

\alias{MultiSet-class}
\alias{initialize,MultiSet-method}
\alias{coerce,eSet,MultiSet-method}

\title{
  Class to Contain and Describe High-Throughput Expression Level Assays.
}

\description{
  Container for high-throughput assays and experimental
  metadata. \code{MutliSet} is derived from
  \code{\link{eSet-class}}. \code{MultiSet} differs from
  \code{\link{ExpressionSet-class}} because \code{MultiSet} can contain
  any element(s) in \code{assayData} (\code{ExpressionSet} must have an
  element named \code{exprs}).
}
\section{Extends}{
   Directly extends class \code{\link{eSet}}.
}
\section{Creating Objects}{

  \code{new('MultiSet',
    phenoData = [AnnotatedDataFrame],
    experimentData = [MIAME],
    annotation = [character],
    protocolData = [AnnotatedDataFrame],
    ...)
  }

  \code{updateOldESet(oldESet,"MultiSet")}

  \code{MultiSet} instances are usually created through
  \code{new("MultiSet", ...)}. The \code{...} arguments to \code{new}
  are matrices of expression data (with features
  corresponding to rows and samples to columns), \code{phenoData},
  \code{experimentData}, \code{annotation}, and \code{protocolData}. 
  \code{phenoData}, \code{experimentData}, \code{annotation}, and
  \code{protocolData} can be missing, in which case they are assigned
  default values.

  \code{\link{updateOldESet}} will take a serialized instance (e.g., saved to a disk
  file with \code{save} object created with earlier definitions of
  the \code{eSet-class}, and update the object to
  \code{MultiSet}. Warnings are issued when direct translation is not
  possible; incorrectly created \code{oldESet} instances may not be updated.
}
\section{Slots}{
  Inherited from \code{eSet}:
   \describe{
      \item{\code{assayData}:}{Contains zero or more matrices with equal
	dimensions, and with column number equal to
	\code{nrow(phenoData)}. Each matrix in \code{assayData} has rows
	representing features (e.g., reporters) and columns representing
	samples. Class:\code{\link{AssayData-class}}}
      \item{\code{phenoData}:}{See \code{\link{eSet-class}}}
      \item{\code{experimentData}:}{See \code{\link{eSet-class}}}
      \item{\code{annotation}:}{See \code{\link{eSet-class}}}
      \item{\code{protocolData}:}{See \code{\link{eSet-class}}}
  }
}
\section{Methods}{

   Class-specific methods: none

   Derived from \code{\link{eSet-class}}:
   \describe{
     \item{\code{updateObject(object, ..., verbose=FALSE)}}{Update instance to current version, if necessary. See \code{\link{updateObject}} and \code{\link{eSet}}}
     \item{\code{isCurrent(object)}}{Determine whether version of object is current. See \code{\link{isCurrent}}}
     \item{\code{isVersioned(object)}}{Determine whether object contains a 'version' string describing its structure . See \code{\link{isVersioned}}}
     \item{\code{sampleNames(MultiSet)} and \code{sampleNames(MultiSet)<-}:}{See \code{\link{eSet-class}}}
     \item{\code{featureNames(MultiSet)}, \code{featureNames(MultiSet, value)<-}:}{See \code{\link{eSet-class}}}
     \item{\code{dims(MultiSet)}:}{See \code{\link{eSet-class}}}
     \item{\code{phenoData(MultiSet)}, \code{phenoData(MultiSet,value)<-}:}{See \code{\link{eSet-class}}}
     \item{\code{varLabels(MultiSet)}, \code{varLabels(MultiSet, value)<-}:}{See \code{\link{eSet-class}}}
     \item{\code{varMetadata(MultiSet)}, \code{varMetadata(MultiSet,value)<-}:}{See \code{\link{eSet-class}}}
     \item{\code{pData(MultiSet)}, \code{pData(MultiSet,value)<-}:}{See \code{\link{eSet-class}}}
     \item{\code{varMetadata(MultiSet)}, \code{varMetadata(MultiSet,value)}}{See \code{\link{eSet-class}}}
     \item{\code{experimentData(MultiSet)},\code{experimentData(MultiSet,value)<-}:}{See \code{\link{eSet-class}}}
     \item{\code{pubMedIds(MultiSet)}, \code{pubMedIds(MultiSet,value)}}{See \code{\link{eSet-class}}}
     \item{\code{abstract(MultiSet)}:}{See \code{\link{eSet-class}}}
     \item{\code{annotation(MultiSet)}, \code{annotation(MultiSet,value)<-}}{See \code{\link{eSet-class}}}
     \item{\code{protocolData(MultiSet)}, \code{protocolData(MultiSet,value)<-}}{See \code{\link{eSet-class}}}
     \item{\code{combine(MultiSet,MultiSet)}:}{See \code{\link{eSet-class}}}
     \item{\code{storageMode(eSet)}, \code{storageMode(eSet,character)<-}:}{See \code{\link{eSet-class}}}
  }

  Standard generic methods:
  \describe{
     \item{\code{initialize(MultiSet)}:}{Object instantiation, used by \code{new}; not to be called directly by the user.}
     \item{\code{validObject(MultiSet)}:}{Validity-checking method, ensuring that all elements of \code{assayData} are matricies with equal dimensions.}
     \item{\code{as(eSet,MultiSet)}:}{Coerce the \code{eSet} portion of an object to \code{MultiSet}.}
     \item{\code{show(MultiSet)}}{See \code{\link{eSet-class}}}
     \item{\code{dim(MultiSet)}, \code{ncol}}{See \code{\link{eSet-class}}}
     \item{\code{MultiSet[(index)}:}{See \code{\link{eSet-class}}}
     \item{\code{MultiSet$}, \code{MultiSet$<-}}{See \code{\link{eSet-class}}}
  }
}

\author{Biocore team}

\seealso{
  \code{\link{eSet-class}}, \code{\link{ExpressionSet-class}}
}

\examples{
# create an instance of ExpressionSet
new("MultiSet")
}

\keyword{classes}
