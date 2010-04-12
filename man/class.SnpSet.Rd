\name{SnpSet}
\docType{class}
\alias{class:SnpSet}
\alias{SnpSet}

\alias{SnpSet-class}
\alias{exprs<-,SnpSet,matrix-method}
\alias{exprs,SnpSet-method}
\alias{snpCall,SnpSet-method}
\alias{snpCall<-,SnpSet,matrix-method}
\alias{snpCallProbability,SnpSet-method}
\alias{snpCallProbability<-,SnpSet,matrix-method}

\alias{initialize,SnpSet-method}

\title{
  Class to Contain Objects Describing High-Throughput SNP Assays.
}

\description{
  Container for high-throughput assays and experimental
  metadata. \code{SnpSet} class is derived from
  \code{\link{eSet}}, and requires matrices \code{call},
  \code{callProbability} as assay data members.}

\section{Extends}{
   Directly extends class \code{\link{eSet}}.
}

\section{Creating Objects}{

  \code{new('SnpSet',
    phenoData = [AnnotatedDataFrame],
    experimentData = [MIAME],
    annotation = [character],
    protocolData = [AnnotatedDataFrame],
    call = [matrix],
    callProbability = [matrix],
    ...)
  }

  \code{SnpSet} instances are usually created through
  \code{new("SnpSet", ...)}. Usually the arguments to \code{new}
  include \code{call} (a matrix of genotypic calls, with features (SNPs)
  corresponding to rows and samples to columns), \code{phenoData},
  \code{experimentData}, \code{annotation}, and \code{protocolData}. 
  \code{phenoData}, \code{experimentData}, \code{annotation} and
  \code{protocolData} can be missing, in which case they are assigned
  default values.
}
\section{Slots}{
  Inherited from \code{eSet}:
   \describe{
      \item{\code{assayData}:}{Contains matrices with equal
	dimensions, and with column number equal to
	\code{nrow(phenoData)}. \code{assayData} must contain a matrix
	\code{call} with rows representing features (e.g., SNPs)
	and columns representing samples, and a matrix
	\code{callProbability} describing the certainty of the
	call. The content of \code{call} and \code{callProbability} are
	not enforced by the class. Additional matrices of
	identical size may also be included in
	\code{assayData}. Class:\code{\link{AssayData-class}}} 
      \item{\code{phenoData}:}{See \code{\link{eSet}}}
      \item{\code{experimentData}:}{See \code{\link{eSet}}}
      \item{\code{annotation}:}{See \code{\link{eSet}}}
      \item{\code{protocolData}:}{See \code{\link{eSet}}}
  }
}
\section{Methods}{

  Class-specific methods:
  \describe{
     \item{\code{snpCall(SnpSet)}, \code{snpCall(SnpSet,matrix)<-}}{Access and
       set elements named \code{call} in the \code{AssayData}
       slot.}

     \item{\code{exprs(SnpSet)}, \code{exprs(SnpSet,matrix)<-}}{Synonym
	   for \code{snpCall}.}

     \item{\code{snpCallProbability(SnpSet)},
	   \code{snpCallProbability<-(SnpSet,matrix)<-}}{ Access and set
	   elements named \code{callProbability} in the \code{AssayData}
	   slot.}  }
  
   Derived from \code{\link{eSet}}:
   \describe{
     \item{\code{updateObject(object, ..., verbose=FALSE)}}{Update instance to current version, if necessary. See \code{\link{updateObject}} and \code{\link{eSet}}}
     \item{\code{isCurrent(object)}}{Determine whether version of object is current. See \code{\link{isCurrent}}}
     \item{\code{isVersioned(object)}}{Determine whether object contains a 'version' string describing its structure . See \code{\link{isVersioned}}}
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
     \item{\code{protocolData(SnpSet)}, \code{protocolData(SnpSet,value)<-}}{See \code{\link{eSet}}}
     \item{\code{combine(SnpSet,SnpSet)}:}{See \code{\link{eSet}}}
     \item{\code{storageMode(eSet)}, \code{storageMode(eSet,character)<-}:}{See \code{\link{eSet}}}
  }

  Standard generic methods:
  \describe{
     \item{\code{initialize(SnpSet)}:}{Object instantiation, used by \code{new}; not to be called directly by the user.}
     \item{\code{validObject(SnpSet)}:}{Validity-checking method, ensuring that \code{call} and \code{callProbability} is a member of \code{assayData}. \code{checkValidity(SnpSet)} imposes this validity check, and the validity checks of \code{eSet}.}
     \item{\code{show(SnpSet)}}{See \code{\link{eSet}}}
     \item{\code{dim(SnpSet)}, \code{ncol}}{See \code{\link{eSet}}}
     \item{\code{SnpSet[(index)}:}{See \code{\link{eSet}}}
     \item{\code{SnpSet$}, \code{SnpSet$<-}}{See \code{\link{eSet}}}
  }
}

\author{Martin Morgan, V.J. Carey, after initial design by R. Gentleman }

\seealso{
  \code{\link{eSet}}, \code{\link{ExpressionSet}}
}

\keyword{classes}
