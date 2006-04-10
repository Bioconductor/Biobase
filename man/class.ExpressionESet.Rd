\name{ExpressionESet}
\docType{class}
\alias{class:ExpressionESet}
\alias{ExpressionESet}

\alias{ExpressionESet-class}

\title{
  Virtual Class to Group Objects Describing High-Throughput Expression Level Assays.
}

\description{
  General container for high-throughput assays and experimental
  metadata. \code{ExpressionESet} class is derived from
  \code{\link{eSet}} class, and is implemented in
  \code{\link{ExpressionSet}} and \code{\link{MultiExpressionSet}}.
}
\section{Extends}{
   Directly extends class \code{\link{eSet}}.
}
\section{Objects from the Class}{
  \code{ExpressionESet} is a virtual class; \code{ExpressionESet} objects cannot be created.
  See classes \code{\link{ExpressionSet}}, \code{\link{MultiExpressionSet}} for
  examples of concrete classes derived from \code{ExpressionESet}.
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
  Methods defined in derived classes (e.g., \code{\link{ExpressionSet}},
  \code{\link{MultiExpressionSet}}) may override the methods described here.

  Class-specific methods: none.

   Derived from \code{\link{eSet}}:
   \describe{
     \item{\code{sampleNames(ExpressionESet)} and \code{sampleNames(ExpressionESet)<-}:}{See \code{\link{eSet}}}
     \item{\code{featureNames(ExpressionESet)}, \code{featureNames(ExpressionESet, value)<-}:}{See \code{\link{eSet}}}
     \item{\code{dims}:}{See \code{\link{eSet}}}
     \item{\code{phenoData(ExpressionESet)}, \code{phenoData(ExpressionESet,value)<-}:}{See \code{\link{eSet}}}
     \item{\code{varLabels(ExpressionESet)}, \code{varLabels(ExpressionESet, value)<-}:}{See \code{\link{eSet}}}
     \item{\code{varMetadata(ExpressionESet)}, \code{varMetadata(ExpressionESet,value)<-}:}{See \code{\link{eSet}}}
     \item{\code{pData(ExpressionESet)}, \code{pData(ExpressionESet,value)<-}:}{See \code{\link{eSet}}}
     \item{\code{varMetadata(ExpressionESet)}, \code{varMetadata(ExpressionESet,value)}}{See \code{\link{eSet}}}
     \item{\code{experimentData(ExpressionESet)},\code{experimentData(ExpressionESet,value)<-}:}{See \code{\link{eSet}}}
     \item{\code{pubMedIds(ExpressionESet)}, \code{pubMedIds(ExpressionESet,value)}}{See \code{\link{eSet}}}
     \item{\code{abstract(ExpressionESet)}:}{See \code{\link{eSet}}}
     \item{\code{annotation(ExpressionESet)}, \code{annotation(ExpressionESet,value)<-}}{See \code{\link{eSet}}}
     \item{\code{combine(ExpressionESet,ExpressionESet)}:}{See \code{\link{eSet}}}
     \item{\code{reporterNames(ExpressionESet)}, \code{reporterNames(ExpressionESet,value)<-}:}{DEPRECATED}
  }
  Standard generic methods:
  \describe{
    \item{\code{initialize(ExpressionESet)}:}{Object instantiate, can be called by
      derived classes but not usually by the user.}
    \item{\code{validObject(ExpressionESet)}:}{Validity-checking method,
	  ensuring (1) all assayData components have the same number of
	  features and samples; (2) the number and names of
	  \code{phenoData} rows match the number and names of
	  \code{assayData} columns}
     \item{\code{show(ExpressionESet)}}{Informatively display object contents.}
     \item{\code{dim(ExpressionESet)}, \code{ncol}}{Access the common
       dimensions (\code{dim}) or column numbers (\code{ncol}), of all
       memebers (\code{dims}) of \code{assayData}.}
     \item{\code{ExpressionESet[(index)}:}{Conducts subsetting of matrices and
       phenoData components }
     \item{\code{ExpressionESet$}, \code{ExpressionESet$<-}}{Access and set named columns in \code{phenoData}}
  }
}
\author{V.J. Carey, after initial design by R. Gentleman }

\seealso{
  \code{\link{eSet}}, \code{\link{ExpressionSet}}, \code{\link{MultiExpressionSet}}
}

\keyword{classes}
