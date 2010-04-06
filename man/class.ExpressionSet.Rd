\name{ExpressionSet}
\docType{class}
\alias{class:ExpressionSet}
\alias{ExpressionSet}
\alias{ExpressionSet-class}

\alias{exprs,ExpressionSet-method}
\alias{exprs<-,ExpressionSet,matrix-method}

\alias{initialize,ExpressionSet-method}
\alias{coerce,exprSet,ExpressionSet-method}
\alias{coerce,eSet,ExpressionSet-method}
\alias{coerce,ExpressionSet,data.frame-method}
\alias{esApply,ExpressionSet-method}
\alias{updateObject,ExpressionSet-method}
\alias{makeDataPackage,ExpressionSet-method}
\alias{as.data.frame.ExpressionSet}
\alias{write.exprs}
\alias{write.exprs,ExpressionSet-method}

\title{
  Class to Contain and Describe High-Throughput Expression Level Assays.
}

\description{
  Container for high-throughput assays and experimental
  metadata. \code{ExpressionSet} class is derived from
  \code{\link{eSet}}, and requires a matrix named \code{exprs} as
  assayData member.
}
\section{Extends}{
   Directly extends class \code{\link{eSet}}.
}
\section{Creating Objects}{

  \code{new("ExpressionSet")}

  \code{new("ExpressionSet",
    phenoData = new("AnnotatedDataFrame"),
    featureData = new("AnnotatedDataFrame"),
    experimentData = new("MIAME"),
    annotation = character(0),
    protocolData = phenoData[,integer(0)],
    exprs = new("matrix"))
  }

  This creates an \code{ExpressionSet} with \code{assayData} implicitly
  created to contain \code{exprs}. Additional named matrix arguments
  with the same dimensions as \code{exprs} are added to
  \code{assayData}; the row and column names of these additional
  matricies should match those of \code{exprs}. 

  \code{new("ExpressionSet",
    assayData = assayDataNew(exprs=new("matrix")),
    phenoData = new("AnnotatedDataFrame"),
    featureData = new("AnnotatedDataFrame"),
    experimentData = new("MIAME"),
    annotation = character(0),
    protocolData = phenoData[,integer(0)])
  }

  This creates an \code{ExpressionSet} with \code{assayData} provided
  explicitly. In this form, the only required named argument is
  \code{assayData}.

  \code{as([exprSet],"ExpressionSet")}

  \code{ExpressionSet} instances are usually created through
  \code{new("ExpressionSet", ...)}. Usually the arguments to \code{new}
  include \code{exprs} (a matrix of expression data, with features
  corresponding to rows and samples to columns), \code{phenoData},
  \code{featureData}, \code{experimentData}, \code{annotation}, and
  \code{protocolData}. 
  \code{phenoData}, \code{featureData}, \code{experimentData},
  \code{annotation}, and \code{protocolData} can be missing, in which case
  they are assigned default values.
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
      \item{\code{featureData}:}{See \code{\link{eSet}}}
      \item{\code{experimentData}:}{See \code{\link{eSet}}}
      \item{\code{annotation}:}{See \code{\link{eSet}}}
      \item{\code{protocolData}:}{See \code{\link{eSet}}}
  }
}
\section{Methods}{

  Class-specific methods.
  \describe{
    \item{\code{as(exprSet,"ExpressionSet")}}{Coerce objects of
      \code{\link{exprSet-class}} to \code{ExpressionSet}}
    \item{\code{as(object,"data.frame")}}{Coerce objects of
      \code{\link{ExpressionSet-class}} to \code{data.frame} by
      transposing the expression matrix and concatenating \code{phenoData}}
     \item{\code{exprs(ExpressionSet)}, \code{exprs(ExpressionSet,matrix)<-}}{Access and
       set elements named \code{exprs} in the \code{AssayData-class}
       slot.}
     \item{\code{esApply(ExpressionSet, MARGIN, FUN,
         ...)}}{'apply'-like function to conveniently operate on
       \code{ExpressionSet} objects. See \code{\link{esApply}}.}
     \item{\code{write.exprs(ExpressionSet)}}{Write expression values to
	 a text file. It takes the same arguments as
	 \code{write.table}}
    }

   Derived from \code{\link{eSet}}:
   \describe{
     \item{\code{updateObject(object, ..., verbose=FALSE)}}{Update instance to current version, if necessary. See \code{\link{updateObject}} and \code{\link{eSet}}}
     \item{\code{isCurrent(object)}}{Determine whether version of object is current. See \code{\link{isCurrent}}}
     \item{\code{isVersioned(object)}}{Determine whether object contains a 'version' string describing its structure . See \code{\link{isVersioned}}}
     \item{\code{assayData(ExpressionSet)}:}{See \code{\link{eSet}}}
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
     \item{\code{protocolData(ExpressionSet)}, \code{protocolData(ExpressionSet,value)<-}}{See \code{\link{eSet}}}
     \item{\code{combine(ExpressionSet,ExpressionSet)}:}{See \code{\link{eSet}}}
     \item{\code{storageMode(ExpressionSet)}, \code{storageMode(ExpressionSet,character)<-}:}{See \code{\link{eSet}}}
  }

  Standard generic methods:
  \describe{
    \item{\code{initialize(ExpressionSet)}:}{Object instantiation, used
      by \code{new}; not to be called directly by the user.}
    \item{\code{updateObject(ExpressionSet)}:}{Update outdated versions
      of \code{ExpressionSet} to their current definition. See
      \code{\link{updateObject}}, \code{\link{Versions-class}}.}
    \item{\code{validObject(ExpressionSet)}:}{Validity-checking method, ensuring
      that \code{exprs} is a member of
      \code{assayData}. \code{checkValidity(ExpressionSet)} imposes this
      validity check, and the validity checks of \code{eSet}.}
    \item{\code{makeDataPackage(object, author, email, packageName, packageVersion, license, biocViews, filePath, description=paste(abstract(object), collapse="\n\n"), ...)}}{
      Create a data package based on an ExpressionSet object. See
      \code{\link{makeDataPackage}}.}
     \item{\code{as(exprSet,ExpressionSet)}:}{Coerce \code{exprSet} to \code{ExpressionSet}.}
     \item{\code{as(eSet,ExpressionSet)}:}{Coerce the \code{eSet} portion of an object to \code{ExpressionSet}.}
     \item{\code{show(ExpressionSet)}}{See \code{\link{eSet}}}
     \item{\code{dim(ExpressionSet)}, \code{ncol}}{See \code{\link{eSet}}}
     \item{\code{ExpressionSet[(index)}:}{See \code{\link{eSet}}}
     \item{\code{ExpressionSet$}, \code{ExpressionSet$<-}}{See \code{\link{eSet}}}
     \item{\code{ExpressionSet[[i]]}, \code{ExpressionSet[[i]]<-}}{See \code{\link{eSet}}}
  }
}

\author{Biocore team}

\seealso{
  \code{\link{eSet-class}}, \code{\link{ExpressionSet-class}}.
}

\examples{
# create an instance of ExpressionSet
new("ExpressionSet")

new("ExpressionSet",
    exprs=matrix(runif(1000), nrow=100, ncol=10))

# update an existing ExpressionSet
data(sample.ExpressionSet)
updateObject(sample.ExpressionSet)

# information about assay and sample data
featureNames(sample.ExpressionSet)[1:10]
sampleNames(sample.ExpressionSet)[1:5]
phenoData(sample.ExpressionSet)
experimentData(sample.ExpressionSet)

# subset: first 10 genes, samples 2, 4, and 10
expressionSet <- sample.ExpressionSet[1:10,c(2,4,10)]

# named features and their expression levels
subset <- expressionSet[c("AFFX-BioC-3_at","AFFX-BioDn-5_at"),]
exprs(subset)

# samples with above-average 'score' in phenoData
highScores <- expressionSet$score > mean(expressionSet$score)
expressionSet[,highScores]

# (automatically) coerce to data.frame
lm(score~AFFX.BioDn.5_at + AFFX.BioC.3_at, data=subset)
}

\keyword{classes}
