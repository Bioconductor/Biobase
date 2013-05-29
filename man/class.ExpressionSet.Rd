\name{ExpressionSet}
\docType{class}
\alias{class:ExpressionSet}
\alias{ExpressionSet}
\alias{ExpressionSet-class}
\alias{ExpressionSet,missing-method}
\alias{ExpressionSet,environment-method}
\alias{ExpressionSet,matrix-method}

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

\usage{
## Instance creation
ExpressionSet(assayData,
    phenoData=annotatedDataFrameFrom(assayData, byrow=FALSE),
    featureData=annotatedDataFrameFrom(assayData, byrow=TRUE),
    experimentData=MIAME(), annotation=character(),
    protocolData=annotatedDataFrameFrom(assayData, byrow=FALSE),
    ...)

## Additional methods documented below
}

\arguments{

  \item{assayData}{A \code{matrix} of expression values, or an
    \code{environment}.

    When \code{assayData} is a \code{matrix}, the rows represent probe
    sets (\sQuote{features} in \code{ExpressionSet} parlance).  Columns
    represent samples. When present, row names identify features and
    column names identify samples. Row and column names must be unique,
    and consistent with row names of \code{featureData} and
    \code{phenoData}, respectively. The assay data can be retrieved with
    \code{exprs()}.

    When \code{assayData} is an environment, it contains identically
    dimensioned matrices like that described in the previous
    paragraph. One of the elements of the environment must be named
    \sQuote{exprs}; this element is returned with \code{exprs()}.

  }

  \item{phenoData}{An optional \code{AnnotatedDataFrame} containing
    information about each sample. The number of rows in
    \code{phenoData} must match the number of columns in
    \code{assayData}. Row names of \code{phenoData} must match column
    names of the matrix / matricies in \code{assayData}.}

  \item{featureData}{An optional \code{AnnotatedDataFrame} containing
    information about each feature. The number of rows in
    \code{featureData} must match the number of rows in
    \code{assayData}. Row names of \code{featureData} must match row
    names of the matrix / matricies in \code{assayData}.}

  \item{experimentData}{An optional \code{MIAME} instance with meta-data
    (e.g., the lab and resulting publications from the analysis) about
    the experiment.}

  \item{annotation}{A \code{character} describing the platform on which
    the samples were assayed. This is often the name of a Bioconductor
    chip annotation package, which facilitated down-stream analysis.}

  \item{protocolData}{An optional \code{AnnotatedDataFrame} containing
    equipment-generated information about protocols. The number of rows
    and row names of \code{protocolData} must agree with the dimension
    and column names of \code{assayData}.}

  \item{\dots}{Additional arguments, passed to
    \code{new("ExpressionSet", ...)} and available for classes that extend
    \code{ExpressionSet}.
  }

}

\section{Extends}{
   Directly extends class \code{\link{eSet}}.
}
\section{Creating Objects}{

  \code{ExpressionSet} instances are usually created through
  \code{ExpressionSet()}.

}
\section{Slots}{
  Inherited from \code{eSet}:
   \describe{
      \item{\code{assayData}:}{Contains matrices with equal
	dimensions, and with column number equal to
	\code{nrow(phenoData)}. \code{assayData} must contain a matrix
	\code{exprs} with rows representing features (e.g., probe sets)
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
ExpressionSet()

ExpressionSet(assayData=matrix(runif(1000), nrow=100, ncol=10))

# update an existing ExpressionSet
data(sample.ExpressionSet)
updateObject(sample.ExpressionSet)

# information about assay and sample data
featureNames(sample.ExpressionSet)[1:10]
sampleNames(sample.ExpressionSet)[1:5]
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
