\name{AnnotatedDataFrame}
\docType{class}
\alias{class:AnnotatedDataFrame}
\alias{AnnotatedDataFrame}
\alias{AnnotatedDataFrame-class}
\alias{AnnotatedDataFrame,missing,missing-method}
\alias{AnnotatedDataFrame,data.frame,missing-method}
\alias{AnnotatedDataFrame,data.frame,data.frame-method}

\alias{dimLabels}
\alias{dimLabels<-}

\alias{[,AnnotatedDataFrame-method}
\alias{[[<-,AnnotatedDataFrame-method}
\alias{[[,AnnotatedDataFrame-method}
\alias{$<-,AnnotatedDataFrame-method}
\alias{$,AnnotatedDataFrame-method}
\alias{coerce,AnnotatedDataFrame,data.frame-method}
\alias{combine,AnnotatedDataFrame,AnnotatedDataFrame-method}
\alias{initialize,AnnotatedDataFrame-method}
\alias{coerce,data.frame,AnnotatedDataFrame-method}
\alias{coerce,phenoData,AnnotatedDataFrame-method}
\alias{head.AnnotatedDataFrame}
\alias{tail.AnnotatedDataFrame}
\alias{dim,AnnotatedDataFrame-method}
\alias{dimnames,AnnotatedDataFrame-method}
\alias{dimnames<-,AnnotatedDataFrame-method}
\alias{dimLabels,AnnotatedDataFrame-method}
\alias{dimLabels<-,AnnotatedDataFrame,character-method}
\alias{ncol,AnnotatedDataFrame-method}
\alias{pData<-,AnnotatedDataFrame,data.frame-method}
\alias{pData,AnnotatedDataFrame-method}
\alias{varMetadata<-,AnnotatedDataFrame,data.frame-method}
\alias{varMetadata,AnnotatedDataFrame-method}
\alias{sampleNames<-,AnnotatedDataFrame,ANY-method}
\alias{sampleNames,AnnotatedDataFrame-method}
\alias{featureNames,AnnotatedDataFrame-method}
\alias{featureNames<-,AnnotatedDataFrame-method}
\alias{show,AnnotatedDataFrame-method}
\alias{updateObject,AnnotatedDataFrame-method}
\alias{varLabels<-,AnnotatedDataFrame-method}
\alias{varLabels,AnnotatedDataFrame-method}

\title{
  Class Containing Measured Variables and Their Meta-Data Description.
}

\description{
  An \code{AnnotatedDataFrame} consists of two parts. There is a collection
  of samples and the values of variables measured on those
  samples. There is also a description of each variable measured. The
  components of an \code{AnnotatedDataFrame} can be accessed with
  \code{\link{pData}} and \code{\link{varMetadata}}.
}
\section{Extends}{
   Versioned
}
\section{Creating Objects}{

  \code{AnnotatedDataFrame(data, varMetadata, dimLabels=c("rowNames", "columnNames"), ...)}

  \code{AnnotatedDataFrame} instances are created using
  \code{AnnotatedDataFrame}. The function can take three arguments, \code{data} is a
  \code{data.frame} of the samples (rows) and measured variables
  (columns).  \code{varMetadata} is a \code{data.frame} with the number
  of rows equal to the number of columns of the \code{data} argument.
  \code{varMetadata} describes aspects of each measured
  variable. \code{dimLabels} provides aesthetic control for labeling rows
  and columns in the \code{show} method. \code{varMetadata} and
  \code{dimLabels} can be missing.

\code{as(data.frame, "AnnotatedDataFrame")} coerces a \code{data.frame} to an
\code{AnnotatedDataFrame}.

\code{\link{annotatedDataFrameFrom}} may be a convenient way to create
an \code{AnnotatedDataFrame} from \code{\link{AssayData-class}}.

}

\section{Slots}{
  Class-specific slots:
   \describe{
      \item{\code{data}:}{A \code{data.frame} containing samples (rows)
	and measured variables (columns).}
	  \item{\code{dimLabels}:}{A \code{character} vector of length 2 that
	provides labels for the rows and columns in the \code{show} method.}
      \item{\code{varMetadata}:}{A \code{data.frame} with number of rows
	equal number of columns in \code{data}, and at least one column,
	named \code{labelDescription}, containing a textual description
	of each variable.}
      \item{\code{.__classVersion__}:}{A \code{Versions} object describing
    the R and Biobase version numbers used to created the instance.
    Intended for developer use.}
  }
}
\section{Methods}{

  Class-specific methods.
  \describe{

    \item{\code{as(annotatedDataFrame, "data.frame")}}{Coerce objects of
      \code{AnnotatedDataFrame} to \code{data.frame}.}
    \item{\code{combine(<AnnotatedDataFrame>,
	<AnnotatedDataFrame>}:}{Bind data from one
      \code{AnnotatedDataFrame} to a second
      \code{AnnotatedDataFrame}, returning the result as an
      \code{AnnotatedDataFrame}. Row (sample) names in each argument
      must be unique. Variable names present in both arguments occupy a
      single column in the resulting \code{AnnotatedDataFrame}. Variable
      names unique to either argument create columns with values
      assigned for those samples where the variable is
      present. \code{varMetadata} in the returned
      \code{AnnotatedDataFrame} is updated to reflect the combination.}
    \item{\code{pData(<AnnotatedDataFrame>)},
      \code{pData(<AnnotatedDataFrame>)<-<data.frame>}:}{Set and
      retrieve the data (samples and variables) in the
      \code{AnnotatedDataFrame}}
    \item{\code{varMetadata(<AnnotatedDataFrame>)},
      \code{varMetadata(<AnnotatedDataFrame>)<-<data.frame>}:}{Set and
      retrieve the meta-data (variables and their descriptions) in the
      \code{AnnotatedDataFrame}}
    \item{\code{featureNames(<AnnotatedDataFrame>)},
	\code{featureNames(<AnnotatedDataFrame>)<-<ANY>}:}{Set and
	  retrieve the feature names in \code{AnnotatedDataFrame}; a
	  synonym for \code{sampleNames}.}
    \item{\code{sampleNames(<AnnotatedDataFrame>)},
      \code{sampleNames(<AnnotatedDataFrame>)<-<ANY>}:}{Set and
      retrieve the sample names in \code{AnnotatedDataFrame}}
    \item{\code{varLabels(<AnnotatedDataFrame>)},
      \code{varLabels(<AnnotatedDataFrame>)<-<data.frame>}:}{Set and
      retrieve the variable labels in the \code{AnnotatedDataFrame}}
    \item{
      \code{dimLabels(<AnnotatedDataFrame>)},
      \code{dimLabels(<AnnotatedDataFrame>) <- <character>}}{Retrieve
      labels used for display of \code{AnnotatedDataFrame}, e.g.,
      \sQuote{rowNames}, \sQuote{columnNames}.}
  }

  Standard generic methods:
  \describe{
    \item{\code{initialize(<AnnotatedDataFrame>)}:}{Object instantiation, used
      by \code{new}; not to be called directly by the user.}
    \item{\code{as(<data.frame>, "AnnotatedDataFrame")}:}{Convert a
      \code{data.frame} to an AnnotatedDataFrame.}
    \item{\code{as(<phenoData>,<AnnotatedDataFrame>)}:}{Convert
      old-style \code{phenoData-class} objects to
      \code{AnnotatedDataFrame}, issuing warnings as appropriate.}
    \item{\code{validObject(<AnnotatedDataFrame>)}:}{Validity-checking
      method, ensuring coordination between \code{data} and
      \code{varMetadata} elements}
     \item{\code{updateObject(object, ..., verbose=FALSE)}}{Update instance to current version, if necessary. See \code{\link{updateObject}}}
     \item{\code{isCurrent(object)}}{Determine whether version of object is current. See \code{\link{isCurrent}}}
     \item{\code{isVersioned(object)}}{Determine whether object contains a 'version' string describing its structure . See \code{\link{isVersioned}}}
     \item{\code{show(<AnnotatedDataFrame>)}}{Abbreviated display of object}
    \item{\code{[<sample>,<variable>}:}{Subset operation, taking two arguments and
      indexing the sample and variable. Returns an
      \code{AnnotatedDataFrame}, i.e., including relevant
      metadata. Unlike a \code{data.frame}, setting \code{drop=TRUE}
      generates an error. }
    \item{\code{[[<variable>}, \code{$<variable>}:}{Selector returning a
      variable (column of \code{pData}).}
    \item{\code{[[<variable>, ...]]<-<new_value>},
      \code{$<variable> <- <new_value>}:}{Replace or add a variable to
      \code{pData}. ... can include named arguments (especially
      \code{labelDescription}) to be added to \code{varMetadata}.}
    \item{\code{head(<AnnotatedDataFrame>, n = 6L, ...)},
      \code{tail(<AnnotatedDataFrame>, n=6L, ...)}}{Select the first
      (last for tail) \code{n} rows; negative \code{n} returns the first
      (last) \code{nrow() - n} rows.}
    \item{\code{dim(<AnnotatedDataFrame>)},
      \code{ncol(<AnnotatedDataFrame>)}:}{Number of samples and
      variables (\code{dim}) and variables (\code{ncol}) in the
      argument.}
    \item{\code{dimnames(<AnnotatedDataFrame>)},
      \code{rownames(<AnnotatedDataFrame>)},
      \code{colnames(<AnnotatedDataFrame>)}:}{row and column names.}
  }
}
\author{V.J. Carey, after initial design by R. Gentleman }

\seealso{
  \code{\link{eSet}}, \code{\link{ExpressionSet}},
  \code{\link{read.AnnotatedDataFrame}}
}
\examples{
df <- data.frame(x=1:6,
                 y=rep(c("Low", "High"),3),
                 z=I(LETTERS[1:6]),
                 row.names=paste("Sample", 1:6, sep="_"))
metaData <-
  data.frame(labelDescription=c(
               "Numbers",
               "Factor levels",
               "Characters"))

AnnotatedDataFrame()
AnnotatedDataFrame(data=df)
AnnotatedDataFrame(data=df, varMetadata=metaData)
as(df, "AnnotatedDataFrame")

obj <- AnnotatedDataFrame()
pData(obj) <- df
varMetadata(obj) <- metaData
validObject(obj)
}
\keyword{classes}
