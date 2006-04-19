\name{AnnotatedDataFrame}
\docType{class}
\alias{class:AnnotatedDataFrame}
\alias{AnnotatedDataFrame}
\alias{AnnotatedDataFrame-class}

\alias{[,AnnotatedDataFrame-method}
\alias{[[<-,AnnotatedDataFrame-method}
\alias{[[,AnnotatedDataFrame-method}
\alias{$<-,AnnotatedDataFrame-method}
\alias{$,AnnotatedDataFrame-method}
\alias{combine,AnnotatedDataFrame,AnnotatedDataFrame-method}
\alias{initialize,AnnotatedDataFrame-method}
\alias{coerce,phenoData,AnnotatedDataFrame-method}
\alias{dim,AnnotatedDataFrame-method}
\alias{ncol,AnnotatedDataFrame-method}
\alias{pData<-,AnnotatedDataFrame,data.frame-method}
\alias{pData,AnnotatedDataFrame-method}
\alias{varMetadata<-,AnnotatedDataFrame,data.frame-method}
\alias{varMetadata,AnnotatedDataFrame-method}
\alias{sampleNames<-,AnnotatedDataFrame-method}
\alias{sampleNames,AnnotatedDataFrame-method}
\alias{show,AnnotatedDataFrame-method}
\alias{varLabels<-,AnnotatedDataFrame-method}
\alias{varLabels,AnnotatedDataFrame-method}

\title{
  Class to Contain Measured Variables and Their Meta-Data Description.
}

\description{
  \code{AnnotatedDataFrame}s consist of two parts. There is a collection
  of samples and the values of variables measured on those
  samples. There is also a description of each variable measured. The
  components of an \code{AnnotatedDataFrame} can be accessed with
  \code{\link{phenoData}} and \code{\link{varMetadata}}.
}
\section{Extends}{
   None
}
\section{Creating Objects}{

  \code{new("AnnotatedDataFrame")}

  \code{new("AnnotatedDataFrame", data=data.frame(), varMetadata=data.frame())}

  \code{AnnotatedDataFrame} instances are created using \code{new}.  The
  initialize method takes two arguments, \code{data} and
  \code{varMetadata}.  \code{data} is a \code{data.frame} of the samples
  (rows) and measured variables (columns).  \code{varMetadata} is a
  \code{data.frame} with the number of rows equal to the number of
  columns of the \code{data} argument.  \code{varMetadata} describes
  aspects of each measured variable.
}
\section{Slots}{
  Class-specific slots:
   \describe{
      \item{\code{data}:}{A \code{data.frame} containing samples (rows)
	and measured variables (columns).}
      \item{\code{varMetadata}:}{A \code{data.frame} with number of rows
	equal number of columns in \code{data}, and at least one column,
	named \code{labelDescription}, containing a textual description
	of each variable.}
  }
}
\section{Methods}{

  Class-specific methods.
  \describe{
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
    \item{\code{sampleNames(<AnnotatedDataFrame>)},
      \code{sampleNames(<AnnotatedDataFrame>)<-<data.frame>}:}{Set and
      retrieve the sample names in the \code{AnnotatedDataFrame}}
    \item{\code{varLabels(<AnnotatedDataFrame>)},
      \code{varLabels(<AnnotatedDataFrame>)<-<data.frame>}:}{Set and
      retrieve the variable labels in the \code{AnnotatedDataFrame}}
    }

  Standard generic methods:
  \describe{
    \item{\code{initialize(<AnnotatedDataFrame>)}:}{Object instantiation, used
      by \code{new}; not to be called directly by the user.}
    \item{\code{coerce(<phenoData>,<AnnotatedDataFrame>)}:}{Convert
      old-style \code{phenoData-class} objects to
      \code{AnnotatedDataFrame}, issuing warnings as appropriate.}
    \item{\code{validObject(<AnnotatedDataFrame>)}:}{Validity-checking
      method, ensuring coordination between \code{data} and
      \code{varMetadata} elements}
     \item{\code{show(<AnnotatedDataFrame>)}}{Abbreviated display of object}
    \item{\code{[<sample>,<variable>}:}{Subset operation, taking two arguments and
      indexing the sample and variable. Returns an
      \code{AnnotatedDataFrame}, i.e., including relevant metadata.}
    \item{\code{[[<sample>}, \code{$<-<sample>}:}{Selector returning a sample and measured
      variables.}
    \item{\code{[[<-<new_sample>}:}{Replace a sample row of data.}
    \item{\code{$<sample>}:}{Selector returning a sample and measured
      variables.}
    \item{\code{dim(<AnnotatedDataFrame>)},
      \code{ncol(<AnnotatedDataFrame>)}:}{Number of samples and
      variables (\code{dim}) and variables (\code{ncol}) in the
      argument.}
  }
}
\author{V.J. Carey, after initial design by R. Gentleman }

\seealso{
  \code{\link{eSet}}, \code{\link{ExpressionSet}}
}

\keyword{classes}
