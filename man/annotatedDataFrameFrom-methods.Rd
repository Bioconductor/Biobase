\name{annotatedDataFrameFrom-methods}
\docType{methods}
\alias{annotatedDataFrameFrom}
\alias{annotatedDataFrameFrom,AssayData-method}
\alias{annotatedDataFrameFrom,matrix-method}
\alias{annotatedDataFrameFrom,NULL-method}

\title{Methods for Function annotatedDataFrameFrom in Package `Biobase'}
\description{
  \code{annotatedDataFrameFrom} is a convenience for creating
  \code{\link{AnnotatedDataFrame}} objects.
}
\section{Methods}{
  Use the method with \code{annotatedDataFrameFrom(object, byrow=FALSE,
    ...)}; the argument \code{byrow} \emph{must} be specified.
\describe{
  \item{\code{signature(object="assayData")}}{This method creates an
    \code{AnnotatedDataFrame} using sample (when \code{byrow=FALSE}) or
    feature (\code{byrow=TRUE}) names and dimensions of an
    \code{\link{AssayData}} object as a template.}
  \item{\code{signature(object="matrix")}}{This method creates an
    \code{AnnotatedDataFrame} using column (when \code{byrow=FALSE}) or
    row (\code{byrow=TRUE}) names and dimensions of a
    \code{\link{matrix}} object as a template.}
  \item{\code{signature(object="NULL")}}{This method (called with 'NULL'
    as the object) creates an empty \code{AnnotatedDataFrame}; provides
    \code{dimLabels} based on value of \code{byrow}.}
}}

\author{Biocore team}

\keyword{methods} 
