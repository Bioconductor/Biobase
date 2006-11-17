\name{read.AnnotatedDataFrame}
\alias{read.AnnotatedDataFrame}
\concept{AnnotatedDataFrame}

\title{Read 'AnnotatedDataFrame'}

\description{
  Create an instance of class AnnotatedDataFrame by reading a file.

  Unfortunately, the 'widget' functionality is not yet implemented for read.AnnotatedDataFrame.
}

\usage{
read.AnnotatedDataFrame(filename = NULL, sampleNames = NULL, widget = getOption("BioC")$Base$use.widgets, ...)
}

\arguments{
   \item{filename}{filename from which to read information for \code{pData} slot.}
   \item{sampleNames}{\code{sampleNames} for rows of \code{pData}. If widgets are used this is required.}
   \item{widget}{logical. Currently NOT implmented. If \code{TRUE} and a filename is not given, widgets are used to enter information.}
   \item{\dots}{Further arguments to \code{read.table} }
}
\details{
  The function \code{\link{read.table}} is used to read \code{pData}. The
  function must be a flat file. The first row must be the column names
  which are the covariate names. The first column must be the
  sampleNames. Entries must be space delimited unless otherwise specied
  through \dots.

  If a filename overides the \code{widget} argument.

  If no filename is given and no widgets are used a dummy AnnotatedDataFrame
  object is created for the sampleNames. See example.}


\value{
   An instance of class \code{AnnotatedDataFrame}
}

\author{Martin Morgan <mtmorgan@fhcrc.org>, after Rafael A. Irizarry <rafa@jhu.edu>}

\seealso{\code{\link{AnnotatedDataFrame}}}

\examples{
pd <- read.AnnotatedDataFrame(sampleNames=c("sample1","sample2"),widget=FALSE)
show(pd)
show(pData(pd))
}
\keyword{file}
\keyword{manip}

