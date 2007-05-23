\name{read.AnnotatedDataFrame2}
\alias{read.AnnotatedDataFrame2}
\concept{AnnotatedDataFrame}

\title{Read 'AnnotatedDataFrame'}

\description{
  Create an instance of class AnnotatedDataFrame by reading a file.
}

\usage{
read.AnnotatedDataFrame2(filename,
                        sep = "\t", header = TRUE, quote = "", as.is = TRUE, 
                        row.names = 1L,
                        varmetadata.char="#", ...)
}

\arguments{
  \item{filename}{file or connection from which to read.}
  \item{row.names}{this argument gets passed on to
    \code{\link{read.table}} and will be used for the row names of the
    phenoData slot.}
  \item{varmetadata.char}{lines beginning with this character are used
    for the \code{varMetadata} slot. See examples.}
  \item{sep, header, quote, as.is, ...}{further arguments that get passed on to
    \code{\link{read.table}}.}
}

\details{
  The function \code{\link{read.table}} is used to read
  \code{pData}. The argument \code{varmetadata.char} is passed on to
  that function as its argument \code{comment.char}.

  Lines beginning with  \code{varmetadata.char} are expected to contain
  further textual information on the column headers in \code{pData},
  see examples.
}

\value{
   An instance of class \code{AnnotatedDataFrame}
}

\author{Martin Morgan <mtmorgan@fhcrc.org> and Wolfgang Huber,
  based on a previous version by Rafael A. Irizarry <rafa@jhu.edu>.}

\seealso{\code{\link{AnnotatedDataFrame}} for additional methods,
  \code{\link{read.table}} for details of reading dataframes from files
  or connections.}

\examples{

exampleFile = system.file("extdata", "pData.txt", package="Biobase")

adf <- read.AnnotatedDataFrame2(exampleFile)
adf
pData(adf)

readLines(exampleFile)[1:7]

}
\keyword{file}
\keyword{manip}

