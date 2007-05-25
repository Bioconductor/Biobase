\name{read.AnnotatedDataFrame}
\alias{read.AnnotatedDataFrame}
\concept{AnnotatedDataFrame}

\title{Read 'AnnotatedDataFrame'}

\description{
  Create an instance of class AnnotatedDataFrame by reading a file.
}

\usage{
read.AnnotatedDataFrame(filename, path,
     sep = "\t", header = TRUE, quote = "", stringsAsFactors = FALSE, 
     row.names = 1L,
     varmetadata.char="#",
     widget = getOption("BioC")$Base$use.widgets,
     sampleNames = character(0), ...) 
}

\arguments{
  \item{filename}{file or connection from which to read.}
  \item{path}{(optional) directory in which to find \code{filename}.}
  \item{row.names}{this argument gets passed on to
    \code{\link{read.table}} and will be used for the row names of the
    phenoData slot.}
  \item{varmetadata.char}{lines beginning with this character are used
    for the \code{varMetadata} slot. See examples.}
  \item{sep, header, quote, stringsAsFactors, ...}{further arguments that get passed on to
    \code{\link{read.table}}.}
  \item{widget}{logical. Currently this is \emph{not} implemented, and
    setting this option to \code{TRUE} will result in an error. In a
    precursor of this function, \code{read.phenoData}, this option could
    be used to open an interactive GUI widget for entering the data.}
  \item{sampleNames}{optional argument that could be used in conjunction
    with \code{widget}; do not use.}
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
  based on \code{read.phenoData} by Rafael A. Irizarry.}

\seealso{\code{\link{AnnotatedDataFrame}} for additional methods,
  \code{\link{read.table}} for details of reading in phenotypic data}

\examples{

exampleFile = system.file("extdata", "pData.txt", package="Biobase")

adf <- read.AnnotatedDataFrame(exampleFile)
adf
head(pData(adf))
head(readLines(exampleFile))

}
\keyword{file}
\keyword{manip}

