\name{read.AnnotatedDataFrame}
\alias{read.AnnotatedDataFrame}
\alias{write.AnnotatedDataFrame}
\concept{AnnotatedDataFrame}

\title{Read and write 'AnnotatedDataFrame'}

\description{

  Create an instance of class AnnotatedDataFrame by reading a file, or
  save an AnnotatedDataFrame to a file.

}

\usage{
read.AnnotatedDataFrame(filename, path,
     sep = "\t", header = TRUE, quote = "", stringsAsFactors = FALSE, 
     row.names = 1L,
     varMetadata.char="#",
     widget = getOption("BioC")$Base$use.widgets,
     sampleNames = character(0), ...) 
write.AnnotatedDataFrame(x, file="", varMetadata.char="#", ...,
     append=FALSE, fileEncoding="")
}

\arguments{
  \item{filename, file}{file or connection from which to read / write.}
  \item{x}{An instance of class \code{AnnotatedDataFrame}.}
  \item{path}{(optional) directory in which to find \code{filename}.}
  \item{row.names}{this argument gets passed on to
    \code{\link{read.table}} and will be used for the row names of the
    phenoData slot.}
  \item{varMetadata.char}{lines beginning with this character are used
    for the \code{varMetadata} slot. See examples.}
  \item{sep, header, quote, stringsAsFactors, ...}{further arguments
    that get passed on to \code{\link{read.table}} or \code{write.table}.}
  \item{widget}{logical. Currently this is \emph{not} implemented, and
    setting this option to \code{TRUE} will result in an error. In a
    precursor of this function, \code{read.phenoData}, this option could
    be used to open an interactive GUI widget for entering the data.}
  \item{sampleNames}{optional argument that could be used in conjunction
    with \code{widget}; do not use.}
  \item{append, fileEncoding}{Arguments as described in
    \code{\link{write.table}}}.
}

\details{

  The function \code{\link{read.table}} is used to read
  \code{pData}. The argument \code{varMetadata.char} is passed on to
  that function as its argument \code{comment.char}.
  Lines beginning with  \code{varMetadata.char} are expected to contain
  further information on the column headers of \code{pData}.
  The format is of the form: \code{# variable: textual explanation of the
    variable, units, measurement method, etc.} (assuming that \code{#}
  is the value of \code{varMetadata.char}). See also examples.

  \code{write.AnnotatedDataFrame} outputs \code{varLabels} and
  \code{varMetadata(x)$labelDescription} as commented header lines, and
  \code{pData(x)} as a with \code{write.table}.

}

\value{

  \code{read.AnnotatedDataFrame}: An instance of class
  \code{AnnotatedDataFrame}

  \code{write.AnnotatedDataFrame}: \code{NULL}, invisibly.

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
head(noquote(readLines(exampleFile)), 11)

write.AnnotatedDataFrame(adf)           # write to console by default

}
\keyword{file}
\keyword{manip}

