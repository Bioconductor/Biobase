\name{readExpressionSet}
\alias{readExpressionSet}
\concept{ExpressionSet}

\title{Read 'ExpressionSet'}

\description{
  Create an instance of class ExpressionSet by reading data from files.
  \sQuote{widget} functionality is not implemented for readExpressionSet.
}

\usage{
readExpressionSet(exprsFile,
      phenoDataFile,
      experimentDataFile,
      notesFile,
      path,
      annotation,
      ## arguments to read.* methods 
      exprsArgs=list(sep=sep, header=header, row.names=row.names,
        quote=quote, ...),
      phenoDataArgs=list(sep=sep, header=header, row.names=row.names,
        quote=quote, stringsAsFactors=stringsAsFactors, ...),
      experimentDataArgs=list(sep=sep, header=header,
        row.names=row.names, quote=quote,
        stringsAsFactors=stringsAsFactors, ...),
      sep = "\t", header = TRUE, quote = "", stringsAsFactors = FALSE,
      row.names = 1L,
      ## widget
      widget = getOption("BioC")$Base$use.widgets,
      ...)
}

\arguments{
  \item{exprsFile}{(character) File or connection from which to read expression
    values. The file should contain a matrix with rows as features and
    columns as samples. \code{\link{read.table}} is called with this
    as its \code{file} argument and further arguments given by \code{exprsArgs}.}
  \item{phenoDataFile}{(character) File or connection from which to read
    phenotypic data. \code{\link{read.AnnotatedDataFrame}} is called
    with this as its \code{file} argument and further arguments given by
    \code{phenoDataArgs}.}
  \item{experimentDataFile}{(character) File or connection from which to read
    experiment data. \code{\link{read.MIAME}} is called with this
    as its \code{file} argument and further arguments given by
    \code{experimentDataArgs}.}
  \item{notesFile}{(character) File or connection from which to read notes;
    \code{\link{readLines}} is used to input the file.}
  \item{path}{(optional) directory in which to find all the above files.}
  \item{annotation}{(character) A single character string indicating the
    annotation associated with this ExpressionSet.}
  \item{exprsArgs}{A list of arguments to be used with
    \code{\link{read.table}} when reading in the expression matrix.}
  \item{phenoDataArgs}{A list of arguments to be used (with
    \code{\link{read.AnnotatedDataFrame}}) when reading
    the phenotypic data.}
  \item{experimentDataArgs}{A list of arguments to be used (with
    \code{\link{read.MIAME}}) when reading the experiment data.}
  \item{sep, header, quote, stringsAsFactors, row.names}{arguments used
    by the \code{\link{read.table}}-like functions.}
  \item{widget}{A boolean value indicating whether widgets can be
    used. Widgets are NOT yet implemented for
    \code{read.AnnotatedDataFrame}.}
  \item{\dots}{Further arguments that can be passed on to the
    \code{\link{read.table}}-like functions.}
}
\details{
  Expression values are read using the \code{\link{read.table}}
  function. Phenotypic data are read using the
  \code{\link{read.AnnotatedDataFrame}} function. Experiment data are
  read using the \code{\link{read.MIAME}} function. Notes are read using
  the \code{\link{readLines}} function. The return value must be a valid
  \code{ExpressionSet}. Only the \code{exprsFile} argument is required.
}
  

\value{
  An instance of the \code{\linkS4class{ExpressionSet}} class.
}

\author{Martin Morgan <mtmorgan@fhcrc.org>}

\seealso{\code{\link{ExpressionSet}} for additional methods.}

\examples{

exprsFile = system.file("extdata", "exprsData.txt", package="Biobase")
phenoFile = system.file("extdata", "pData.txt", package="Biobase")

## Read ExpressionSet with appropriate parameters
obj = readExpressionSet(exprsFile, phenoFile, sep = "\t", header=TRUE)
obj

}
\keyword{file}
\keyword{manip}
