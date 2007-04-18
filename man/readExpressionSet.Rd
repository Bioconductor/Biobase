\name{readExpressionSet}
\alias{readExpressionSet}
\concept{ExpressionSet}

\title{Read 'ExpressionSet'}

\description{
  Create an instance of class ExpressionSet by reading data from files.

  Unfortunately, 'widget' functionality is not yet implemented for read.ExpressionSet.
}

\usage{
readExpressionSet(exprsFile,
                   phenoDataFile=character(0),
                   experimentDataFile=character(0),
                   notesFile=character(0),
                   annotation=character(0),
                   ## arguments to read.* methods 
                   exprsArgs=list(...),
                   phenoDataArgs=list(...),
                   experimentDataArgs=list(...),
                   notesArgs=list(...),
                   ## widget
                   widget = getOption("BioC")$Base$use.widgets,
                   ...)
}

\arguments{
  \item{exprsFile}{(character) File name from which to read expression
	values. File should contain a matrix with rows as features and
	columns as samples. Tab-delimited files can be read with the
	additional argument \code{sep="\t"}.}
  \item{phenoDataFile}{(character) File name from which to read
	phenotypic data. See \code{\link{read.AnnotatedDataFrame}} for
	details.}
  \item{experimentDataFile}{(character) File name from which to read
	experiment data. See \code{\link{read.MIAME}} for details.}
  \item{notesFile}{(character) File name from which to read notes;
	\code{\link{readLines}} is used to input the file.}
  \item{annotation}{(character) A single character string indicating the
	annotation associated with this ExpressionSet.}
  \item{exprsArgs}{A list of arguments to be used (with
	\code{read.table})  when reading in the expression matrix.}
  \item{phenoDataArgs}{A list of arguments to be used (with
	\code{\link{read.AnnotatedDataFrame}}) when reading
	\code{phenoData}.}
  \item{experimentDataArgs}{A list of arguments to be used (with
	\code{\link{read.MIAME}}) when reading experiment data.}
  \item{notesArgs}{A list of arguments to be used (with
	\code{\link{readLines}}) when reading notes.}
  \item{widget}{A boolean value indicating whether widgets can be
	used. Widgets are NOT yet implemented for
	\code{read.AnnotatedDataFrame}.}
  \item{\dots}{Arugments passed by default to all methods used to read
	data.}
}
\details{
  Expression values are read using the \code{\link{read.table}}
  function. Phenotypic data are read using the
  \code{\link{read.AnnotatedDataFrame}} function. Experiment data are
  read using the \code{\link{read.MIAME}} function. Notes are read using
  the \code{\link{readLines}} function. The return value must be a valid
  \code{ExpressionSet}.
}
  

\value{
   An instance of \code{\link{ExpressionSet-class}}.
}

\author{Martin Morgan <mtmorgan@fhcrc.org>}

\seealso{\code{\link{ExpressionSet}} for additional methods.}

\examples{

## Create a temporary file for expression data

exprsFile <- tempfile()
data(geneData)
write.table(geneData[,1:6], exprsFile, quote = FALSE, sep = "\t",
            row.names = TRUE, col.names = TRUE)

## Create a temporary file with a structure like that from 'Save
## as...' text file from a popular spreadsheet (see
## read.AnnotatedDataFrame)

phenoDataFile <- tempfile()
df <- data.frame(Id=paste("Sample", 1:6),
                 x=1:6,
                 y=rep(c("Low", "High"), 3),
                 z=I(letters[1:6]))
write.table(df, phenoDataFile, quote=FALSE, sep="\t", row.names=FALSE)

## Read ExpressionSet with appropriate parameters

obj <- readExpressionSet(exprsFile, phenoDataFile, header=TRUE, sep="\t")
obj

unlink(c("exprsFile", "phenoDataFile"))

}
\keyword{file}
\keyword{manip}

