\name{read.AnnotatedDataFrame}
\alias{read.AnnotatedDataFrame}
\concept{AnnotatedDataFrame}

\title{Read 'AnnotatedDataFrame'}

\description{
  Create an instance of class AnnotatedDataFrame by reading a file.

  Unfortunately, the 'widget' functionality is not yet implemented for read.AnnotatedDataFrame.
}

\usage{
read.AnnotatedDataFrame(filename = NULL,
                        sampleNames = character(0),
                        widget = getOption("BioC")$Base$use.widgets,
                        ...)
}

\arguments{
   \item{filename}{file name from which to read information for \code{pData} slot.}
   \item{sampleNames}{Optional \code{sampleNames} for rows of \code{pData}. If widgets are used this is required.}
   \item{widget}{logical. Currently NOT implmented. If \code{TRUE} and a \code{filename} is not given, widgets are used to enter information.}
   \item{\dots}{Further arguments to \code{read.table}.}
}
\details{
  The function \code{\link{read.table}} is used to read
  \code{pData}. The structure of the \code{pData} file is flexible (see
  the example, below, and \code{\link{read.table}}).

  If a \code{filename} is present, it overides the \code{widget}
  argument.

  If no file name is given and no widget used, an
  \code{AnnotatedDataFrame} is created for \code{sampleNames}.}


\value{
   An instance of class \code{AnnotatedDataFrame}
}

\author{Martin Morgan <mtmorgan@fhcrc.org>, after Rafael A. Irizarry <rafa@jhu.edu>}

\seealso{\code{\link{AnnotatedDataFrame}} for additional methods,
  \code{\link{read.table}} for details of reading in phenotypic data}

\examples{

## Create a temporary file with a structure like that from 'Save
## as...' text file from a popular spreadsheet:

## Id	x	y	z
## Sample A	1	Low	a
## Sample B	2	High	b
## Sample C	3	Low	c
## Sample D	4	High	d
## Sample E	5	Low	e
## Sample F	6	High	f

ff <- tempfile()
df <- data.frame(Id=paste("Sample", 1:6),
                 x=1:6,
                 y=rep(c("Low", "High"), 3),
                 z=I(letters[1:6]))
write.table(df, ff, quote=FALSE, sep="\t", row.names=FALSE)

## read.AnnotatedDataFrame uses read.table(file, ...) to read the
## phenotypic data. We need to consult the help page for read.table to
## figure out the arguments required to read the file correctly.

## By default, read.table treats any 'white space' as a column
## delimiter, whereas our file uses a tab. To read the table
## correctly, we add the argument 'sep="\t"'

## read.table recognizes a 'header' line containing column names if
## the first line has one fewer columns than the rest of the
## table. Our file has column headers that do not conform to this
## convention, so read.table needs to be informed of these explicitly
## by adding the argument 'header=TRUE'

## The column labeled 'Id' is meant to be the row.name of the data,
## instead of just another column. We tell read.table this with the
## argument row.names="Id"

## Columns with characters in them are coerced by read.table to be a
## 'factor'. This is as desired for column y, but we want column z to
## be treated as 'character'. The argument colClasses gives us this
## kind of control, colClasses=c(z="character")

## We are now ready to read our data:

adf <- read.AnnotatedDataFrame(ff,
                               header=TRUE, sep="\t",
                               row.names="Id",
                               colClasses=c(z="character"))
adf
pData(adf)
sapply(pData(adf), class)

## This illustrates the format that could be read into R with
## read.AnnotatedDataFrame(ff, colClasses=c(z="character")

write.table(pData(adf))

## Perhaps we want to provide our own sampleNames

adf <- read.AnnotatedDataFrame(ff,
                               sampleNames=paste(
                                 "OurId", LETTERS[1:6], sep="_"),
                               header=TRUE, sep="\t",
                               colClasses=c(z="character"))
adf
pData(adf)

## clean up
unlink(ff)

}
\keyword{file}
\keyword{manip}

