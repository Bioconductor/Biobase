\name{read.exprSet}
\alias{read.exprSet}
\concept{exprSet}

\title{Read Experimental Data and into an 'exprSet' Object}

\description{
   An exprSet object has a slot named \code{exprs} for experimental data. \code{read.exprSet} reads the experimental data stored as a text file and then generates an exprSet object using the data. Data for other slots can be also read in as options.
}

\usage{
read.exprSet(exprs, se.exprs, phenoData, annotation, description, notes, seps = "\t")
}
\arguments{
   \item{exprs}{\code{exprs} a character string for the name of the text file that stores the experimental data to be read in. The text file is assumed to be a delimited file with observed expression levels arranged in rows and columns. The first row of the file is sample names and the first column is gene names/ids. The file will be read in using read.table with header set to TRUE}
   \item{se.exprs}{\code{se.exprs} a character stirng for the name of a delimited text file contains standard error estimates for the estimated expression in exprs. The file is of the same dimensions as 'exprs'}
   \item{phenoData}{\code{phenoData} a character string for the name of an optional file containing information for 'pData' slot of a phenoData object to be returned. If missing a default phenoData object with one covariate ranging from 1 to the number of columns of 'exprs' will be provided}
   \item{annotation}{\code{annotation} a character string for the name of an optional text file that contains a string identifying the annotation that may be used for the 'exprSet' instance}
   \item{description}{\code{description} a character string for the name of an optional file containing MIAME information}
   \item{notes}{\code{notes} a character string for the name of an optional text file containing explanatory text}
   \item{seps}{\code{seps} a vector of character strings defining the delimitors used by 'exprs' and 'se.exprs'. The length of 'seps' can be 1 when both file have the same delimiter or 'se.exprs' is missing or 2 other wise. The delimitor for 'exprs' is expected to be the first element}
}

\details{
  If not missing, 'phenoData' will be read in using \code{\link{read.phenoData}} and description using
  \code{\link{read.MIAME}}. The help files of the two functions contain detailed descriptions of the two functions.
}
\value{
  This function returns an \code{exprSet} object.
}
\author{Jianhua Zhang}

\note{
   This function is part of the Bioconductor project at Dana-Farber Cancer Institute to provide bioinformatics functionalities through R
}
\seealso{
   \code{\link{exprSet}}, \code{\link{phenoData}}, \code{\link{MIAME}}, \code{\link{read.phenoData}}, \code{\link{read.MIAME}}
}
\examples{
# Creating a test data
testFile <- tempfile()
data(geneData)
write.table(geneData, testFile, quote = FALSE, sep = "\t", row.names =
TRUE, col.names = TRUE)
# Create an exprSet object using textFile
eSet <- read.exprSet(testFile)
eSet
}
\keyword{file}
\keyword{manip}
