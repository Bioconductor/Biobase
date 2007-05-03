\name{read.MIAME}
\alias{read.MIAME}

\title{
   Read MIAME Information into an Instance of Class 'MIAME'
}

\description{
   Reads MIAME information from a file or using a widget.
}

\usage{
read.MIAME(filename = NULL, widget = getOption("BioC")$Base$use.widgets, ...)
}

\arguments{
   \item{filename}{Filename from which to read MIAME information.}
   \item{widget}{Logical. If \code{TRUE} and a filename is not given, a widget is used to enter information.}
   \item{\dots}{Further arguments to \code{scan}.}
}

\details{
  Notice that the \code{\link{MIAME}}
  class tries to cover the MIAME entries that are not covered by other
  classes in Bioconductor. Namely, experimental design, samples,
  hybridizations, normalization controls, and pre-processing
  information.

  The function \code{\link{scan}} is used to read.
  The file must be a flat file with the
  different entries for the instance
  of MIAME class separated by carriage returns. The order should be: name, lab,
  contact, title, abstract, and url.

  Alternatively a widget can be used.
}
\value{An object of class \code{\link{MIAME}}.}

\author{Rafael Irizarry <rafa@jhu.edu>}

\seealso{\code{\link{MIAME}},\code{\link[tkWidgets]{tkMIAME}} }

\examples{
miame <- read.MIAME(widget=FALSE) ##creates an empty instance
show(miame)
}
\keyword{file}

