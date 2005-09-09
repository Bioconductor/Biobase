\name{strbreak}
\alias{strbreak}
\title{Break Character Strings to Fit Width}
\description{
  Inserts line breaks (\code{collapse}) into input character strings.
  The main intention of this
  function is to prepare long strings for printing, so the output
  is not wider than \code{width}.
}
\usage{
  strbreak(x, width=getOption("width"), exdent=2, collapse="\n")
}
\arguments{
  \item{x}{a character vector}
  \item{width}{a positive integer giving the width of the output.}
  \item{exdent}{a positive integer specifying the indentation of
     subsequent lines after the first line.}
  \item{collapse}{a character.  This is inserted to break lines.}
}
\author{Wolfgang Huber \url{http://www.ebi.ac.uk/huber}}
\seealso{\code{\link{strwrap}}, \code{\link{substring}}}

\examples{
longString = paste(rep(LETTERS, 10), collapse="", sep="")
cat(strbreak(longString))
}
\keyword{character}
