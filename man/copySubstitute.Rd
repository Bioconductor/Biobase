\name{copySubstitute}
\alias{copySubstitute}
\title{copy from one file or connection to another, with configure-like name-value substitution}
\description{
  Given a named list of character strings, a read and a write
  connection, replace all occurences of a SYMBOL by the corresponding
  value. (see example).
}
\usage{copySubstitute(cin, cout, symbolValues, symbol.delimiter="@", allow.unresolved.symbols=FALSE)}
\arguments{
  \item{cin}{A connection object or a character string.}
  \item{cout}{A connection object or a character string.}
  \item{symbolValues}{A named list of character strings.}
  \item{symbol.delimiter}{A character string of with a single character
    in it.}
  \item{allow.unresolved.symbols}{If not \code{TRUE}, then the function
    will execute \code{\link{stop}} if it comes across SYMBOLs that are
    not defined in \code{symbolValues}.}
}

\details{One use of this function is in \code{\link{createPackage}} for
  the automatic generation of packages, where template files for
  \code{.R}, \code{.Rd}, \code{.c} and \code{DESCRIPTION}
  files may be customized by substituting placeholders by values.
  
  A SYMBOL is any text between a pair of symbol delimiters. By
  default the symbol delimiter is \code{@}.

  \code{cin} is passed on to \code{\link{readLines}}, 
  \code{cout} to \code{\link{writeLines}}.
  If \code{cin} (\code{cout}) is a connection,
  it must be open for read (write). If it is a character string,
  \code{\link{readLines}} (\code{\link{writeLines}}) opens a
  file with that name, using \code{\link{file}}.
}
\value{
  The function returns the return value of the \code{\link{writeLines}}
  function that it calls.
}

\author{Wolfgang Huber \url{http://www.dkfz.de/mga/whuber}}
\keyword{connection}
\keyword{programming}

\examples{
## create an example file
fin  = tempfile()
fout = tempfile()

writeLines(text=c("We will perform in @WHAT@:",
  "So, thanks to @WHOM@ at once and to each one,",
  "Whom we invite to see us crown'd at @WHERE@."),
  con=fin) 

## create the symbol table
z = list(WHAT="measure, time and place", WHOM="all", WHERE="Scone")

## run copySubstitute
copySubstitute(fin, fout, z)

## display the results
readLines(fout)

}

