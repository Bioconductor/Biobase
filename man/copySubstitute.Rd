\name{copySubstitute}
\alias{copySubstitute}
\title{copy from one file or connection to another, with configure-like name-value substitution}
\description{
  Given a named list of character strings, a read and a write
  connection, replace all occurences of a SYMBOL by the corresponding
  value. (see example).
}
\usage{copySubstitute(symbolValues, cin, cout, symbol.delimiter="@", allow.unresolved.symbols=FALSE)}
\arguments{
  \item{symbolValues}{A named list of character strings.}
  \item{cin}{A connection object or a character string.}
  \item{cout}{A connection object or a character string.}
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
writeLines(text=
  "This is a @FOO@ file. It has the meaning of @BAR@.\n\n\t\tAnd the rest is @DADA@.",
  con = "cstest1.txt")

## create the symbol table
z = list(FOO="test",
         DADA="silence",
         BAR="dada")

## run copySubstitute
copySubstitute(z, "cstest1.txt", "cstest2.txt")

## display the results
cat(readLines("cstest2.txt"))

## clean up
file.remove(c("cstest1.txt", "cstest2.txt"))

}

