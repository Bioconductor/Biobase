\name{copySubstitute}
\alias{copySubstitute}
\title{copy from one file or connection to another, with configure-like name-value substitution}
\description{
  Given a named list of character strings, a read and a write
  connection, replace all occurences of a SYMBOL by the corresponding
  value. (see example).
}
\usage{copySubstitute(x, cin, cout, symbol.delimiter="@", allow.unresolved.symbols=FALSE)}
\arguments{
  \item{x}{A named list of character strings.}
  \item{cin}{A connection object or a character string.}
  \item{cout}{A connection object or a character string.}
  \item{symbol.delimiter}{A character string of with a single character
    in it.}
  \item{allow.unresolved.symbols}{If not \code{TRUE}, then the function
    will execute \code{\link{stop}} if it comes across SYMBOLs that are
    not defined in \code{x}.}
}

\details{One use of this function is in the automatic generation of
  packages, where template files for .R, .c, .Rd and DESCRIPTION
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
writeLines(text=
  "This is a @FOO@ file. It has the meaning of @BAR@.\n\n\t\tAnd the rest is @DADA@.",
  con = "test1.txt")

conn1 = file("test1.txt", open="rt")
conn2 = file("test2.txt", open="wt")

z = list(FOO="test",
         DADA="silence",
         BAR="dada")

copySubstitute(z, conn1, conn2)

close(conn1)
close(conn2)
}

