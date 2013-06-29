\name{copySubstitute}
\alias{copySubstitute}

\title{Copy Between Connections or Files with Configure-Like Name-Value Substitution}

\description{
  Copy files, directory trees or between connections and
  replace all occurences of a symbol by the corresponding value.
}

\usage{
copySubstitute(src, dest, symbolValues, symbolDelimiter="@", allowUnresolvedSymbols=FALSE,
               recursive = FALSE, removeExtension = "\\\\.in$")
}

\arguments{
  \item{src}{Source, either a character vector with filenames and/or
    directory names, or a connection object.}
  \item{dest}{Destination, either a character vector of length 1
    with the name of an existing, writable directory, or a connection
    object. The class of the \code{dest} argument must match that of the
    \code{src} argument.}
  \item{symbolValues}{A named list of character strings.}
  \item{symbolDelimiter}{A character string of length one with a single
    character in it.}
  \item{allowUnresolvedSymbols}{Logical. If \code{FALSE}, then the function
    will execute \code{\link[base:stop]{stop}} if it comes across
    symbols that are not defined in \code{symbolValues}.}
  \item{recursive}{Logical. If \code{TRUE}, the function
    works recursively down a directory tree (see details).}
  \item{removeExtension}{Character. Matches to this regular expression are removed
    from filenames and directory names.}
}

\details{
  Symbol substitution: this is best explained with an example. If the list
  \code{symbolValues} contains an element with name \code{FOO} and value
  \code{bar}, and symbolDelimiter is \code{@}, then any occurrence of
  \code{@FOO@} is replaced by \code{bar}. This applies both the text
  contents of the files in \code{src} as well as to the filenames. See examples.

  If \code{recursive} is \code{FALSE}, both \code{src} and \code{dest}
  must be connection or a filenames. The text in \code{src} is read
  through the function \code{\link[base:readLines]{readLines}},
  symbols are replaced by their values, and the result is written to
  \code{dest} through the function \code{\link[base:readLines]{writeLines}}.

  If \code{recursive} is \code{TRUE},
  \code{\link[Biobase:copySubstitute]{copySubstitute}}
  works recursively down a directory tree (see details and example).
  \code{src} must be a character vector with multiple filenames or
  directory names, \code{dest} a directory name.

  One use of this function is in
  \code{\link[Biobase:createPackage]{createPackage}}
  for the automatic generation of packages from a template package directory.
}

\value{
  None. The function is called for its side effect.
}

\author{Wolfgang Huber \url{http://www.dkfz.de/mga/whuber}}
\keyword{connection}
\keyword{programming}

\examples{
## create an example file
infile  = tempfile()
outfile = tempfile()

writeLines(text=c("We will perform in @WHAT@:",
  "So, thanks to @WHOM@ at once and to each one,",
  "Whom we invite to see us crown'd at @WHERE@."),
  con = infile)

## create the symbol table
z = list(WHAT="measure, time and place", WHOM="all", WHERE="Scone")

## run copySubstitute
copySubstitute(infile, outfile, z)

## display the results
readLines(outfile)



##--------------------------------------------------------------
## This is a slightly more complicated example that demonstrates
## how copySubstitute works on nested directories
##--------------------------------------------------------------
d = tempdir()
my.dir.create = function(x) {dir.create(x); return(x)}

unlink(file.path(d, "src"), recursive=TRUE)
unlink(file.path(d, "dest"), recursive=TRUE)

## create some directories and files:
src  = my.dir.create(file.path(d, "src"))
dest = file.path(d, "dest")
d1   = my.dir.create(file.path(src, "dir1.in"))
d2   = my.dir.create(file.path(src, "dir2@FOO@.in"))
d3   = my.dir.create(file.path(d2, "dir3"))
d4   = my.dir.create(file.path(d3, "dir4"))
d5   = my.dir.create(file.path(d4, "dir5@BAR@"))
writeLines(c("File1:", "FOO: @FOO@"),     file.path(d1, "file1.txt.in"))
writeLines(c("File2:", "BAR: @BAR@"),     file.path(d2, "file2.txt.in"))
writeLines(c("File3:", "SUN: @SUN@"),     file.path(d3, "file3.txt.in"))
writeLines(c("File4:", "MOON: @MOON@"),   file.path(d4, "@SUN@.txt"))

## call copySubstitute
copySubstitute(src, dest, recursive=TRUE,
               symbolValues = list(FOO="thefoo", BAR="thebar",
                                   SUN="thesun", MOON="themoon"))

## view the result
listsrc  = dir(src,  full.names=TRUE, recursive=TRUE)
listdest = dir(dest, full.names=TRUE, recursive=TRUE)
listsrc
listdest

cat(unlist(lapply(listsrc,  readLines)), sep="\n")
cat(unlist(lapply(listdest, readLines)), sep="\n")
}

