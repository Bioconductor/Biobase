\name{copySubstitute}
\alias{copySubstitute}
\title{Copy files, directory trees or between connections with configure-like name-value substitution}
\description{
  Copy files, directory trees or between connections and
  replace all occurences of a symbol by the corresponding value. 
}
\usage{copySubstitute(src, dest, symbolValues, symbolDelimiter="@",
allowUnresolvedSymbols=FALSE, removeExtension = "\\.in$")}
\arguments{
  \item{src}{Source: either a character vector with filenames and/or
    directory names, or a connection object.}
  \item{dest}{Destination: either a character vector of length 1
    with the name of an existing, writeable directory, or a connection
    object. The class of the \code{dest} argument must match that of the
    \code{src} argument.}
  \item{symbolValues}{A named list of character strings.}
  \item{symbolDelimiter}{A character string of length one with a single
    character in it.}
  \item{allowUnresolvedSymbols}{Logical. If \code{FALSE}, then the function
    will execute \code{\link[base:stop]{stop}} if it comes across symbols that are
    not defined in \code{symbolValues}.}
  \item{removeExtension}{Matches to this regular expression are removed
    from filenames and directory names.}
}

\details{
  Symbol substitution: this is best explained with an example. If the list
  \code{symbolValues} contains an element with name \code{FOO} and value
  \code{bar}, and symbolDelimiter is \code{@}, then any occurence of
  \code{@FOO@} is replaced by \code{bar}. This applies both the text
  contents of the files in \code{src} as well as to the filenames. See examples.
  
  If \code{src} is a connection or a character vector of length 1 with a
  filename, the text is read through the function
  \code{\link[base:readLines]{readLines}}. Symbols are replaced by their
  values and the result is written to \code{dest}
  through the function \code{\link[base:readLines]{writeLines}}. 
  
  If \code{src} is a character vector with multiple filenames or
  contains directory names,
  \code{\link[Biobase:copySubstitute]{copySubstitute}} is called on each
  file in turn and descends into the directories recursively.

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
thedir  = tempdir()
thefile = file.path(thedir, "macbeth.txt.in")

writeLines(text=c("We will perform in @WHAT@:",
  "So, thanks to @WHOM@ at once and to each one,",
  "Whom we invite to see us crown'd at @WHERE@."),
  con = thefile) 

## create the symbol table
z = list(WHAT="measure, time and place", WHOM="all", WHERE="Scone")

## run copySubstitute
copySubstitute(thefile, thedir, z)

## display the results
readLines(file.path(thedir, "macbeth.txt"))

}

