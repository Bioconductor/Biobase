\name{createPackage}
\alias{createPackage}
\title{Create the raw structure of a package from a collection of
  template files and a symbol-value table}
\description{
  Create the raw structure of a package from a collection of
  template files (.R, .c, .Rd, DESCRIPTION) and a symbol-value table}
\usage{createPackage(pkgname, destinationDir, originDir, symbolValues,
                     force=FALSE, quiet=FALSE)}
\arguments{
  \item{pkgname}{The name of the package to be written.}
  \item{destinationDir}{The path to a directory where the package is to be written.}
  \item{originDir}{A directory that contains template files. These are files
    with extensions \code{.R}, \code{.Rd}, \code{.c} will be copied into the
    package subdirectories \code{R}, \code{man}, \code{src}
    respectively, and a file named \code{DESCRIPTION}.
    In all files, symbols are replaced by their respectives values, see
    the parameter \code{symbolValues} and the function \code{\link{copySubstitute}}.}
  \item{symbolValues}{A named list of character strings, the symbol-to-value
    mapping. See \code{\link{copySubstitute}} for details.}
  \item{force}{If the destinationDir already contains a file or
    directory with the name pkgname directA named list of character
    strings, the symbol-to-value mapping. See
    \code{\link{copySubstitute}} for details.}
  \item{quiet}{If TRUE, do not print information messages on the console.}
}

\details{No syntactic or other checking of the package is
  performed. For this, use R CMD check.
}
\value{
  The function returns a \code{list} with one element
  \code{pkgdir}: the path to the package
}

\author{Wolfgang Huber \url{http://www.dkfz.de/mga/whuber}}
\keyword{programming}

\examples{
   ## library(makeProbeTools)
   ## symbols = list(ARRAYTYPE = "hgu232a")
   ## createPackage("foo",
   ##     somedir,
   ##     file.path(.path.package("makeProbeTools"), "Code"),
   ##     symbols)
}

