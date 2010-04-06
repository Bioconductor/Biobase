\name{createPackage}
\alias{createPackage}

\title{Create a Package Directory from a Template}

\description{
  Create a package directory from a template, with symbol-value substitution
}
\usage{
createPackage(pkgname, destinationDir, originDir, symbolValues, unlink=FALSE, quiet=FALSE)
}

\arguments{
  \item{pkgname}{Character. The name of the package to be written.}
  \item{destinationDir}{Character. The path to a directory where the
    package is to be written.}
  \item{originDir}{Character. The path to a directory that contains
    the template package. Usually, this will contain a file named
    \code{DESCRIPTION}, and subdirectories \code{R}, \code{man},
    \code{data}. In all files and filenames, symbols will be replaced
    by their respective values, see the parameter \code{symbolValues}.}
  \item{symbolValues}{Named list of character strings. The symbol-to-value
    mapping. See
    \code{\link[Biobase:copySubstitute]{copySubstitute}} for details.}
  \item{unlink}{Logical. If \code{TRUE}, and \code{destinationDir}
    already contains a file or directory with the name \code{pkgname},
    try to unlink (remove) it.}
  \item{quiet}{Logical. If \code{TRUE}, do not print information
    messages.}
}

\details{The intended use of this function is for the automated mass
  production of data packages, such as the microarray annotation,
  CDF, and probe sequence packages.

  No syntactic or other checking of the package is performed. For this,
  use R CMD check.

  The symbols \code{@PKGNAME@} and \code{@DATE@} are automatically
  defined with the values of \code{pkgname} and \code{date()}, respectively.
}

\value{
  The function returns a \code{list} with one element
  \code{pkgdir}: the path to the package.
}

\seealso{\code{\link[Biobase:copySubstitute]{copySubstitute}},
  the reference manual \emph{Writing R extensions}.}

\author{Wolfgang Huber \url{http://www.dkfz.de/mga/whuber}}

\keyword{programming}

\examples{
  sym  = list(AUTHOR = "Hesiod", VERSION = "1.0",
         TITLE = "the nine muses",
         FORMAT = "Character vector containg the names of the 9 muses.")

  res  = createPackage("muses",
           destinationDir = tempdir(),
           originDir      = system.file("Code", package="Biobase"),
           symbolValues   = sym,
           unlink = TRUE, quiet = FALSE)

  muses = c("Calliope", "Clio", "Erato", "Euterpe", "Melpomene",
            "Polyhymnia", "Terpsichore", "Thalia", "Urania")

  dir.create(file.path(res$pkgdir, "data"))

  save(muses, file = file.path(res$pkgdir, "data", "muses.rda"))

  res$pkgdir
}

