\name{getPkgVigs}
\alias{getPkgVigs}

\title{List Vignette Files for a Package}

\description{
  This function will return a listing of all vignettes  stored in a package's \code{doc} directory.
}
\usage{
getPkgVigs(package = NULL)
}
\arguments{
  \item{package}{A character vector of packages to search or \code{NULL}. The latter is for all attached packages (in \code{\link{search}()}).}
}
\value{
  A named vector, with elements being file paths of PDF files on a
  user's system and the names being the actual PDF filename without the
  \code{.pdf} extension (example: \file{/R/library/test/doc/foo.pdf} has
  \code{foo} as a name).
}
\author{Jeff Gentry}
   \seealso{\code{\link{openVignette}}}
\examples{
 z <- getPkgVigs()
 z # and look at them
}
\keyword{utilities}
