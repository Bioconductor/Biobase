\name{getPkgVigs}
\alias{getPkgVigs}
\title{A function to list PDF files for a package}
\description{
  This function will return a listing of all PDF files stored in a
  package's \code{doc} directory.
}
\usage{
getPkgVigs(package = NULL)
}
\arguments{
  \item{package}{A character vector of packages to search.}
}
\details{
  If \code{package} is NULL, all attached packages are used for the search
  space.
}
\value{
  A named vector, with elements being file paths of PDF files on a
  user's system and the names being the actual PDF filename without the
  \code{.pdf} extension (example: /R/library/test/doc/foo.pdf has foo as
  a name).
}
\author{Jeff Gentry}
\seealso{\code{\link{openVignette}}}
\examples{
  z <- getPkgVigs()
}
\keyword{utilities}
