\name{getPkgVigs}
\alias{getPkgVigs}

\title{List Vignette Files for a Package}

\description{
This function will return a listing of all vignettes stored in a package's \code{doc} directory.
}
\usage{
getPkgVigs(package = NULL)
}
\arguments{
  \item{package}{A character vector of packages to search or
    \code{NULL}. The latter is for all attached packages (in \code{\link{search}()}).}
}
\value{
A data.frame with columns \code{package}, \code{filename}, \code{title}.
}
\author{Jeff Gentry, modifications by Wolfgang Huber.}
\seealso{\code{\link{openVignette}}}
\examples{
 z <- getPkgVigs()
 z # and look at them
}
\keyword{utilities}
