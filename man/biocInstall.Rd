\name{biocInstall}
\alias{biocInstall}
\title{Conveniently Install Bioconductor Packages}
\description{
  This functions provides an easy mechanism to install Bioconductor
  packages (analysis and annotation data) along with required
  dependencies.

  Essentially this is just a wrapper for \code{\link{install.packages}}
  but with the \code{repos} argument set appropriately for Bioconductor
  packages repositories.
}
\usage{
biocInstall(pkgs, lib, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pkgs}{character vector of packages to install}
  \item{lib}{package library path where packages will be installed.
  Default is \code{.libPaths()[1]}}
  \item{\dots}{Additional arguments to pass to \code{install.packages}}
}
\details{
  
}
\value{
  At present, it is difficult to trap errors and obtain the result of
  calls to install.packages.  Your best bet is to verify that requested
  packages are loadable via a call to \code{require} or \code{library}.
}
\author{Seth Falcon}
\seealso{\code{\link{install.packages}}}
\examples{
  \dontrun{
    biocInstall(c("affy", "MLinterfaces", "xtable"))
  }
}
\keyword{utilities}
