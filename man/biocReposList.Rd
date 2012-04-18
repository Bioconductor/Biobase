\name{biocReposList}
\alias{biocReposList}

\title{Return a list of Bioconductor package repositories}
\description{
  WARNING: The \code{biocReposList} function is deprecated!

  This function returns a named character vector of Bioconductor package
  repositories.

  The vector can be used as the \code{repos} argument to
  \code{install.packages} and friends.
}
\usage{
biocReposList()
}

\details{
  The repository URLs are hardcoded for each release.
}
\value{
  \item{bioc}{URL for Bioc software package repository}
  \item{aData}{URL for Bioc annotation data package repository}
  \item{eData}{URL for Bioc experiment data package repository}
  \item{extra}{URL for Bioc extra repository}
  \item{cran}{URL for CRAN package repository.}
}
\author{S. Falcon}
\examples{
## This function is deprecated. Please use biocLite() to install packages:
if (interactive()) {
  source("http://bioconductor.org/biocLite.R")
  biocLite("hgu95av2.db")
}
## If you really need to get the list of Bioconductor package repositories
## (like biocReposList() does), then do:
source("http://bioconductor.org/biocLite.R")
biocinstallRepos()  # PLEASE USE THIS, NOT biocReposList()
}
\keyword{utilities}

