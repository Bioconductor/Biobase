\name{biocReposList}
\alias{biocReposList}

\title{Return a list of Bioconductor package repositories}
\description{
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
  \item{bioc }{URL of main Bioc package repository}
  \item{aData }{URL for Bioc annotation data package repository}
  \item{eData }{URL for Bioc experiment data package repository}
  \item{oh }{URL for Bioc Omegahat package repository.  This repository
    contains the versions of Omegahat packages that were tested with the
    current Bioc release.}
  \item{li }{URL for Bioc Lindsey package repository.}
  \item{cran }{URL for Bioc CRAN package repository.  This is just a
    normal CRAN repository.}
}
\author{S. Falcon}
\examples{
brl <- biocReposList()
}
\keyword{utilities}

