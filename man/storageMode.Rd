\name{storageMode}
\alias{storageMode}
\alias{storageMode<-}

\title{Retrieve or set storage mode for eSets.}
\description{
  These generic functions report or change the storage mode used for \code{assayData}.
}
\usage{
storageMode(object)
storageMode(object) <- value
}
\arguments{
  \item{object}{Object, derived from class \code{eSet}}
  \item{value}{Character vector containing
    \code{"lockedEnvironment"},\code{"environment"}, or
    \code{"list"}. See \code{AssayData-class} for details.}
}
\value{
  \code{storageMode} returns a length-1 character vector
}
\author{Biocore}

\seealso{\code{\link{AssayData-class}}, \code{\link{eSet-class}} \code{\link{ExpressionSet-class}}, \code{\link{SnpSet-class}}}

\keyword{manip}
