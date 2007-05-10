\name{channelNames}
\alias{channelNames}

\title{Retrieve channel names from object}
\description{
  This generic function reports the channels present in an object.
}
\usage{
channelNames(object, ...)
}
\arguments{
  \item{object}{An S4 object, typically derived from class
    \code{\link{eSet}}}
  \item{...}{Additional argument, not currently used.}
}
\value{
  \code{character}.
}
\author{Biocore}

\examples{
obj <- new("NChannelSet",
           R=matrix(runif(100), 20, 5),
           G=matrix(runif(100), 20, 5))
channelNames(obj)
  }
\keyword{manip}
