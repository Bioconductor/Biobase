\name{selectChannels}
\alias{selectChannels}

\title{Create a new NChannelSet instance by selecting specific channels}
\description{
  This generic function extracts specific elements from an object,
  returning a instance of that object.
}
\usage{
selectChannels(object, names, ...)
}
\arguments{
  \item{object}{An S4 object, typically derived from class
    \code{\link{eSet}}}
  \item{names}{Character vector of named channels.}
  \item{...}{Additional arguments.}
}
\value{
  Instance of class \code{object}.
}
\author{Biocore}

\examples{
obj <- NChannelSet(R=matrix(runif(100), 20, 5), G=matrix(runif(100), 20, 5))

## G channel as NChannelSet
selectChannels(obj, "G")
  }
\keyword{manip}
