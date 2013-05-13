\name{channelNames}
\alias{channelNames}
\alias{channelNames<-}

\title{Retrieve and set channel names from object}
\description{
  This generic function reports or updates the channels in an object.
}
\usage{
channelNames(object, ...)
channelNames(object, ...) <- value
}
\arguments{
  \item{object}{An S4 object, typically derived from class
    \code{\link{eSet}}}
  \item{value}{Replacement value, either a character vector (to
    re-order existing channel names or a named character vector or list
    (to change channel names from the vector elements to the
    corresponding names).}
  \item{...}{Additional argument, not currently used.}
}

\details{

  \code{channelNames} returns the names of the channels in a defined
  order. Change the order using the replacement method with a permuation
  of the channel names as \code{value}. Rename channels using the
  replacement method with a named list, where the vector elements are a
  permutation of the current channels, with corresponding names the new
  identifier for the channel.
  
}

\value{
  \code{character}.
}
\author{Biocore}

\examples{
## channelNames default to alphabetical order of channels
obj <- NChannelSet(
           R=matrix(runif(100), 20, 5),
           G=matrix(-runif(100), 20, 5))
channelNames(obj)
channelNames(obj) <- c(Gn="G", Rd="R")   ## rename
channelNames(obj)
channelNames(obj) <- c("Rd", "Gn")       ## reorder
channelNames(obj)
all(assayData(obj)[["Gn"]] <= 0)
all(assayData(obj)[["Rd"]] >= 0)
}
\keyword{manip}
