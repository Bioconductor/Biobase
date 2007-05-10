\name{channel}
\alias{channel}

\title{Create a new ExpressionSet instance by selecting a specific channel}
\description{
  This generic function extracts a specific element from an object,
  returning a instance of ExpressionSet.
}
\usage{
channel(object, name, ...)
}
\arguments{
  \item{object}{An S4 object, typically derived from class
    \code{\link{eSet}}}
  \item{name}{A (length one) character vector channel names.}
  \item{...}{Additional arguments.}
}
\value{
  Instance of class \code{\link{ExpressionSet}}.
}
\author{Biocore}

\examples{
obj <- new("NChannelSet",
           R=matrix(runif(100), 20, 5),
           G=matrix(runif(100), 20, 5))
## G channel as ExpressionSet
channel(obj, "G")
  }
\keyword{manip}
