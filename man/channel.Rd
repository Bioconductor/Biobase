\name{channel}
\alias{channel}

\title{Create a new ExpressionSet instance by selecting a specific channel}
\description{
  This generic function extracts a specific element from an 
  object, returning a instance of the ExpressionSet class.
}
\usage{
channel(object, name, ...)
}
\arguments{
  \item{object}{An S4 object, typically derived from class
    \code{\link{eSet}}}
  \item{name}{The name of the channel, a (length one) character vector.}
  \item{...}{Additional arguments.}
}
\value{
  An instance of class \code{\link{ExpressionSet}}.
}
\author{Biocore}

\examples{
obj <- NChannelSet(
           R=matrix(runif(100), 20, 5),
           G=matrix(runif(100), 20, 5))
## G channel as ExpressionSet
channel(obj, "G")
}
\keyword{manip}
