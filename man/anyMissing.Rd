\name{anyMissing}
\alias{anyMissing}


\title{Checks if there are any missing values in an object or not}

\description{
  IMPORTANT NOTE: \code{anyMissing()} was deprecated in \pkg{Biobase} 2.69.1.
  Please use \code{anyNA()} instead.

  Checks if there are any missing values in an object or not.
}

\usage{anyMissing(x=NULL)}

\arguments{
 \item{x}{A \code{\link[base]{vector}}.}
}

\value{
  Returns \code{\link[base:logical]{TRUE}} if a missing value was detected, otherwise \code{\link[base:logical]{FALSE}}.
}

\details{
  The implementation of this method is optimized for both speed and memory.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
## IMPORTANT NOTE: anyMissing() was deprecated in Biobase 2.69.1.
## Please use anyNA() instead.
\dontrun{
x <- rnorm(n=1000)
x[seq(300,length(x),by=100)] <- NA
stopifnot(anyMissing(x) == any(is.na(x)))
}
}

\keyword{iteration}
\keyword{logic}
