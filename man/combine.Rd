\name{combine}
\alias{combine}
\alias{combine,phenoData,phenoData-method}
\alias{combine,phenoData,ANY-method}

\title{Combine Data into a single object.  }
\description{
This generic function handles methods for combining or merging different
Bioconductor data structures. It should, given an arbitrary number of
arguments of the same class (possibly by inheritance), combine them into
a single instance in a sensible way (your mileage may vary).
}
\usage{
combine(x, y, ...)
}
\arguments{
  \item{x}{One of the values. }
  \item{y}{A second value. }
  \item{\dots}{Any other objects of the same class as \code{x} and \code{y}. }
}
\details{
  There are two basic combine strategies. One is an intersection
  strategy. The returned value should only have rows (or columns) that
  are found in all input data objects. The union strategy says that the
  return value will have all rows (or columns) found in any one of the
  input data objects (in which case some indication of what to use for
  missing values will need to be provided).

  These functions and methods are currently under construction. Please
  let us know if there are features that you require.
}
\value{
  A single value of the same class as the most specific common ancestor
  (in class terms) of the input values. This will contain the
  appropriate combination of the data in the input values.
}
\author{Biocore }

\seealso{\code{\link{merge}}}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
}
\keyword{manip}
