\name{Aggregate}
\alias{Aggregate}
\title{A Simple Aggregation Mechanism.  }
\description{
   Given an environment and an aggregator (an object of class \code{aggregate} simple aggregations are made.
}
\usage{
Aggregate(x, agg)
}
\arguments{
   \item{x}{ The data to be aggregated. }
   \item{agg}{The aggregator to be used. }
}
\details{
  Given some data, \code{x} the user can accumulate (or aggregate)
  information in \code{env} using the two supplied functions.
  See the accompanying documentation for a more complete example of this
  function and its use.
}
\value{
 No value is returned. This function is evaluated purely for side
 effects. The symbols and values in \code{env} are altered.
}

\author{R. Gentleman }

\seealso{
   \code{\link{new.env}},  \code{\link{class:aggregator}}
}

\examples{
  agg1 <- new("aggregator")
  Aggregate(letters[1:10], agg1)
  # the first 10 letters should be symbols in env1 with values of 1
  Aggregate(letters[5:11], agg1)
  # now letters[5:10] should have value 2
  bb <- mget(letters[1:11], env=aggenv(agg1), ifnotfound=NA)
  t1 <- as.numeric(bb); names(t1) <- names(bb)
  t1
# a b c d e f g h i j k
# 1 1 1 1 2 2 2 2 2 2 1
}

\keyword{programming}
\keyword{methods}
