\name{Aggregate}
\alias{Aggregate}
\title{ A simple aggregation mechanism.  }
\description{
 Given an environment and two functions \code{initfun} and \code{agfun}
 simple Aggregations are made.
}
\usage{
Aggregate(x, env, agfun, initfun)
}
\arguments{
  \item{x}{ The data to be aggregated. }
  \item{env}{An environment. }
  \item{agfun}{The aggregate function. }
  \item{initfun}{The initialize function. }
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

\seealso{ \code{\link{new.env}} }

\examples{
  agfun1 <- function(name, current, val) current+1
  initfun1 <- function(name, val) 1

  env1 <- new.env(hash=TRUE, parent=NULL)

  Aggregate(letters[1:10], env1, agfun1, initfun1)
  #the first 10 letters should be symbols in env1 with values
  # of 1
  Aggregate(letters[5:11], env1, agfun1, initfun1)
  ## now letters[5:10] should have value 2
  bb <- multiget(letters[1:11], env=env1)
  t1 <- as.numeric(bb); names(t1) <- names(bb)
  t1
# a b c d e f g h i j k 
# 1 1 1 1 2 2 2 2 2 2 1 

}
\keyword{ programming }
