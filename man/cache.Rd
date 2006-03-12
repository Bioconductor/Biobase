\name{cache}
\alias{cache}
\docType{function}

\title{Evaluate an expression if its value is not already cached.}
\description{
  Cache the evaluation of an expression in the file system.
}

\usage{
    cache(name, expr)
}
\arguments{
    \item{name}{A character of length 1.}
    \item{expr}{An expression.}
}

\details{
  This function can be useful during the development of computationally
  intensive workflows, for example in vignettes or scripts.
  The function uses a cache file in the current working directory whose
  name is obtained by \code{paste("tmp-", name, ".RData", sep="")}.
  
  When \code{cache} is called and the cache file exists, it is loaded and
  the object whose name is given by \code{name} is returned. \code{expr}
  is \emph{not} evaluted.

  When \code{cache} is called and the cache file does not exist,
  \code{expr} is evaluted, and its value is saved into the file under
  the name given by \code{name} and returned.

  To delete all cache files, command like \code{rm tmp-*} should do.
}

\value{
    The (cached) value of \code{expr}.
}

\author{
    Wolfgang Huber, \email{huber@ebi.ac.uk}
}

\examples{
    myComplicatedObject <- cache("myComplicatedObject", { runif(10) })
}

\keyword{manip}
\keyword{array}

