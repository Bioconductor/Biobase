\name{cache}
\alias{cache}
\docType{methods}

\title{Evaluate an expression if its value is not already cached.}
\description{
  Cache the evaluation of an expression in the file system.
}

\usage{
    cache(expr, dir=".", prefix="tmp_R_cache_")
}
\arguments{
    \item{expr}{An expression of the form \code{LHS <- RHS}, Where
      \code{LHS} is a variable name, \code{RHS} is any valid expression,
      and \code{<-} must be used (\code{=} will not work).}

    \item{dir}{A string specifying the directory into which cache files
      should be written (also where to go searching for an appropriate
      cache file).}

    \item{prefix}{A string giving the prefix to use when naming and
      searching for cache files.  The default is \code{"tmp_R_cache_"}}

}

\details{
  This function can be useful during the development of computationally
  intensive workflows, for example in vignettes or scripts.  The
  function uses a cache file in \code{dir} which defaults to the current
  working directory whose name is obtained by \code{paste(prefix, name,
  ".RData", sep="")}.
  
  When \code{cache} is called and the cache file exists, it is loaded
  and the object whose name is given on the left of \code{<-} in
  \code{expr} is returned.  In this case, \code{expr} is \emph{not}
  evaluted.

  When \code{cache} is called and the cache file does not exist,
  \code{expr} is evaluted, its value is saved into a cache file, and
  then its value is returned.

  The \code{expr} argument must be of the form of \code{someVar <-
    {expressions}}.  That is, the left hand side must be a single symbol
    name and the next syntactic token must be \code{<-}.
  
  To flush the cache and force recomputation, simply remove the cache
  files.  You can use \code{\link{file.remove}} to do this.

}

\value{
    The (cached) value of \code{expr}.
}

\author{
  Wolfgang Huber, \email{huber@ebi.ac.uk}
  Seth Falcon, \email{sfalcon@fhcrc.org}
}

\section{Note}{

  The first version of this function had a slightly different interface
  which is no longer functional.  The old version has arguments
  \code{name} and \code{expr} and the intended usage is: \code{foo <-
  cache("foo", expr)}.

}

\examples{
    bigCalc <- function() runif(10)
    cache(myComplicatedObject <- bigCalc())
    aCopy <- myComplicatedObject
    remove(myComplicatedObject)
    cache(myComplicatedObject <- bigCalc())
    stopifnot(all.equal(myComplicatedObject, aCopy))
    allCacheFiles <-
       list.files(".", pattern="^tmp_R_cache_.*\\\\.RData$", full.name=TRUE)
    file.remove(allCacheFiles)
}

\keyword{manip}
\keyword{array}

