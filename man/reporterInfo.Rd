\name{reporterInfo}
\alias{reporterInfo}
\alias{reporterInfo<-}


\title{Retrieve and set information about reporters.}
\description{
  This generic function accesses information about reporters (e.g.,
  probes). Currently implemented methods are for \code{exprSet-class};
  \code{reporterInfo} is not present in \code{eSet-class}
}
\usage{
reporterInfo(object)
reporterInfo(object) <- value
}
\arguments{
  \item{object}{Object, possibly derived from class \code{exprSet}}
  \item{value}{Object containing reporter information}
}
\value{
  \code{reporterInfo} returns a \code{data.frame} containing information
  about reporters.
}
\author{Biocore}

\seealso{\code{\link{exprSet-class}}}

\keyword{manip}
