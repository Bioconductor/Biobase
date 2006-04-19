\name{description}
\alias{description}
\alias{description<-}

\title{Retrieve and set overall experimental information eSet-like classes.}
\description{

  These generic functions access experimental information
  associated with \code{\link{eSet-class}} or
  \code{\link{exprSet-class}}. 

}
\usage{
description(object)
description(object) <- value
}
\arguments{
  \item{object}{Object, possibly derived from class \code{\link{eSet-class}}.}
  \item{value}{Structured information describing the experinement, e.g.,
    of \code{\link{MIAME-class}}.}
}
\value{
  \code{description} returns an object of \code{\link{MIAME-class}}.
}
\author{Biocore}

\seealso{\code{\link{exprSet-class}}, \code{\link{eSet-class}}, \code{\link{MIAME-class}}}

\keyword{manip}
