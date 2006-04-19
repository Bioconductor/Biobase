\name{notes}
\alias{notes}
\alias{notes<-}

\title{Retrieve and set eSet and exprSet notes.}
\description{

  These generic functions access notes (unstructured descriptive data)
  associated \code{\link{eSet-class}} or \code{\link{exprSet-class}}.

}
\usage{
notes(object)
notes(object) <- value
}
\arguments{
  \item{object}{Object, possibly derived from class \code{eSet-class}}
  \item{value}{Character vector containing unstructured information describing the experinement.}
}
\value{
  \code{notes} returns a character vector.
}
\author{Biocore}

\seealso{\code{\link{exprSet-class}}, \code{\link{ExpressionSet-class}}, \code{\link{SnpSet-class}}}

\keyword{manip}
