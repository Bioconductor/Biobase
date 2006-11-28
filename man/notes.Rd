\name{notes}
\alias{notes}
\alias{notes<-}

\title{Retrieve and set eSet and exprSet notes.}
\description{

  These generic functions access notes (unstructured descriptive data)
  associated \code{\link{eSet-class}} or \code{\link{exprSet-class}}.

  \code{notes(<ExpressionSet> <- <character>} is unusual, in that the
  character vector is appended to the list of notes; use
  \code{notes(<ExpressionSet>) <- <list>} to entirely replace the list.

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
  \code{notes} returns a list (for \code{ExpressionSet}) or character
  vector (for \code{exprSet}).
}
\author{Biocore}

\seealso{\code{\link{exprSet-class}}, \code{\link{ExpressionSet-class}}, \code{\link{SnpSet-class}}}

\keyword{manip}
