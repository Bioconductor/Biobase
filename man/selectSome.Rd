\name{selectSome}
\alias{selectSome}
\title{Extract elements of a vector for concise rendering}
\description{
Extract the first and last several elements of a vector for concise
rendering; insert ellipses to indicate elided elements. This function
is primarily meant for developer rather than end-user use.
}
\usage{
selectSome(obj, maxToShow=5)
}
\arguments{
  \item{obj}{A vector.}
  \item{maxToShow}{The number of elements (including "...") to render.}
}
\details{
This function can be used in 'show' methods to give users exemplars of
the tokens used in a vector.  For example, an
\code{\link{ExpressionSet}} built from a yeast experiment might have
features enumerated using systematic gene names (e.g., YPR181C) or
standard gene names (e.g., SEC23).  The \code{\link{show}} method for
\code{\link{ExpressionSet}} uses \code{selectSome} to alert the user to
the tokens used, and thereby to indicate what vocabulary must be
understood to work with the feature names.
}

\value{
A string vector with at most \code{maxToShow} plus 1 elements,
where an ellipsis ("...") is included to indicate incompleteness
of the excerpt.
}
\author{Martin Morgan <mtmorgan@fhcrc.org>}
\examples{
selectSome(1:20)
}
\keyword{utilities}
