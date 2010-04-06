\name{updateOldESet}
\alias{updateOldESet}

\title{Update previously created eSet object to current eSet structure}

\description{

  This function updates \code{eSet} objects created in previous versions
  of Biobase to the current class structure. Warnings indicate when
  coercions change how data in the \code{from} object are
  altered. If the \code{from} object was not a valid object of the
  original eSet class, then \code{updateOldESet} may fail.
}
\usage{
updateOldESet(from, toClass, ...)
}
\arguments{
  \item{from}{Object created using a previous version of the eSet class.}
  \item{toClass}{Character string identifying new class, e.g.,
    \code{"ExpressionSet"}}
  \item{...}{Additional arguments passed to the initialization method
    for class \code{toClass}}
}
\value{
  Valid object of class \code{toClass}.
}
\author{Biocore}

\seealso{\code{\link{eSet-class}}, \code{\link{ExpressionSet-class}}, \code{\link{SnpSet-class}}}

\examples{
\dontrun{
updateOldESet(oldESet, "ExpressionSet")
}}

\keyword{manip}
