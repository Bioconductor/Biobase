\name{exprs}
\alias{exprs}
\alias{exprs<-}

\title{Retrieve expression data from eSets.}
\description{
This generic function accesses the `expression' assay
data stored in an object derived from the \code{\link{eSet}} or \code{\link{exprSet}} class.
}
\usage{
exprs(object)
exprs(object) <- value
}
\arguments{
  \item{object}{Object derived from class \code{eSet}, or \code{exprSet}}
  \item{value}{matrix with rows representing features and columns samples.}
}
\value{
  \code{exprs} returns a (usually large!) matrix of expression values.
}
\author{Biocore}

\seealso{\code{\link{exprSet}}, \code{\link{eSet-class}}, \code{\link{ExpressionSet-class}}, \code{\link{SnpSet-class}}}

\keyword{manip}
