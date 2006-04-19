\name{exprs}
\alias{exprs}
\alias{exprs<-}
\alias{se.exprs}
\alias{se.exprs<-}

\title{Retrieve expression data from eSets.}
\description{
  These generic functions access the expression and error measurements
  of assay data stored in an object derived from the \code{\link{eSet-class}}
  or \code{\link{exprSet-class}}.
}
\usage{
exprs(object)
exprs(object) <- value
se.exprs(object)
se.exprs(object) <- value
}
\arguments{
  \item{object}{Object derived from class \code{eSet}, or \code{exprSet}}
  \item{value}{Matrix with rows representing features and columns samples.}
}
\value{
  \code{exprs} returns a (usually large!) matrix of expression values;
  \code{se.exprs} returns the corresponding matrix of standard errors,
  when available.
}
\author{Biocore}

\seealso{\code{\link{exprSet-class}}, \code{\link{eSet-class}}, \code{\link{ExpressionSet-class}}, \code{\link{SnpSet-class}}}

\keyword{manip}
