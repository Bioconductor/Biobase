\name{annotation}
\alias{annotation}
\alias{annotation<-}

\title{Annotate eSet data.}
\description{
This generic function handles methods for adding and retrieving
`annotation' and `description' information for eSets. An annotation is the name of the file describing the chip used for the experiment.
}
\usage{
annotation(object)
annotation(object) <- "hgu95av2"

}
\arguments{
  \item{object}{Object derived from class eSet}
}
\value{
  \code{annotation(object)} returns a character vector indicating the
  annotation package.
}
\author{Biocore}

\seealso{\code{\link{eSet-class}},
  \code{\link{ExpressionSet-class}}, \code{\link{SnpSet-class}}}

\keyword{manip}
