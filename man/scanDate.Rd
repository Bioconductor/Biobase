\name{scanDates}
\alias{scanDates}
\alias{scanDates<-}

\title{Scan / Digitization dates for eSet data.}
\description{
This generic function handles methods for adding and retrieving
scan/digitization dates of the samples in eSets.
}
\usage{
scanDates(object)
scanDates(object) <- c("08/29/03 15:12:30", "01/23/04 14:30:57")
}
\arguments{
  \item{object}{Object derived from class eSet}
}
\value{
  \code{scanDates(object)} returns a character vector indicating the
  scan dates for the samples.
}
\author{Biocore}

\seealso{\code{\link{eSet-class}},
  \code{\link{ExpressionSet-class}}, \code{\link{SnpSet-class}}}

\keyword{manip}
