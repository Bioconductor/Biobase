\name{testBioCConnection}
\alias{testBioCConnection}
\title{A function to check internet connectivity to Bioconductor}
\description{
  This function will attempt to determine if the user has internet
  connectivity to the Bioconductor website.  This is useful in many
  situations dealing with code that uses automated downloads and other
  such things.
}
\usage{
testBioCConnection()
}
\value{
  TRUE if a connection is possible, FALSE if not.
}
\author{Jeff Gentry}
\examples{
 z <- testBioCConnection()
}
\keyword{utilities}
