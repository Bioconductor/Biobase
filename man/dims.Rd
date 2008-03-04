\name{dims}
\alias{dims}

\title{Retrieve dimensions of all elements in a list or environment}
\description{
  This function returns the dimensions of element members in lists or
  environments such as \code{AssayData-class}.
}
\usage{
dims(object)
}
\arguments{
  \item{object}{List or environment object containing one or several matrices}
}
\value{
  matrix of row and column dimensions, (in rows) for each element in
  \code{object} (columns). 
}
\author{Biocore}

\seealso{\code{\link{eSet-class}}}

\keyword{manip}
