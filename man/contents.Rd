\name{contents}
\alias{contents}
\title{Function to retrieve contents of environments}
\description{
  The contents method is used to retrieve the values stored in an
  environment.
}
\usage{
contents(object, all.names)
}
\arguments{
    \item{object}{The environment (data table) that you want to get all contents from}
    \item{all.names}{a logical indicating whether to copy all values in \code{as.list.environment}}
}
\value{
  A named list is returned, where the elements are the objects stored in
  the environment.  The names of the elements are the names of the
  objects.

  The \code{all.names} argument is identical to the one used in
  \code{as.list.environment}.
}
\author{R. Gentleman}
\seealso{\code{\link{as.list.environment}}}
\examples{
  z <- new.env()
  multiassign(letters, 1:26, envir=z)
  contents(z)
}
\keyword{manip}

