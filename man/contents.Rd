\name{contents}
\alias{contents}
\alias{contents,environment-method}
\title{Function to retrieve contents of environments}
\description{
  The contents method is used to retrieve the values stored in an
  environment.
}
\usage{
contents(object)
}
\arguments{
    \item{object}{The environment (data table) that you want to get all
    contents from}
}
\value{
  A named list is returned, where the elements are the objects stored in
  the environment.  The names of the elements are the names of the objects.
}
\author{R. Gentleman}
\seealso{\code{\link{mget}}}
\examples{
  z <- new.env()
  multiassign(letters, 1:26, envir=z)
  contents(z)
}
\keyword{manip}

