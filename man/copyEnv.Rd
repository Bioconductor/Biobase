\name{copyEnv}
\alias{copyEnv}


\title{List-Environment interactions}

\description{
  These functions can be used to make copies of environments, or to
  get/assign all of the objects inside of an environment.
}

\usage{
copyEnv(oldEnv, newEnv, all.names=FALSE)
}

\arguments{
  \item{oldEnv}{An environment to copy from}
  \item{newEnv}{An environment to copy to.  If missing, a new environment
  with the same parent environment as oldEnv.}
  \item{all.names}{Whether to retrieve objects with names that start
    with a dot.}
}

\details{
  \code{copyEnv}: This function will make a copy of the contents from
  \code{oldEnv} and place them into \code{newEnv}.
}

\author{Jeff Gentry and R. Gentleman}

\seealso{\code{\link{environment}}, \code{\link{as.list}}}

\examples{
   z <- new.env(hash=TRUE, parent=emptyenv(), size=29L)
   multiassign(c("a","b","c"), c(1,2,3), z)

   a <- copyEnv(z)
   ls(a)
}
\keyword{utilities}
