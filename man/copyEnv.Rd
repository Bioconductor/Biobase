\name{copyEnv}
\alias{copyEnv}

\alias{l2e}
\alias{listLen}

\title{List-Environment interactions}

\description{
  These functions can be used to make copies of environments, or to
  get/assign all of the objects inside of an environment.
}

\usage{
copyEnv(oldEnv, newEnv = new.env(hash=TRUE, parent =
parent.env(oldEnv)), all.names=FALSE)
listLen(list)
l2e(vals, envir)
}

\arguments{
  \item{oldEnv}{An environment to copy from}
  \item{newEnv}{An environment to copy to.  Default is a new environment
  with the same parent environment as oldEnv.}
  \item{envir}{An environment to get/set values to}
  \item{vals}{A named list of objects to assign into an environment}
  \item{all.names}{Whether to retrieve objects with names that start
    with a dot.}
  \item{list}{A list}
}

\details{
  \code{listLen}:  This function returns a vector of the same lenght as
  the list containng the lengths of each element of the supplied argument.
  \code{l2e}:  This function takes a named list and assigns all
  of its elements into the environment (using the names to name the
  objects).
  \code{copyEnv}:  This function will make a copy of the contents from
  \code{oldEnv} and place them into \code{newEnv}.
}

\author{Jeff Gentry and R. Gentleman}

\seealso{\code{\link{environment}}, \code{\link{as.list}}}

\examples{
   z <- new.env()
   multiassign(c("a","b","c"),c(1,2,3),z)

   a <- copyEnv(z)
   ls(a)

   q <- as.list(z)
   g <- new.env()
   g <- l2e(q,g)
   ls(g)
}
\keyword{utilities}
