\name{copyEnv}
\alias{copyEnv}
\alias{list2env}
\alias{env2list}
\title{Functionality to copy environments}
\description{
  These functions can be used to make copies of environments, or to
  get/assign all of the objects inside of an environment
}
\usage{
copyEnv(oldEnv, newEnv = new.env(parent = parent.env(oldEnv)), recurse = FALSE)
env2list(envir, recurse=FALSE)
list2env(vals, envir, recurse=FALSE)
}
\arguments{
  \item{oldEnv}{An environment to copy from}
  \item{newEnv}{An environment to copy to.  Default is a new environment
  with the same parent environment as oldEnv.}
  \item{envir}{An environment to get/set values to}
  \item{vals}{A named list of objects to assign into an environment}
  \item{recurse}{Whether to recurse upwards through the environment hierarchy}
}
\details{
  \code{env2list}:  This function will retrieve the objects from an
  environment and return them in a named list (names being the names of
  the objects).
  \code{list2env}:  This function will take a named list and assign all
  of its elements into the environment (using the names to name the
  objects).
  \code{copyEnv}:  This function will make a copy of the contents from
  \code{oldEnv} and place them into \code{newEnv}.
}
\author{Jeff Gentry}
\seealso{\code{\link{environment}}}
\examples{
   z <- new.env()
   multiassign(c("a","b","c"),c(1,2,3),z)

   a <- copyEnv(z)
   ls(a)

   q <- env2list(z)
   g <- new.env()
   g <- list2env(q,g)   
   ls(g)
}
\keyword{utilities}
