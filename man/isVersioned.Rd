\name{isVersioned}
\alias{isVersioned}

\alias{isVersioned,ANY-method}
\alias{isVersioned,character-method}

\title{Determine whether object or class contains versioning information}
\description{

  This generic function checks to see whether \code{\link{Versioned-class}} information
  is present. When the argument to \code{isVersioned} is a character
  string, the prototype of the class corresponding to the string is
  consulted.

  By default, \code{isVersioned} has the following behaviors:
  \describe{
    \item{\code{isVersioned(Versioned-instance)}}{Returns \code{TRUE} when the instance have version information.}
    \item{\code{isCurrent("class-name")}}{Returns \code{TRUE} when the named class extends \code{\link{Versioned-class}}.}
    \item{\code{isVersioned(ANY)}}{Returns \code{FALSE}}
  }

}
\usage{
isVersioned(object)
}
\arguments{
  \item{object}{Object or class name to check for version information, as described above.}
}
\value{
  \code{isVersioned} returns a logical indicating whether version information is present.
}

\author{Biocore team}

\seealso{\code{\link{Versions-class}}}

\examples{

obj <- new("VersionedBiobase")
isVersioned(obj)

isVersioned(1:10) # FALSE

A <- setClass("A", contains="VersionedBiobase",
         prototype=prototype(new("VersionedBiobase", versions=c(A="1.0.0"))))
a <- A()
isVersioned(a)

removeClass("A")
}

\keyword{manip}
