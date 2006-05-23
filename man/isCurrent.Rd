\name{isCurrent}
\alias{isCurrent}

\alias{isCurrent,ANY,ANY-method}

\title{Use version information to test whether class is current}
\description{


  This generic function uses \code{\link{Versioned-class}} information
  to ask whether an instance of a class (e.g., read from disk) has
  current version information.

  By default, \code{isCurrent} has the following behaviors:
  \describe{
    \item{\code{isCurrent(Versioned-instance)}}{Returns a vector of logicals, indicating whether each version matches the current version from the class prototype.}
    \item{\code{isCurrent(ANY)}}{Return \code{NA}, indicating that the
      version cannot be determined}
    \item{\code{isCurrent(Versioned-instance, "class")}}{Returns a logical vector indicating whether version identifiers shared between \code{Versioned-instance} and \code{"class"} are current.}
  }

}
\usage{
isCurrent(object, value)
}
\arguments{
  \item{object}{Object whose version is to be determined, as described above.}
  \item{value}{(Optional) character string identifying a class with which to compare versions.}
}
\value{
  \code{isCurrent} returns a logical vector.
}

\author{Biocore team}

\seealso{\code{\link{Versions-class}}}

\examples{



obj <- new("VersionedBiobase")
isCurrent(obj)

isCurrent(1:10) # NA

setClass("A", contains="VersionedBiobase",
         prototype=prototype(new("VersionedBiobase", versions=c(A="1.0.0"))))

a <- new("A")
classVersion(a)

isCurrent(a, "VersionedBiobase") # is the 'VersionedBiobase' portion current?
classVersion(a)["A"] <- "1.0.1"
classVersion(a)
isCurrent(a, "VersionedBiobase")
isCurrent(a) # more recent, so does not match 'current' defined by prototype

removeClass("A")
}

\keyword{manip}