\name{classVersion}
\alias{classVersion}
\alias{classVersion<-}

\alias{classVersion,ANY-method}
\alias{classVersion,character-method}

\title{Retrieve information about versioned classes}
\description{

  These generic functions return version information for classes
  derived from \code{\link{Versioned-class}}, or
  \code{\link{VersionsNull-class}} for unversioned objects. The
  version information is an object of \code{\link{Versions-class}}.

  By default, \code{classVersion} has the following behaviors:
  \describe{
    \item{\code{classVersion(Versioned-instance)}}{Returns a \code{Versions-class} object obtained from the object.}
    \item{\code{classVersion{"class"}}}{Consults the definition of \code{class} and return the current version information, if available.}
    \item{\code{classVersion(ANY)}}{Return a \code{\link{VersionsNull-class}} object to indicate no version information available.}
  }

  By default, the \code{classVersion<-} method has the following behavior:
  \describe{
    \item{\code{classVersion(Versioned-instance)["id"] <- value}}{Assign (update or add) \code{value} to \code{Versions-instance}. \code{value} is coerced to a valid version description. see \code{\link{Versions-class}} for additional access methods.}
  }

}
\usage{
classVersion(object)
classVersion(object) <- value
}
\arguments{
  \item{object}{Object whose version is to be determined, as described above.}
  \item{value}{\code{Version-class} object to assign to object of \code{Versioned-class} object.}
}
\value{
  \code{classVersion} returns an instance of \code{\link{Versions-class}}
}

\author{Biocore team}

\seealso{\code{\link{Versions-class}}}

\examples{
obj <- new("VersionedBiobase")

classVersion(obj)
classVersion(obj)["Biobase"]
classVersion(1:10) # no version
classVersion("ExpressionSet") # consult ExpressionSet prototype

classVersion(obj)["MyVersion"] <- "1.0.0"
classVersion(obj)
}

\keyword{manip}
