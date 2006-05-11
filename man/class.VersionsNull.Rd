\name{VersionsNull}
\docType{class}
\alias{VersionsNull-class}
\alias{VersionsNull}

\alias{initialize,VersionsNull-method}
\alias{show,VersionsNull-method}

\title{Class "VersionsNull"}

\description{

  A class used to represent the `version' of unversioned
  objects. Useful primarily for method dispatch.

}

\section{Methods}{

  The following are defined; package developers may write additional methods.
  \describe{
    \item{\code{new("VersionsNull", ...)}}{Create a new
      \code{VersionsNull-class} instance, ignoring any additional arguments.}
    \item{\code{show(object)}}{Display ``No version''.}
  }
}

\author{Biocore}

\seealso{
  \code{\link{classVersion}}
}

\examples{

obj <- new("VersionsNull")
obj

obj <- new("VersionsNull", A="1.0.0") # warning
obj

}

\keyword{classes}
