\name{Versioned}
\docType{class}
\alias{Versioned-class}
\alias{Versioned}

\alias{initialize,Versioned-method}
\alias{isVersioned,Versioned-method}
\alias{classVersion,Versioned-method}
\alias{classVersion<-,Versioned,Versions-method}
\alias{isCurrent,Versioned,missing-method}
\alias{isCurrent,Versioned,character-method}
\alias{show,Versioned-method}

\title{Class "Versioned"}

\description{

  Use this class as a `superclass' for classes requiring information
  about versions.

}

\section{Methods}{

  The following are defined; package developers may write additional methods.
  \describe{
    \item{\code{new("Versioned", ..., versions=list())}}{Create a new \code{Versioned-class} instance,
      perhaps with additional named version elements (the contents of
      \code{versions}) added. Named elements of \code{versions} are
      character strings that can be coerced using \code{\link{package_version}}, or \code{package_version} instances.}
    \item{\code{classVersion(object)}}{Obtain version information about instance \code{object}. See \code{\link{classVersion}}.}
    \item{\code{classVersion(object) <- value}}{Set version information on instance \code{object} to \code{value}; useful when \code{object} is an instance of a class that contains \code{VersionClass}. See \code{\link{classVersion}}.}
    \item{\code{classVersion(object)["id"] <- value}}{Create or update version information \code{"id"} on instance \code{object} to \code{value}; useful when \code{object} is an instance of a class that contains \code{VersionClass}. See \code{\link{classVersion}}.}
    \item{\code{show(object)}}{Default method returns \code{invisible},
      to avoid printing confusing information when your own class does
      not have a \code{show} method defined. Use
      \code{classVersion(object)} to get or set version information.}
  }
}

\author{Biocore}

\seealso{
  \code{\link{Versions-class}}
}

\examples{

obj <- new("Versioned", versions=list(A="1.0.0"))
obj
classVersion(obj)

A <- setClass("A", contains="Versioned")

classVersion("A")
a <- A()
a # 'show' nothing by default
classVersion(a)

B <- setClass("B", contains="Versioned",
         prototype=prototype(new("Versioned",versions=list(B="1.0.0"))))

classVersion("B")
b <- B()
classVersion(b)

classVersion(b)["B"] <- "1.0.1"
classVersion(b)
classVersion("B")

classVersion("B") < classVersion(b)
classVersion(b) == "1.0.1"

C <- setClass("C",
         representation(x="numeric"),
         contains=("VersionedBiobase"),
         prototype=prototype(new("VersionedBiobase", versions=c(C="1.0.1"))))

setMethod("show", signature(object="C"),
          function(object) print(object@x))

c <- C(x=1:10)
c

classVersion(c)

}

\keyword{classes}
