\name{Versions}
\docType{class}
\alias{Versions-class}
\alias{Versions}

\alias{initialize,Versions-method}
\alias{[,Versions-method}
\alias{[<-,Versions-method}
\alias{[[<-,Versions-method}
\alias{$<-,Versions-method}
\alias{Compare,Versions,Versions-method}
\alias{Compare,Versions,character-method}
\alias{Compare,character,Versions-method}
\alias{updateObject,Versions-method}
\alias{show,Versions-method}
\alias{coerce,Versions,character-method}

\title{Class "Versions"}

\description{

  A class to record version number information. This class is used to
  report versions; to add version information to your own class, use
  \code{\link{Versioned-class}}.

}

\section{Methods}{

  The following are defined; package developers may write additional methods.
  \describe{
    \item{\code{new("Versions", ...)}}{Create a new \code{Versions-class} instance,
      perhaps with named version elements (the contents of
      \code{...}) added. Named elements of \code{versions} are
      character strings that can be coerced using
      \code{\link{package_version}}, or \code{package_version}
      instances, \code{Versions-class} objects.}
    \item{\code{object["id"]}}{Obtain version information \code{"id"} from \code{object}.}
    \item{\code{object["id"] <- value}}{Create or update version information \code{"id"} on instance \code{object}.}
    \item{\code{object[["id"]]}}{Obtain version information \code{"id"} from \code{object}. The result is a list of integers, corresponding to entries in the version string.}
    \item{\code{object[["id"]] <- value}}{Create or update version information \code{"id"} on instance \code{object}.}
    \item{\code{object$id}}{Obtain version information \code{"id"} from \code{object}.The result is a list of integers, corresponding to entries in the version string.}
    \item{\code{object$id <- value}}{Create or update version
      information \code{"id"} on instance \code{object}.}
    \item{\code{show(object)}}{Display version information.}
    \item{\code{updateObject(object)}}{Update \code{object} to the
      current \code{Versions-class} representation. Note that this does \emph{not}
      update another class that uses \code{Versions-class} to track the
      class version.}
    \item{\code{as(object, "character")}}{Convert \code{object} to character representation, e.g., \code{1.0.0}}
    \item{\code{object1 < object2}}{Compare \code{object1} and \code{object2} using version class information. Symbols in addition to \code{<} are admissible; see \code{?Ops}}
  }
}

\author{Biocore}

\seealso{
  \code{\link{classVersion}}
  \code{\link{isCurrent}}
  \code{\link{isVersioned}}
}

\examples{

obj <- new("Versions", A="1.0.0")
obj

obj["A"] <- "1.0.1"
obj
obj["B"] <- "2.0"
obj

obj1 <- obj
obj1["B"] <- "2.0.1"

obj1 == obj
obj1["B"] > "2.0.0"
obj["B"] == "2.0" # TRUE!

}

\keyword{classes}
