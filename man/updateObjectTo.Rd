\name{updateObjectTo}
\alias{updateObjectTo}
\alias{updateObjectTo,ANY,ANY-method}

\title{Update an object to the class definition of a template}
\description{
  The \code{updateObjectTo} generic function returns an instance
  of \code{object} updated to the class definition of \code{template}.

  It requires that the class of the returned object be the same as the
  class of the \code{template} argument, and that the object is valid.
  Usually, updating proceeds by modifying slots in
  \code{template} with information from \code{object}, and returning
  \code{template}. Use \code{\link{as}} to coerce an object from one
  type to another; \code{updateObjectTo} might be useful to update a
  virtual superclass. By default, \code{updateObjectTo} has the following
  behavior:
  \describe{
    \item{\code{updateObjectTo(ANY-object,ANY-template)}}{Attempt \code{as(ANY-object,class(ANY-template))}.}
  }
}
\usage{
updateObjectTo(object, template, \dots, verbose=FALSE)
}
\arguments{
  \item{object}{Object to be updated.}
  \item{template}{Instance representing a template for updating object.}
  \item{\dots}{Additional arguments, for use in specific update methods.}
  \item{verbose}{A logical, indicating whether information about the
    update should be reported. Use \code{message} to report this.}
}
\value{
  \code{updateObjectTo} returns a valid instance of \code{template}.
}

\author{Biocore team}

\seealso{
  \code{\link[BiocGenerics]{updateObject}},
  \code{\link{Versions-class}}
}

\keyword{manip}
