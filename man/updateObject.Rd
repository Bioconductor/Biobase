\name{updateObject}
\alias{updateObject}
\alias{updateObjectTo}

\alias{updateObject,ANY-method}
\alias{updateObject,list-method}
\alias{updateObject,environment-method}
\alias{updateObjectTo,ANY,ANY-method}

\alias{updateObjectFromSlots}
\alias{getObjectSlots}

\title{Update an object to its current class definition}
\description{

  These generic functions return an instance of \code{object} updated to
  its current class definition (or to the class definition of
  \code{template}, in the case of \code{updateObjectTo}).

  Updating objects is primarily useful when an object has been
  serialized (e.g., stored to disk) for some time (e.g., months), and the
  class definition has in the mean time changed. Because of the changed
  class definition, the serialized instance is no longer valid.

  \code{updateObject} requires that the class of the returned object be
  the same as the class of the argument \code{object}, and that the
  object is valid (see \code{\link[methods]{validObject}}). By default,
  \code{updateObject} has the following behaviors:
  \describe{
    \item{\code{updateObject(ANY, ..., verbose=FALSE)}}{By default,
      \code{updateObject} returns the object passed to it. The optional
      named argument \code{verbose} causes a message to be printed describing
      the action. Arguments \code{...} are ignored.}
    \item{\code{updateObject(list, ..., verbose=FALSE)}}{Visit each element in \code{list},
      applying \code{updateObject(list[[elt]], ..., verbose=verbose)}.}
    \item{\code{updateObject(environment, ..., verbose=FALSE)}}{Visit
      each element in \code{environment}, applying
      \code{updateObject(environment[[elt]], ..., verbose=verbose)}}
  }

  \code{updateObjectTo} requires that the class of the returned object
  be the same as the class of the \code{template} argument, and that the
  object is valid. Usually, updating proceeds by modifying slots in
  \code{template} with information from \code{object}, and returning
  \code{template}. Use \code{\link{as}} to coerce an object from one
  type to another; \code{updateObjectTo} might be useful to update a
  virtual superclass. By default, \code{updateObjectTo} has the following
  behavior:
  \describe{
    \item{\code{updateObjectTo(ANY-object,ANY-template)}}{Attempt \code{as(ANY-object,class(ANY-template))}.}
  }

  Sample methods are illustrated below.

  \code{updateObjectFromSlots(object, objclass = class(object), ..., verbose=FALSE)} is a
  utility function that identifies the intersection of slots defined in
  the \code{object} instance and \code{objclass} definition. The
  corresponding elements in \code{object} are then updated (with
  \code{updateObject(elt, ..., verbose=verbose)}) and used as arguments to
  a call to \code{new(class, ...)}, with \code{...} replaced by slots
  from the original object. If this fails, \code{updateObjectFromSlots}
  then tries \code{new(class)} and assigning slots of \code{object} to
  the newly created instance.

  \code{getObjectSlots(object)} extracts the slot names and contents from
  \code{object}. This is useful when \code{object} was created by a class
  definition that is no longer current, and hence the contents of
  \code{object} cannot be determined by accessing known slots.
}
\usage{
updateObject(object, ..., verbose=FALSE)
updateObjectTo(object, template, ..., verbose=FALSE)
updateObjectFromSlots(object, objclass=class(object), ..., verbose=FALSE)
getObjectSlots(object)
}
\arguments{
  \item{object}{Object to be updated, or for slot information to be
    extracted from.}
  \item{template}{Instance representing a template for updating object.}
  \item{objclass}{Optional character string naming the class of the object to be created.} 
  \item{verbose}{A logical, indicating whether information about the
    update should be reported. Use \code{message} to report this.}
  \item{...}{Additional arguments, for use in specific update methods}
}
\value{
  \code{updateObject} returns a valid instance of \code{object}.
  \code{updateObjectTo} returns a valid instance of \code{template}.
  \code{updateObjectFromSlots} returns an instance of class \code{objclass}.
  \code{getObjectSlots} returns a list of named elements, with each
  element corresponding to a slot in \code{object}.
}

\author{Biocore team}

\seealso{\code{\link{Versions-class}}}

\examples{
## update object, same class
data(sample.ExpressionSet)
obj <- updateObject(sample.ExpressionSet)

## generally, use 'as' to coerce objects between classes
data(sample.eSet)
obj <- as(sample.eSet, "MultiSet")

setClass("UpdtA", representation(x="numeric"), contains="data.frame")
setMethod("updateObject", signature(object="UpdtA"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject object = 'A'")
              object <- callNextMethod()
              object@x <- -object@x
              object
})

a <- new("UpdtA", x=1:10)
## See steps involved
updateObject(a)

removeClass("UpdtA")
removeMethod("updateObject", "UpdtA")
}

\keyword{manip}
