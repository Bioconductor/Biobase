#Copyright 2001, R. Gentleman, all rights reserved.

#A simple class structure for containers
#These are simply lists where the list can contain only
#objects of the specified class

require(methods)

.initContainer <- function(where) {
setClass("container", representation( x = "list", content =
                                     "character", locked = "logical"),
         prototype = list(x=vector("list", 0), content="object",
                                     locked=FALSE), where=where)

if( !isGeneric("content") )
    setGeneric("content", function(object)
 standardGeneric("content"), where=where)

setMethod("content", "container", function(object) object@content,
          where=where )

if( !isGeneric("locked") )
    setGeneric("locked", function(object)
 standardGeneric("locked"), where=where)

setMethod("locked", "container", function(object) object@locked,
          where=where )

setReplaceMethod("[[", "container", function(x, i, j,..., value) {
    if( locked(x) )
        stop("cannot assign into a locked container")
    cv <- class(value)
    cont <- content(x)
    if( !extends(cv, cont) )
        stop(paste("the container is class", cont,
                   "the object is class", cv, "cannot assign",
                   sep=" "))
    x@x[[i]] <- value
    x
}, where=where)

setMethod("[[", "container", function(x, i, j, ...) {
    x@x[[i]]
}, where=where)

setMethod("length", "container", function(x) length(x@x)
          , where=where)

setMethod("show", "container", function(object) {
    cat("Container of ", content(object), "\n", sep="")
    print(object@x)
}, where=where)

setMethod("[", "container",
    def = function(x, i, j, ..., drop = F){
      new("container", content = content(x), x = x@x[i],
          locked = locked(x))
}, where=where)

}
