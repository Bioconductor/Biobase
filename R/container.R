#Copyright 2001, R. Gentleman, all rights reserved.

#A simple class structure for containers
#These are simply lists where the list can contain only
#objects of the specified class


setClass("container", representation( x = "list", content =
                                     "character", locked = "logical"),
         prototype = list(x=vector("list", 0), content="object",
                                     locked=FALSE))

if( !isGeneric("content") )
    setGeneric("content", function(object)
 standardGeneric("content"))

setMethod("content", "container", function(object) object@content)

if( !isGeneric("locked") )
    setGeneric("locked", function(object)
 standardGeneric("locked"))

setMethod("locked", "container", function(object) object@locked)



setReplaceMethod("[[", "container", function(x, i, j, ..., value) {
    if( locked(x) )
        stop("cannot assign into a locked container")
    cv <- class(value)
    cont <- content(x)
    if( !extends(cv, cont) )
        stop(paste("the container is class", cont,
                   "the object is class", cv, "cannot assign",
                   sep=" "))
    object@x[[i]] <- value
})

setMethod("[[", "container", function(x, i, j,...) {
    x@x[[i]]
})

setMethod("print", "container", function(x, ...) {
    cat("Container of ", content(x), "\n", sep="")
    print(x@x)
})

setMethod("[", "container",
    def = function(x, i, j, ..., drop = F){
      new("container", content = content(x), x = x@x[i],
          locked = locked(x))
})

