# ==========================================================================
setMethod("content", "container", function(object) object@content)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("locked", "container", function(object) object@locked)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", "container",
   def = function(x, i, j, ..., drop = F) {
      .container(content = content(x), x = x@x[i], locked = locked(x))
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[[", "container",
   function(x, i, j, ...) {
      x@x[[i]]
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setReplaceMethod("[[", "container",
   function(x, i, j,..., value) {
      if( locked(x) )
         stop("cannot assign into a locked container")
      cv <- class(value)
      cont <- content(x)
      if( !extends(cv, cont) )
         stop("the container is class '", cont,
              "' the object is class '", cv, "' cannot assign")
      x@x[[i]] <- value
      x
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("length", "container", function(x) length(x@x))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("show", "container",
   function(object) {
      cat("Container of ", content(object), "\n", sep="")
      print(object@x)
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

