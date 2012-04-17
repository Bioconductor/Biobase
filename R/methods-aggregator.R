setMethod("initialize",
          signature(.Object="aggregator"),
          function(.Object, aggenv=new.env(hash=TRUE), ...) {
              callNextMethod(.Object, aggenv=aggenv, ...)
          })
# ==========================================================================
setMethod("aggenv", "aggregator", function(object) object@aggenv)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("initfun", "aggregator", function(object) object@initfun)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("aggfun", "aggregator", function(object) object@aggfun)
# ==========================================================================
Aggregate <- function(x, agg) {
   if( !is(agg, "aggregator") )
      stop("second argument must be an aggregator")
   if( is.null(x) || length(x) == 0 )
      return()
   if(is.character(x)) {
      for( i in 1:length(x) ) {
         nm <- x[i]
         if( !exists(nm, envir=aggenv(agg), inherits=FALSE) )
            assign(nm, envir=aggenv(agg), initfun(agg)(nm, x))
         else {
            v1 <- get(nm, envir=aggenv(agg))
            assign(nm, aggfun(agg)(nm, v1), envir=aggenv(agg))
         }
      }
   }
   else
      if(is.list(x)) {
         nms <- names(x)
         for( i in 1:length(x) ) {
            nm <- nms[i]
            if( !exists(nm, envir=aggenv(agg), inherits=FALSE) )
               assign(nm, envir=aggenv(agg), initfun(agg)(nm, x[[i]]))
            else {
               v1 <- get(nm, envir=aggenv(agg))
               assign(nm, envir=aggenv(agg), aggfun(agg)(nm, v1, x[[i]]))
            }
         }
      }
      else
         stop("bad type for Aggregate")
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

