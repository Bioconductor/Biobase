##Copyright R. Gentleman, 2001
##All rights reserved

##a simple aggregator
##data are aggregated in the environment env
##if they are not there then the get assigned with
##initfun, if they are there they get aggregated with
##agfun

require("methods")

Aggregate <- function(x, env, agfun, initfun)
 {
   if( !is.environment(env) )
     stop("second argument must be an environment")
   if( !is.function(agfun) || !is.function(initfun) )
     stop("agfun and initfun must be functions")

   if(is.character(x)) {
     for( i in 1:length(x) ) {
       nm <- x[i]
       if( !exists(nm, env=env, inherits=FALSE) )
          assign(nm, env=env, initfun(nm, x))
       else {
          v1 <- get(nm, env=env)
          assign(nm, agfun(nm, v1), env=env)
       }
     }
   }
   else if(is.list(x)) {
     nms <- names(x)
     for( i in 1:length(x) ) {
        nm <- nms[i]
        if( !exists(nm, env=env, inherits=FALSE) )
          assign(nm, env=env, initfun(nm, x[[i]]))
        else {
           v1 <- get(nm, env=env)
           assign(nm, env=env, agfun(nm, v1, x[[i]]))
        }
     }
   }
   else stop("bad type for Aggregate")
 }

setClass("aggregator", representation( aggenv = "environment", initfun =
                                     "function", aggfun = "function"),
         prototype = list(env = new.env(hash=TRUE), initfun =
                                     function(name, val) 1,
         aggfun =  function(name, current, val) current+1 ) )


if( !isGeneric("aggenv") )
    setGeneric("aggenv", function(object) standardGeneric("aggenv"))

setMethod("aggenv", "aggregator", function(object) object@aggenv)

if( !isGeneric("initfun") )
    setGeneric("initfun", function(object) standardGeneric("object"))

setMethod("initfun", "aggregator", function(object) object@initfun)

if( !isGeneric("aggfun") )
    setGeneric("aggfun", function(object) standardGeneric("aggfun"))

setMethod("aggfun", "aggregator", function(object) object@aggfun)
