##Copyright R. Gentleman, 2001
##All rights reserved

##a simple aggregator
##data are aggregated in the environment env
##if they are not there then the get assigned with
##initfun, if they are there they get aggregated with
##agfun

Aggregate <- function(x, agg)
{
    if( !inherits(agg, "aggregator") )
        stop("second argument must be an aggregator")
    if( is.null(x) || length(x) == 0 )
        return()
    if(is.character(x)) {
        for( i in 1:length(x) ) {
            nm <- x[i]
            if( !exists(nm, env=aggenv(agg), inherits=FALSE) )
                assign(nm, env=aggenv(agg), initfun(agg)(nm, x))
            else {
                v1 <- get(nm, env=aggenv(agg))
                assign(nm, aggfun(agg)(nm, v1), env=aggenv(agg))
            }
        }
    }
    else if(is.list(x)) {
        nms <- names(x)
        for( i in 1:length(x) ) {
            nm <- nms[i]
            if( !exists(nm, env=aggenv(agg), inherits=FALSE) )
                assign(nm, env=aggenv(agg), initfun(agg)(nm, x[[i]]))
            else {
                v1 <- get(nm, env=aggenv(agg))
                assign(nm, env=aggenv(agg), aggfun(agg)(nm, v1, x[[i]]))
            }
        }
    }
    else stop("bad type for Aggregate")
}

    setClass("aggregator", representation( aggenv = "environment",
                                          initfun = "function",
                                          aggfun = "function"),
             prototype = list(aggenv = new.env(hash=TRUE), initfun =
             function(name, val) 1,
             aggfun =  function(name, current, val) current+1 ))


    if( !isGeneric("aggenv") )
        setGeneric("aggenv", function(object) standardGeneric("aggenv"))

    setMethod("aggenv", "aggregator", function(object) object@aggenv)

    if( !isGeneric("initfun") )
        setGeneric("initfun", function(object) standardGeneric("initfun"))

    setMethod("initfun", "aggregator", function(object) object@initfun)

    if( !isGeneric("aggfun") )
        setGeneric("aggfun", function(object) standardGeneric("aggfun"))

    setMethod("aggfun", "aggregator", function(object) object@aggfun)

