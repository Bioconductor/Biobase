#Copyright R. Gentleman, 2001
#multiput and multiget
#ideally these will be internalized at some point

#FIXME: I think Luke's Dynamic variables should be used rather than
#the on.exit kludge


multiassign <- function (x, value, envir = parent.frame(), inherits =
                         FALSE)
{
    if( ! is.environment(envir) )
        stop("envir argument is not an environment")
    if( missing(value) ) {
        nx <- names(x)
        if( any(nchar(nx) == 0) )
            stop("value is missing and x does not have named components")
        value <- x
        x <- nx
    }
    lenx <- length(x)
    for(i in 1:lenx) {
        i2 <- (i-1)%%lenx+1
        if( is.list(x) ) {
            if( is.list(value) )
                assign(x[[i]], value[[i2]], envir=envir,
                         inherits=inherits)
            else
                assign(x[[i]], value[i2], envir=envir,
                         inherits=inherits)
        }
        else {
            if( is.list(value) )
                assign(x[i], value[[i2]], envir=envir,
                         inherits=inherits)
            else
                assign(x[i], value[i2], envir=envir,
                         inherits=inherits)
        }
    }
}
