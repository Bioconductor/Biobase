#Copyright R. Gentleman, 2001
#multiput and multiget
#ideally these will be internalized at some point

#FIXME: I think Luke's Dynamic variables should be used rather than
#the on.exit kludge

multiget <- function(x, pos=-1, envir=as.environment(pos), mode =
                     "any",inherits = TRUE, iffail=NA)
{
    .Deprecated("mget", "base")
    lenx <- length(x)
    ans <- vector("list", length=lenx)
    if( ! is.environment(envir) )
        stop("envir argument is not an environment")
    options(show.error.messages = FALSE)
    on.exit(options(show.error.messages = TRUE))
    for(i in 1:lenx)
        if( is.list(x) )
            ans[[i]] <- try(get(x[[i]],pos,envir, mode, inherits))
        else
            ans[[i]] <- try(get(x[i],pos,envir, mode, inherits))
    options(show.error.messages = TRUE)
    on.exit(NULL)

    failfun <- function(x) {
        cx <- class(x)
        if( !is.null(cx) && cx == "try-error")
            TRUE
        else
            FALSE
    }
    failed <- sapply(ans, failfun)
    ans[failed] <- iffail

    names(ans) <- x
    ans
}

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
