env2list <- function(envir, recurse=FALSE) {
    if (!is.environment(envir))
        stop("envir argument is not an environment")

    ans <- list()
    ## This is basically a lot like multiget() except there isn't
    ## need for checking that what you're looking for exists as
    ## you know it does
    vals <- ls(envir)
    for (i in seq(along=vals))
        ans[[i]] <- get(vals[i], envir=envir, inherits=FALSE)
    names(ans) <- vals

    if ((recurse == TRUE)&&(!is.null(parent.env(envir))))
        ans <- c(ans, list2env(parent.env(envir), recurse))

    ans
}

list2env <- function(vals, envir, recurse=FALSE) {
    if (!is.environment(envir))
        stop("envir argument is not an environment")

    if (!is.list(vals))
        stop("vals argument is not a list")

    names <- names(vals)
    ## !!! Does this work the way it is intended when recurse=TRUE?
    for (i in seq(length(vals)))
        assign(names[i], vals[[i]], envir=envir, inherits=recurse)

    envir
}

copyEnv <- function(oldEnv, recurse=FALSE) {
    oldVals <- env2list(oldEnv, recurse)
    newEnv <- new.env(parent=parent.env(oldEnv))
    newEnv <- list2env(oldVals, newEnv)
    return(newEnv)
}
