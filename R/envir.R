##Copyright R. Gentleman, 2004, all rights reserved
##Functions for lists -> in C for efficiency

listLen <- function(list)
    .Call("listLen", list, PACKAGE="Biobase")

##see if we can speed things up

l2e <- function(vals, envir) {
    if(missing(envir)) envir <- new.env(hash=TRUE)
    .Call("listToEnv", vals, envir, PACKAGE="Biobase")
}

rowQ <- function(imat, which) {
    if( any(is.na(imat)) )
       stop("cannot handle missing values  -- yet")

    if( !is.finite(which) || length(which)!=1 || !is.numeric(which) )
        stop("which must be length one, and numeric")
    .Call("rowQ", imat, which, PACKAGE="Biobase")
}

rowMedians <- function(imat) {
    nr = ncol(imat)
    half <- (nr + 1)/2
    if( nr%%2 == 1 )
        return(rowQ(imat, half))
    else
        return((rowQ(imat, half) + rowQ(imat, half+1))/2)
}


rowMin <- function(imat)
    rowQ(imat, 1)

rowMax <- function(imat)
    rowQ(imat, ncol(imat))


copyEnv <- function(oldEnv, newEnv=new.env(hash=TRUE,
                    parent=parent.env(oldEnv)), all.names=FALSE) {
    oldVals <- as.list(oldEnv, all.names)
    l2e(oldVals, newEnv)
}
