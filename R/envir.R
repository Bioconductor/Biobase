##Copyright R. Gentleman, 2004, all rights reserved
##Functions for lists -> in C for efficiency

listLen <- function(list)
    .Call("listLen", list, PACKAGE="Biobase")

##see if we can speed things up

l2e <- function(vals, envir) {
    if(missing(envir)) envir <- new.env(hash=TRUE)
    .Call("listToEnv", vals, envir, PACKAGE="Biobase")
}

rowMedians <- function(imat) {
    if( any(is.na(imat) ) )
      stop("cannot handle missing values - yet")
    .Call("rowMeds", imat, PACKAGE="Biobase")
}

copyEnv <- function(oldEnv, newEnv=new.env(hash=TRUE,
                    parent=parent.env(oldEnv)), all.names=FALSE) {
    oldVals <- as.list(oldEnv, all.names)
    l2e(oldVals, newEnv)
}
