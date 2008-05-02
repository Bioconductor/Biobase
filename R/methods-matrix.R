setMethod("combine", c("matrix", "matrix"), function(x, y, ...) {
    if (length(y)==0)
        return(x)
    else if (length(x)==0)
        return(y)
    if (typeof(x) != typeof(y))
        stop("matrix types ", typeof(x), ", ", typeof(y), " differ")
    xdim <- dimnames(x)
    ydim <- dimnames(y)
    if (is.null(xdim) || is.null(ydim) ||
        any(sapply(xdim, is.null)) ||
        any(sapply(ydim, is.null)))
        stop("matricies must have dimnames for 'combine'")
    sharedRows <- intersect(xdim[[1]], ydim[[1]])
    sharedCols <- intersect(xdim[[2]], ydim[[2]])
    ok <- all.equal(x[sharedRows, sharedCols], y[sharedRows, sharedCols]) 
    if (!isTRUE(ok))
        stop("matrix shared row and column elements differ: ",
             ok)
    unionRows <- union(xdim[[1]], ydim[[1]])
    unionCols <- union(xdim[[2]], ydim[[2]])

    m <- matrix(new(class(as.vector(x))),
                nrow=length(unionRows), ncol=length(unionCols),
                dimnames=list(unionRows, unionCols))
    m[rownames(x), colnames(x)] <- x
    m[rownames(y), colnames(y)] <- y
    m
})
