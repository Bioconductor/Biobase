checkAssayDataCombine <- function(nr, nc) {
    obj1 <- assayDataNew(exprs=
                         matrix(runif(nr*nc), nrow=nr, ncol=nc,
                                dimnames=list(
                                  if (nr > 0) letters[1:nr] else NULL,
                                  if (nc > 0) LETTERS[1:nc] else NULL)))
    obj <- combine(obj1,obj1)
    checkTrue(!identical(obj, obj1)) # different environments
    checkTrue(identical(obj1$exprs, obj$exprs))

    storageMode(obj1) <- "list"
    obj <- combine(obj1,obj1)
    checkTrue(identical(obj1, obj)) # same list
    checkTrue(identical(obj1$exprs, obj$exprs))

    if (nc > 2) {
        ## combine distinct cols
        obj1 <- assayDataNew(exprs=
                             matrix(runif(nr*nc), nrow=nr, ncol=nc,
                                    dimnames=list(
                                      if (nr > 0) letters[1:nr] else nr,
                                      LETTERS[1:nc])))
        obj2 <- obj1
        sampleNames(obj2)[3] <- letters[3]
        obj <- combine(obj1, obj2)
        checkTrue(all(dim(obj$exprs)==c(nr,nc+1)))
        checkTrue(identical(obj$exprs[,1:nc],obj1$exprs))
        checkTrue(identical(obj$exprs[,nc+1], obj2$exprs[,3]))
    }

    if (nc > 1) {
        ## inconsistent data -- list, otherwise both copies change!
        storageMode(obj1) <- "list"
        obj2 <- obj1
        obj2$exprs[,1] <- runif(nr)
        checkException(combine(obj1, obj2), silent=TRUE)
    }
}

testAssayDataCombine <- function() {
    checkAssayDataCombine(5,3)
    checkAssayDataCombine(0,0)
    checkAssayDataCombine(1,0)
    checkAssayDataCombine(0,1)
}
