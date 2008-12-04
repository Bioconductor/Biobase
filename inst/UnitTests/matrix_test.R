testCombineMatrixDifferentModes <- function() {
    m <- matrix(1:20, nrow=5, dimnames=list(LETTERS[1:5], letters[1:4]))
    n <- matrix(as.numeric(1:20),
                nrow=5, dimnames=list(LETTERS[1:5], letters[1:4]))
    res <- combine(m, n)                # modes coerced to same
    checkEquals("numeric", mode(res))
    n <- matrix(as.character(1:20),
                nrow=5, dimnames=list(LETTERS[1:5], letters[1:4]))
    checkException(combine(m, n))       # modes differ
}

testCombineMatrix <- function() {
    ## dimnames
    m <- matrix(1:20, nrow=5, dimnames=list(LETTERS[1:5], letters[1:4]))
    checkEquals(m, combine(m, m))
    checkEquals(m, combine(m[1:3,], m[4:5,]))
    checkEquals(m, combine(m[,1:3], m[,4, drop=FALSE]))
    ## overlap
    checkEquals(m, combine(m[1:3,], m[3:5,]))
    checkEquals(m, combine(m[,1:3], m[,3:4]))
    checkEquals(matrix(c(1:3, NA, NA, 6:8, NA, NA, 11:15, NA, NA, 18:20),
                       nrow=5,
                       dimnames=list(LETTERS[1:5], letters[1:4])),
                combine(m[1:3,1:3], m[3:5, 3:4]))
    ## row reordering
    checkEquals(m[c(1,3,5,2,4),], combine(m[c(1,3,5),], m[c(2,4),]))
    ## Exceptions
    checkException(combine(m, matrix(0, nrow=5, ncol=4)),
                   silent=TRUE)         # types differ
    checkException(combine(m, matrix(0L, nrow=5, ncol=4)),
                   silent=TRUE)         # attributes differ
    m1 <- matrix(1:20, nrow=5)
    checkException(combine(m, m1), silent=TRUE) # dimnames required
}
