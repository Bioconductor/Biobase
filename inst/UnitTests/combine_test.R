testCombineOne <- function() {
    data(sample.ExpressionSet)
    checkIdentical(sample.ExpressionSet,
                   combine(sample.ExpressionSet))
}

testCombineTwo <- function() {
    data(sample.ExpressionSet)
    x <- sample.ExpressionSet
    checkTrue(all.equal(x, combine(x[,1:5],x[,seq(6, ncol(x))])))
}

testCombineThree <- function() {
    data(sample.ExpressionSet)
    x <- sample.ExpressionSet
    y <- combine(x[,1:5],x[, 6:10], x[,seq(11, ncol(x))])
    checkTrue(all.equal(x, y))
}

testCombineThreeDF <- function() {
    ## data.frame's are tricky, because c(df, list(...)) unlists df
    x <- data.frame(x=1:5,
                    y=factor(letters[1:5], levels=letters[1:8]),
                    row.names=letters[1:5])
    y <- data.frame(z=3:7,
                    y=factor(letters[3:7], levels=letters[1:8]),
                    row.names=letters[3:7])
    w <- data.frame(w=4:8,
                    y=factor(letters[4:8], levels=letters[1:8]),
                    row.names=letters[4:8])
    res <- combine(w, x, y)

    e <- data.frame(w=c(4:8, rep(NA, 3)),
                    y=c(letters[c(4:8, 1:3)]),
                    x=c(4:5, rep(NA, 3), 1:3),
                    z=as.integer(c(4:7, rep(NA, 3), 3)),
                    row.names=letters[c(4:8, 1:3)])
    checkIdentical(e, res)
}

testCombineDF_POSIXct <- function()
{
    ## class(x) can have length > 1 as in Sys.time()
    t0 <- Sys.time()
    df1 <- data.frame(i = 1:3, t = rep(t0, 3), row.names=letters[1:3])
    df2 <- data.frame(i = 1:3, t = c(t0, t0 + 500, t0 + 1000),
                      row.names=c("a", "d", "e"))
    e <- data.frame(i = c(1L, 2L, 3L, 2L, 3L),
                    t = c(t0, t0, t0, t0 + 500, t0 + 1000),
                    row.names=c("a", "b", "c", "d", "e"))
    res <- combine(df1, df2)
    checkIdentical(e, res)
}


testCombineWithNamedArgs <- function() {
    x <- data.frame(x=1:5,
                    y=factor(letters[1:5], levels=letters[1:8]),
                    row.names=letters[1:5])
    y <- data.frame(z=3:7,
                    y=factor(letters[3:7], levels=letters[1:8]),
                    row.names=letters[3:7])
    w <- data.frame(w=4:8,
                    y=factor(letters[4:8], levels=letters[1:8]),
                    row.names=letters[4:8])
    checkIdentical(combine(w, y, x), combine(w, x, y=y))
    checkIdentical(combine(w, y, x), combine(w, y=y, x))
    checkIdentical(combine(x, y, w), combine(w, y=y, x=x))
    checkIdentical(combine(x, y, w), combine(y=y, x=x, w))
}
