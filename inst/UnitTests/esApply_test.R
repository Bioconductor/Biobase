test_esApply_base <- function() {
    data(sample.ExpressionSet)
    target <- with(pData(sample.ExpressionSet),
                   apply(exprs(sample.ExpressionSet), 1, sum))
    current <- esApply(sample.ExpressionSet, 1, sum)
    checkIdentical(target, current)
}

test_esApply_local_args <- function() {
    data(sample.ExpressionSet)
    f <- function(x, t) {
        xx <- split(x, t)
        mean(xx[[1]]) - mean(xx[[2]])
    }
    target <- with(pData(sample.ExpressionSet),
                   apply(exprs(sample.ExpressionSet), 1, f, sex))
    current<- esApply(sample.ExpressionSet, 1, f, sex)
    checkIdentical(target, current, msg="unnamed local arg")

    target <- with(pData(sample.ExpressionSet),
                   apply(exprs(sample.ExpressionSet), 1, f, t=sex))
    checkIdentical(current, target, msg="named local arg")
}

test_esApply_lexical_scope <- function() {
    data(sample.ExpressionSet)
    f <- function() {
        x <- 0
        function(y) x <<- x + 1
    }
    target <- with(pData(sample.ExpressionSet),
                   apply(exprs(sample.ExpressionSet), 1, f()))
    current <- esApply(sample.ExpressionSet, 1, f())
    checkIdentical(target, current)
}

test_esApply_global_args <- function() {
    data(sample.ExpressionSet)
    f <- function(x, t) t*sum(x)
    target <- with(pData(sample.ExpressionSet),
                   apply(exprs(sample.ExpressionSet), 1, f, 10))
    current <- esApply(sample.ExpressionSet, 1, f, 10)
    checkIdentical(target, current, msg="unnamed global arg")

    current <- esApply(sample.ExpressionSet, 1, f, t=10)
    checkIdentical(target, current, msg="named global arg")
}
