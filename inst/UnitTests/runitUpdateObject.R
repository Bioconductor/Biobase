testUpdateObjectList <- function() {
    setClass("A", representation(x="numeric"), prototype(x=1:10))
    a <- new("A")
    l <- list(a,a)
    checkTrue(identical(l, updateObject(l)))
    
    setMethod("updateObject", "A",
              function(object, ..., verbose=FALSE) {
                  if (verbose) message("updateObject object = 'A'")
                  object@x <- -object@x
                  object
              })

    obj <- updateObject(l)
    checkTrue(identical(lapply(l, function(elt) { elt@x <- -elt@x; elt }),
                        obj))
    removeMethod("updateObject", "A")
}

testUpdateObjectEnv <- function() {
    e <- new.env()
    e$x=1
    e$.x=1
    obj <- updateObject(e)
    checkTrue(identical(e,obj))         # modifies environment

    lockEnvironment(e)
    obj <- updateObject(e)              # copies environment
    checkTrue(identical(lapply(ls(e, all=TRUE), function(x) x),
                        lapply(ls(obj, all=TRUE), function(x) x)))
    checkTrue(!identical(e, obj))       # different environments

    e <- new.env()
    e$x=1
    e$.x=1
    lockBinding("x", e)
    checkException(updateObject(e))

    lockEnvironment(e)
    obj <- updateObject(e)
##     checkTrue(TRUE==bindingIsLocked("x", obj)) # R bug, 14 May, 2006
    checkTrue(FALSE==bindingIsLocked(".x", obj))
}

testUpdateObjectDefaultsBroken <- function() {
    x <- 1:10
    checkTrue(class(updateObjectTo(x, numeric())) == "numeric")
}

testUpdateObjectDefaults <- function() {
    x <- 1:10
    checkTrue(identical(x, updateObject(x)))
    checkTrue(identical(1:10, updateObjectTo(x, 10:1)))
    x <- as.numeric(1:10)
    checkTrue(identical(as.integer(1:10), updateObjectTo(x, integer())))
    checkTrue(!identical(as.numeric(1:10), updateObjectTo(x, integer())))

    setClass("A",
             representation(x="numeric"),
             prototype=prototype(x=1:10))
    a <- new("A")
    checkTrue(identical(a,updateObject(a)))
    a1 <- new("A",x=10:1)
    checkTrue(identical(a, updateObjectTo(a, a1)))

    setClass("B", representation(x="numeric"))
    b <- new("B")
    checkException(updateObjectTo(a, b))

    setAs("A", "B", function(from) {
        b <- new("B")
        b@x <- from@x
        b
    })
    obj <- updateObjectTo(a,b)
    checkTrue(class(obj)=="B")
    checkIdentical(obj@x, a@x)
    removeMethod("coerce", c("A","B"))
}

testUpdateExpressionSet <- function() {
    obj <- new("ExpressionSet")
    checkTrue(identical(obj, updateObject(obj)))
    checkTrue(!identical(new("ExpressionSet"), updateObject(obj))) # different environments
    obj <- new("ExpressionSet", storage.mode="list")
    checkTrue(identical(obj, updateObject(obj)))
    checkTrue(identical(new("ExpressionSet", storage.mode="list"), updateObject(obj))) # same class -- list

    data(sample.ExpressionSet)
    checkException(validObject(sample.ExpressionSet))

    obj <- updateObject(sample.ExpressionSet)
    checkTrue(isVersioned(obj))
    checkTrue(all(isCurrent(obj)))
    checkTrue(validObject(obj))
    checkTrue(identical(lapply(ls(assayData(obj), all=TRUE), function(x) x),
                        lapply(ls(assayData(sample.ExpressionSet),all=TRUE), function(x) x)))
    checkTrue(!identical(assayData(obj), assayData(sample.ExpressionSet))) # different environments
    checkTrue(identical(phenoData(obj), phenoData(sample.ExpressionSet)))
    checkTrue(identical(experimentData(obj), experimentData(sample.ExpressionSet)))
    checkTrue(identical(annotation(obj), annotation(sample.ExpressionSet)))

    obj1a <- updateObjectTo(sample.ExpressionSet, new("ExpressionSet"))
    ## next better written as(sample.ExpressionSet, "MultiSet")
    obj1b <- updateObjectTo(sample.ExpressionSet, new("MultiSet"))
    obj2 <- updateObject(obj)           # stop after eSet
}

testUpdateExprSet <- function() {
    data(sample.exprSet)
    obj <- updateObject(sample.exprSet,verbose=TRUE)
}

testUpdateESetMisc <- function() {
    data(sample.exprSet)
    obj <- as(sample.exprSet, "ExpressionSet")

    data(sample.eSet)
    obj <- as(sample.eSet, "MultiSet")
}
