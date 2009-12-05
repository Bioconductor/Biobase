testUpdateObjectList <- function() {
    setClass("A",
             representation(x="numeric"), prototype(x=1:10),
             where=.GlobalEnv)
    a <- new("A")
    l <- list(a,a)
    checkTrue(identical(l, updateObject(l)))
    
    setMethod("updateObject", "A",
              function(object, ..., verbose=FALSE) {
                  if (verbose) message("updateObject object = 'A'")
                  object@x <- -object@x
                  object
              },
              where=.GlobalEnv)

    obj <- updateObject(l)
    checkTrue(identical(lapply(l, function(elt) { elt@x <- -elt@x; elt }),
                        obj))
    removeMethod("updateObject", "A", where=.GlobalEnv)
    removeClass("A", where=.GlobalEnv)
}

testUpdateObjectEnv <- function() {
    opts <- options()
    options(warn=-1)
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
    checkException(updateObject(e), silent=TRUE)

    lockEnvironment(e)
    obj <- updateObject(e)
    checkTrue(TRUE==bindingIsLocked("x", obj)) # R bug, 14 May, 2006, fixed
    checkTrue(FALSE==bindingIsLocked(".x", obj))
    options(opts)
}

testUpdateObjectDefaults <- function() {
    x <- 1:10
    checkTrue(identical(x, updateObject(x)))
    checkTrue(identical(1:10, updateObjectTo(x, 10:1)))
    x <- as.numeric(1:10)
    checkTrue(identical(as.integer(1:10), updateObjectTo(x, integer())))
    checkTrue(!identical(as.numeric(1:10), updateObjectTo(x, integer())))
}

testUpdateObjectS4 <- function() {
    setClass("A", 
             representation=representation(
               x="numeric"),
             prototype=list(x=1:5),
             where=.GlobalEnv)
    .__a__ <- new("A")
    setClass("A",
             representation=representation(
               x="numeric",
               y="character"),
             where=.GlobalEnv)
    checkException(validObject(.__a__), silent=TRUE)      # now out-of-date
    .__a__@x <- 1:5
    a <- updateObject(.__a__)
    checkTrue(validObject(a))
    checkIdentical(1:5, a@x)
    removeClass("A", where=.GlobalEnv)
}

testUpdateObjectSetClass <- function() {
    setClass("A",
             representation(x="numeric"),
             prototype=prototype(x=1:10),
             where=.GlobalEnv)
    a <- new("A")
    checkTrue(identical(a,updateObject(a)))
    a1 <- new("A",x=10:1)
    checkTrue(identical(a, updateObjectTo(a, a1)))

    setClass("B",
             representation(x="numeric"),
             where=.GlobalEnv)
    b <- new("B")
    checkException(updateObjectTo(a, b), silent=TRUE)

    setAs("A", "B", function(from) {
        b <- new("B")
        b@x <- from@x
        b
    }, where=.GlobalEnv)
    obj <- updateObjectTo(a,b)
    checkTrue(class(obj)=="B")
    checkIdentical(obj@x, a@x)
    removeMethod("coerce", c("A","B"), where=.GlobalEnv)
    removeClass("B", where=.GlobalEnv)
    removeClass("A", where=.GlobalEnv)
}

testUpdateExpressionSet <- function() {
    opts <- options()
    options(warn=-1)
    obj <- new("ExpressionSet")
    checkTrue(all.equal(obj, updateObject(obj)))
    checkTrue(!identical(new("ExpressionSet"), updateObject(obj))) # different environments
    obj <- new("ExpressionSet", storage.mode="list")
    checkTrue(identical(obj, updateObject(obj)))
    checkTrue(identical(new("ExpressionSet", storage.mode="list"), updateObject(obj))) # same class -- list

    data(sample.ExpressionSet)
    classVersion(sample.ExpressionSet)["eSet"] <- "1.0.0"
    checkException(validObject(sample.ExpressionSet), silent=TRUE)

    suppressMessages(obj <- updateObject(sample.ExpressionSet))
    checkTrue(isVersioned(obj))
    checkTrue(all(isCurrent(obj)))
    checkTrue(validObject(obj))
    checkTrue(identical(lapply(ls(assayData(obj), all=TRUE), function(x) x),
                        lapply(ls(assayData(sample.ExpressionSet),all=TRUE), function(x) x)))
    checkTrue(identical(annotation(obj), annotation(sample.ExpressionSet)))

    suppressMessages(obj1a <- updateObjectTo(sample.ExpressionSet, new("ExpressionSet")))
    ## next better written as(sample.ExpressionSet, "MultiSet")
    suppressMessages(obj1b <- updateObjectTo(sample.ExpressionSet, new("MultiSet")))
    obj2 <- updateObject(obj)           # stop after eSet
    options(opts)
}

testUpdateESetMisc <- function() {
    opts <- options()
    options(warn=-1)
    fp <- system.file("UnitTests", "VersionedClass_data", "devel", "sample.exprSet.rda",
                      package="Biobase")
    load(fp)
    suppressMessages(obj <- as(sample.exprSet, "ExpressionSet"))
    checkTrue(validObject(obj, complete=TRUE))
    checkTrue(all(sapply(c("phenoData", "experimentData", "featureData"),
                         function(nm) isS4(eval(parse(text=paste(nm,"(obj)", sep="")))))))

    fp <- system.file("UnitTests", "VersionedClass_data", "devel", "sample.eSet.rda",
                      package="Biobase")
    load(fp)
    obj <- as(sample.eSet, "MultiSet")
    checkTrue(validObject(obj, complete=TRUE))
    checkTrue(all(sapply(c("phenoData", "experimentData", "featureData"),
                         function(nm) isS4(eval(parse(text=paste(nm,"(obj)", sep="")))))))

    fp <- system.file("UnitTests", "VersionedClass_data", "devel", "eset.rda",
                      package="Biobase")
    load(fp)
    obj <- as(eset, "ExpressionSet")
    checkTrue(validObject(obj, complete=TRUE))
    checkTrue(all(sapply(c("phenoData", "experimentData", "featureData"),
                         function(nm) isS4(eval(parse(text=paste(nm,"(obj)", sep="")))))))

    options(opts)
}

testUpdateMiscPreviousInstances <- function() {
    opts <- options("warn")
    options(warn=-1)
    on.exit(options(opts))

    rda <- list.files(system.file("UnitTests", "VersionedClass_data", package="Biobase"),
                       full.names=TRUE, recursive=TRUE, pattern="^([^(ExpressionSet)]).*\\.Rda")
    for (nm in rda) {
        cat(basename(nm), "\n")
        env <- new.env(parent=emptyenv())
        load(nm, env)
        eapply(env,
               function(elt) {
                   suppressMessages(obj <- updateObject(elt))
                   checkTrue(isS4(obj))
                   checkTrue(validObject(obj, complete=TRUE))
               })
               
    }
}

testUpdatePreviousExpressionSet <- function() {
    opts <- options("warn")
    options(warn=-1)
    on.exit(options(opts))

    rda <- list.files(system.file("UnitTests", "VersionedClass_data", package="Biobase"),
                      full.names=TRUE, recursive=TRUE, pattern="^ExpressionSet.*\\.Rda")

    for (nm in rda) {
        env <- new.env(parent=emptyenv())
        load(nm, env)
        eapply(env,
               function(elt) {
                   suppressMessages(obj <- updateObject(elt))
                   checkTrue(validObject(obj, complete=TRUE))
                   ## S4
                   checkTrue(all(sapply(c("phenoData", "experimentData", "featureData"),
                                        function(nm) isS4(eval(parse(text=paste(nm,"(obj)", sep="")))))))
                   ## content
                   checkTrue(identical(exprs(obj), slot(elt, "assayData")[["exprs"]]))
                   checkTrue(identical(pData(phenoData(obj)),
                                       slot(slot(elt, "phenoData"), "data")))
                   checkTrue(identical(varMetadata(phenoData(obj)),
                                       slot(slot(elt, "phenoData"), "varMetadata")))
                   nms <- names(getSlots("MIAME"))
                   nms <- nms[!nms %in% ".__classVersion__"]
                   lapply(nms, function(nm)
                          checkTrue(identical(slot(experimentData(obj), nm),
                                              slot(slot(elt, "experimentData"), nm))))
               })
    }
}
