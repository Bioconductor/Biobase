testCombineFeatureData <- function() {
    data(sample.ExpressionSet)          # use as source for exprs data
    suppressMessages(obj <- updateObject(sample.ExpressionSet)[1:20,1:10])

    obj1 <- new("ExpressionSet", phenoData=phenoData(obj), exprs=exprs(obj))
    obj2 <- obj1

    pData(featureData(obj1))[["x"]] <- FALSE
    pData(featureData(obj1))[["y"]] <- FALSE
    varMetadata(featureData(obj1)) <-
      rbind(varMetadata(featureData(obj1)),
            x=list(labelDescription="the x"),
            y=list(labelDescription="the y"))
    validObject(obj1)

    sampleNames(obj2) <- letters[1:dim(obj1)[[2]]]
    pData(featureData(obj2))[["y"]] <- FALSE
    pData(featureData(obj2))[["z"]] <- TRUE
    varMetadata(featureData(obj2)) <-
      rbind(varMetadata(featureData(obj2)),
            y=list(labelDescription="the y"),
            z=list(labelDescription="the z"))
    validObject(obj2)
    obj <- combine(obj1,obj2)
    checkTrue(all(varLabels(featureData(obj1)) %in% varLabels(featureData(obj))))
    checkTrue(all(varLabels(featureData(obj2)) %in% varLabels(featureData(obj))))

    ## conflicting feature pData
    pData(featureData(obj2))[["y"]] <- TRUE
    validObject(obj2)
    checkException(combine(obj1, obj2), silent=TRUE)
}

testAddTextNotes <- function() {
    eset <- new("ExpressionSet")
    notes(eset) <- "a note"
    checkTrue(identical(notes(eset), list("a note")))
    notes(eset) <- "another"
    checkTrue(identical(notes(eset), list("a note", "another")))
}

testExtraSlotExpressionClassInitialize1 <- function() {
    ## pass if no error
    checkTrue(validObject(new("ExtraSlotExpressionSet")))
}

testExtraSlotExpressionClassInitialize2 <- function() {
    e <- new("ExtraSlotExpressionSet",
             exprs=new("matrix"),
             extraSlot="hello",
             storage.mode="environment")
    checkEquals("hello", e@extraSlot)
    checkEquals("exprs", ls(assayData(e)))
    checkEquals("environment", storageMode(e))
}

testExtraSlotExpressionClassInitialize3 <- function() {
    e <- new("ExtraSlotExpressionSet",
             assayData=assayDataNew(
               exprs=new("matrix"),
               storage.mode="environment"),
             extraSlot="hello")
    checkEquals("hello", e@extraSlot)
    checkEquals("exprs", ls(assayData(e)))
    checkEquals("environment", storageMode(e))
}
