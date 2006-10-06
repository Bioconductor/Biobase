dat <- list(x=factor(1:10), y=I(1:10), z=I(letters[1:10]))
obj1 <- new("AnnotatedDataFrame",
            data=data.frame(x=factor(1:10), y=I(1:10), z=I(letters[1:10]),
              row.names=LETTERS[1:10]),
            varMetadata=data.frame(
              labelDescription=names(dat),
              class=sapply(dat, class), typeof=sapply(dat, typeof), mode=sapply(dat, mode),
              row.names=c("x","y","z")))

obj2 <- local({
    obj2 <- obj1
    sampleNames(obj2) <- letters[1:dim(obj1)[[1]]]
    obj2
})

checkAsp <- function(obj1, obj) {
    checkTrue(all(sapply(obj[,colnames(obj1), drop=FALSE], typeof)==sapply(obj1, typeof)))
    checkTrue(all(sapply(obj[,colnames(obj1), drop=FALSE], class)==sapply(obj1, class)))
    checkTrue(all(sapply(obj[,colnames(obj1), drop=FALSE], mode)==sapply(obj1, mode)))
}
checkData <- function(obj1, obj2, obj) {
    checkTrue(all(colnames(obj1) %in% colnames(obj)))
    checkTrue(all(colnames(obj2) %in% colnames(obj)))
    checkAsp(obj1, obj)
    checkAsp(obj2, obj)
}
checkVarMetadata <- function(obj1, obj2, obj) {
    checkTrue(all(colnames(obj1) %in% colnames(obj)))
    checkTrue(all(colnames(obj2) %in% colnames(obj)))
    checkTrue(all(rownames(obj1) %in% rownames(obj)))
    checkTrue(all(rownames(obj2) %in% rownames(obj)))

    checkAsp(obj1, obj)
    checkAsp(obj2, obj)
}
check <- function(obj1, obj2, obj) {
    checkData(pData(obj1), pData(obj2), pData(obj))
    checkVarMetadata(varMetadata(obj1), varMetadata(obj2), varMetadata(obj))
}
checkUnchangedPData <- function(p1, p2, p) {
    checkTrue(all(lapply(colnames(p1),
                         function(nm) identical(p[1:dim(p1)[[1]],nm], p1[,nm]))))
    checkTrue(all(lapply(colnames(p2),
                         function(nm) identical(p[dim(p1)[[1]] + 1:dim(p1)[[1]],nm], p2[,nm]))))
}

testEmptyCombine <- function() {
    obj <- new("AnnotatedDataFrame")
    checkTrue(identical(obj, combine(obj, obj)))
}

testAnnotatedDataFrameCombine <- function() {
    options(warn=-1)
    ## duplicate sampleNames
    checkTrue(identical(obj1, combine(obj1,obj1)))

    ## two distint pData
    obj <- combine(obj1, obj2)
    check(obj1, obj2, obj)
    checkTrue(identical(varMetadata(obj1), varMetadata(obj))) # varMetadata unchanged
    checkTrue(identical(varMetadata(obj2), varMetadata(obj)))

    ## warning about coercing pData factors
    obj2a <- obj2
    pData(obj2a)[["x"]] <- factor(letters[1:10])
    checkException(combine(obj1,obj2a), silent=TRUE)

    ## varMetadata with different numbers of columns
    obj4 <- obj2
    varMetadata(obj4)[,"int"] <- 1:dim(varMetadata(obj1))[[1]]
    varMetadata(obj4)[,"char"] <- I(letters[1:dim(varMetadata(obj1))[[1]]])
    obj <- combine(obj1, obj4)
    checkUnchangedPData(pData(obj1), pData(obj4), pData(obj))
    check(obj1, obj4, obj)
    
    ## varMetadata content mismatch
    obj3 <- obj2
    varMetadata(obj3)[,1] <- varMetadata(obj3)[,4]
    colnames(varMetadata(obj3)) <- colnames(varMetadata(obj2))
    checkException(combine(obj1, obj3), silent=TRUE)

    ## varMetadata multi-column mismatch
    obj3 <- obj2
    varMetadata(obj3)[,1:2] <- varMetadata(obj3)[,4:3]
    colnames(varMetadata(obj3)) <- colnames(varMetadata(obj2))
    checkException(combine(obj1, obj3), silent=TRUE)

    ## varMetadata extra columns
    obj5 <- obj2
    pData(obj5)[,"int"] <- 1:dim(pData(obj1))[[1]]
    pData(obj5)[,"char"] <- I(letters[1:dim(pData(obj1))[[1]]])
    varMetadata(obj5)[c("int","char"),] <- NA
    obj <- combine(obj1, obj5)
    checkUnchangedPData(pData(obj1), pData(obj5), pData(obj))
    check(obj1, obj5, obj)

    ## varMetadata with conflicting information (NAs)
    obj6 <- obj2
    varMetadata(obj6)["class"] <- NA
    varMetadata(obj6)[2,"typeof"] <- NA
    checkException(combine(obj1, obj6), silent=TRUE)
}

testVarMetadataAssign <- function() {
    ## previously coerced labelData to 'character'
    obj2 <- obj1
    varMetadata(obj2) <- varMetadata(obj1)
    checkTrue(identical(obj1, obj2))
}

testMetadataFactors <- function() {
    pd  = data.frame(covar="Z")
    vmd = data.frame(labelDescription=I("Meta 'covar'"))
    rownames(vmd) = colnames(pd)
    rownames(pd) = "Z"
    a = new("AnnotatedDataFrame", data=pd,  varMetadata=vmd)

    ## factor recode should throw an error
    pd  = data.frame(covar=LETTERS[1])
    rownames(pd) = LETTERS[1]
    b = new("AnnotatedDataFrame", data=pd,  varMetadata=vmd)
    checkException(combine(a,b), silent=TRUE)
}

testNoSharedCols <- function() {
    obj1 <- new("AnnotatedDataFrame",
                data=data.frame(x=factor(1:10)),
                varMetadata=data.frame(labelDescription="x", row.names=c("x")))
    obj2 <- new("AnnotatedDataFrame",
                data=data.frame(y=factor(1:10), row.names=letters[1:10]),
                varMetadata=data.frame(labelDescription="y", row.names=c("y")))
    obj <- combine(obj1,obj2)
    checkTrue(all(pData(obj)[1:10,colnames(pData(obj1)),drop=FALSE]==pData(obj1)))
    checkTrue(all(pData(obj)[11:20,colnames(pData(obj2)),drop=FALSE]==pData(obj2)))
    
}

testValidAnnotatedDataFrame <- function() {
    obj <- obj1
    pData(obj)[["Z"]] <- NA
    checkException(validObject(obj, complete=TRUE), silent=TRUE)
}

testPhenoDataFactors <- function() {
    data(sample.ExpressionSet)
    suppressMessages(obj1 <- updateObject(sample.ExpressionSet))
    obj2 <- obj1
    sampleNames(obj2) <- letters[1:dim(obj1)[[2]]]
    obj1 <- phenoData(obj1)
    obj2 <- phenoData(obj2)
    obj <- combine(obj1, obj2)
    checkTrue(all(pData(obj)[1:nrow(obj1),colnames(pData(obj1)),drop=FALSE]==
                  pData(obj1)))
    checkTrue(all(pData(obj)[nrow(obj1)+1:nrow(obj2),colnames(pData(obj2)),drop=FALSE]==
                  pData(obj2)))
}

testDimLabels <- function() {
    x <- new("AnnotatedDataFrame")
    y <- x
    y@dimLabels <- c("x","y")
    checkException(combine(x,y), silent=TRUE)
}

testNewCovariate <- function() {
    x <- new("AnnotatedDataFrame",data=data.frame(x=1:10))
    x$y <- 1:10
    checkTrue(validObject(x))
    x[["z"]] <- 1:10
    checkTrue(validObject(x))

    x <- new("AnnotatedDataFrame",data=data.frame(x=1:10))
    varMetadata(x)$meta1 <- TRUE
    x[["w"]] <- letters[1:10]
    checkTrue(identical(dim(varMetadata(x)), as.integer(c(2,2))))
    checkTrue(identical(varMetadata(x)["x",,drop=TRUE],
                        list(labelDescription=as.character(NA),meta1=TRUE)))

    x <- new("AnnotatedDataFrame",data=data.frame(x=1:10))
    pData(x) <- pData(x)[1:5,,drop=FALSE]
    checkTrue(validObject(x))
    checkTrue(identical(as.vector(dim(x),"integer"), as.integer(c(5,1))))
}
