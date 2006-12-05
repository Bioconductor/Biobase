baseClass <- list(eSet = NULL)
## featureSubclases <-
##   list(PmOneChannelExpressionSet = c("pm"),
##        PmMmOneChannelExpressionSet = c("pm", "mm"),
##        PmMmOneChannelSNPSet = c("pm", "mm"))
featureSubclases <- NULL
targetSubclasses <-
  list(ExpressionSet = c("exprs"),
       MultiSet = c("myElt", "yourElt"),
       SnpSet = c("call", "callProbability"))

allClasses <- c( baseClass, featureSubclases, targetSubclasses )
allSubclasses <- c( featureSubclases, targetSubclasses )
exprsEnabledSubclasses <- c("ExpressionSet","SnpSet")
nonEmptySubclasses <- exprsEnabledSubclasses

modes <- c("lockedEnvironment","environment","list")

.features <- 20
.samples <- 10

helperNew <- function(obj, ...) {
  .pData <- data.frame(numeric(.samples),
                       row.names=I(1:.samples))[,FALSE]
  .fData <- data.frame(numeric(.features),
                       row.names=I(1:.features))[,FALSE]
  args <- function(names) {
    obj <- lapply(names, function(nm) new("matrix", nr = .features, nc = .samples))
    names(obj) <- names
    c(obj, list(phenoData = new("AnnotatedDataFrame", data=.pData),
                featureData = new("AnnotatedDataFrame", data=.fData)
                ))
  }
  argsSnpN <- function(n) {
    list(call = new("array", dim=c(.features,.samples, n)),
         callProbability = new("array", dim=c(.features,.samples, n)),
         phenoData = new("AnnotatedDataFrame", data=.pData),
         featuerData = new("AnnotatedDataFrame", data=.fData))
  }
  argsSnpDetail <- function(names) {
    obj <- lapply(names, function(nm) {
      if (nm != "copyNumber")
        new("array", dim=c(.features, .samples, 2))
      else
        new("array",dim=c(.features, .samples, 3))
    })
    names(obj) <- names
    c(obj, list(phenoData = new("AnnotatedDataFrame", data=.pData),
                featureData = new("AnnotatedDataFrame", data=.fData)))
  }
  dots <- if (length(list(...))>0) list(...) else NULL
  switch(obj,
         SnpSetDetail = do.call("new", c(obj,argsSnpDetail(allClasses[[obj]]), dots)),
         SnpSet2 = do.call("new", c(obj,argsSnpN(2),dots)),
         SnpSet4 = do.call("new", c(obj,argsSnpN(8),dots)),           
         eSet = new("eSet", ...),
         ## default
         do.call("new", c(obj, args(allClasses[[obj]]), dots)))
}

helperFillWithNoise <- function( obj ) {
  aData <- assayData(obj)
  storage.mode <- storageMode(obj)
  names <- if (is(aData,"environment")) ls(aData) else names(aData)
  if (is(aData,"environment")) aData <- new.env(parent=emptyenv())
  adim <- as.numeric(dim(obj))
  n <- prod(adim)
  for( nm in names ) {
    if (length(adim)==2)
      aData[[nm]] <- matrix(runif(n), nr=adim[[1]], nc=adim[[2]])
    else
      aData[[nm]] <- array(runif(n),adim)
  }
  sampleNames(aData) <- sampleNames(obj)
  featureNames(aData) <- featureNames(obj)
  if (storage.mode=="lockedEnvironment") lockEnvironment(aData, bindings=TRUE)
  assayData(obj) <- aData
  obj
}

testNewSubclassCreation <- function() {
  for (mode in modes)
    lapply( names(allSubclasses), new, storage.mode=mode)
}

testStorageMode <- function() {
    lapply(names(allSubclasses), function(s) {
        obj <- new(s)
        checkTrue(class(assayData(obj))=="environment")
        obj <- new(s, storage.mode="list")
        checkTrue(class(assayData(obj))=="list")
        obj <- new(s, storage.mode="environment")
        checkTrue(class(assayData(obj))=="environment")
        ## is helperNew behaving?
        obj <- helperNew(s)
        checkTrue(class(assayData(obj))=="environment")
        obj <- helperNew(s, storage.mode="list")
        checkTrue(class(assayData(obj))=="list")
        obj <- helperNew(s, storage.mode="environment")
        checkTrue(class(assayData(obj))=="environment")
    })
    ## update storage modes
    data(sample.ExpressionSet)
    storageMode(sample.ExpressionSet) <- "environment"
    checkTrue(storageMode(sample.ExpressionSet)=="environment")
    storageMode(sample.ExpressionSet) <- "lockedEnvironment"
    checkTrue(storageMode(sample.ExpressionSet)=="lockedEnvironment")
    storageMode(sample.ExpressionSet) <- "list"
    checkTrue(storageMode(sample.ExpressionSet)=="list")
    storageMode(sample.ExpressionSet) <- "environment"
    checkTrue(storageMode(sample.ExpressionSet)=="environment")
}

testNewValidSubclasses <- function() {
  for (mode in modes)
    lapply(names(allSubclasses), function( s ) {
      obj <- new(s, storage.mode=mode)
      checkTrue(validObject(obj))
    })
  for (mode in modes)
    lapply(names(allSubclasses), function(s) {
      obj <- helperNew(s, storage.mode=mode)
      checkTrue(class(obj)==s)
      checkTrue(validObject(obj))
    })
}

testPreallocSubclasses <- function() {
  for (mode in modes)
    lapply(names(allSubclasses), function(s) {
      obj <- helperNew(s, storage.mode=mode)
      checkTrue( all( dim(obj)[1:2] == c( .features, .samples )))
      checkTrue( all( dim(phenoData(obj)) == c( .samples, 0 )))
    })
}

testInitializeWithNames <- function() {
    ## do names applied to one component get picked up?
    exprs <- matrix(1:10, ncol=2, dimnames=list(list(), c("A", "B")))
    phenoData <- new("AnnotatedDataFrame", data=data.frame(x=1:2, row.names=c("A","B")))
    obj <- new("ExpressionSet", phenoData=phenoData, exprs=exprs)
    checkTrue(identical(sampleNames(obj), c("A","B")))

    exprs <- matrix(1:10, ncol=2)
    phenoData <- new("AnnotatedDataFrame", data=data.frame(x=1:2))
    obj <- new("ExpressionSet", phenoData=phenoData, exprs=exprs)
    checkTrue(identical(sampleNames(obj), c("1", "2")))

    exprs <- matrix(1:10, ncol=2)
    phenoData <- new("AnnotatedDataFrame", data=data.frame(x=1:2, row.names=c("A", "B")))
    obj <- new("ExpressionSet", phenoData=phenoData, exprs=exprs)
    checkTrue(identical(sampleNames(obj), c("A", "B")))
}

testValidation <- function() {
  for (mode in modes)
    lapply(names(allSubclasses), function(s) {
      obj <- helperNew(s, storage.mode=mode)
      checkTrue(validObject( obj ))
      phenoData(obj) <- new("AnnotatedDataFrame")
      checkEquals( validObject( obj, test = TRUE ), "sample numbers differ between assayData and phenoData")
    })
}

testNColSubclasses <- function() {
  for (mode in modes)
    lapply(nonEmptySubclasses, function( s ) {
      obj <- new( s, storage.mode = mode )
      checkTrue( ncol( obj ) == 0 )
    })
  for (mode in modes)
    lapply(names(allSubclasses), function( s ) {
      obj <- helperNew(s, storage.mode=mode)
      checkTrue( ncol( obj ) == .samples )
    })
}

testSubsetEsetSubclasses <- function() {
  subset <- function(s, ...) {
    obj <- helperNew(s, ...)
    obj1 <- obj[1:15, 1:5]
    checkTrue(validObject(obj1))
    checkTrue(all(dims(obj1)[1:2,] == c( 15, 5 )))
    ## original unmodified
    if (storageMode(obj)!="environment") {
      checkTrue(validObject(obj))
      checkTrue(all(dims(obj)[1:2,] == c(.features,.samples)))
      obj <- obj[1:15, 1:5]
      checkTrue(validObject(obj))
      checkTrue(all(dims(obj)[1:2,] == c( 15, 5 )))
    }
  }
  for (mode in modes )
    lapply(names(allSubclasses), subset, storage.mode=mode)
}

testAssayDataReplacement <- function() {
  lapply(names(allSubclasses), function( s ) {
    obj <- helperNew(s)
    assayData(obj) <- new.env()
  })
}

testAssayDataElement <- function() {
    checkObj <- function(obj) {
        checkTrue(identical(assayDataElementNames(obj), "exprs"))
        checkTrue(identical(assayDataElement(obj, "exprs"),
                            new("matrix",0,dimnames=list(list(),list()))[FALSE,FALSE,drop=FALSE]))
        m <- matrix(1:10, nrow=2)
        obj <- assayDataElementReplace(obj, "exprs", m)
        checkTrue(identical(assayDataElement(obj, "exprs"), m))
    }
    checkObj(new("ExpressionSet"))
    checkObj(new("ExpressionSet", storage.mode="list"))
    checkObj(new("ExpressionSet", storage.mode="environment"))
}

testOtherSlots <- function() {
  for (mode in modes)
    lapply(names(allSubclasses), function( s ) {
      obj <- helperNew(s, storage.mode=mode)
      experimentData( obj ) <- new( "MIAME" )
      annotation( obj ) <- new( "character" )
    })
}

testShow <- function() {                # just 'does it show'
    capture.output(
  for (mode in modes)
    lapply(names(allSubclasses), function( s ) {
      obj <- helperNew (s, storage.mode=mode)
      tryCatch(show(obj), error=function(e) checkTrue( 1==0 ))
    })
                   )
}

testSampleNames <- function() {
  nameCheck <- function( obj ) {
    checkTrue( all( sampleNames( obj ) == sampleNames( phenoData( obj ))))
    checkException( sampleNames( obj ) <- 1:(.features+1), silent=TRUE )
    sampleNames( obj ) <- letters[ 1:dim( obj )[[2]] %% 26 ]
    checkTrue(all(sampleNames(assayData(obj)) == sampleNames(obj)))
    checkTrue(validObject(obj), "original")

    orig <- sampleNames(obj)
    mod <- 1:length(orig)
    obj1 <- obj
    sampleNames(obj1) <- mod
    checkTrue(all(sampleNames(obj) == orig))
    if (storageMode(obj)!="environment")
      checkTrue(validObject(obj), "after modification")
    else
      checkEquals(validObject(obj, test = TRUE ), "sampleNames differ between assayData and phenoData")
    checkTrue(all(sampleNames(obj1)==mod))
    checkTrue(validObject(obj1))
    
    sampleNames(assayData(obj)) <- mod
    validObject(obj,test=TRUE)
    checkEquals(validObject( obj, test = TRUE ), "sampleNames differ between assayData and phenoData")
  }
  for (mode in modes)
    lapply(names(allSubclasses), function(s) nameCheck(helperNew(s, storage.mode=mode)))
}

testExprs <- function() {
  exprsCheck <- function(obj) {
    newExprs <- switch(class(obj),
                       SnpSet2 = {
                         ex <- new("array", dim = c(.features, .samples, 2))
                         ex[,,] <- runif(length(ex))
                         ex
                       },
                       SnpSet4 = {
                         ex <- new("array", dim = c(.features, .samples, 8))
                         ex[,,] <- runif(length(ex))
                         ex
                       },
                       SnpSetDetail = {
                         ex <- new("array", dim = c(.features, .samples, 2))
                         ex[,,] <- runif(length(ex))
                         ex
                       },
                       {
                         ex <- new("matrix", nr = .features, nc = .samples)
                         ex[,] <- runif(length(ex))
                         ex
                       })
    fNames <- featureNames(obj)
    sNames <- sampleNames(obj)
    oldExprs <- exprs(obj)
    exprs(obj) <- newExprs
    checkTrue( identical(exprs(obj), newExprs))
    if (storageMode(obj)!="environment")
      checkTrue(!identical(exprs(obj), oldExprs))
    sampleNames(assayData(obj)) <- sNames
    featureNames(assayData(obj)) <- fNames
    checkTrue(validObject(obj))
    obj
  }
  lapply(exprsEnabledSubclasses, function(s) exprsCheck(helperNew(s)))
  for (mode in modes)
    lapply(exprsEnabledSubclasses, function(s) exprsCheck(helperNew(s, storage.mode=mode)))
  ## copy semantics
  lapply(exprsEnabledSubclasses, function(s) {
    obj <- helperNew(s, storage.mode="lockedEnvironment")
    oldExprs <- exprs(obj)
    obj1 <- exprsCheck(obj)
    checkTrue( identical(exprs(obj), oldExprs))
    checkTrue(!identical(exprs(obj1),exprs(obj)))
    checkTrue(validObject(obj1))
  })
  lapply(exprsEnabledSubclasses, function(s) { # lockedEnvironment implicit
    obj <- helperNew(s)
    oldExprs <- exprs(obj)
    obj1 <- exprsCheck(obj)
    checkTrue( identical(exprs(obj), oldExprs))
    checkTrue(!identical(exprs(obj1),exprs(obj)))
    checkTrue(validObject(obj1))
  })
}

testCombineEsetSubclasses <- function() {
  combineEmpty <- function(s, ...) {
    if (s=="MultiSet") return(TRUE)
    obj1 <- new(s)
    obj2 <- new(s)
    obj <- combine(obj1,obj2)
    checkTrue(validObject(obj))
  }
  combineObj <- function(s, ...) {
    obj1 <- helperFillWithNoise(helperNew(s,...))
    obj2 <- helperFillWithNoise(helperNew(s,...))
    sampleNames(obj2) <- letters[1:length(sampleNames(obj2))%%26]
    obj <- combine(obj1,obj2)
    checkTrue(validObject(obj))
    checkTrue(all.equal(obj[,1:.samples],obj1))
    checkTrue(all.equal(obj[,(.samples+1):(2*.samples)],obj2))
  }
  for (mode in modes)
    lapply(names(allSubclasses), combineEmpty, storage.mode=mode)
  for (mode in modes)
    lapply(names(allSubclasses), combineObj, storage.mode=mode)
}

testSetAs <- function() {
  checkNewAndOld <- function(new, old) {
    checkTrue(identical(pData(new),pData(old)))
    checkTrue(all.equal(exprs(new),exprs(old),check.attributes=FALSE))
    checkTrue(identical(sampleNames(new),sampleNames(old)))
    checkTrue(identical(featureNames(new),geneNames(old)))
  }
  checkNewGolubMerge <- function(new,old) {
    checkTrue(identical(pData(new),pData(old)))
    checkTrue(all.equal(exprs(new),exprs(old),check.attributes=FALSE))
    checkTrue(identical(sampleNames(new),sampleNames(old)))
    checkTrue(identical(featureNames(new),featureNames(old)))
  }
  checkNewSampleEset <- function(new, old)   {
      checkTrue(identical(pData(new),pData(old)))
      checkTrue(all(names(assayData(old)) == names(assayData(new))))
      dups <- duplicated(old@reporterNames)
      mapply(function(x,y) checkTrue(all.equal(x[!dups,],y,check.attributes=FALSE)),
             assayData(old), assayData(new))
      checkTrue(identical(sampleNames(new),old@sampleNames))
      checkTrue(identical(featureNames(new),old@reporterNames[!dups]))
  }
  opts <- options()
  options(warn=-1)
  ## would like to be able to specify storage.mode, but how to specify?...
  data(sample.exprSet)
  suppressMessages(e <- as(sample.exprSet,"ExpressionSet"))
  checkNewAndOld(e,sample.exprSet)

  data(sample.exprSet.1)
  e1 <- as(sample.exprSet.1,"ExpressionSet")
  checkNewAndOld(e1,sample.exprSet.1)

  library(golubEsets)
  data(Golub_Merge)
  gm <- as(Golub_Merge,"ExpressionSet")
  checkNewGolubMerge(gm,Golub_Merge)
  pubMedIds(gm) = "10521349"
  checkNewGolubMerge(gm,Golub_Merge)

  data(sample.eSet)
  suppressMessages(es <- updateOldESet(sample.eSet, "SwirlSet"))
  checkNewSampleEset(es, sample.eSet)
  options(opts)
}

testFeatureNamesReplace <- function() {
    e <- new("ExpressionSet", exprs=matrix(nrow=5,ncol=2))
    featureNames(e) <- letters[5:1]
    checkIdentical(letters[5:1], featureNames(e))
    checkTrue(validObject(e))
}

testExtraSlotClassInitialize1 <- function() {
    ## pass if no error
    e <- new("ExtraSlotSet")
}

testExtraSlotClassInitialize2 <- function() {
    e <- new("ExtraSlotSet", R=new("matrix"),
             G=new("matrix"), extraSlot="hello",
             storage.mode="environment")
    checkEquals("hello", e@extraSlot)
    checkEquals(c("G", "R"),
                ls(assayData(e)))
    checkEquals("environment", storageMode(e))
}
