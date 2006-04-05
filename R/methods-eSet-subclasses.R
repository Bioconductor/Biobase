## 
## ExpressionESet
##

## ExpressionSet

setMethod("initialize", "ExpressionSet",
          function(.Object,
                   phenoData = new("AnnotatedDataFrame"),
                   experimentData = new("MIAME"),
                   annotation = character(),
                   exprs = new("matrix"),
                   se.exprs = new("matrix"),
                   ... ) {
            if (!missing(exprs) && missing(se.exprs)) {
              se.exprs <- new("matrix", nrow=nrow(exprs), ncol=ncol(exprs))
              rownames(se.exprs) <- rownames(exprs)
            }
            callNextMethod(.Object,
                           assayData = assayDataNew(
                             exprs=exprs,
                             se.exprs=se.exprs,
                             ...),
                           phenoData = phenoData,
                           experimentData = experimentData,
                           annotation = annotation)
          })

setAs("exprSet", "ExpressionSet", function(from) {
  desc <- description(from)
  if (class(desc)!="MIAME") {
    warning("missing or mis-formed MIAME 'description' in original object; creating new, empty description")
    desc <- new("MIAME")
  }
  exprs <- assayData(from)
  dims <- dim(exprs)
  if (all(dim(from@se.exprs) == dims))
    se.exprs <- from@se.exprs
  else {
    warning("missing or mis-shaped 'se.exprs' in original object; creating new, empty, se.exprs")
    se.exprs <- new("matrix", nrow=dims[[1]], ncol=dims[[2]])
    rownames(se.exprs) <- rownames(exprs)
    colnames(se.exprs) <- colnames(exprs)
  }
  ## now if it is a MIAME instance that lacks pubMedIds, we do not
  ## just want to shuffle it across!
  pmid = try( pubMedIds(desc) , silent=TRUE) # should only error if slot nonexistent
  if ( inherits(pmid, "try-error") )
     desc = updateOldMiame(desc)
  new("ExpressionSet",
      phenoData=as(phenoData(from), "AnnotatedDataFrame"),
      experimentData=desc,
      annotation=annotation(from),
      exprs=exprs,
      se.exprs=se.exprs)
})

setValidity("ExpressionSet", function(object) {
  assayDataValidMembers(assayData(object), c("exprs"))
})

setMethod("exprs", "ExpressionSet", function(object) assayDataElement(object,"exprs"))

setReplaceMethod("exprs", "ExpressionSet",
                 function(object, value) assayDataElementReplace(object, "exprs", value))

## GeneralizedExpressionSet

setMethod("initialize", "GeneralizedExpressionSet",
          function(.Object,
                   phenoData = new("AnnotatedDataFrame"),
                   experimentData = new("MIAME"),
                   annotation = character(),
                   ... ) {
            callNextMethod(.Object,
                           assayData = assayDataNew(...),
                           phenoData = phenoData,
                           experimentData = experimentData,
                           annotation = annotation)
          })

updateOldESet <- function(from, ...) {  # to GeneralizedExpressionSet
  metadata <- varMetadata(from)
  if (!is.null(metadata[["varName"]])) {
    rownames(metadata) <- metadata[["varName"]]
    metadata[["varName"]] <- NULL
  } else if (!is.null(colnames(pData(from))))
    rownames(metadata) <- colnames(pData(from))
  if (!is.null(metadata[["varLabels"]])) {
    names(metadata)[names(metadata)=="varLabels"] <- "labelDescription"
    metadata[["labelDescription"]] <- as.character(metadata[["labelDescription"]])
  }
  ## data
  pData <- pData(from)
  phenoData <- new("AnnotatedDataFrame", data=pData, varMetadata=metadata)
  ## sampleNames
  if (any(sampleNames(assayData(from))!=sampleNames(phenoData))) {
    warning("setting assayData colnames to sampleNames")
    sampleNames(assayData(from)) <- sampleNames(phenoData)
  }
  ## reporterNames
  if (length(from@reporterNames == dim(from)[[1]])) {
    if (any(sapply(assayData(from),rownames)!=from@reporterNames))
      warning("setting assayData rownames to reporterNames")
    featureNames(assayData(from)) <- from@reporterNames
  }
  ## description
  description <- from@description
  if (is(description,"MIAME")) {
    if (length(from@notes)!=0) {
      warning("addding 'notes' to 'description'")
      description@other <- c(description@other@other,from@notes)
    }
    if (length(from@history)!=0) {
      warning("adding 'history' to 'description'")
      description@other <- c(description@other@other,from@history)
    }
  } else {
    warning("'description' is not of class MIAME; ignored")
    description <- NULL
  }
  ## reporterInfo
  if (any(dim(from@reporterInfo)!=0))
    warning("reporterInfo data not transfered to GeneralizedExpressionSet object")
  ## new object
  object <- new("GeneralizedExpressionSet",
                experimentData = description,
                annotation = from@annotation)
  assayData(object) <- from@assayData
  phenoData(object) <- phenoData
  validObject(object)
  object
}

## 
## SnpESet -- virtual base classs for SnpSet's
## 

setMethod("exprs", "SnpESet", function(object) assayDataElement(object, "call"))

setReplaceMethod("exprs", c("SnpESet"), function(object, value) {
  assayDataElementReplace(object, "call", value)
})

## SnpSet -- basic SNP set

setMethod("initialize", "SnpSet",
          function(.Object,
                   phenoData = new("AnnotatedDataFrame"),
                   experimentData = new("MIAME"),
                   annotation = character(),
                   call = new("matrix"),
                   callProbability = new("matrix"),
                   ... ) {
            callNextMethod(.Object,
                           assayData = assayDataNew(
                             call = call,
                             callProbability = callProbability,
                             ...),
                           phenoData = phenoData,
                           experimentData = experimentData,
                           annotation = annotation)
          })

setValidity("SnpSet", function(object) {
  assayDataValidMembers(assayData(object), c("call", "callProbability"))
})

## 
## SnpSetN -- virtual base class for SNP data stored in arrays
## 

setMethod("dim", "SnpSetN", function(x) {
  data <- assayData(x)
  d <-
    if (is(data, "environment")) dim(data[[ ls(data)[[1]] ]]) else dim(data[[1]])
  names(d) <- c("SNPs", "Samples", "Max. variants")
  d
})

setMethod("dims", "SnpSetN", function( object ) {
  dims <- callNextMethod()
  rownames(dims) <- c( "SNPs", "Samples", "Max. variants" )
  dims
})

setMethod("[", "SnpSetN", function( x, i, j, ..., drop = FALSE ) {
  if (length(list( ... )) == 0)
    callNextMethod(x, i, j, , drop = drop)
  else
    callNextMethod(x, i, j, ..., drop = drop)
})

## SnpSet2

setMethod("initialize", "SnpSet2",
          function(.Object,
                   phenoData = new("AnnotatedDataFrame"),
                   experimentData = new("MIAME"),
                   annotation = character(),
                   call = new("array", dim=c(0,0,2)), # 1 SNP (+ve [common], -ve [alt]) x 2 strand
                   callProbability = new("array", dim=c(0,0,2)),
                   ... ) {
            callNextMethod(.Object,
                           assayData = assayDataNew(
                             call = call,
                             callProbability = callProbability,
                             ...),
                           phenoData = phenoData,
                           experimentData = experimentData,
                           annotation = annotation )
          })

setValidity("SnpSet2", function(object) {
  msg <- assayDataValidMembers(assayData(object), c("call","callProbability"))
  if (dim(object)[[3]]!=2)
    msg <- paste(msg, "too many dimensions in exprs", sep="\n  ")
  msg
})

## SnpSet4

setMethod("initialize", "SnpSet4",
          function(.Object,
                   phenoData = new("AnnotatedDataFrame"),
                   experimentData = new("MIAME"),
                   annotation = character(),
                   call = new("array", dim=c(0,0,8)), # 4 SNP (A, T, G, C) x 2 strands
                   callProbability = new("array", dim=c(0,0,8)),
                   ... ) {
            callNextMethod(.Object,
                           assayData = assayDataNew(
                             call = call,
                             callProbability = callProbability,
                             ...),
                           phenoData = phenoData,
                           experimentData = experimentData,
                           annotation = annotation )
          })

setValidity("SnpSet4", function(object) {
  msg <- assayDataValidMembers(assayData(object), c("call","callProbability"))
  if (dim((object))[[3]]!=8)
    msg <- paste(msg, "too many dimensions in exprs", sep="\n  ")
  msg
})

## SnpSetDetail

setMethod("initialize", "SnpSetDetail",
          function(.Object,
                   phenoData = new("AnnotatedDataFrame"),
                   experimentData = new("MIAME"),
                   annotation = character(),
                   evidence = new("array", dim = c(0, 0, 2)), # SNP, sample, strand
                   level = new("array", dim = c(0, 0, 2)),
                   call = new("array", dim = c(0, 0, 2)),
                   callProbability = new("array", dim = c(0, 0, 2)),
                   copyNumber = new("array", dim = c(0, 0, 3)), # strand + total
                   ...) {
            callNextMethod(.Object,
                           assayData = assayDataNew(
                             evidence=evidence,
                             level=level,
                             call=call, callProbability=callProbability,
                             copyNumber=copyNumber, ...),
                           phenoData=phenoData,
                           experimentData=experimentData,
                           annotation=annotation)
          })

setValidity("SnpSetDetail", function( object ) {
  assayDataValidMembers(assayData(object),
                        c("evidence", "level", "call", "callProbability", "copyNumber"))
})
