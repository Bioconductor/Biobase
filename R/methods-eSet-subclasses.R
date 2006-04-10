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

setReplaceMethod("exprs", c("ExpressionSet","matrix"),
                 function(object, value) assayDataElementReplace(object, "exprs", value))

## MultiExpressionSet

setMethod("initialize", "MultiExpressionSet",
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

updateOldESet <- function(from, ...) {  # to MultiExpressionSet
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
    warning("reporterInfo data not transfered to MultiExpressionSet object")
  ## new object
  object <- new("MultiExpressionSet",
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

setMethod("exprs", c("SnpESet"), function(object) assayDataElement(object, "call"))

setReplaceMethod("exprs", c("SnpESet", "matrix"), function(object, value) {
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

