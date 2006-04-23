# ==========================================================================
# eSet Class Validator
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("initialize",
          signature(.Object="eSet"),
          function( .Object,
                   assayData = assayDataNew(...),
                   phenoData = new("AnnotatedDataFrame"),
                   experimentData = new( "MIAME" ),
                   annotation = character(),
                   ...) {
            .Object@assayData <- assayData
            if (is(phenoData,"phenoData")) {
              warning("updating phenoData argument to 'AnnotatedDataFrame'", call.=FALSE)
              phenoData <- as(phenoData,"AnnotatedDataFrame")
            }
            .Object@phenoData <- phenoData
            .Object@experimentData <- experimentData
            .Object@annotation <- annotation
            ## coordinate sample names
            adSampleNames <- sampleNames(assayData)
            pdSampleNames <- sampleNames(phenoData)
            if (all(sapply(adSampleNames,is.null)) && is.null(pdSampleNames))
              sampleNames(.Object) <- 1:dim(assayData)[[2]]
            else if (all(sapply(adSampleNames,is.null)))
              sampleNames(.Object) <- pdSampleNames
            else if (is.null(pdSampleNames)) {
              nms <- assayDataElementNames(.Object)
              if (length(nms)==1 ||
                  (length(nms)>1 && all(adSampleNames[,1]==adSampleNames)))
                sampleNames(.Object) <- sampleNames(assayData)
              else
                stop("conflicting colnames in assayData elements")
            }
            validObject(.Object)
            .Object
})

updateOldESet <- function(from, toClass, ...) {  # to MultiExpressionSet
  metadata <- varMetadata(from)
  if (all(dim(metadata)==0)) {
    warning("replacing apparently empty varMetadata")
    metadata <- data.frame(numeric(ncol(pData(from))))[,FALSE]
  }
  if (!is.null(metadata[["varName"]])) {
    rownames(metadata) <- metadata[["varName"]]
    metadata[["varName"]] <- NULL
  } else if (!is.null(colnames(pData(from)))) {
    rownames(metadata) <- colnames(pData(from))
  }
  if (!is.null(metadata[["varLabels"]])) {
    names(metadata)[names(metadata)=="varLabels"] <- "labelDescription"
    metadata[["labelDescription"]] <- as.character(metadata[["labelDescription"]])
  }
  ## phenoData
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
      description@other <- c(description@other,from@notes)
    }
    if (length(from@history)!=0) {
      warning("adding 'history' to 'description'")
      description@other <- c(description@other,from@history)
    }
  } else {
    warning("'description' is not of class MIAME; ignored")
    description <- NULL
  }
  ## reporterInfo
  if (any(dim(from@reporterInfo)!=0))
    warning("reporterInfo data not transfered to '",toClass, "' object")
  ## new object
  object <- new(toClass,
                experimentData = description,
                annotation = from@annotation)
  assayData(object) <- from@assayData
  phenoData(object) <- phenoData
  validObject(object)
  object
}

setValidity("eSet", function( object ) {
  msg <- NULL
  if (!is(object, "eSet"))
    msg <- paste(msg, paste("cannot validate object of class", class( object )), sep = "\n  ")
  dims <- dims(object)
  if (!is.na(dims[[1]])) {
    if (any( dims[1,] != dims[1,1]))
      msg <- paste( msg, "row numbers differ for assayData members", sep = "\n  " )
    if (any(dims[2,] != dims[2,1]))
      msg <- paste( msg, "sample numbers differ for assayData members", sep = "\n  " )
    if ( dims[2,1] != dim( phenoData( object ))[[1]] )
      msg <- paste( msg, "sample numbers differ between assayData and phenoData", sep = "\n  " )
    if (!all(sampleNames(assayData(object))==sampleNames(object)))
      msg <- paste(msg, "sampleNames differ between assayData and phenoData", sep="\n  ")
    if (!all(featureNames(assayData(object))==featureNames(object)))
      msg <- paste(msg, "featureNames differ between assayData elements", sep="\n  ")
  } else if (dim(phenoData(object))[[1]] != 0 ) {
    msg <- paste( msg, "sample numbers differ between assayData and phenoData", sep = "\n  " )
  }
  ##   if (length(object) != 0 && length(reporterNames(object)) != d[1,1])
  ##     return("number of assayData rows different from number of reporterNames")
  if (is.null(msg)) TRUE else msg
})

setMethod("show", "eSet", function(object) {
  cat("Instance of", class( object ), "\n")
  cat("\nassayData\n")
  cat("  Storage mode:", storageMode(object), "\n")
  nms <- selectSome(featureNames(object))
  cat("  featureNames:", paste(nms, collapse=", "))
  if ((len <- length(featureNames(object))) > length(nms))
    cat(" (", len, " total)", sep="")
  cat("\n  Dimensions:\n")
  print(dims(object))
  cat("\nphenoData\n")
  show(phenoData(object))
  cat("\n")
  show(experimentData(object))
  cat("\nAnnotation ")
  show(annotation(object))
})

setMethod("storageMode", "eSet", function(object) storageMode(assayData(object)))

setReplaceMethod("storageMode", c("eSet", "character"), function(object, value) {
  storageMode(assayData(object)) <- value
  object
})

setMethod("sampleNames", signature(object="eSet"),
          function(object) sampleNames(phenoData(object)))

setReplaceMethod("sampleNames", c("eSet", "ANY"), function(object, value) {
  sampleNames(assayData(object)) <- value
  sampleNames(phenoData(object)) <- value
  validObject(object)
  object
})

setMethod("featureNames", "eSet", function(object) {
  featureNames(assayData(object))
})

setReplaceMethod("featureNames", "eSet", function(object, value) {
  featureNames(assayData(object)) <- value
  object
})

setMethod("varLabels", "eSet", function(object) varLabels(phenoData(object)))

setReplaceMethod("varLabels", "eSet", function(object, value) {
  varLabels(phenoData(object)) <- value
  object
})

setMethod("varMetadata", "eSet", function(object) varMetadata(phenoData(object)))

setReplaceMethod("varMetadata", c("eSet", "data.frame"), function(object, value) {
  varMetadata(phenoData(object)) <- value
  object
})

setMethod("dim", "eSet", function(x) assayDataDim(assayData(x)))

setMethod("dims", "eSet", function(object) assayDataDims(assayData(object)))

setMethod("ncol", "eSet", function(x) {
  d <- dim( x )
  if (length(d) == 1 && is.na(d)) d
  else d[[2]]
})

setMethod("[", "eSet", function(x, i, j, ..., drop = FALSE) {
  if (missing(drop)) drop <- FALSE
  if (missing(i) && missing(j)) return(x[,, ..., drop = drop])
  if (!missing(j))
    phenoData(x) <- phenoData(x)[j,, ..., drop = drop]
  ## assayData; implemented here to avoid function call
  orig <- assayData(x)
  storage.mode <- assayDataStorageMode(orig)
  assayData(x) <-
    switch(storage.mode,
           environment =,
           lockedEnvironment = {
             aData <- new.env(parent=emptyenv())
             if (missing(i))                     # j must be present
               for(nm in ls(orig)) aData[[nm]] <- orig[[nm]][, j, ..., drop = drop]
             else {                              # j may or may not be present
               if (missing(j))
                 for(nm in ls(orig)) aData[[nm]] <- orig[[nm]][i,, ..., drop = drop]
               else
                 for(nm in ls(orig)) aData[[nm]] <- orig[[nm]][i, j, ..., drop = drop]
             }
             if ("lockedEnvironment" == storage.mode) assayDataEnvLock(aData)
             aData
           },
           list = {
             if (missing(i))                     # j must be present
               lapply(orig, function(obj) obj[, j, ..., drop = drop])
             else {                              # j may or may not be present
               if (missing(j))
                 lapply(orig, function(obj) obj[i,, ..., drop = drop])
               else
                 lapply(orig, function(obj) obj[i, j, ..., drop = drop])
             }
           })
  x
})

setMethod("$", "eSet", function(x, name) phenoData(x)[[name]] )

setReplaceMethod("$", "eSet", function(x, name, value) {
  phenoData(x)[[name]] = value
  x
})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("assayData", "eSet", function(object) object@assayData)

setReplaceMethod("assayData", c( "eSet", "AssayData" ), function(object, value) {
  object@assayData <- value
  object
})

setMethod("phenoData", "eSet", function(object) object@phenoData)

setReplaceMethod("phenoData", c("eSet", "AnnotatedDataFrame"), function(object, value) {
  object@phenoData <- value
  object
})

setMethod("pData", "eSet", function(object) pData(phenoData(object)))

setReplaceMethod("pData", c("eSet","data.frame"), function(object, value) {
  pData(phenoData(object)) <- value
  object
})

setMethod("varMetadata", "eSet", function(object) varMetadata(phenoData(object)))

setReplaceMethod("varMetadata", c("eSet","data.frame"), function(object, value) {
  varMetadata(phenoData(object)) <- value
  object
})

setMethod("experimentData", signature(object="eSet"), function(object) object@experimentData)

setReplaceMethod("experimentData", signature(object="eSet",value="MIAME"),
                 function(object, value) {
                     object@experimentData <- value
                     object
                 })

setMethod("description", signature(object="eSet"),
          function(object) {
              experimentData(object)
          })

setReplaceMethod("description", signature(object="eSet", value="MIAME"),
                 function(object, value) {
                     object@experimentData <- value
                     object
                 })

setMethod("notes", signature(object="eSet"),
          function(object) otherInfo(experimentData(object)))

setReplaceMethod("notes", signature(object="eSet", value="list"),
                 function(object, value) {
                     otherInfo(experimentData(object)) <- value
                     object
                 })

setMethod("pubMedIds", signature(object="eSet"),
          function(object) pubMedIds(experimentData(object)))

setReplaceMethod("pubMedIds", signature("eSet","character"), function(object, value) {
  pubMedIds(experimentData(object)) <- value
  object
})

setMethod("abstract", "eSet", function(object) abstract(experimentData(object)))

setMethod("annotation", "eSet", definition = function(object) object@annotation)

setReplaceMethod("annotation", signature(object="eSet", value="character"),
                 function(object, value) {
                     object@annotation <- value
                     object
                 })

setMethod("combine", c("eSet", "eSet"), function(x, y, ...) {
  if (class(x) != class(y))
    stop(paste("objects must be the same class, but are ",
               class(x), ", ", class(y), sep=""))
  if (any(annotation(x) != annotation(y)))
    stop("objects have different annotations: ",
         annotation(x), ", ", annotation(y))
  assayData(x) <- combine(assayData(x), assayData(y))
  phenoData(x) <- combine(phenoData(x), phenoData(y))
  experimentData(x) <- combine(experimentData(x),experimentData(y))
  ## annotation -- constant
  x
})

## 
## Deprecated
## 

setMethod("reporterNames", "eSet", function(object) {
  .Deprecated("featureNames", "Biobase")
  featureNames(object)
})

setReplaceMethod("reporterNames", c("eSet", "character"), function(object, value) {
  .Deprecated("featureNames<-", "Biobase")
  featureNames(object) <- value
})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# VC:

#this check should be done at
# construction time -- if creator doesn't have it right,
# then no eSet is made.  any manipulations should guaranteed
# to preserve name validity
#
##check to see if the vector x is the right length to
##replace the sample names in eSet
##not sure if this should be exported

#eSetValidNames = function(eSet, x) {
#  if( !is.character(x) ) return(FALSE)
#  lenx = length(x)
#  if(nrow(pData(eSet)) != lenx ) return(FALSE)
#  nc = ncol(eSet)
#  if(any(nc != lenx)) return(FALSE)
#  TRUE
#}

##reset the eList col names - not for export

# VC commented this out -- wants to handle names
# directly as slots as much as possible
#seteListColNames = function(eList, names) {
#  if(is.environment(eList@eList) )
#    for( j in ls(eList@eList) )
#       dimnames(j)[[2]] = names
#  else if (is.list(eList@eList) )
#    for( j in eList@eList )
#       dimnames(j)[[2]] = names
#  else
#    stop("invalid eList")
#  eList
#}
#
###should we have column names on the elements of eList
###and if so how do we control their values
#setReplaceMethod("sampleNames", "eSet",
#  function(object, value) {
#     if( !is.character(value) )
#       stop("replacement names must be character strings")
#     if( !eSetValidNames(object, value) )
#       stop("wrong number of sample names")
#     object@sampleNames = value
#     object@eList = seteListColNames(object@eList, names)
#     object
#})

# VC -- leave this stuff to another factor, related to varMetadata
#if( !isGeneric("eMetadata<-") )
#    setGeneric("eMetadata<-", function(object, value)
#               standardGeneric("eMetadata<-"))
#
#setReplaceMethod("eMetadata", c("eSet", "data.frame"),
#     function(object, value) {
#        object@eList@eMetadata <- value
#        object
#     })

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("eList", "eSet", function(object) {
  .Deprecated("assayData", "Biobase")
  assayData(object)
})
setReplaceMethod("eList", c("eSet", "AssayData"), function(object, value) {
  .Deprecated("assayData<-", "Biobase")
  assayData(object) <- value
})
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## An extractor for named experimental data
## FIXME: how much do we want to do to ensure that these are matrices?
## this FIXME is still valid in the new design!  I am not sure we
## want them to be required to be matrices.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("getExpData", c("eSet", "character"), function(object, name) {
  .Deprecated("assayData", "Biobase")
  assayData(object)[[name]]
})
