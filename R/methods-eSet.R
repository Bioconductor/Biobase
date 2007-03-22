# ==========================================================================
# eSet Class Validator
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("initialize",
          signature(.Object="eSet"),
          function( .Object,
                   assayData,
                   phenoData = annotatedDataFrameFrom(assayData, byrow=FALSE),
                   featureData = annotatedDataFrameFrom(assayData, byrow=TRUE),
                   experimentData = new( "MIAME" ),
                   annotation = character(0),
                   ...) {
              ## NB: Arguments provided in '...' are used to initialize
              ## slots if possible (when called from some subclass).
              ## Otherwise, extra args in '...' are added as elements
              ## to assayData.  We do this to allow subclasses to
              ## rely on default contructor behavior for initializing
              ## slots.
              ##
              ## NB2: Extra args to the assayData constructor will
              ## be passed along as long as current class doesn't
              ## have a slot with a matching name.
              mySlots <- slotNames(.Object)
              dotArgs <- list(...)
              isSlot <- names(dotArgs) %in% mySlots
              if (missing(assayData))
                assayData <- do.call(assayDataNew, dotArgs[!isSlot], envir=parent.frame())
              else {
                  checkClass(assayData, "AssayData", class(.Object))
                  nms <-
                    if (storageMode(assayData)=="list") names(assayData)
                    else ls(assayData)
                  dupNames <- nms %in% names(dotArgs[!isSlot])
                  if (any(dupNames))
                    warning("initialize argument '",
                            paste(nms[dupNames], collapse="' "),
                            "' also present in 'assayData'; argument ignored")
              }
              if (is(phenoData,"phenoData")) {
                  warning("updating phenoData argument to 'AnnotatedDataFrame'", call.=FALSE)
                  phenoData <- as(phenoData,"AnnotatedDataFrame")
              } else if (!missing(phenoData)) {
                  checkClass(phenoData, "AnnotatedDataFrame", class(.Object))
              }
              if (!missing(featureData))
                checkClass(featureData, "AnnotatedDataFrame", class(.Object))
              ## coordinate sample names
              adSampleNames <- sampleNames(assayData)
              if (all(sapply(adSampleNames,is.null)))
                sampleNames(assayData) <- sampleNames(phenoData)
              ## where do feature names come from? assayData or featureData
              adFeatureNames <- featureNames(assayData)
              if (all(sapply(adFeatureNames, is.null)))
                featureNames(assayData) <- featureNames(featureData)
              ## create new instance from 'extra' dotArgs, and from instance
              for (s in names(dotArgs)[isSlot])
                   slot(.Object, s) <- dotArgs[[s]]
              callNextMethod(.Object,
                             assayData=assayData,
                             phenoData=phenoData, featureData=featureData,
                             experimentData=experimentData, annotation=annotation)
          })

updateOldESet <- function(from, toClass, ...) {  # to MultiExpressionSet
  metadata <- varMetadata(from)
  if (all(dim(metadata)==0)) {
    warning("replacing apparently empty varMetadata")
    metadata <- data.frame(numeric(ncol(pData(from))))[,FALSE]
  }
  if (!is.null(metadata[["varName"]])) {
    row.names(metadata) <- metadata[["varName"]]
    metadata[["varName"]] <- NULL
  } else if (!is.null(names(pData(from)))) {
    row.names(metadata) <- names(pData(from))
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
    warning("creating assayData colnames from phenoData sampleNames")
    sampleNames(assayData(from)) <- sampleNames(phenoData)
  }
  ## reporterNames
  if (length(from@reporterNames) == dim(from)[[1]]) {
      if (any(sapply(assayData(from),rownames)!=from@reporterNames))
        warning("creating assayData featureNames from reporterNames")
      featureNames(assayData(from)) <- from@reporterNames
  } else {
      warning("creating numeric assayData featureNames")
      featureNames(assayData(from)) <- 1:dim(from)[[1]]
  }
  if (sum(dups <- duplicated(featureNames(assayData(from))))>0) {
      warning("removing ", sum(dups), " duplicated featureNames")
      from@assayData <- lapply(from@assayData, function(elt) elt[!dups,])
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
                assayData = from@assayData,
                phenoData = phenoData,
                featureData = annotatedDataFrameFrom(from@assayData, byrow=TRUE),
                experimentData = updateObject(description),
                annotation = from@annotation)
  validObject(object)
  object
}

setAs("eSet", "ExpressionSet", function(from, to) updateOldESet(from, "ExpressionSet"))
setAs("eSet", "MultiSet", function(from, to) updateOldESet(from, "MultiSet"))

updateESetTo <- function(object, template, ..., verbose=FALSE) {
    if (verbose) message("updateESetTo(object = 'eSet' template = '", class(template), "')")
    ## cannot instantiate a 'virtual' class, so use accessor functions
    ## to update class components. Better than direct slot access?
    funcs <- c("assayData", "phenoData", "experimentData", "annotation")
    eval(parse(text=paste(funcs,"(template)<-",
                 "updateObject(", funcs, "(object), ..., verbose=verbose)")))
    result <- try(featureData(template) <- featureData(object), silent=TRUE)
    if (class(result)=="try-error")
      featureData(template) <- annotatedDataFrameFrom(assayData(object), byrow=TRUE)
    vers <- classVersion("eSet")
    classVersion(template)[names(vers)] <- vers # current class version, eSet & 'below' only
    template
}

setMethod("updateObject", signature(object="eSet"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'eSet')")
              if (isVersioned(object) && isCurrent(object)["eSet"])
                return(callNextMethod())
              ## storage.mode likely to be useful to update versioned classes, too
              storage.mode.final <- storageMode(object)
              storage.mode <-
                if (storage.mode.final == "lockedEnvironment") "environment"
                else storage.mode.final
              if (!isVersioned(object)) {
                  object <- updateESetTo(object, new(class(object), storage.mode=storage.mode), ..., verbose=verbose)
                  storageMode(object) <- storage.mode.final
              } else if (classVersion(object)["eSet"]=="1.0.0") {
                  ## added featureData slot; need to update phenoData
                  object <-
                    new(class(object),
                        assayData=updateObject(assayData(object), ..., verbose=verbose),
                        phenoData=new("AnnotatedDataFrame",
                          data=pData(object), varMetadata=varMetadata(object),
                          dimLabels=c("sampleNames", "sampleColums")),
                        featureData(object) <- annotatedDataFrameFrom(assayData(object), byrow=TRUE),
                        experimentData = updateObject(experimentData(object), ..., verbose=verbose),
                        annotation = annotation(object))
              }
              else stop("cannot update object of class '", class(object),
                        "', claiming to be eSet version '", as(classVersion(object)["eSet"], "character"), "'")
              object
          })

setMethod("updateObjectTo", signature(object="eSet", template="eSet"), updateESetTo)

setValidity("eSet", function( object ) {
    msg <- validMsg(NULL, isValidVersion(object, "eSet"))
    dims <- dims(object)
    if (!is.na(dims[[1]])) {
        ## assayData
        if (any( dims[1,] != dims[1,1]))
          msg <- validMsg(msg, "row numbers differ for assayData members")
        if (any(dims[2,] != dims[2,1]))
          msg <- validMsg(msg, "sample numbers differ for assayData members")
        msg <- validMsg(msg, assayDataValidMembers(assayData(object)))
        ## featureData
        if ( dims[1,1] != dim( featureData(object))[[1]] )
          msg <- validMsg(msg, "feature numbers differ between assayData and featureData")
        if (!all(featureNames(assayData(object))==featureNames(featureData(object))))
          msg <- validMsg(msg, "featureNames differ between assayData and featureData")
        ## phenoData
        if ( dims[2,1] != dim( phenoData( object ))[[1]] )
          msg <- validMsg(msg, "sample numbers differ between assayData and phenoData")
        if (!all(sampleNames(assayData(object))==sampleNames(phenoData(object))))
          msg <- validMsg(msg, "sampleNames differ between assayData and phenoData")
    }
    if (is.null(msg)) TRUE else msg
})

setMethod("preproc", "eSet", function(object)
       preproc(experimentData(object)))

setReplaceMethod("preproc", "eSet", function(object, value) {
        preproc(experimentData(object)) <- value
        object
})

setMethod("show", "eSet", function(object) {
  cat(class( object ), " (storageMode: ", storageMode(object), ")\n", sep="")
  adim <- dim(object)
  if (length(adim)>1)
  cat("assayData:",
      if (length(adim)>1) paste(adim[[1]], "features,", adim[[2]], "samples") else NULL,
      "\n")
  cat("  element names:", paste(assayDataElementNames(object), collapse=", "), "\n")
  cat("phenoData\n")
  show(phenoData(object))
  cat("featureData\n")
  show(featureData(object))
  cat("experimentData: use 'experimentData(object)'\n")
  pmids <- pubMedIds(object)
  if (length(pmids) > 0 && all(pmids != ""))
      cat("  pubMedIds:", paste(pmids, sep=", "), "\n")
  cat("Annotation ")
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
  object
})

setMethod("featureNames", "eSet", function(object) {
  featureNames(assayData(object))
})

setReplaceMethod("featureNames", "eSet", function(object, value) {
  featureNames(assayData(object)) <- value
  featureNames(featureData(object)) <- value
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

setMethod("[", "eSet", function(x, i, j, ..., drop = FALSE) {
  if (missing(drop)) drop <- FALSE
  if (missing(i) && missing(j)) {
      if (length(list(...))!=0)
        stop("specify genes or samples to subset; use '",
             substitute(x), "$", names(list(...))[[1]],
             "' to access phenoData variables")
      return(x)
  }
  if (!missing(j))
    phenoData(x) <- phenoData(x)[j,, ..., drop = drop]
  if (!missing(i))
    featureData(x) <- featureData(x)[i,,..., drop=drop]
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

setMethod("[[", "eSet", function(x, i, j, ...) phenoData(x)[[i]])

setReplaceMethod("[[", "eSet",
                 function(x, i, j, ..., value) {
                     phenoData(x)[[i]] <- value
                     x
                 })

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("assayData", "eSet", function(object) object@assayData)

setReplaceMethod("assayData", c( "eSet", "AssayData" ), function(object, value) {
  object@assayData <- value
  object
})

assayDataElementNames <- function(obj) {
    if (storageMode(obj) == "list") names(assayData(obj))
    else ls(assayData(obj))
}

assayDataElement <- function(obj, elt) assayData(obj)[[elt]]

assayDataElementReplace <- function(obj, elt, value) {
    storage.mode <- storageMode(obj)
    switch(storageMode(obj),
           "lockedEnvironment" = {
               aData <- copyEnv(assayData(obj))
               if (is.null(value)) rm(list=elt, envir=aData)
               else aData[[elt]] <- value
               assayDataEnvLock(aData)
               assayData(obj) <- aData
           },
           "environment" = {
               if (is.null(value)) rm(list=elt, envir=assayData(obj))
               else assayData(obj)[[elt]] <- value
           },
           list = assayData(obj)[[elt]] <- value)
    obj
}

setMethod("featureData",
          signature(object="eSet"),
          function(object) object@featureData)

setReplaceMethod("featureData",
                 signature(object="eSet", value="AnnotatedDataFrame"),
                 function(object, value) {
                     object@featureData <- value
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

setReplaceMethod("notes", signature(object="eSet", value="ANY"),
                 function(object, value) {
                     notes(experimentData(object)) <- value
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
    featureData(x) <- combine(featureData(x), featureData(y))
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
