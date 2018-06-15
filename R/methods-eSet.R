# ==========================================================================
# eSet Class Validator
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("initialize", signature(.Object="eSet"),
    function(.Object, assayData,
        phenoData=annotatedDataFrameFrom(assayData, byrow=FALSE),
        featureData=annotatedDataFrameFrom(assayData, byrow=TRUE),
        experimentData=MIAME(), annotation=character(0),
        protocolData=phenoData[,integer(0)], ...)
{
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
            warning("initialize argument(s) '",
                    paste(nms[dupNames], collapse="' '"),
                    "' also present in 'assayData'; argument(s) ignored")
    }
    if (!missing(phenoData))
        checkClass(phenoData, "AnnotatedDataFrame", class(.Object))
    dimLabels(phenoData) <- c("sampleNames", "sampleColumns")
    if (!missing(featureData))
        checkClass(featureData, "AnnotatedDataFrame", class(.Object))
    dimLabels(featureData) <- c("featureNames", "featureColumns")
    ## create the protocolData, if necessary
    if (!missing(protocolData)) {
        checkClass(protocolData, "AnnotatedDataFrame", class(.Object))
        dimLabels(protocolData) <- c("sampleNames", "sampleColumns")
    }
    ## coordinate sample names
    adSampleNames <- sampleNames(assayData)
    if (all(sapply(adSampleNames, is.null)))
        sampleNames(assayData) <- sampleNames(phenoData)
    pdSampleNames <- sampleNames(protocolData)
    if (all(sapply(pdSampleNames, is.null)))
        sampleNames(protocolData) <- sampleNames(phenoData)
    ## where do feature names come from? assayData or featureData
    adFeatureNames <- featureNames(assayData)
    if (all(sapply(adFeatureNames, is.null)))
        featureNames(assayData) <- featureNames(featureData)
    ## create new instance from 'extra' dotArgs, and from instance
    for (s in names(dotArgs)[isSlot])
        slot(.Object, s) <- dotArgs[[s]]
    callNextMethod(.Object, assayData=assayData, phenoData=phenoData,
                   featureData=featureData, experimentData=experimentData,
                   annotation=annotation, protocolData=protocolData)
})

updateOldESet <- function(from, toClass, ...) {  # to MultiExpressionSet
  from <- asS4(from)
  ophenoData <- asS4(phenoData(from))
  metadata <- ophenoData@varMetadata
  if (all(dim(metadata)==0)) {
    warning("replacing apparently empty varMetadata")
    metadata <- data.frame(numeric(ncol(ophenoData@pData)))[,FALSE]
  }
  if (!is.null(metadata[["varName"]])) {
    row.names(metadata) <- metadata[["varName"]]
    metadata[["varName"]] <- NULL
  } else if (!is.null(names(ophenoData@pData))) {
    row.names(metadata) <- names(ophenoData@pData)
  }
  if (!is.null(metadata[["varLabels"]])) {
    names(metadata)[names(metadata)=="varLabels"] <- "labelDescription"
    metadata[["labelDescription"]] <- as.character(metadata[["labelDescription"]])
  }
  ## phenoData
  pData <- ophenoData@pData
  phenoData <- AnnotatedDataFrame(data=pData, varMetadata=metadata)
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
              object <- asS4(object)
              if (isVersioned(object) && isCurrent(object)["eSet"])
                return(callNextMethod())
              ## storage.mode likely to be useful to update versioned classes, too
              storage.mode.final <- storageMode(object)
              storage.mode <-
                if (storage.mode.final == "lockedEnvironment") "environment"
                else storage.mode.final
              additionalSlots <- setdiff(slotNames(class(object)), slotNames("eSet"))
              names(additionalSlots) <- additionalSlots
              if (!isVersioned(object)) {
                object <- updateESetTo(object, new(class(object), storage.mode=storage.mode), ..., verbose=verbose)
                storageMode(object) <- storage.mode.final
              } else if (classVersion(object)["eSet"]=="1.0.0") {
                ## added featureData slot; need to update phenoData
                object <-
                  do.call(new,
                          c(list(class(object),
                                 assayData = updateObject(assayData(object), ..., verbose=verbose),
                                 phenoData = AnnotatedDataFrame(data=pData(object),
                                                 varMetadata=varMetadata(object)),
                                 featureData = annotatedDataFrameFrom(assayData(object), byrow=TRUE),
                                 experimentData = updateObject(experimentData(object), ..., verbose=verbose),
                                 annotation = annotation(object)),
                            lapply(additionalSlots, function(x) slot(object, x))))
              } else if (classVersion(object)["eSet"]=="1.1.0") {
                ## added scanDates slot
                object <-
                  do.call(new,
                          c(list(class(object),
                                 assayData = updateObject(assayData(object)),
                                 phenoData = updateObject(phenoData(object)),
                                 featureData = updateObject(featureData(object)),
                                 experimentData = updateObject(experimentData(object)),
                                 annotation = annotation(object)),
                            lapply(additionalSlots, function(x) slot(object, x))))
              } else if (classVersion(object)["eSet"]=="1.2.0") {
                ## added protocolData slot, removed scanDates slot
                scanDates <- object@scanDates
                protocolData <- phenoData(object)[,integer(0)]
                if (length(scanDates) > 0) {
                  protocolData[["ScanDate"]] <- scanDates
                }
                object <-
                  do.call(new,
                          c(list(class(object),
                                 assayData = updateObject(assayData(object)),
                                 phenoData = updateObject(phenoData(object)),
                                 featureData = updateObject(featureData(object)),
                                 experimentData = updateObject(experimentData(object)),
                                 annotation = annotation(object),
                                 protocolData = protocolData),
                            lapply(additionalSlots,
                                   function(x) updateObject(slot(object, x)))))
              } else {
                stop("cannot update object of class '", class(object),
                     "', claiming to be eSet version '",
                     as(classVersion(object)["eSet"], "character"), "'")
              }
              object
          })

setMethod("updateObjectTo", signature(object="eSet", template="eSet"), updateESetTo)

setValidity("eSet", function(object) {
    msg <- validMsg(NULL, isValidVersion(object, "eSet"))
    dims <- dims(object)
    if (ncol(dims) > 0) {
        ## assayData
        msg <- validMsg(msg, assayDataValidMembers(assayData(object)))
        if (any(dims[1,] != dims[1,1]))
          msg <- validMsg(msg, "row numbers differ for assayData members")
        if (any(dims[2,] != dims[2,1]))
          msg <- validMsg(msg, "sample numbers differ for assayData members")
        ## featureData
        if (dims[1,1] != dim( featureData(object))[[1]])
          msg <- validMsg(msg, "feature numbers differ between assayData and featureData")
        if (!identical(featureNames(assayData(object)), featureNames(featureData(object))))
          msg <- validMsg(msg, "featureNames differ between assayData and featureData")
        ## phenoData
        if (dims[2,1] != dim(phenoData(object))[[1]])
          msg <- validMsg(msg, "sample numbers differ between assayData and phenoData")
        if (!identical(sampleNames(assayData(object)), sampleNames(phenoData(object))))
          msg <- validMsg(msg, "sampleNames differ between assayData and phenoData")
        ## protocolData
        if (dim(phenoData(object))[[1]] != dim(protocolData(object))[[1]])
          msg <- validMsg(msg, "sample numbers differ between phenoData and protocolData")
        if (!identical(sampleNames(phenoData(object)), sampleNames(protocolData(object))))
          msg <- validMsg(msg, "sampleNames differ between phenoData and protocolData")
    }
    if (is.null(msg)) TRUE else msg
})

setMethod("preproc", "eSet", function(object) preproc(experimentData(object)))

setReplaceMethod("preproc",
                 signature=signature(object="eSet"),
                 function(object, value) {
                     ed <- experimentData(object)
                     preproc(ed) <- value
                     object@experimentData <- ed
                     object
                 })

setMethod("show",
          signature=signature(object="eSet"),
          function(object) {
              cat(class(object), " (storageMode: ",
                  storageMode(object), ")\n", sep="")
              adim <- dim(object)
              if (length(adim)>1)
                  cat("assayData:",
                      if (length(adim)>1)
                      paste(adim[[1]], "features,",
                            adim[[2]], "samples") else NULL,
                      "\n")
              cat("  element names:",
                  paste(assayDataElementNames(object), collapse=", "), "\n")
              .showAnnotatedDataFrame(protocolData(object),
                                      labels=list(object="protocolData"))
              .showAnnotatedDataFrame(phenoData(object),
                                      labels=list(object="phenoData"))
              .showAnnotatedDataFrame(featureData(object),
                                      labels=list(
                                        object="featureData",
                                        sampleNames="featureNames",
                                        varLabels="fvarLabels",
                                        varMetadata="fvarMetadata"))
              cat("experimentData: use 'experimentData(object)'\n")
              pmids <- pubMedIds(object)
              if (length(pmids) > 0 && all(pmids != ""))
                  cat("  pubMedIds:", paste(pmids, sep=", "), "\n")
              cat("Annotation:", annotation(object), "\n")
          })

setMethod("storageMode", "eSet", function(object) storageMode(assayData(object)))

setReplaceMethod("storageMode",
                 signature=signature(
                   object="eSet", value="character"),
                 function(object, value) {
                     ad <- assayData(object)
                     storageMode(ad) <- value
                     object@assayData <- ad
                     object
                 })

setMethod("sampleNames",
          signature(object="eSet"),
          function(object) sampleNames(phenoData(object)))

setReplaceMethod("sampleNames",
                 signature=signature(object="eSet", value="ANY"),
                 function(object, value) {
                     pd <- phenoData(object)
                     sampleNames(pd) <- value
                     ad <- assayData(object)
                     sampleNames(ad) <- value
                     prd <- protocolData(object)
                     if (nrow(prd) == 0) {
                         prd <- pd[,integer(0)]
                     } else {
                         sampleNames(prd) <- value
                     }
                     object@phenoData <- pd
                     object@protocolData <- prd
                     unsafeSetSlot(object, "assayData", ad)
                 })

setMethod("featureNames",
          signature=signature(object="eSet"),
          function(object) featureNames(assayData(object)))

setReplaceMethod("featureNames",
                 signature=signature(object="eSet", value="ANY"),
                 function(object, value) {
                     fd <- featureData(object)
                     featureNames(fd) <- value
                     ad <- assayData(object)
                     featureNames(ad) <- value
                     object@featureData <- fd
                     unsafeSetSlot(object, "assayData", ad)
                 })

setMethod("dimnames", "eSet", function(x) {
    list(featureNames(x), sampleNames(x))
})

setReplaceMethod("dimnames", "eSet", function(x, value) {
    featureNames(x) <- value[[1]]
    sampleNames(x) <- value[[2]]
    x
})

setMethod("dim", "eSet", function(x) assayDataDim(assayData(x)))

setMethod("dims", "eSet", function(x) assayDataDims(assayData(x)))

setMethod("[", "eSet", function(x, i, j, ..., drop = FALSE) {
  if (missing(drop))
    drop <- FALSE
  if (missing(i) && missing(j)) {
      if (!missing(...))
        stop("specify genes or samples to subset; use '",
             substitute(x), "$", names(list(...))[[1]],
             "' to access phenoData variables")
      return(x)
  }
  if (!isVersioned(x) || !isCurrent(x)["eSet"])
    x <- updateObject(x)
  if (!missing(j)) {
    phenoData(x) <- phenoData(x)[j,, ..., drop = drop]
    protocolData(x) <- protocolData(x)[j,, ..., drop = drop]
  }
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

## $ stops dispatching ?!
##setMethod("$", "eSet", function(x, name) `$`(phenoData(x), name))
setMethod("$", "eSet", function(x, name) {
    eval(substitute(phenoData(x)$NAME_ARG, list(NAME_ARG=name)))
})

.DollarNames.eSet <- function(x, pattern = "")
    grep(pattern, names(pData(x)), value=TRUE)

setReplaceMethod("$", "eSet", function(x, name, value) {
  phenoData(x)[[name]] = value
  x
})

setMethod("[[", "eSet", function(x, i, j, ...) phenoData(x)[[i]])

setReplaceMethod("[[", "eSet",
                 function(x, i, j, ..., value) {
                     phenoData(x)[[i, ...]] <- value
                     x
                 })

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("assayData", "eSet", function(object) object@assayData)

setReplaceMethod("assayData",
                 signature=signature(
                   object="eSet",
                   value="AssayData"),
                 function(object, value) {
                     object@assayData <- value
                     object
                 })

assayDataElementNames <- function(object) {
    if (storageMode(object) == "list") names(assayData(object))
    else ls(assayData(object))
}

assayDataElement <- function(object, elt) assayData(object)[[elt]]

.validate_assayDataElementReplace <- function(obj, value) {
    if (!is.null(value)) {
        dimvalue <- dim(value)
        dimobj <- dim(obj)[seq_along(dimvalue)]
        if (!isTRUE(all.equal(unname(dimvalue), unname(dimobj))))
            stop("object and replacement value have different dimensions")
    }
    
    if (!is.null(value)) {
        if (!is.null(dimnames(value))) {
            ## validate and harmonize dimnames
            vd <- Map(function(od, vd) {
                if (is.null(vd))
                    ## update vd to contain indexes into matrix
                    od <- seq_along(od)
                else if (!setequal(od, vd))
                    stop("object and replacement value dimnames differ")
                od
            }, dimnames(obj), dimnames(value))
            ## re-arrange value to have dimnames in same order as obj
            value <- do.call(`[`, c(list(value), vd, drop=FALSE))
        }
        dimnames(value) <- dimnames(obj)
    }
    value
}

assayDataElementReplace <- function(obj, elt, value, validate=TRUE) {
    ## 'validate' added later, needs to be last for position matching
    if (validate)
        value <- .validate_assayDataElementReplace(obj, value)

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

`assayDataElement<-` <- function(obj, elt, ..., value)
    ## 'value' is always the last argument, but needs to be 3rd for
    ## assayDataElementReplace
    assayDataElementReplace(obj, elt, value, ...)

setMethod("phenoData", "eSet", function(object) object@phenoData)

setReplaceMethod("phenoData",
                 signature=signature(
                   object="eSet",
                   value="AnnotatedDataFrame"),
                 function(object, value) {
                     object@phenoData <- value
                     if (nrow(protocolData(object)) == 0) {
                         protocolData(object) <- value[,integer(0)]
                     }
                     object
                 })

setMethod("pData", "eSet", function(object) pData(phenoData(object)))

setReplaceMethod("pData",
                 signature=signature(
                   object="eSet",
                   value="data.frame"),
                 function(object, value) {
                     pd <- phenoData(object)
                     pData(pd) <- value
                     phenoData(object) <- pd
                     object
                 })

setMethod("varMetadata",
          signature=signature(object="eSet"),
          function(object) varMetadata(phenoData(object)))

setReplaceMethod("varMetadata",
                 signature=signature(
                   object="eSet",
                   value="data.frame"),
                 function(object, value) {
                     pd <- phenoData(object)
                     varMetadata(pd) <- value
                     object@phenoData <- pd
                     object
                 })

setMethod("varLabels",
          signature=signature(object="eSet"),
          function(object) varLabels(phenoData(object)))

setReplaceMethod("varLabels",
                 signature=signature(
                   object="eSet",
                   value="ANY"),
                 function(object, value) {
                     pd <- phenoData(object)
                     varLabels(pd) <- value
                     object@phenoData <- pd
                     object
                 })

setMethod("featureData",
          signature(object="eSet"),
          function(object) object@featureData)

setReplaceMethod("featureData",
                 signature=signature(
                   object="eSet",
                   value="AnnotatedDataFrame"),
                 function(object, value) {
                     object@featureData <- value
                     object
                 })

setMethod("fData",
          signature=signature(object="eSet"),
          function(object) pData(featureData(object)))

setReplaceMethod("fData",
                 signature=signature(
                   object="eSet",
                   value="data.frame"),
                 function(object, value) {
                     fd <- featureData(object)
                     pData(fd) <- value
                     object@featureData <- fd
                     object
                 })

setMethod("fvarMetadata",
          signature=signature(object="eSet"),
          function(object) varMetadata(featureData(object)))

setReplaceMethod("fvarMetadata",
                 signature=signature(
                   object="eSet",
                   value="data.frame"),
                 function(object, value) {
                     fd <- featureData(object)
                     varMetadata(fd) <- value
                     object@featureData <- fd
                     object
                 })

setMethod("fvarLabels",
          signature=signature(object="eSet"),
          function(object) varLabels(featureData(object)))

setReplaceMethod("fvarLabels",
                 signature=signature(
                   object="eSet",
                   value="ANY"),
                 function(object, value) {
                     pd <- featureData(object)
                     varLabels(pd) <- value
                     object@featureData <- pd
                     object
                 })

setMethod("experimentData", signature(object="eSet"), function(object) object@experimentData)

setReplaceMethod("experimentData",
                 signature=signature(
                   object="eSet",
                   value="MIAME"),
                 function(object, value) {
                     object@experimentData <- value
                     object
                 })

setMethod("description", signature(object="eSet"),
          function(object, ...) {
              experimentData(object)
          })

setReplaceMethod("description",
                 signature=signature(
                   object="eSet",
                   value="MIAME"),
                 function(object, value) {
                     object@experimentData <- value
                     object
                 })

setMethod("notes", signature(object="eSet"),
          function(object) otherInfo(experimentData(object)))

setReplaceMethod("notes",
                 signature=signature(
                   object="eSet",
                   value="ANY"),
                 function(object, value) {
                     ed <- experimentData(object)
                     notes(ed) <- value
                     object@experimentData <- ed
                     object
                 })

setMethod("pubMedIds", signature(object="eSet"),
          function(object) pubMedIds(experimentData(object)))

setReplaceMethod("pubMedIds",
                 signature=signature(
                   object="eSet",
                   value="character"),
                 function(object, value) {
                     ed <- experimentData(object)
                     pubMedIds(ed) <- value
                     object@experimentData <- ed
                     object
                 })

setMethod("abstract", "eSet", function(object) abstract(experimentData(object)))

setMethod("annotation", "eSet", definition = function(object) object@annotation)

setReplaceMethod("annotation",
                 signature=signature(
                   object="eSet",
                   value="character"),
                 function(object, value) {
                     object@annotation <- value
                     object
                 })

setMethod("protocolData", "eSet",
          function(object) {
              tryCatch(object@protocolData,
                       error = function(x) {
                         phenoData(object)[,integer(0)]
                       })
          })

setReplaceMethod("protocolData",
                 signature=signature(
                   object="eSet",
                   value="AnnotatedDataFrame"),
                 function(object, value) {
                     if (class(try(object@protocolData, silent = TRUE)) == "try-error")
                       object <- updateObject(object)
                     object@protocolData <- value
                     object
                 })

setMethod("combine",
          signature=signature(
            x="eSet", y="eSet"),
          function(x, y, ...) {
              if (class(x) != class(y))
                stop("objects must be the same class, but are '",
                     class(x), "', '", class(y), "'")
              if (any(annotation(x) != annotation(y)))
                stop("objects have different annotations: ",
                     annotation(x), ", ", annotation(y))
              if (!isCurrent(x)[["eSet"]])
                  x <- updateObject(x)
              assayData(x) <- combine(assayData(x), assayData(y))
              phenoData(x) <- combine(phenoData(x), phenoData(y))
              featureData(x) <- combine(featureData(x), featureData(y))
              experimentData(x) <- combine(experimentData(x),experimentData(y))
              protocolData(x) <- combine(protocolData(x), protocolData(y))
              ## annotation -- constant
              x
          })

