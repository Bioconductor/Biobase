NChannelSet <- .NChannelSet

.init_NChannelSet <-              # copy constructor, validation
    selectMethod(initialize, "ANY")

setMethod("initialize", "NChannelSet",
    function(.Object, assayData, phenoData, ...)
{
    mySlots <- slotNames(.Object)
    dotArgs <- list(...)
    isSlot <- names(dotArgs) %in% mySlots
    if (missing(assayData)) {
        assayData <- do.call(assayDataNew, dotArgs[!isSlot],
                             envir=parent.frame())
    }
    if (missing(phenoData)) {
        phenoData <- annotatedDataFrameFrom(assayData, byrow=FALSE)
    }
    if (is.null(varMetadata(phenoData)[["channel"]])) {
        varMetadata(phenoData)[["channel"]] <- 
            factor(rep("_ALL_", nrow(varMetadata(phenoData))),
                   levels=c(assayDataElementNames(assayData), "_ALL_"))
    }
    ## ensure sample names OK -- all assayData with names;
    ## phenoData with correct names from assayData
    nms <- if (storageMode(assayData) == "list")
        names(assayData)
    else
        ls(assayData)
    assaySampleNames <- vector("list", length(nms))
    names(assaySampleNames) <- nms
    for (nm in nms) {
        cnames <- colnames(assayData[[nm]])
        assaySampleNames[[nm]] <- if (is.null(cnames)) {
            sampleNames(phenoData)
        } else cnames
    }

    sampleNames(assayData) <- assaySampleNames
    sampleNames(phenoData) <- sampleNames(assayData)
    do.call(callNextMethod,
            c(.Object,
              assayData = assayData, phenoData = phenoData,
              dotArgs[isSlot]))
})

.invalid_NChannelAssayNames <- function(object)
{
    phenoChannels <- levels(varMetadata(object)[["channel"]])
    assayChannels <- c("_ALL_", assayDataElementNames(object))
    setdiff(union(assayChannels, phenoChannels),
            intersect(assayChannels, phenoChannels))
}

setValidity("NChannelSet",
    function(object) 
{
    msg <- validMsg(NULL, isValidVersion(object, "NChannelSet"))
    if (!"channel" %in% names(varMetadata(object))) {
        txt <- "\n  'NChannelSet' varMetadata must have a 'channel' column"
        msg <- validMsg(msg, txt)
    } else {
        channel <-
            varMetadata(object)[["channel"]]
        if (!is(channel, "factor")) {
            txt <- paste("\n  'NChannelSet' varMetadata column 'channel'",
                         "must be class 'factor'")
            msg <- validMsg(msg, txt)
        } else if (0 < length(levels(channel))) {
            if (length(.invalid_NChannelAssayNames(object))) {
                txt <- 'NChannelSet levels(varMetadata(object)$channel) /
                        assayDataElementNames() mismatch; see
                        ?"channelNames<-,NChannelSet,character-method"'
                txt <- paste(strwrap(c("\n", txt), exdent=2), collapse="\n  ")
                msg <- validMsg(msg, txt)
            }
        }
        if (!("_ALL_" %in% levels(channel))) {
            txt <- paste("\n  'NChannelSet' varMetadata 'channel'",
                         "requires '_ALL_' as a level")
            msg <- validMsg(msg, txt)
        }
    }
    if (is.null(msg)) TRUE else msg
})

.assayDataGets_NChannelSet <-
    function(object, value)
{
    phenoData <- phenoData(object)
    ## update channel names, making some attempt to preserve order
    from <- channelNames(object)
    to <- assayDataElementNames(value)
    lvls <- c(from[from %in% to], to[!to %in% from], "_ALL_")
    varMetadata(phenoData)$channel <- 
        factor(as.character(varMetadata(phenoData)$channel), levels=lvls)

    .init_NChannelSet(object, assayData=value, phenoData=phenoData)
}

setReplaceMethod("assayData", c("NChannelSet", "environment"),
    .assayDataGets_NChannelSet)

setReplaceMethod("assayData", c("NChannelSet", "list"),
    .assayDataGets_NChannelSet)

setMethod("channelNames", "NChannelSet", function(object, ...) {
    nms <- assayDataElementNames(object)
    lvls <- levels(varMetadata(object)$channel)
    if (all(nms %in% lvls))
        ## FIXME: this accomodates invalid (previous) NChannelSet instances
        ## order to match channelNames
        nms <- lvls[lvls %in% nms]
    nms
})

setGeneric("channelNames<-",
    function(object, ..., value) standardGeneric("channelNames<-"))

setReplaceMethod("channelNames", c("NChannelSet", "character"),
    function(object, ..., value)
{
    if (!is.null(names(value))) {
        ## re-name & re-order
        channelNames(object) <- as.list(value)
    } else {
        ## re-order
        if (!all(sort(value) == sort(channelNames(object))))
            stop("'value' elements must include all channelNames()")
        varMetadata(object)$channel <-
            factor(varMetadata(object)$channel, levels=c(value, "_ALL_"))
        validObject(object)
    }
    object
})

setReplaceMethod("channelNames", c("NChannelSet", "list"),
    function(object, ..., value)
{
    from <- unlist(value, use.names=FALSE)
    if (!all(sort(from) == sort(channelNames(object))))
       stop("'value' elements must include all channelNames()")
    to <- names(value)
    if (any(duplicated(to)))
        stop("duplicated channelNames are not allowed")

    assayData <- assayData(object)
    levels(varMetadata(object)$channel) <- c(value, list("_ALL_" = "_ALL"))
    if (is.list(assayData)) {
        idx <- match(from, names(assayData))
        names(assayData)[idx] <- to
    } else {
        env <- new.env(parent=emptyenv())
        for (i in seq_along(value))
            env[[ to[i] ]] = assayData[[ from[i] ]]
        if (storageMode(object) == "lockedEnvironment")
            assayDataEnvLock(env)
        assayData <- env
    }
    assayData(object) <- assayData
    object
})

setMethod("channel",
          signature = signature(
            object = "NChannelSet",
            name = "character"),
          function(object, name, ...) {
              if (length(name) != 1)
                  stop("\n  'NChannelSet' channel 'name' must be one element",
                       "\n    was: '", paste0(name, collapse="', '"), "'")
              obj <- selectChannels(object, name) # subset phenoData appropriately
              sampleNames(phenoData(obj)) <- sampleNames(assayData(obj))
              ExpressionSet(assayData(obj)[[name]],
                  phenoData = phenoData(obj),
                  featureData = featureData(obj),
                  experimentData = experimentData(obj),
                  annotation=annotation(obj),
                  protocolData=protocolData(obj),
                  ...)
          })

setMethod("selectChannels",
          signature = signature(
            object = "NChannelSet",
            names="character"),
          function(object, names, ...) {
              if (any(duplicated(names)))
                stop("NChannelSet' channels 'names' must be unique")
              channelNames <- channelNames(object)
              badNames <- !names %in% channelNames
              if (any(badNames))
                stop("NChannelSet' channels 'names' must be channels")
              dropChannels <- channelNames[!channelNames %in% names]
              if (0 == length(dropChannels))
                return(object)
              ## assayData
              assayData <-
                assayDataSubsetElements(assayData(object), names)
              ## phenoData -- drop unneeded info
              metadata <- varMetadata(object)[["channel"]]
              okMetadata <- !metadata %in% dropChannels
              phenoData <- phenoData(object)[,okMetadata]
              ## reduce factor levels
              varMetadata(phenoData)[["channel"]] <-
                factor(metadata[okMetadata], levels=unique(c(names, "_ALL_")))
              initialize(object,
                         assayData = assayData,
                         phenoData = phenoData,
                         featureData = featureData(object),
                         experimentData=experimentData(object),
                         annotation=annotation(object),
                         protocolData=protocolData(object),
                         ...)
          })

setMethod("sampleNames",
          signature=signature(object="NChannelSet"),
          function(object) {
              assayData <- assayData(object)
              nms <- if (storageMode(object) == "list")
                  names(assayData)
              else
                  ls(assayData)
              res <- vector("list", length(nms))
              names(res) <- nms
              for (nm in nms)
                  res[[nm]] <- colnames(assayData[[nm]])
              ident <- sapply(res[-1], function(elt, ref) all(elt==ref),
                              res[[1]])
              if (all(ident)) res[[1]]
              else res
          })

setReplaceMethod("sampleNames", c("NChannelSet", "list"),
    function(object, value)
{
    assayData <- assayData(object)
    sampleNames(assayData) <- value
    phenoData <- phenoData(object)
    sampleNames(phenoData) <- sampleNames(assayData)
    protocolData <- protocolData(object)
    sampleNames(protocolData) <- sampleNames(assayData)

    .init_NChannelSet(object, assayData=assayData,
                      phenoData=phenoData, protocolData=protocolData)
})
