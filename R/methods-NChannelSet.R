setMethod("initialize",
          signature(.Object="NChannelSet"),
          function(.Object, assayData, phenoData,
                   ...) {
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
              appl <-
                  if (storageMode(assayData)=="list") lapply
                  else eapply
              assaySampleNames <-
                  appl(assayData, function(elt) {
                      cnames <- colnames(elt)
                      if (is.null(cnames)) sampleNames(phenoData)
                      else cnames
                  })
              sampleNames(assayData) <- assaySampleNames
              sampleNames(phenoData) <- sampleNames(assayData)
              do.call(callNextMethod,
                      c(.Object,
                        assayData = assayData, phenoData = phenoData,
                        dotArgs[isSlot]))
          })

setValidity("NChannelSet",
            function(object) {
                msg <- validMsg(NULL, isValidVersion(object, "NChannelSet"))
                if (!"channel" %in% names(varMetadata(object))) {
                    msg <- validMsg(msg,
                                    "\n  'NChannelSet' varMetadata must have a 'channel' column")
                } else {
                    channel <-
                      varMetadata(object)[["channel"]]
                    if (!is(channel, "factor"))
                      msg <- validMsg(msg,
                                      "\n  'NChannelSet' varMetadata column 'channel' must be class 'factor'")
                    else if (0 < length(channel)) {
                        phenoChannels <- unique(channel)
                        phenoChannels <- phenoChannels[!is.na(phenoChannels)]
                        okChannels <-
                            phenoChannels %in% c("_ALL_", channelNames(object))
                        if (!all(okChannels)) {
                            txt <- paste("\n  'NChannelSet' varMetadata 'channel' entries not in assayData: '",
                                         paste(phenoChannels[!okChannels],
                                               collapse="', '", sep=""),
                                         "'", sep="")
                            msg <- validMsg(msg, txt)
                        }
                        
                    }
                    if (!("_ALL_" %in% levels(channel)))
                        msg <- validMsg(msg,
                                        "\n  'NChannelSet' varMetadata 'channel' requires '_ALL_' as a level")
                }
                if (is.null(msg)) TRUE else msg
            })

setMethod("channelNames",
          signature = signature(
            object = "NChannelSet"),
          assayDataElementNames)

setMethod("channel",
          signature = signature(
            object = "NChannelSet",
            name = "character"),
          function(object, name, ...) {
              if (length(name) != 1)
                  stop("\n  'NChannelSet' channel 'name' must be one element",
                       "\n    was: '", paste(name, sep="", collapse="', '"), "'")
              obj <- selectChannels(object, name) # subset phenoData appropriately
              sampleNames(phenoData(obj)) <- sampleNames(assayData(obj))
              new("ExpressionSet",
                  phenoData = phenoData(obj),
                  featureData = featureData(obj),
                  experimentData = experimentData(obj),
                  annotation=annotation(obj),
                  protocolData=protocolData(obj),
                  exprs = assayData(obj)[[name]],
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
              appl <- 
                if (storageMode(object)=="list") lapply
                else eapply
              res <- appl(assayData(object), colnames)
              ident <- sapply(res[-1], function(elt, ref) all(elt==ref),
                              res[[1]])
              if (all(ident)) res[[1]]
              else res
          })

setReplaceMethod("sampleNames",
                 signature=signature(
                   object="NChannelSet",
                   value="list"),
                 function(object, value) {
                     sampleNames(assayData(object)) <- value
                     sampleNames(phenoData(object)) <-
                         sampleNames(assayData(object))
                     sampleNames(protocolData(object)) <-
                         sampleNames(assayData(object))
                     object
                 })
