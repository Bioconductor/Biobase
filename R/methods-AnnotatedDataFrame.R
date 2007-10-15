setMethod("initialize", signature(.Object="AnnotatedDataFrame"),
          function(.Object, data = data.frame(), varMetadata = data.frame(),...) {
              tryCatch({
                  if (missing(varMetadata)) {
                      if (!missing(data)) checkClass(data, "data.frame", class(.Object))
                      varMetadata <- data.frame(labelDescription = rep(NA, ncol(data)))
                      row.names(varMetadata) <- as.character(colnames(data))
                  } else {
                      checkClass(varMetadata, "data.frame", class(.Object))
                      if (!"labelDescription" %in% colnames(varMetadata))
                          varMetadata[["labelDescription"]] <- rep(NA, nrow(varMetadata))
                      row.names(varMetadata) <- names(data)
                  }
                  varMetadata[["labelDescription"]] <- as.character(varMetadata[["labelDescription"]])
              }, error=function(err) {
                  stop(conditionMessage(err),
                       "\n  AnnotatedDataFrame 'initialize' could not update varMetadata:",
                       "\n  perhaps pData and varMetadata are inconsistent?")
              })
              callNextMethod(.Object, data=data, varMetadata=varMetadata, ...)
          })

validAnnotatedDataFrame <- function( object ) {
    msg <- NULL
    if (!is(object, "AnnotatedDataFrame"))
      msg <- paste(msg, paste("Cannot validate", class(object), "as AnnotatedDataFrame" ), sep = "\n  ")
    if (length(row.names(varMetadata(object))) != length(colnames(pData(object))))
      msg <- paste(msg, "All AnnotatedDataFrame pData column names must be present as rows in varMetadata, and vice versa", sep="\n")
    else if (any(row.names(varMetadata(object)) != colnames(pData(object))))
      msg <- paste(msg, "AnnotatedDataFrame colnames of data differ from row.names of varMetadata", sep="\n  ")
    if ( !("labelDescription" %in% colnames(varMetadata(object))))
      msg <- paste(msg, "AnnotatedDataFrame varMetadata missing labelDescription column", sep="\n  ")
    if (length(dimLabels(object))!=2)
      msg <- paste(msg, "dimLabels must be a character vector of length 2", sep="\n  ")
    if (is.null(msg)) TRUE else msg
}

setValidity("AnnotatedDataFrame", validAnnotatedDataFrame)

setMethod("updateObject", signature(object="AnnotatedDataFrame"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'AnnotatedDataFrame')")
              if (isVersioned(object) && isCurrent(object)["AnnotatedDataFrame"])
                callNextMethod()
              else {
                  ## version 1.0.0. -> 1.1.0 needs a new slot "dimLabels"
                  to <- new("AnnotatedDataFrame")
                  varMetadata(to) <- updateObject(varMetadata(object))
                  pData(to) <- updateObject(pData(object))
                  to
              }
          })

annotatedDataFrameFromMatrix <- function(object, byrow=FALSE, ...) {
    ## contract: 'object' is matrix-like, with dim, rownames, colnames
    ## methods. Returns AnnotatedDataFrame with appropriate dimensions.
    dims <- dim(object)
    if (is.null(dims) || all(dims==0))
        annotatedDataFrameFrom(NULL, byrow=byrow, ...)
    else {
        n <- if (byrow) dims[1] else dims[2]
        nms <-
            if(byrow) rownames(object)
            else colnames(object)
        data <- data.frame(numeric(n), row.names=nms)[,FALSE]
        dimLabels <-
            if (byrow) c("featureNames", "featureColumns")
            else c("sampleNames", "sampleColumns")
        new("AnnotatedDataFrame", data=data, dimLabels=dimLabels)
    }
}

setMethod("annotatedDataFrameFrom",
          signature(object="matrix"),
          annotatedDataFrameFromMatrix)

annotatedDataFrameFromNull <- function(object, byrow=FALSE, ...) {
    dimLabels <-
        if (byrow) c("featureNames", "featureColumns")
        else c("sampleNames", "sampleColumns")
    new("AnnotatedDataFrame", data=data.frame(), dimLabels=dimLabels)
}

setMethod("annotatedDataFrameFrom",
          signature(object="NULL"),
          annotatedDataFrameFromNull)

annotatedDataFrameFromAssayData <- function(object, byrow=FALSE, ...) {
    eltNames <-
        if (is(object, "environment")) ls(object)
        else names(object)
    if (length(eltNames)==0)
        annotatedDataFrameFrom(NULL, byrow=byrow, ...)
    else
        annotatedDataFrameFrom(object[[eltNames[1]]], byrow=byrow, ...)
}

setMethod("annotatedDataFrameFrom",
          signature(object="AssayData"),
          annotatedDataFrameFromAssayData)

setMethod("dim", "AnnotatedDataFrame", function( x ) {
  d <- dim(pData(x))
  names(d) <- dimLabels(x)
  d
})

setMethod("dimLabels", "AnnotatedDataFrame", function(object) {
    object@dimLabels
})

setReplaceMethod("dimLabels",
                 signature=signature(
                   object="AnnotatedDataFrame",
                   value="character"),
                 function(object, value) {
                     object@dimLabels <- value
                     object
                 })

setMethod("pData", "AnnotatedDataFrame", function(object) object@data)

setReplaceMethod("pData",
                 signature=signature(
                   object="AnnotatedDataFrame",
                   value="data.frame"),
                 function(object, value) {
                     varMetadata <-
                         varMetadata(object)[names(value),,drop=FALSE]
                     row.names(varMetadata) <- names(value)
                     new("AnnotatedDataFrame",
                         data=value, varMetadata=varMetadata)
                 })

setMethod("sampleNames", "AnnotatedDataFrame", function(object) row.names(object@data))

setReplaceMethod("sampleNames", c("AnnotatedDataFrame", "ANY"), function(object, value) {
  if (length(value) != dim(object@data)[[1]])
    stop(paste("number of new names (",
               length(value),
               ") should equal number of rows in AnnotatedDataFrame (",
               dim( object )[[1]], ")",sep=""))
  row.names(object@data) <- value
  object
})

setMethod("featureNames",
          signature(object="AnnotatedDataFrame"),
          function(object) sampleNames(object))

setReplaceMethod("featureNames",
                 signature(object="AnnotatedDataFrame", value="ANY"),
                 function(object, value) {
                     sampleNames(object) <- value
                     object
                 })

setMethod("varLabels", "AnnotatedDataFrame", function(object) colnames(object@data))

setReplaceMethod("varLabels", c("AnnotatedDataFrame", "ANY"), function(object, value) {
  if (length(value) != dim(object@data)[[2]])
    stop(paste("number of new varLabels (",
               length(value),
               ") should equal number of columns in AnnotatedDataFrame (",
               dim(object)[[2]], ")", sep=""))
  colnames(object@data) <- value
  row.names(object@varMetadata) <- value
  object
})

setMethod("varMetadata", "AnnotatedDataFrame", function(object) object@varMetadata )

setReplaceMethod("varMetadata", c("AnnotatedDataFrame", "data.frame"), function(object, value) {
  if (!("labelDescription" %in% colnames(value)))
    warning("varMetadata must have column named 'labelDescription'")
  if (ncol(pData(object))>0)
    row.names(value) <- names(pData(object))
  object@varMetadata <- value
  object
})
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[",
          signature(x="AnnotatedDataFrame"),
          function(x, i, j, ..., drop) {
              if (missing(drop)) drop = FALSE
              else if (drop)
                stop("'AnnotatedDataFrame' does not support drop = TRUE")
              if(missing(j)) {
                  mD <- x@varMetadata
                  pD <- x@data[i,,drop = drop]
              } else {
                  mD <- x@varMetadata[j,,drop = drop]
                  if( missing( i ))
                    pD <- x@data[,j,drop = drop]
                  else
                    pD <- x@data[i,j,drop = drop]
              }
              new("AnnotatedDataFrame",
                  data=pD, varMetadata=mD, dimLabels=dimLabels(x))
          })

##setMethod("$", "AnnotatedDataFrame", function(x, name) `$`(pData(x), name))
setMethod("$", "AnnotatedDataFrame", function(x, name) {
    eval(substitute(pData(x)$NAME_ARG, list(NAME_ARG=name)))
})

setReplaceMethod("$", "AnnotatedDataFrame", function(x, name, value) {
    x[[name]] <- value
    x
})

setMethod("[[", "AnnotatedDataFrame", function(x, i, j, ...) pData(x)[[i]] )

setReplaceMethod("[[",
                 signature=signature(x="AnnotatedDataFrame"),
                 function(x, i, j, ..., value) {
                     pData(x)[[i]] <- value
                     for (metadata in names(list(...)))
                       varMetadata(x)[i, metadata] <- list(...)[[metadata]]
                     x
                 })

setAs("phenoData", "AnnotatedDataFrame", function(from) {
  ## data
  data <- pData(from)
  ## varMetadata
  cnames <- colnames(pData(from))
  varMetadata <- varMetadata(from)
  if (all(dim(varMetadata)==0)) {
    varMetadata <- data.frame(numeric(length(cnames)),row.names=cnames)[,FALSE]
  }
  ## varLabels -- as column in varMetadata,or warn
  varLabels <- varLabels(from)
  if (length(varLabels)>0 && !("labelDescription" %in% colnames(varMetadata)))
    varMetadata[["labelDescription"]] <- as.character(varLabels[cnames])
  else if (length(varLabels)>0)
    warning("contents of varLabels ignored\n", call.=FALSE)
  else
    varMetadata[["labelDescription"]] <- as.character(rep(NA, nrow(varLabels)))
  new("AnnotatedDataFrame",
      data=data,
      varMetadata=varMetadata,
      dimLabels=c("sampleNames", "sampleColumns"))
})

setAs("data.frame", "AnnotatedDataFrame",
      function(from) new("AnnotatedDataFrame", data=from))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
selectSome <- function(obj, maxToShow=5) {
  len <- length(obj)
  if (maxToShow<3) maxToShow <- 3
  if (len > maxToShow) {
      maxToShow <- maxToShow-1
      bot <- ceiling(maxToShow/2)
      top <- len-(maxToShow-bot-1)
      nms <- obj[c(1:bot, top:len)]
      c(nms[1:bot], "...", nms[-c(1:bot)])
  }
  else obj
}

setMethod("selectSomeIndex",
          signature(object="data.frame"),
          function(object, maxToShow=5, byrow=TRUE, ...) {
              len <-
                if (byrow) dim(object)[[1]]
                else dim(object)[[2]]
              if (maxToShow < 3) maxToShow <- 3
              if (len > maxToShow) {
                  maxToShow <- maxToShow - 1
                  bot <- ceiling(maxToShow/2)
                  top <- len-(maxToShow-bot-1)
                  list(1:bot, "...", top:len)
              } else if (len >= 1) list(1:len, NULL, NULL)
              else list(NULL, NULL, NULL)
          })

.showAnnotatedDataFrame <- function(object, labels=list(0)) {
    lbls <- list(object=paste("An object of class \"",
                   class(object), "\"", sep=""),
                 sampleNames=dimLabels(object)[[1]],
                 varMetadata="varMetadata",
                 varLabels="varLabels")
    lbls[names(labels)] <- labels
    ## create a simplified object for extracting names
    idx <- selectSomeIndex(pData(object), maxToShow=4)
    idy <- selectSomeIndex(pData(object), byrow=FALSE, maxToShow=4)
    pData <- pData(object)[c(idx[[1]], idx[[3]]), c(idy[[1]], idy[[3]]), drop=FALSE]
    rnms <- rownames(pData)
    nms <- c(rnms[idx[[1]]], idx[[2]],
             if (!is.null(idx[[1]])) rnms[-idx[[1]]] else NULL)

    cat(lbls$object, "\n", sep="")
    outs = paste("  ", lbls$sampleNames, ": ",
      paste(nms, collapse=", "), sep="")
    if (nrow(object)>length(nms))
        outs = c(outs, paste(" (",nrow(object)," total)", sep=""))
    cat(strbreak(outs))
    
    cat("\n  ", lbls$varLabels, " and ", lbls$varMetadata, " description:", sep="")
    cnms <- colnames(pData)
    if (length(cnms)>0) {
        cat("\n")
        metadata <- varMetadata(object)
        vars <- c(cnms[idy[[1]]], idy[[2]], cnms[-idy[[1]]])
        meta <- selectSome(as.character(metadata[["labelDescription"]]), maxToShow=4)
        mapply(function(nm, meta) {
            outs = paste("    ",nm,": ", meta, "\n", sep="")
            cat(strbreak(outs))
        }, vars, meta)
        
        if (nrow(metadata)>length(meta))
            cat("    (", nrow(metadata), " total)\n", sep="")
        if (ncol(metadata)>1) {
            mnms <- selectSome(colnames(metadata)[-1], maxToShow=4)
            cat("  additional ", lbls$varMetadata, ": ", paste(mnms, collapse=", "),
                "\n", sep="")
        }
    } else cat(" none\n")
}

setMethod("show",
          signature=signature(object="AnnotatedDataFrame"),
          function(object) .showAnnotatedDataFrame(object))

setMethod("combine",
          signature(x="AnnotatedDataFrame", y="AnnotatedDataFrame"),
          function(x, y) {
              if (!identical(dimLabels(x),dimLabels(y)))
                stop("AnnotatedDataFrame dimLabels differ")

              pDataX <- pData(x)
              pDataY <- pData(y)
              pData <- combine(pDataX, pDataY)

              varMetadataX <- varMetadata(x)
              varMetadataY <- varMetadata(y)
              ## labelDescription is required, likely a factor with conflicting levels
              if (is.factor(varMetadataX$labelDescription) &&
                  is.factor(varMetadataY$labelDescription)) {
                  f <- factor(c(as.character(varMetadataX$labelDescription),
                                as.character(varMetadataY$labelDescription)))
                  varMetadataX$labelDescription <-
                    factor(as.character(varMetadataX$labelDescription), levels=levels(f))
                  varMetadataY$labelDescription <-
                    factor(as.character(varMetadataY$labelDescription), levels=levels(f))
                }
              vM <- combine(varMetadataX, varMetadataY)

              new("AnnotatedDataFrame",
                  data=pData, varMetadata=vM, dimLabels=dimLabels(x))
          })


## the following was the old implementation, used until version 1.15.12
##
## read.AnnotatedDataFrame <- function(filename = NULL, sampleNames = character(0),
#                                     widget = getOption("BioC")$Base$use.widgets,...) {
#     ## used by affy
#     if(widget) {
#         ## see read.phenoData for implementation hints
#         stop("sorry, tkWidgets support not available for read.AnnotatedDataFrame")
#     }
#     else {
#         if(is.character(filename) || inherits(filename, "connection")) {
#             pData <- read.table(filename, ...)
#             if(0!=length(sampleNames))
#               row.names(pData) <- sampleNames
#             return(new("AnnotatedDataFrame",
#                        data=pData,
#                        varMetadata=data.frame(
#                          labelDescription=rep("read from file", ncol(pData)),
#                          row.names=names(pData))))
#         }
#         else {
#             if(!is.null(filename))
#               stop("incorrect 'filename' given")
#             if(0==length(sampleNames)){
#                 return(new("AnnotatedDataFrame")) ##return a blank!
#             } else {
#                 pData <- data.frame(
#                              sample=1:length(sampleNames),
#                              row.names=sampleNames)
#                 return(new("AnnotatedDataFrame",
#                            data=pData,
#                            varMetadata=data.frame(
#                              labelDescription="arbitrary numbering",
#                              row.names=names(pData))))
#             }
#         }
#     }
# }

read.AnnotatedDataFrame = function(filename, path,
                                   sep = "\t", header = TRUE, quote = "", stringsAsFactors = FALSE, 
                                   row.names = 1L,
                                   varMetadata.char="#",
                                   widget = getOption("BioC")$Base$use.widgets,
                                   sampleNames = character(0),
                                   ...) {
  
  if(!(is.character(varMetadata.char)&&(identical(nchar(varMetadata.char), 1L))))
    stop("Invalid  'varMetadata.char'")

  ## For backward (or forward?) compatibility:
  if(widget)
    stop("Sorry: tkWidgets support is not available for read.AnnotatedDataFrame")
    ## see read.phenoData for implementation hints

  if(length(sampleNames)>0)
    stop("'sampleNames' argument is not supported, please provide the sample",
         "names in the input file and use the option 'row.names'.")

  ## read phenoData table (the lines without leading "#")
  if(!missing(path))
    filename = file.path(path, filename)
  pData = read.table(filename, sep=sep, header=header, quote=quote, stringsAsFactors=stringsAsFactors, 
    row.names=row.names, comment.char=varMetadata.char, ...)
  
  ## read varMetadata section (the lines with leading "#")
  vmd = grep(paste("^",  varMetadata.char, sep=""), readLines(filename), value=TRUE)
  svmd = strsplit(vmd, ":")
  varNames = sub("^# *", "", sapply(svmd, "[", 1L))
  varMetad = sapply(svmd, "[", 2L)

  ## link varMetadata names with pData colnames
  mt = match(colnames(pData), varNames)
  varMetad = ifelse(!is.na(mt), varMetad[mt], "")

  vmd = data.frame(labelDescription=varMetad, row.names=colnames(pData))

  ## add provenance information. Alapping it on as an attribute is a bit tacky, if Martin likes the idea
  ## at all, maybe this can be made a proper slot...
  provenance = sprintf("Read from file %s on %s at %s.", filename, Sys.info()["nodename"], date())
  attr(vmd, "provenance") = provenance
  
  new("AnnotatedDataFrame", data=pData, varMetadata=vmd)
  
}


