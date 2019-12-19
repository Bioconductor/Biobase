setMethod("initialize", signature(.Object="AnnotatedDataFrame"),
          function(.Object, data = data.frame(), varMetadata = data.frame(),
                   ...)
{
    tryCatch({
        if (missing(varMetadata)) {
            if (!missing(data)) checkClass(data, "data.frame", class(.Object))
            varMetadata <- data.frame(
                labelDescription = rep.int(NA_character_, ncol(data)),
                stringsAsFactors = FALSE
            )
            row.names(varMetadata) <- as.character(colnames(data))
        } else {
            checkClass(varMetadata, "data.frame", class(.Object))
            if (!"labelDescription" %in% colnames(varMetadata))
                varMetadata[["labelDescription"]] <-
                    rep.int(NA_character_, nrow(varMetadata))
            row.names(varMetadata) <- names(data)
        }
        varMetadata[["labelDescription"]] <-
            as.character(varMetadata[["labelDescription"]])
    }, error=function(err) {
        stop(conditionMessage(err),
             "\n  AnnotatedDataFrame 'initialize' could not update varMetadata:",
             "\n  perhaps pData and varMetadata are inconsistent?")
    })
    callNextMethod(.Object, data=data, varMetadata=varMetadata, ...)
})

validAnnotatedDataFrame <- function( object ) 
{
    msg <- NULL
    if (!is(object, "AnnotatedDataFrame"))
        msg <- c(msg, paste("cannot validate", class(object),
                            "as AnnotatedDataFrame" ))
    if (length(row.names(varMetadata(object))) !=
        length(colnames(pData(object))))
        msg <- c(msg, paste("all AnnotatedDataFrame pData column names",
                            "must be present as rows in varMetadata",
                            "and vice versa"))
    else if (any(row.names(varMetadata(object)) != colnames(pData(object))))
      msg <- c(msg, paste("AnnotatedDataFrame colnames of data differ",
                          "from row.names of varMetadata"))
    if ( !("labelDescription" %in% colnames(varMetadata(object))))
      msg <- c(msg, paste("AnnotatedDataFrame varMetadata",
                          "missing labelDescription column"))
    if (length(dimLabels(object))!=2)
      msg <- c(msg, "dimLabels must be a character vector of length 2")
    if (is.null(msg)) TRUE else msg
}

setValidity("AnnotatedDataFrame", validAnnotatedDataFrame)

setAs("AnnotatedDataFrame", "data.frame", function(from) {
    pData(from)
})

setMethod("updateObject", signature(object="AnnotatedDataFrame"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'AnnotatedDataFrame')")
              object <- asS4(object)
              if (isVersioned(object) && isCurrent(object)["AnnotatedDataFrame"])
                callNextMethod()
              else {
                  ## version 1.0.0. -> 1.1.0 needs a new slot "dimLabels"
                  to <- new(class(object))
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
        AnnotatedDataFrame(data=data, dimLabels=dimLabels)
    }
}

setMethod("annotatedDataFrameFrom",
          signature(object="matrix"),
          annotatedDataFrameFromMatrix)

annotatedDataFrameFromNull <- function(object, byrow=FALSE, ...) {
    dimLabels <-
        if (byrow) c("featureNames", "featureColumns")
        else c("sampleNames", "sampleColumns")
    AnnotatedDataFrame(data=data.frame(), dimLabels=dimLabels)
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

head.AnnotatedDataFrame <- function(x, n=6L, ...) {
    ## essentially, utils:::head.data.frame
    stopifnot(length(x) == 1L)
    n <- if (n < 0L) {
        max(nrow(x) + n, 0L)
    } else min(n, nrow(x))
    x[seq_len(n), ]
}

tail.AnnotatedDataFrame <- function(x, n=6L, ...) {
    ## essentially, utils:::tail.data.frame
    stopifnot(length(x) == 1L)
    nrx <- nrow(x)
    n <- if (n < 0L) 
        max(nrx + n, 0L)
    else min(n, nrx)
    x[seq.int(to = nrx, length.out = n), , drop = FALSE]
}

setMethod("dimnames", "AnnotatedDataFrame", function(x) {
    dimnames(pData(x))
})

setReplaceMethod("dimnames", "AnnotatedDataFrame", function(x, value) {
    sampleNames(x) <- value[[1]]
    varLabels(x) <- value[[2]]
    x
})

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
                     idx <- match(names(value), rownames(varMetadata(object)))
                     varMetadata <- varMetadata(object)[idx,,drop=FALSE]
                     row.names(varMetadata) <- names(value)
                     initialize(object, data=value, varMetadata=varMetadata)
                 })

setMethod("sampleNames", "AnnotatedDataFrame",
    function(object) row.names(object@data))

setReplaceMethod("sampleNames",
    signature(object="AnnotatedDataFrame", value="ANY"),
    function(object, value) 
{
    if (!is.null(value) && (length(value) != dim(object@data)[[1]]))
        stop("number of new names (", length(value), ") ",
             "should equal number of rows in AnnotatedDataFrame (",
             dim( object )[[1]], ")")
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

setReplaceMethod("varLabels",
    signature("AnnotatedDataFrame", "ANY"),
    function(object, value) 
{
    if (!is.null(value) && (length(value) != dim(object@data)[[2]]))
        stop("number of new varLabels (", length(value), ") ",
             "should equal number of columns in AnnotatedDataFrame (",
             dim(object)[[2]], ")")
    if (!is.null(value)) {
        ## silently ignore attempts to set colnames to NULL
        colnames(object@data) <- value
        row.names(object@varMetadata) <- value
    }
    object
})

setMethod("varMetadata", "AnnotatedDataFrame",
          function(object) object@varMetadata )

setReplaceMethod("varMetadata", c("AnnotatedDataFrame", "data.frame"),
                 function(object, value)
{
    if (!("labelDescription" %in% colnames(value)))
        warning("'varMetadata' must have column 'labelDescription'")
    rinfo <- .row_names_info(value)
    if (0L != length(varLabels(object))) {
        if (0L <= rinfo) {
            ## not 'automatic'
            bad <- setdiff(row.names(value), varLabels(object))
            if (0L != length(bad)) {
                fmt <- "'%s' not in 'varLabels(<AnnotatedDataFrame>)'"
                nms <- paste(selectSome(bad), collapse="', '")
                stop(sprintf(fmt, nms))
            }
        }
        if (ncol(object) != abs(rinfo)) {
            fmt <- "varMetadata has %d row(s), 'value' has %d"
            stop(sprintf(fmt, ncol(object), nrow(value)))
        }
        if (0 < ncol(pData(object)))
            row.names(value) <- names(pData(object))
    }
    object@varMetadata <- value
    object
})

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
              initialize(x, data=pD, varMetadata=mD)
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
  from <- asS4(from)
  ## data
  data <- from@pData
  ## varMetadata
  cnames <- colnames(data)
  varMetadata <- from@varMetadata
  if (all(dim(varMetadata)==0)) {
    varMetadata <- data.frame(numeric(length(cnames)),row.names=cnames)[,FALSE]
  }
  ## varLabels -- as column in varMetadata,or warn
  varLabels <- from@varLabels
  if (length(varLabels)>0 && !("labelDescription" %in% colnames(varMetadata)))
    varMetadata[["labelDescription"]] <- as.character(varLabels[cnames])
  else if (length(varLabels)>0)
    warning("contents of varLabels ignored\n", call.=FALSE)
  else
    varMetadata[["labelDescription"]] <- rep.int(NA_character_, nrow(varLabels))
  AnnotatedDataFrame(
      data=data,
      varMetadata=varMetadata,
      dimLabels=c("sampleNames", "sampleColumns"))
})

setAs("data.frame", "AnnotatedDataFrame",
      function(from) AnnotatedDataFrame(data=from))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
selectSome <- function(obj, maxToShow=5) {
  len <- length(obj)
  if (maxToShow<3) maxToShow <- 3
  if (len > maxToShow) {
      maxToShow <- maxToShow-1
      bot <- ceiling(maxToShow/2)
      top <- len-(maxToShow-bot-1)
      nms <- obj[c(1:bot, top:len)]
      c(as.character(nms[1:bot]), "...", as.character(nms[-c(1:bot)]))
  }
  else if (is.factor(obj))
      as.character(obj)
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


.wrapcat <-
    function(lbl, nms, total, ..., indent=2, exdent=4)
{
    lbl <- sprintf("%s:", lbl)
    txt <- paste(c(lbl,  nms), collapse=" ")
    ext <-
        if (length(nms) < total) sprintf("(%d total)", total)
        else character()
    txt <- paste(c(lbl,  nms, ext), collapse=" ")
    cat(strwrap(txt, ..., indent=indent, exdent=exdent), sep="\n")
}

.showAnnotatedDataFrame <-
    function(object, labels=list(0)) 
{
    lbls <- list(object=paste0("An object of class '", class(object), "'"),
                 sampleNames=dimLabels(object)[[1]],
                 varMetadata="varMetadata",
                 varLabels="varLabels")
    lbls[names(labels)] <- labels
    if (ncol(object) == 0) {            # early exit for empty objects
        cat(lbls[["object"]], ": none\n", sep="")
        return()
    }
    ## create a simplified object for extracting names
    idx <- selectSomeIndex(pData(object), maxToShow=4)
    idy <- selectSomeIndex(pData(object), byrow=FALSE, maxToShow=4)
    pData <- pData(object)[c(idx[[1]], idx[[3]]), c(idy[[1]], idy[[3]]),
                           drop=FALSE]
    rnms <- rownames(pData)
    nms <- c(rnms[idx[[1]]], idx[[2]],
             if (!is.null(idx[[1]])) rnms[-idx[[1]]] else NULL)

    cat(lbls$object, "\n", sep="")
    .wrapcat(lbls$sampleNames, nms, nrow(object))

    cnms <- colnames(pData)
    if (length(cnms) > 0) {
        vars <- c(cnms[idy[[1]]], idy[[2]], cnms[-idy[[1]]])
        .wrapcat(lbls$varLabels, vars, ncol(object))

        mnms <- selectSome(colnames(varMetadata(object)), maxToShow=4)
        .wrapcat(lbls$varMetadata, mnms, length(mnms))
    } else cat("\n  ", lbls$varLabels, ": none", sep="")
}

setMethod("show",
          signature=signature(object="AnnotatedDataFrame"),
          function(object) .showAnnotatedDataFrame(object))

setMethod("combine",
          signature(x="AnnotatedDataFrame", y="AnnotatedDataFrame"),
          function(x, y) {
              if (class(x) != class(y)) {
                  fmt <- "'%s' objects have diffrenent classes '%s', '%s'"
                  txt0 <- "combine,AnnotatedDataFrame,AnnotatedDataFrame-method"
                  stop(sprintf(fmt, txt0, class(x), class(y)))
              }
              if (!identical(dimLabels(x),dimLabels(y))) {
                  fmt <- paste("AnnotatedDataFrame dimLabels differ:\n",
                               "    %s\n",
                               "    %s\n  try 'updateObject'?")
                  stop(sprintf(fmt, paste(dimLabels(x), collapse=", "),
                               paste(dimLabels(y), collapse=", ")))
              }

              pDataX <- pData(x)
              pDataY <- pData(y)
              pData <- combine(pDataX, pDataY)

              varMetadataX <- varMetadata(x)
              varMetadataY <- varMetadata(y)
              ## labelDescription is required, likely a factor with
              ## conflicting levels
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

              initialize(x, data=pData, varMetadata=vM)
          })


read.AnnotatedDataFrame <-
    function(filename, path, sep = "\t", header = TRUE, quote = "",
             stringsAsFactors = FALSE, row.names = 1L,
             varMetadata.char="#", widget =
             getOption("BioC")$Base$use.widgets, sampleNames =
             character(0), ...)
{
    
    if(!(is.character(varMetadata.char) &&
         (identical(nchar(varMetadata.char), 1L))))
        stop("Invalid  'varMetadata.char'")

    ## For backward (or forward?) compatibility:
    if(widget)
        stop("Sorry: tkWidgets is not available for read.AnnotatedDataFrame")

    if(length(sampleNames)>0)
        stop("'sampleNames' argument is not supported, provide the sample",
             "names in the input file and use the option 'row.names'.")

    if(!missing(path))
        filename = file.path(path, filename)
    pData = read.table(filename, sep=sep, header=header, quote=quote,
      stringsAsFactors=stringsAsFactors, 
      row.names=row.names, comment.char=varMetadata.char, ...)
    
    ## read varMetadata section (the lines with leading "#")
    vmd = grep(paste0("^",  varMetadata.char),
      readLines(filename), value=TRUE)
    svmd = strsplit(vmd, ": *")
    varNames = sub("^# *", "", sapply(svmd, "[", 1L))
    varMetad = sapply(svmd, "[", 2L)

    ## link varMetadata names with pData colnames
    mt = match(colnames(pData), varNames)
    varMetad = ifelse(!is.na(mt), varMetad[mt], "")

    vmd = data.frame(labelDescription=varMetad, row.names=colnames(pData))

    ## add provenance information. Alapping it on as an attribute is a
    ## bit tacky, if Martin likes the idea at all, maybe this can be
    ## made a proper slot...
    provenance = sprintf("Read from file %s on %s at %s.",
      filename, Sys.info()["nodename"], date())
    attr(vmd, "provenance") = provenance
    
    AnnotatedDataFrame(data=pData, varMetadata=vmd)
    
}

write.AnnotatedDataFrame <-
    function(x, file="", varMetadata.char="#", ..., append=FALSE,
             fileEncoding="")
{
    ## file handling from write.table
    if (file == "") 
        file <- stdout()
    else if (is.character(file)) {
        file <- if (nzchar(fileEncoding)) 
            file(file, ifelse(append, "a", "w"), encoding = fileEncoding)
        else file(file, ifelse(append, "a", "w"))
        on.exit(close(file))
    } else if (!isOpen(file, "w")) {
        open(file, "w")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) 
        stop("'file' must be a character string or connection")

    writeLines(sprintf("%s %s: %s", varMetadata.char, varLabels(x),
                       varMetadata(x)$labelDescription),
               file)
    write.table(pData(x), file, append=append, fileEncoding=fileEncoding,
                ...)
}

setMethod("AnnotatedDataFrame",
          signature(data="missing", varMetadata="missing"),

    function(data, varMetadata, dimLabels=c("rowNames", "columnNames"), ...)
{
    .AnnotatedDataFrame(data=data.frame(), varMetadata=data.frame(),
                        dimLabels=dimLabels, ...)
})

setMethod("AnnotatedDataFrame",
          signature(data="data.frame", varMetadata="missing"),
    function(data, varMetadata,
             dimLabels=c("rowNames", "columnNames"), ...)
{
    varMetadata <- data.frame(
        labelDescription = rep.int(NA_character_, ncol(data)),
        stringsAsFactors = FALSE
    )
    row.names(varMetadata) <- names(data)
    .AnnotatedDataFrame(data=data, varMetadata=varMetadata,
                        dimLabels=dimLabels, ...)
})

setMethod("AnnotatedDataFrame",
           signature(data="data.frame", varMetadata="data.frame"),
    function(data, varMetadata,
             dimLabels=c("rowNames", "columnNames"), ...)
{
    if (!"labelDescription" %in% colnames(varMetadata))
        varMetadata[["labelDescription"]] <-
            rep.int(NA_character_, nrow(varMetadata))
    row.names(varMetadata) <- names(data)
    varMetadata[["labelDescription"]] <-
        as.character(varMetadata[["labelDescription"]])
    .AnnotatedDataFrame(data=data, varMetadata=varMetadata,
                        dimLabels=dimLabels, ...)
})




