setMethod("initialize", signature(.Object="AnnotatedDataFrame"),
          function(.Object, data = data.frame(), varMetadata = data.frame(),...) {
              if (missing(varMetadata)) {
                  varMetadata <- data.frame(labelDescription = rep(NA, ncol(data)))
                  row.names(varMetadata) <- as.character(colnames(data))
              } else if (!"labelDescription" %in% colnames(varMetadata)) {
                  varMetadata[["labelDescription"]] <- rep(NA, nrow(varMetadata))
              }
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

setMethod("annotatedDataFrameFrom",
          signature(object="AssayData"),
          function(object, byrow=FALSE, ...) {
              dims <- assayDataDims(object)
              data <- 
                if (all(is.na(dims))) data.frame()
                else {
                    n <- if (byrow) dims[1,1] else dims[2,1]
                    eltNames <-
                      if (is(object, "environment")) ls(object)
                      else names(object)
                    nms <-
                      if(byrow) rownames(object[[eltNames[[1]]]])
                      else colnames(object[[eltNames[[1]]]])
                    data.frame(numeric(n), row.names=nms)[,FALSE]
                }
              dimLabels <-
                if (byrow) c("featureNames", "featureColumns")
                else c("sampleNames", "sampleColumns")
              new("AnnotatedDataFrame", data=data, dimLabels=dimLabels)
          })

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("dim", "AnnotatedDataFrame", function( x ) {
  d <- dim(x@data)
  names(d) <- dimLabels(x)
  d
})

setMethod("nrow", "AnnotatedDataFrame", function(x) nrow(x@data))

setMethod("ncol", "AnnotatedDataFrame", function(x) ncol(x@data))

setMethod("dimLabels", "AnnotatedDataFrame", function(object) {
    object@dimLabels
})
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("pData", "AnnotatedDataFrame", function(object) object@data)

setReplaceMethod("pData", c("AnnotatedDataFrame", "data.frame"), function(object, value) {
  object@data <- value
  object
})

setMethod("sampleNames", "AnnotatedDataFrame", function(object) row.names(object@data))

setReplaceMethod("sampleNames", c("AnnotatedDataFrame", "ANY"), function(object, value) {
  if (length(value) != dim(object@data)[[1]])
    stop(paste("number of new names (",
               length(value),
               ") should equal number of rows in phenoData (",
               dim( object )[[1]], ")",sep=""))
  row.names(object@data) <- value
  object
})

setMethod("featureNames",
          signature(object="AnnotatedDataFrame"),
          function(object) sampleNames(object))

setReplaceMethod("featureNames",
                 signature(object="AnnotatedDataFrame", value="ANY"),
                 function(object, value) sampleNames(object) <- value)

setMethod("varLabels", "AnnotatedDataFrame", function(object) colnames(object@data))

setReplaceMethod("varLabels", c("AnnotatedDataFrame", "ANY"), function(object, value) {
  if (length(value) != dim(object@data)[[2]])
    stop(paste("number of new varLabels (",
               length(value),
               ") should equal number of columns in phenoData (",
               dim(object)[[2]], ")", sep=""))
  colnames(object@data) <- value
  row.names(object@varMetadata) <- value
  object
})

setMethod("varMetadata", "AnnotatedDataFrame", function(object) object@varMetadata )

setReplaceMethod("varMetadata", c("AnnotatedDataFrame", "data.frame"), function(object, value) {
  if (!("labelDescription" %in% colnames(value)))
    warning("varMetadata must have column named 'labelDescription'")
  object@varMetadata <- value
  object
})
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", "AnnotatedDataFrame", function(x, i, j, ..., drop = FALSE) {
  if (missing(drop)) drop <- FALSE
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
 new("AnnotatedDataFrame", data=pD, varMetadata=mD )
})

setMethod("$", "AnnotatedDataFrame", function(x, name) x@data[[name]] )

setReplaceMethod("$", "AnnotatedDataFrame", function(x, name, value) {
  x@data[[name]] = value
  x
})

setMethod("[[", "AnnotatedDataFrame", function(x, i, j, ...) x@data[[i]] )

setReplaceMethod("[[", "AnnotatedDataFrame",
   function(x, i, j, ..., value) {
      x@data[[i]] <- value
      x
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
    varMetadata[["labelDescription"]] <- varLabels[cnames]
  else if (length(varLabels)>0)
    warning("contents of varLabels ignored\n", call.=FALSE)
  else
    varMetadata[["labelDescription"]] <- character(length(varLabels))
  new("AnnotatedDataFrame",
      data=data,
      varMetadata=varMetadata)
})
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
selectSome <- function(obj, maxToShow=5) {
  len <- length(obj)
  if (maxToShow<2) maxToShow <- 2
  if (len > maxToShow) {
    bot <- ceiling(maxToShow/2)
    top <- len-(maxToShow-bot-1)
    c(obj[1:bot], "...", obj[top:len])
  }
  else obj
}

setMethod("show", "AnnotatedDataFrame", function(object) {
  nms <- selectSome(sampleNames(object))
  cat("  ", dimLabels(object)[[1]], ": ", paste(nms, collapse=", "), sep="")
  if (nrow(object)>length(nms))
    cat(" (",nrow(object)," total)", sep="")
  cat("\n  varLabels and descriptions:\n")
  metadata <- varMetadata(object)
  vars <- selectSome(varLabels(object), maxToShow=9)
  meta <- selectSome(as.character(metadata[["labelDescription"]]), maxToShow=9)
  mapply(function(nm, meta) cat("    ",nm,": ", meta, "\n", sep=""),
         vars, meta)
  if (nrow(metadata)>length(meta))
    cat("    (", nrow(metadata), " total)\n", sep="")
  if (ncol(metadata)>1) {
    mnms <- selectSome(colnames(metadata))
    cat("  varMetadata:", paste(mnms, collapse=", "), "\n")
  }
})
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

              new("AnnotatedDataFrame", data=pData, varMetadata=vM, dimLabels=dimLabels(x))
          })
