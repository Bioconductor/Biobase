validAnnotatedDataFrame <- function( object ) {
  msg <- NULL
  if (!is(object, "AnnotatedDataFrame"))
    msg <- paste(msg, paste("Cannot validate", class(object), "as AnnotatedDataFrame" ), sep = "\n  ")
  if (any(rownames(object@varMetadata) != colnames(object@data)))
    msg <- paste(msg, "AnnotatedDataFrame colnames of data differ from rownames of varMetadata", sep="\n  ")
  if ( !("labelDescription" %in% colnames(varMetadata(object))))
    msg <- paste(msg, "AnnotatedDataFrame varMetadata missing labelDescription column", sep="\n  ")
  if (is.null(msg)) TRUE else msg
}

setMethod("initialize", "AnnotatedDataFrame",
          function(.Object,
                   data = data.frame(),
                   varMetadata = data.frame()) {
            .Object@data = data
            if (!("labelDescription" %in% colnames(varMetadata)))
              varMetadata[["labelDescription"]] <- rep(NA, nrow(varMetadata))
            else
              varMetadata[["labelDescription"]] <- as.character(varMetadata[["labelDescription"]])
            .Object@varMetadata = varMetadata
            validObject(.Object)
            .Object
          })
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("dim", "AnnotatedDataFrame", function( x ) {
  d <- dim(x@data)
  names(d) <- c("Samples", "Variables")
  d
})

setMethod("nrow", "AnnotatedDataFrame", function(x) nrow(x@data))

setMethod("ncol", "AnnotatedDataFrame", function(x) ncol(x@data))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("pData", "AnnotatedDataFrame", function(object) object@data)

setReplaceMethod("pData", c("AnnotatedDataFrame", "data.frame"), function(object, value) {
  object@data <- value
  object
})

setMethod("sampleNames", "AnnotatedDataFrame", function(object) rownames(object@data))

setReplaceMethod("sampleNames", c("AnnotatedDataFrame", "ANY"), function(object, value) {
  if (length(value) != dim(object@data)[[1]])
    stop(paste("number of new names (",
               length(value),
               ") should equal number of rows in phenoData (",
               dim( object )[[1]], ")",sep=""))
  rownames(object@data) <- value
  object
})


setMethod("varLabels", "AnnotatedDataFrame", function(object) colnames(object@data))

setReplaceMethod("varLabels", c("AnnotatedDataFrame", "ANY"), function(object, value) {
  if (length(value) != dim(object@data)[[2]])
    stop(paste("number of new varLabels (",
               length(value),
               ") should equal number of columns in phenoData (",
               dim(object)[[2]], ")", sep=""))
  colnames(object@data) <- value
  rownames(object@varMetadata) <- value
  object
})

setMethod("varMetadata", "AnnotatedDataFrame", function(object) object@varMetadata )

setReplaceMethod("varMetadata", c("AnnotatedDataFrame", "data.frame"), function(object, value) {
  if (!("labelDescription" %in% colnames(value)))
    warning("varMetadata must have column named 'labelDescription'")
  else
    value[["labelDescription"]] <- as.character(value[["labelDescription"]])
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
  cat("  sampleNames:", paste(nms, collapse=", "))
  if (nrow(object)>length(nms))
    cat(" (",nrow(object)," total)", sep="")
  cat("\n  varLabels:\n")
  metadata <- varMetadata(object)
  vars <- selectSome(varLabels(object), maxToShow=9)
  meta <- selectSome(metadata[["labelDescription"]], maxToShow=9)
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
setMethod("combine", c("AnnotatedDataFrame", "AnnotatedDataFrame"), function(x, y) {
  ## data
  sNames <- c(sampleNames(x),sampleNames(y))
  vNames <- unique(c(varLabels(x),varLabels(y)))
  if (any(duplicated(sNames)))
      stop(paste("AnnotatedDataFrames must have different sample names,",
                 "identical names are:",
                 paste(sNames[duplicated(sNames)],collapse=", "),
                 sep="\n\t"))
  df <- data.frame(rep(0,length(sNames)),row.names=sNames)[,FALSE]
  for (nm in vNames) {
    xdata <- if (is.null(x[[nm]])) rep(NA,nrow(x)) else x[[nm]]
    ydata <- if (is.null(y[[nm]])) rep(NA,nrow(y)) else y[[nm]]
    df[[nm]] <- c(xdata,ydata)
  }
  colnames(df) <- vNames
  pData(x) <- df
  ## varMetadata
  vx <- varMetadata(x)
  vy <- varMetadata(y)

  vmRownames <- c(rownames(vx),rownames(vy))
  uniqueRownames <- unique(vmRownames)
  duplRownames <- duplicated(vmRownames)
  vmColnames <- unique(c(colnames(vx),colnames(vy)))
  df <- data.frame(rep(0,length(uniqueRownames)),row.names=uniqueRownames)[,FALSE]

  if (0!=nrow(df)) 
    for (nm in vmColnames) df[uniqueRownames,nm] <-
      ifelse(is.na(vx[uniqueRownames,nm]) ||
             identical(vx[uniqueRownames,nm],vy[uniqueRownames,nm]),
             vy[uniqueRownames,nm],
             ifelse(is.na(vy[uniqueRownames,nm]),
                    vx[uniqueRownames,nm],
                    stop(paste("AnnotatedDataFrame varMetadata contains conflicting descriptions:",
                               paste("column:",nm),
                               vx[uniqueRownames,nm],
                               vy[uniqueRownames,nm],
                               sep="\n\t  "))))
  varMetadata(x) <- df
  x
})
