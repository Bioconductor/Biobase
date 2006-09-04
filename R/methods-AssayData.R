assayDataNew <- function(storage.mode = c("lockedEnvironment", "environment", "list"), ...) {
  storage.mode <- match.arg(storage.mode) # defaults to "lockedEnvironment"
  assayData <- switch(storage.mode,
                      lockedEnvironment =,
                      environment = new.env(parent=emptyenv()),
                      list = list())
  arglist <- list(...)
  for (nm in names(arglist)) assayData[[nm]] <- arglist[[nm]]
  if (storage.mode == "lockedEnvironment") assayDataEnvLock(assayData)
  assayData
}

assayDataValidMembers <- function(assayData, required) {
    msg <- NULL
    storageMode <- assayDataStorageMode(assayData)
    if (!missing(required)) {
        names <- if (storageMode == "list") names(assayData) else ls(assayData)
        absent <- required[ !(required %in% names)]
        if (length(absent) != 0)
          msg <- paste(msg, 
                       paste("missing '", absent ,"' in assayData" , sep = "", collapse = "\n\t"),
                       sep="\n")
    }
    if (length(assayData)>1) {
        nms <-
          if (storageMode == "list") lapply(assayData, rownames)
          else eapply(assayData, rownames)
        if (!all(sapply(nms, is.null))) 
          if (any(sapply(nms, is.null)) ||
              any(nms[[1]] != unlist(nms[-1], use.names=FALSE)))
            msg <- paste(msg, "featureNames differ between AssayData members", sep="\n")
    }
    if (is.null(msg)) TRUE else msg
}

assayDataStorageMode <- function(object) {
  if (is(object, "list"))
    "list"
  else if (environmentIsLocked(object))
    "lockedEnvironment"
  else
    "environment"
}

setMethod("storageMode", "AssayData", assayDataStorageMode)

assayDataStorageModeReplace <- function(object, value) {
    storageMode <- assayDataStorageMode(object)
    if (storageMode == value) return(object)
    names <- if (storageMode == "list") names(object) else ls(object)
    switch(value,
           lockedEnvironment = {
               assayData <- new.env(parent=emptyenv())
               for (nm in names) assayData[[nm]] <- object[[nm]]
               assayDataEnvLock(assayData)
               assayData
           }, environment = {
               assayData <- new.env(parent=emptyenv())
               for (nm in names) assayData[[nm]] <- object[[nm]]
               assayData
           }, list = as.list(object))
}

setReplaceMethod("storageMode", c("AssayData", "character"), assayDataStorageModeReplace)

assayDataEnvLock <- function(assayData) lockEnvironment(assayData, bindings=TRUE)

assayDataElementNames <- function(obj)
  if (assayDataStorageMode(obj) == "list") names(assayData(obj)) else ls(assayData(obj))

assayDataElement <- function(obj, elt) assayData(obj)[[elt]]

assayDataElementReplace <- function(obj, elt, value) {
  storage.mode <- assayDataStorageMode(assayData(obj))
  if ("lockedEnvironment" == storage.mode) {
    aData <- copyEnv(assayData(obj))
    aData[[elt]] <- value
    assayDataEnvLock(aData)
    assayData(obj) <- aData
  } else {                              # list, environment
    assayData(obj)[[elt]] <- value
  }
  obj
}

setMethod("sampleNames", signature(object="AssayData"),
          function(object) {
              if (!length(object))
                return(character(0))
              switch(assayDataStorageMode(object),
                     list=colnames(object[[1]]),
                     colnames(object[[ls(object)[1]]]))
          })

setReplaceMethod("sampleNames", c("AssayData", "ANY"), function(object, value) {
  switch(assayDataStorageMode(object),
         lockedEnvironment = {
           object <- copyEnv(object)
           for (nm in ls(object)) colnames(object[[nm]]) <- value
           assayDataEnvLock(object)
         },
         environment = for (nm in ls(object)) colnames(object[[nm]]) <- value,
         list = for (nm in names(object)) colnames(object[[nm]]) <- value
         )
  object
})

setMethod("featureNames", signature(object="AssayData"),
          function(object) {
              if (!length(object))
                return(character(0))
              switch(assayDataStorageMode(object),
                     list=rownames(object[[1]]),
                     rownames(object[[ls(object)[1]]]))
          })


setReplaceMethod("featureNames", signature(object="AssayData", value="ANY"),
                 function(object, value) {
    switch(assayDataStorageMode(object),
         lockedEnvironment = {
           object <- copyEnv(object)
           for (nm in ls(object)) rownames(object[[nm]]) <- value
           assayDataEnvLock(object)
         },
         environment = for (nm in ls(object)) rownames(object[[nm]]) <- value,
         list = for (nm in names(object)) rownames(object[[nm]]) <- value,
         )
  object
})

setMethod("combine", c("AssayData", "AssayData"), function(x, y, ...) {
    combineElement <- function(x, y) {
        d <- dim(x)
        if (length(d) != length(dim(y)))
          stop("assayData elements have different dimension lengths: ",
               d, ", ", dim(y))
        if (!all({d==dim(y)}[-2]))
          stop("assayData elements have incompatible dimensions: ",
               d, ", ", dim(y))
        if (length(d)==2) {
            sharedCols <- intersect(colnames(x), colnames(y))
            if (length(sharedCols)>0) {
                if (!identical(x[,sharedCols, drop=FALSE], y[,sharedCols, drop=FALSE])) {
                    diff <- !sapply(sharedCols,
                                    function(nm) identical(x[, nm, drop=FALSE],
                                                           y[, nm, drop=FALSE]))
                    stop("assayData elements must have identical data",
                         "\n\tnon-conforming sample(s): ", 
                         paste(sharedCols[diff], collapse=", "), sep="")
                }
                cbind(x, y[,colnames(y) %in% setdiff(colnames(y), sharedCols), drop=FALSE])
            } else cbind(x,y)
        } else stop("Cannot combine AssayData with dim length ", length(dim(x)))
    }
    storage.mode <- assayDataStorageMode(x)
    nmfunc <- if ("environment"==class(x)) ls else names

    if (assayDataDim(x)[[1]] != assayDataDim(y)[[1]])
      stop("objects have different numbers of features: ",
           assayDataDim(x)[[1]], ", ", assayDataDim(y)[[1]])
    if (assayDataStorageMode(y) != storage.mode)
      stop(paste("assayData must have same storage, but are ",
                 storage.mode, ", ", assayDataStorageMode(y), sep=""))
    if (length(nmfunc(x)) != length(nmfunc(y)))
      stop("assayData have different numbers of elements:\n\t",
           paste(nmfunc(x), collapse=" "), "\n\t",
           paste(nmfunc(y), collapse=" "))
    if (!all(nmfunc(x) == nmfunc(y)))
      stop(paste("assayData have different element names:",
                 paste(nmfunc(x), collapse=" "),
                 paste(nmfunc(y), collapse=" "), sep="\n\t"))
    if ("list" == storage.mode) {
        aData <- lapply(names(x), function(nm) combineElement(x[[nm]],y[[nm]]))
        names(aData) <- names(x)
    } else {
        aData <- new.env(parent=emptyenv())
        for (nm in ls(x)) aData[[nm]] <- combineElement(x[[nm]], y[[nm]])
        if ("lockedEnvironment" == storage.mode) assayDataEnvLock(aData)
    }
    aData
})

assayDataDim <- function(object) {
  nms <- if (assayDataStorageMode(object) == "list") names(object) else ls(object)
  if ( length( nms ) == 0 ) return( NA )
  d <- dim( object[[ nms[[1]] ]])
  names(d) <- c( "Features", "Samples", rep("...", max(length(d)-2, 0)))
  d
}

assayDataDims <- function( object ) {
  nms <- if (assayDataStorageMode(object) == "list") names(object) else ls(object)
  if ( length( nms ) == 0 ) return( NA )
  d = sapply(nms, function(i) dim(object[[i]]))
  rownames(d) <- c("Features", "Samples", rep("...", nrow(d)-2))
  colnames(d) <- nms
  d[,order(colnames(d)), drop=FALSE]
}
