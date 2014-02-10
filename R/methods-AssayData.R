assayDataNew <-
    function(storage.mode = c("lockedEnvironment", "environment", "list"),
             ...)
{
    storage.mode <- match.arg(storage.mode) ## defaults to "lockedEnvironment"
    assayData <- switch(storage.mode,
                        lockedEnvironment =,
                        environment = new.env(parent=emptyenv()),
                        list = list())
    
    arglist <- list(...)
    if((length(arglist)>0L) &&
       ((is.null(names(arglist))) || any(names(arglist)=="")))
        stop("all arguments must be named")

    for (nm in names(arglist)) {
        elt <- arglist[[nm]]
        if (!is.null(dimnames(elt)))
            dimnames(elt) <- lapply(dimnames(elt), unname)
        assayData[[nm]] <- elt
    }
    if (storage.mode == "lockedEnvironment")
        assayDataEnvLock(assayData)
    msg <- assayDataValidMembers(assayData)
    if (!is.logical(msg)) stop(msg)
    assayData
}

assayDataValidMembers <- function(assayData, required) {
    msg <- NULL
    eltNames <-
      if ("list" == assayDataStorageMode(assayData)) names(assayData)
      else ls(assayData)
    if (!missing(required)) {
        absent <- required[!required %in% eltNames]
        if (length(absent) != 0)
          msg <- c(msg, paste0("'AssayData' missing '", absent ,"'" ,
                               collapse = "\n\t"))
    }
    dimsOk <-
      sapply(eltNames, function(elt)
             tryCatch(length(dim(assayData[[elt]]))>1,
                      error=function(err) FALSE))
    if (!all(dimsOk)) 
      msg <- c(msg, paste0("'AssayData' elements with invalid dimensions: '",
                           paste(eltNames[!dimsOk], collapse="' '"), "'"))
    if (length(assayData)>1) {
        eltRowNames <- rownames(assayData[[eltNames[[1]]]])
        rowNamesOk <- 
          all(sapply(eltNames[-1], function(elt)
                     all(eltRowNames == rownames(assayData[[elt]]))))
        if (!rowNamesOk)
          msg <- c(msg, "'AssayData' elements with different rowNames")
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

setReplaceMethod("storageMode",
                 signature=c(object="AssayData", value="character"),
                 assayDataStorageModeReplace)

assayDataEnvLock <- function(assayData)
  lockEnvironment(assayData, bindings=TRUE)

assayDataSubsetElements <-
  function(object, elts, storageMode = assayDataStorageMode(object)) {
    if (any(duplicated(elts)))
      stop("'AssayData' element names must be unique")
    names <-
        if (storageMode(object)=="list") names(object)
        else ls(object)
    if (!all(elts %in% names))
      stop("'AssayData' missing elements: '",
           paste0(elts[!elts %in% names], collapse="', '"), "'")
    switch(storageMode,
           lockedEnvironment = {
               assayData <- new.env(parent = emptyenv())
               for (nm in elts) assayData[[nm]] <- object[[nm]]
               assayDataEnvLock(assayData)
               assayData
           },
           environment = {
               assayData <- new.env(parent = emptyenv())
               for (nm in elts) assayData[[nm]] <- object[[nm]]
               assayData
           },
           list = {
               object[elts]
           })
}

setMethod("assayData",
          signature=signature(object="AssayData"),
          function(object) object)

.assayDataDimnames <- function(assayData) {
    switch(storageMode(assayData), lockedEnvironment=, environment = {
        result <- vector("list", length(assayData))
        names(result) <- ls(assayData)
        for (nm in ls(assayData))
            result[[nm]] <- dimnames(assayData[[nm]])
        result
    }, list=lapply(assayData, dimnames))
}

setMethod("sampleNames", signature(object="AssayData"),
          function(object) {
              if (!length(object))
                return(character(0))
              safe.colnames <-
                function(x) if (ncol(x) == 0) character(0) else colnames(x)
              switch(assayDataStorageMode(object),
                     list=safe.colnames(object[[1]]),
                     safe.colnames(object[[ls(object)[1]]]))
          })

setReplaceMethod("sampleNames", signature(object="AssayData", value="list"),
    function(object, value) 
{
    .names_found_unique <- function(names, table)
    {
        ok <- !is.null(names) && all(names %in% table) && 
              !any(duplicated(names))
        if (!ok) {
            txt <- "'sampleNames' replacement list must have unique named elements
                    corresponding to assayData element names"
            stop(paste(strwrap(txt, exdent=2), colapse="\n"))
        }
    }
    mode <- assayDataStorageMode(object)
    switch(mode, lockedEnvironment = {
        .names_found_unique(names(value), ls(object))
        object <- copyEnv(object)
    }, environment = {
        .names_found_unique(names(value), ls(object))
    }, list= {
        .names_found_unique(names(value), names(object))
    })
    for (nm in names(value)) {
        dn <- dimnames(object[[nm]])
        if (is.null(dn))
            dn <- vector("list", length(dim(object[[nm]])))
        dn[[2]] <- value[[nm]]
        dimnames(object[[nm]]) <- dn
    }
    if (mode == "lockedEnvironment")
        assayDataEnvLock(object)
    object
})

setReplaceMethod("sampleNames", signature(object="AssayData", value="ANY"),
    function(object, value)
{
    mode <- assayDataStorageMode(object)
    dims <- switch(mode, lockedEnvironment=, environment = {
        result <- vector("list", length(object))
        names(result) <- ls(object)
        for (nm in ls(object))
            result[[nm]] <- ncol(object[[nm]])
        result
    }, list = {
        lapply(object, ncol)
    })
    
    if (length(dims)==0 && length(value) !=0)
        return(object)                    # early exit; no samples to name
    if (!all(dims==length(value))) {
        txt <- sprintf("'value' length (%d) must equal sample number in AssayData (%d)",
                       length(value), dims[[1]])
        stop(paste(strwrap(txt, exdent=2), collapse="\n"))
    }

    nms <- switch(mode, lockedEnvironment = {
        object <- copyEnv(object)
        ls(object)
    }, environment = ls(object), list = names(object))

    for (nm in nms) {
        dn <- dimnames(object[[nm]])
        if (is.null(dn))
            dn <- vector("list", length(dim(object[[nm]])))
        dn[[2]] <- value
        dimnames(object[[nm]]) <- dn
    }

    if (mode == "lockedEnvironment")
        assayDataEnvLock(object)
    object
})

setMethod("featureNames", signature(object="AssayData"),
          function(object) {
              if (!length(object))
                return(character(0))
              safe.rownames <-
                function(x) if (nrow(x) == 0) character(0) else rownames(x)
              switch(assayDataStorageMode(object),
                     list=safe.rownames(object[[1]]),
                     safe.rownames(object[[ls(object)[1]]]))
          })

setReplaceMethod("featureNames", signature(object="AssayData", value="ANY"),
    function(object, value) 
{
    mode <- assayDataStorageMode(object)
    dims <- switch(mode, lockedEnvironment=, environment = {
        result <- vector("list", length(object))
        names(result) <- ls(object)
        for (nm in ls(object)) result[[nm]] <- nrow(object[[nm]])
        result
    }, list = lapply(object, nrow))
    if (length(dims)==0 && length(value) !=0)
        return(object)                    # early exit; no features to name
    if (!all(dims==length(value))) {
        txt <- sprintf("'value' length (%d) must equal feature number in AssayData (%d)",
                       length(value), dims[[1]])
        stop(paste(strwrap(txt, exdent=2), collapse="\n"))
    }

    nms <- switch(mode, lockedEnvironment = {
        object <- copyEnv(object)
        ls(object)
    }, environment = ls(object), list=names(object))

    for (nm in nms) {
        dn <- dimnames(object[[nm]])
        if (is.null(dn))
            dn <- vector("list", length(dim(object[[nm]])))
        dn[[1]] <- value
        dimnames(object[[nm]]) <- dn
    }

    if (mode == "lockedEnvironment")
        assayDataEnvLock(object)
    object
})

setMethod("combine", c("AssayData", "AssayData"), function(x, y, ...) {
    storage.mode <- assayDataStorageMode(x)
    nmfunc <- if ("environment"==class(x)) ls else names

    if (assayDataStorageMode(y) != storage.mode)
      stop("assayData must have same storage, but are '",
           storage.mode, "', '", assayDataStorageMode(y))
    if (length(nmfunc(x)) != length(nmfunc(y)))
      stop("assayData have different numbers of elements:\n\t",
           paste(nmfunc(x), collapse=" "), "\n\t",
           paste(nmfunc(y), collapse=" "))
    if (!all(nmfunc(x) == nmfunc(y)))
      stop(paste("assayData have different element names:",
                 paste(nmfunc(x), collapse=" "),
                 paste(nmfunc(y), collapse=" "), sep="\n\t"))
    if ("list" == storage.mode) {
        aData <- lapply(names(x), function(nm) combine(x[[nm]],y[[nm]]))
        names(aData) <- names(x)
    } else {
        aData <- new.env(parent=emptyenv())
        for (nm in ls(x)) aData[[nm]] <- combine(x[[nm]], y[[nm]])
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

##FIXME: RG says I don't know if you should ignore non-matrix objects or
## not -  for now I have put in an informative error message
assayDataDims <- function(object) {
  nms <- if (assayDataStorageMode(object) == "list") names(object) else ls(object)
  if (length(nms) == 0)
    return(matrix(integer(0), nrow = 2, ncol = 0, 
                  dimnames = list(c("Features", "Samples"), character(0))))
  d <- sapply(nms, function(i) dim(object[[i]]))
  rownames(d) <- c("Features", "Samples", rep("...", nrow(d)-2))
  colnames(d) <- nms
  d[,order(colnames(d)), drop=FALSE]
}
