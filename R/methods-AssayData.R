assayDataNew <- function(storage.mode = c("lockedEnvironment", "environment", "list"), ...) {
  storage.mode <- match.arg(storage.mode) # defaults to "lockedEnvironment"
  assayData <- switch(storage.mode,
                      lockedEnvironment =,
                      environment = new.env(parent=emptyenv()),
                      list = list())
  arglist <- list(...)
  for (nm in names(arglist)) assayData[[nm]] <- arglist[[nm]]
  adim <- assayDataDim(assayData)
##   if (length(adim)!=1 && !is.na(adim)) {
##     if (adim[[1]]>0 && all(sapply(featureNames(assayData), is.null)))
##       featureNames(assayData) <- 1:adim[[1]]
##     if (adim[[2]]>0 && all(sapply(sampleNames(assayData), is.null)))
##       sampleNames(assayData) <- 1:adim[[2]]
##   }
  if (storage.mode == "lockedEnvironment") assayDataEnvLock(assayData)
  assayData
}

assayDataValidMembers <- function(assayData, required) {
  names <- if (is(assayData, "environment")) ls(assayData) else names(assayData)
  absent <- required[ !(required %in% names)]
  if (length(absent) == 0)
    TRUE
  else
    paste("missing '", absent ,"' in assayData" , sep = "", collapse = "\n\t")
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
  current.mode <- assayDataStorageMode(object)
  if (current.mode == value) return(object)
  if (is(object,"environment")) {
    nms <- ls(object)
    object <- lapply(object,function(elt) elt)
    names(object) <- nms
  }  
  do.call("assayDataNew", c(storage.mode=value, object))
}

setReplaceMethod("storageMode", c("AssayData", "character"), assayDataStorageModeReplace)

assayDataEnvLock <- function(assayData) lockEnvironment(assayData, bindings=TRUE)

assayDataElementNames <- function(obj)
  if (is(assayData(obj),"environment")) ls(assayData(obj)) else names(assayData(obj))

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

setMethod("sampleNames", "AssayData", function(object) sapply(object, colnames))

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

setMethod("featureNames", "AssayData", function(object) sapply(object, rownames))

setReplaceMethod("featureNames", "AssayData", function(object, value) {
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
  combineElement <- function(x,y) {
    d <- dim(x)
    if (length(d) != length(dim(y)))
      stop("assayData elements have different dimension lengths: ",
           d, ", ", dim(y))
    if (!all({d==dim(y)}[-2]))
        stop("assayData elements have incompatible dimensions: ",
             d, ", ", dim(y))
    if (2==length(d))
      cbind(x,y)
    else if (3==length(d)) {
      d <- dim(x)
      xd <- dim(x)[[2]]; yd <- dim(y)[[2]]
      d[[2]] <- xd + yd
      obj <- array(0,d)
      if (xd != 0) obj[,1:xd,] <- x
      if (yd != 0) obj[,(xd+1):(xd+yd),] <- y
      rownames(obj) <- rownames(x)
      colnames(obj) <- c(colnames(x),colnames(y))
      obj
    } else {
      stop("Cannot combine AssayData with dim length ", length(dim(x)))
    }}
  storage.mode <- assayDataStorageMode(x)
  nmfunc <- if ("environment"==class(x)) ls else names

  if (assayDataDim(x)[[1]] != assayDataDim(y)[[1]])
    stop("objects have different numbers of features or targets: ",
         assayDataDim(x)[[1]], ", ", assayDataDim(y)[[1]])
  if (assayDataStorageMode(y) != storage.mode)
    stop(paste("assayData must have same storage, but are ",
               storage.mode, ", ", assayDataStorageMode(y), sep=""))
  if (length(nmfunc(x)) != length(nmfunc(y)))
    stop("assayData have different numbers of elements:\n\t",
         paste(nmfunc(x), collapse=" "), "\n\t",
         paste(nmfunc(y), collapse=" "))
  if (!all(nmfunc(x)==nmfunc(y)))
    stop(paste("assayData have different element names:",
               paste(nmfunc(x), collapse=" "),
               paste(nmfunc(y), collapse=" "), sep="\n\t"))
  if ("list"==storage.mode) {
    aData <- lapply(names(x), function(nm) combineElement(x[[nm]],y[[nm]]))
    names(aData) <- names(x)
  } else {
    aData <- new.env(parent=emptyenv())
    for (nm in ls(x)) aData[[nm]] <- combineElement(x[[nm]],y[[nm]])
    if ("lockedEnvironment" == storage.mode) assayDataEnvLock(aData)
  }
  aData
})

assayDataDim <- function(object) {
  nms <- if(is(object,"environment")) ls(object) else names(object)
  if ( length( nms ) == 0 ) return( NA )
  d <- dim( object[[ nms[[1]] ]])
  names(d) <- c( "Rows", "Samples", rep("...", max(length(d)-2, 0)))
  d
}

assayDataDims <- function( object ) {
  dims <- sapply( object, dim )
  if (length(dims) == 0) return( NA )
  rownames(dims) <- c("Rows", "Samples", rep("...", max(dim(dims)[[1]]-2, 0)))
  dims[,order(colnames(dims))]
  dims
}
