# ==========================================================================
# phenoData class validator
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
validator.phenoData <- function(object) {
    msg <- NULL
    if (!isCurrent(object))
        msg <- c(msg, "missing 'Versioned' base class; use updateObject")
    dm <- dim(object@pData)
    if(dm[2] != length(object@varLabels) )
        msg <- c(msg,"number of varLabels not equal to number of columns of pData data.frame")
    if (is.null(msg)) TRUE else msg
}

setMethod("updateObject", signature(object="phenoData"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object='phenoData')")
              object <- callNextMethod()
              if (isVersioned(object) && isCurrent(object)["phenoData"]) return(object)
              new("phenoData",
                  pData = updateObject(pData(object), ..., verbose=verbose),
                  varLabels = updateObject(varLabels(object), ..., verbose=verbose),
                  varMetadata = updateObject(varMetadata(object), ..., verbose=verbose))
          })

# ==========================================================================
setMethod("pData", "phenoData", function(object) object@pData)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setReplaceMethod("pData", "phenoData",
   function(object, value) {
      object@pData <- value
      object
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("$", "phenoData",
   function(x, name) {
      x@pData[[name]]
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setReplaceMethod("$", "phenoData",
   function(x, name, value) {
      x@pData[[name]] = value
      x
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("varLabels", "phenoData", function(object) object@varLabels)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", "phenoData", function(x, i, j, ..., drop=FALSE) {
      if( missing(drop) ) drop<-FALSE
      vL <- varLabels(x)
      if( missing(j) ) {
         if( missing(i))
            pD <- x@pData
         else
            pD <- x@pData[i, ,drop=FALSE]
      }
      else {
         vL <- vL[j]
         if(missing(i))
            pD <- x@pData[,j,drop=drop]
         else
            pD <- x@pData[i, j,drop=FALSE]
      }
      new("phenoData", pData=pD, varLabels=vL)
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[[", "phenoData", function(x, i, j, ...) x@pData[[i]])
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setReplaceMethod("[[", "phenoData",
   function(x, i, j, ..., value) {
      x@pData[[i]] <- value
      x
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("show", "phenoData",
   function(object) {
      dm <- dim(object@pData)
      cat("\tphenoData object with ", dm[2], " variables", sep="")
      cat(" and ", dm[1], " cases\n", sep="")
      vL <- object@varLabels
      cat("\tvarLabels\n")
      nm <- names(vL)
      for(i in seq(along=vL) )
         cat("\t\t", nm[[i]], ": ", vL[[i]], "\n", sep="")
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# given an arbitrary number of arguments that are phenoData instances,
# combine into a single phenoData instance in a sensible way.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("combine", c("phenoData", "phenoData"),
   function(x, y, ...) {
      ## merge here will reproduce rows
      nl <- varLabels(x)
      if (dim(pData(x))[2] == dim(pData(y))[2] && all(names(pData(x))==names(pData(y))))
         npd <- rbind(pData(x), pData(y))
      else {
         alln <- union(nx <- names(dx <- pData(x)), ny <- names(dy <- pData(y)))
         if (length(xx <- setdiff(alln,nx))>0)
            for (i in 1:length(xx))
               dx[[ xx[i] ]] <- NA
         if (length(xx <- setdiff(alln,ny))>0)
            for (i in 1:length(xx))
               dy[[ xx[i] ]] <- NA
         npd <- rbind(dx,dy)
         allvl <- list()
         nvl1 <- names(varLabels(x))
         nvl2 <- names(varLabels(y))
         for (i in 1:length(varLabels(x)))
            allvl[[ nvl1[i] ]] <- varLabels(x)[[i]]
         for (i in 1:length(varLabels(y)))
            allvl[[ nvl2[i] ]] <- varLabels(y)[[i]]
         nl <- list()
         for (i in 1:ncol(dx))
            nl[[ names(dx)[i] ]] <- allvl[[ names(dx)[i] ]]
      }
      new("phenoData", pData=npd, varLabels=nl)
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("combine", "phenoData",
   function(x, y, ...) {
      return(x)
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("varMetadata", "phenoData",
   function(object) {
      lk <- try(slot(object, "varMetadata"), silent=TRUE)
      if (inherits(lk, "try-error"))
         object@varMetadata <- data.frame()
      object@varMetadata
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FIXME - it should be a replace method, otherwise it is not clear how should it work
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("addVarMetadataEntry", c("phenoData", "character", "character", "ANY"),
   function(object, vname, attname, attval) {
       .Deprecated()
      if (attname == "varName")
         stop("varName should not be used as a metadata attribute name")
      vm <- varMetadata(object)
      tmp <- list(attval)
      names(tmp) <- attname
      tmp <- data.frame(tmp)
      tmp <- cbind(data.frame(varName=vname), tmp)
      if (nrow(vm)==0) {
         object@varMetadata <- tmp
         return(object)
      }
      nm <- merge(vm, tmp, all=TRUE)
      object@varMetadata <- nm
      object
  }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("getVarMetadata", c("phenoData", "character", "character"),
   function(object, vname, attname) {
      vm <- varMetadata(object)
      row <- which(vm$varName == vname)
      if (length(row)==0)
         stop(paste(vname, "not present in varMetadata of object"))
      col <- which( names(vm) == attname )
      if (length(col)==0)
         stop(paste(attname, "not an attribute of varMetadata of object"))
      vm[row,col]
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("getVarMetadata", c("phenoData", "character", "missing"),
   function(object, vname, attname) {
      vm <- varMetadata(object)
      row <- which(vm$varName == vname)
      if (length(row)==0)
         stop(paste(vname, "not present in varMetadata of object"))
      vm[row,]
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("getUnits", c("phenoData", "character"),
   function(object,vname) {
       .Deprecated()
      getVarMetadata(object, vname, "units")
   }
)
# --------------------------------------------------------------------------
setMethod("convertVarLabels", "phenoData",
   function(object) {
      if (length(object@varLabels) == 0) {
         warning("no varLabels to convert")
         return(object)
      }
      vn <- names(object@varLabels)
      vv <- as.character(unlist(object@varLabels))
      tmp <- data.frame(varName=vn, varLabels=vv)
      vm <- varMetadata(object)
      if (nrow(vm) == 0)
         object@varMetadata <- tmp
      else
         object@varMetadata <- merge(vm, tmp, all=TRUE)
      object
   }
)
# ==========================================================================
# function to coerce between data.frame and phenoData
# some checking of varLabels - none of varMetadata
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
df2pD = function(x, varLabels, varMetadata) {
   if( !is.data.frame(x) )
      stop("x must be a data.frame")
   if( missing(varLabels) ) {
      varLabels = as.list(rep("not initialized", ncol(x)))
      names(varLabels) = names(x)
   }
   if( length(varLabels) != ncol(x) )
      stop("incorrect length of varLabels")
   if(missing(varMetadata) )
      varMetadata = data.frame(matrix(nr=0, nc=0))
   new("phenoData", pData=x, varLabels=varLabels,
      varMetadata=varMetadata)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# not for export
tdf2pD = function(from) df2pD(from)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
read.pD = function(filename = NULL, ...) {
   if( is.character(filename) || inherits(filename, "connection") )
      df = read.table(filename, ...)
   else
      stop("incorrect file")
   df2pD(df)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
read.phenoData <- function(filename = NULL, sampleNames = NULL,
                           widget = getOption("BioC")$Base$use.widgets,...) {

   if(widget) {
      require("tkWidgets") || stop("Package tkWidgets unavailable")
      if(!is.null(filename))
         # defined in tkWidgets
         phenoD <- importPhenoData(fileName = filename, sampleNames = sampleNames, from = "file")
      else
         # defined in tkWidgets
         phenoD <- importPhenoData(sampleNames = sampleNames)
      return(phenoD)
   }
   else {
      if(is.character(filename) || inherits(filename, "connection")) {
         pData <- read.table(filename,...)
         if(!is.null(sampleNames))
            row.names(pData) <- sampleNames
         varLabels <- as.list(rep("read from file",ncol(pData)))
         names(varLabels) <- names(pData)
         return(new("phenoData",pData=pData,varLabels=varLabels))
      }
      else {
         if( !is.null(filename) )
            stop("incorrect filename given")
         if(is.null(sampleNames)){
            return(new("phenoData")) ##return a blank!
         } else {
            pdata <- data.frame(sample=1:length(sampleNames), row.names=sampleNames)
            return(new("phenoData",pData=pdata, varLabels=list(sample="arbitrary numbering")))
         }
      }
   }
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setAs("data.frame", "phenoData", def=tdf2pD)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
