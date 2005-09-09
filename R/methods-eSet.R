# ==========================================================================
# eSet Class Validator
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
validEset <- function(object) {
   # add other checks here
   d <- dim(object)
   if (any(d[1,] != d[1,1]))
      return("assayData components have unequal numbers of reporters")
   if (any(d[2,] != d[2,1]))
      return("assayData components have unequal numbers of samples")
   n <- nrow(pData(object))
   nrep <- d[1,1]
   if (any(d[2,] != n))
      return("ncol of assayData != nrow pData")
   if (length(sampleNames(object)) != n)
      return("length of sampleNames != nrow pData")
#   if (length(reporterNames(object)) != nrep)
#      return("length of reporterNames != nrow assayData")
  return(TRUE)
}
# ==========================================================================
setMethod("assayData", "eSet", function(object) object@assayData)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setReplaceMethod("assayData", c("eSet", "listOrEnv"),
   function(object, value) {
      object@assayData <- value
      validObject(object)
      object
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# works for list or env!
setMethod("exprs", "eSet", function(object) object@assayData[["exprs"]])
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setReplaceMethod("exprs", c("eSet", "listOrEnv"),
   function(object, value) {
      .Deprecated("assayData<-", "Biobase")
      object@assayData <- value
      validObject(object)
      object
   }
)
# FIXME: REMOVE THIS OLD IMPLEMENTATION?!
#setReplaceMethod("exprs", "eSet",
#   function(object, value) {
#       object@eList[["exprs"]] = value
#       return(object)
#   })
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("dim", "eSet",
   function(x) {
      isEnv <- is(assayData(x), "environment")
      if (!isEnv)
         tmp <- sapply(assayData(x), dim)
      else {
         enms <- ls(assayData(x))
         tmp <- sapply(enms,function(z) dim(get(z,assayData(x))))
      }
      dimnames(tmp)[[1]] <- c("nrow", "ncol")
      tmp
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("ncol", "eSet",
   function(x) {
      isEnv <- is(assayData(x), "environment")
      if (!isEnv)
         sapply(assayData(x), ncol)
      else {
         enms <- ls(assayData(x))
         sapply(enms,function(z) ncol(get(z,assayData(x))))
      }
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", "eSet",
   function(x, i, j, ..., drop=FALSE) {
      if(length(list(...)) > 0)
         stop("extra subscripts cannot be handled")
      if(missing(j)) {
         pdata <- phenoData(x)
         sn <- sampleNames(x)
      }
      else {
         pdata <- phenoData(x)[j,, ..., drop=FALSE]
         sn <- sampleNames(x)[j]
      }
      if(missing(i))
         rn <- reporterNames(x)
      else
         rn <- reporterNames(x)[i]
      isEnv <- is(assayData(x), "environment")
      if (!isEnv) {
         if (missing(i)) {
            if (missing(j))
               return(x)
            else
               xd <- lapply( assayData(x), function(x)x[,j,drop=drop] )
         }
         else {
            if (missing(j))
               xd <- lapply( assayData(x), function(x)x[i,,drop=drop] )
            else
               xd <- lapply( assayData(x), function(x)x[i,j,drop=drop] )
         }
      }
      else {
         # getting an environment, which is assumed to hold matrices (22 aug 2005)
         enms <- ls(assayData(x))
         xd <- new.env()
         for (nm in enms) {
            if (missing(i)) {
               if (missing(j))
                  return(x)
               else assign(nm, get(nm, env=assayData(x), inherits=FALSE)[,j,drop=drop], env=xd)
            }
            else {
               if (missing(j))
                  assign(nm, get(nm, env=assayData(x), inherits=FALSE)[i,,drop=drop], env=xd)
               else
                  assign(nm, get(nm, env=assayData(x), inherits=FALSE)[i,j,drop=drop], env=xd)
            }
         }
      }
      new("eSet", phenoData=pdata, assayData=xd, reporterNames=rn, sampleNames=sn)
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FIXME: WHAT IS THIS CODE FOR? TEST?
#data(eset)
#pd <- phenoData(eset)
#xl <- list(e1=assayData(eset))
#es2 <- new("eSet", phenoData=pd, assayData=xl)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("show", "eSet",
   function(object) {
      cat("instance of eSet\n")
      cat("assayData component is of class", class(assayData(object)),"\n")
      cat("dimensions of the assayData components:\n")
      print(dim(object))
      show(phenoData(object))
      cat("first reporterNames:\n")
      print(reporterNames(object)[1:min(5,length(reporterNames(object)))])
      cat("first sampleNames:\n")
      print(sampleNames(object)[1:min(5,length(reporterNames(object)))])
   }
)
# ==========================================================================
# following are legacy, seem OK
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("description", signature="eSet", function(object) object@description)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setReplaceMethod("description", "eSet",
   function(object, value) {
      object@description = value
      object
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("annotation", "eSet", definition = function(object) object@annotation)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setReplaceMethod("annotation", signature="eSet",
   definition =  function(object, value) {
      object@annotation <- value
      object
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("notes", signature="eSet", function(object) object@notes)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setReplaceMethod("notes", signature(object="eSet", value="ANY"),
   function(object, value) {
      object@notes <- value
      object
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# VC:

#this check should be done at
# construction time -- if creator doesn't have it right,
# then no eSet is made.  any manipulations should guaranteed
# to preserve name validity
#
##check to see if the vector x is the right length to
##replace the sample names in eSet
##not sure if this should be exported

#eSetValidNames = function(eSet, x) {
#  if( !is.character(x) ) return(FALSE)
#  lenx = length(x)
#  if(nrow(pData(eSet)) != lenx ) return(FALSE)
#  nc = ncol(eSet)
#  if(any(nc != lenx)) return(FALSE)
#  TRUE
#}

##reset the eList col names - not for export

# VC commented this out -- wants to handle names
# directly as slots as much as possible
#seteListColNames = function(eList, names) {
#  if(is.environment(eList@eList) )
#    for( j in ls(eList@eList) )
#       dimnames(j)[[2]] = names
#  else if (is.list(eList@eList) )
#    for( j in eList@eList )
#       dimnames(j)[[2]] = names
#  else
#    stop("invalid eList")
#  eList
#}
#
###should we have column names on the elements of eList
###and if so how do we control their values
#setReplaceMethod("sampleNames", "eSet",
#  function(object, value) {
#     if( !is.character(value) )
#       stop("replacement names must be character strings")
#     if( !eSetValidNames(object, value) )
#       stop("wrong number of sample names")
#     object@sampleNames = value
#     object@eList = seteListColNames(object@eList, names)
#     object
#})

# VC -- leave this stuff to another factor, related to varMetadata
#if( !isGeneric("eMetadata<-") )
#    setGeneric("eMetadata<-", function(object, value)
#               standardGeneric("eMetadata<-"))
#
#setReplaceMethod("eMetadata", c("eSet", "data.frame"),
#     function(object, value) {
#        object@eList@eMetadata <- value
#        object
#     })

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("eList", "eSet",
   function(object) {
      .Deprecated("exprs", "Biobase")
      exprs(object)
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# VC: note the validObject calls -- I do not think setReplaceMethods do constructions
# (next 5 methods)
setReplaceMethod("eList", c("eSet", "listOrEnv"),
   function(object, value) {
      .Deprecated("assayData<-", "Biobase")
      object@eList <- value
      validObject(object)
      object
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("sampleNames", "eSet", function(object) object@sampleNames)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setReplaceMethod("sampleNames", c("eSet", "character"),
   function(object, value) {
      object@sampleNames <- value
      validObject(object)
      object
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("reporterNames", "eSet", function(object) object@reporterNames)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setReplaceMethod("reporterNames", c("eSet", "character"),
   function(object, value) {
      object@reporterNames <- value
      validObject(object)
      object
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## An extractor for named experimental data
## FIXME: how much do we want to do to ensure that these are matrices?
## this FIXME is still valid in the new design!  I am not sure we
## want them to be required to be matrices.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("getExpData", c("eSet", "character"),
   function(object, name) {
      .Deprecated("exprs", "Biobase")
      exprs(object)[[name]]
      #object@eList@eList[[name]]
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# VC:
# combine generic: given an arbitrary number of arguments
# that are eSet instances, combine into a single eSet
# instance in a sensible way.
#
# current restrictions: only works for assayData slots that are
# lists
#
# phenoData parts of combine are now in annotatedDataset.R
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("combine", c("eSet", "eSet"),
   function(x, y, ...) {
      # it would be nice to do a combine on the assayData elements, but
      # exprList is a virtual class...
      if (is.environment(assayData(x)))
         stop("not currently supporting environment-valued assayData")
      if (!(all(c(is(assayData(x), class(assayData(y))), is(assayData(y), class(assayData(x)))))))
         stop("not currently supporting assayData of nonequivalent classes")
      if (!all(names(assayData(x))==names(assayData(y))))
         stop("assayData have different element names")
      o <- list()
      n <- names(assayData(x))
      for (e in n)
         o[[e]] <- cbind(assayData(x)[[e]], assayData(y)[[e]])
      new("eSet", assayData=o,
          phenoData=combine(phenoData(x), phenoData(y)),
          reporterInfo=rbind(reporterInfo(x), reporterInfo(y)))
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("combine", "eSet",
   function(x, y, ...) {
      return(x)
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
