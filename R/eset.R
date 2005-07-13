##Copyright R. Gentleman, 2003, all rights reserved

##the new, more general exprSet class
##a list/set/hash of matrices of dimension N by K and a
##single data frame of dimension K by P
##there are K labeled samples. For Affy this is the same as K arrays
##but for cDNA it is different.

if (!isClass("exprListNM"))  # no metadata
    setClassUnion("exprListNM", c("list", "environment"))

if (!isClass("exprList"))
    setClass("exprList", representation(eMetadata="data.frame"),
		contains="exprListNM")

if(!isGeneric("eMetadata")) setGeneric("eMetadata",
	function(object) standardGeneric("eMetadata"))
setMethod("eMetadata", "exprList", function(object)
	object@eMetadata)

setMethod("show", "exprListNM", function(object) {
 cat("metadata-free exprList; use object@eList@.Data to view") })
setMethod("show", "exprList", function(object) {
 cat("exprList object with ")
 if (is.list(object@.Data)) cat(length(object@.Data), " list element(s).")
 else if (is.environment(object@.Data)) 
	cat(length(ls(object@.Data)), "environment elements.")
 cat("\nexprList level metadata:\n ")
 show(eMetadata(object))
})

if (!isGeneric("eMetadata")) setGeneric("eMetadata",
	function(object) standardGeneric("eMetadata"))

##just the two slots, everyone else extends this class
setClass("eSet", representation(eList="exprList"),
         contains=c("annotatedDataset"))

setMethod("eMetadata", c("eSet"), function(object) {
	object@eList@eMetadata })
#
# some concepts: getExpData will look for a component named
# "exprs" in the list or environment, and this is central
# to the show/exprs method
#


if( !isGeneric("eMetadata<-") )
    setGeneric("eMetadata<-", function(object, value)
               standardGeneric("eMetadata<-"))

setReplaceMethod("eMetadata", c("eSet", "data.frame"),
		function(object, value) {
			object@eList@eMetadata <- value
			object
		})

if (!isGeneric("eList"))
 setGeneric("eList", function(object)standardGeneric("eList"))

setMethod("eList", "eSet", function(object) object@eList)

if( !isGeneric("eList<-") )
    setGeneric("eList<-", function(object, value)
               standardGeneric("eList<-"))

setReplaceMethod("eList", c("eSet", "exprList"),
                   function(object, value) {
                       object@eList <- value
                       object })

setReplaceMethod("eList", c("eSet", "environment"),
                   function(object, value) {
                       object@eList <- value
                       object })

setReplaceMethod("eList", c("eSet", "list"),
                   function(object, value) {
                       object@eList <- value
                       object })

##an extractor for specific experimental data

setGeneric("getExpData", function(object, name)
           standardGeneric("getExpData"))

##FIXME: how much do we want to do to ensure that these are matrices?
setMethod("getExpData", c("eSet", "character"),
          function(object, name) {
              if( is.list(object@eList))
                  object@eList[[name]]
              else if( is.environment(object@eList) ){
                  ans <- try(get(name, env=object@eList,
                                 inherits=FALSE) )
                  if(inherits(ans, "try-error"))
                      return(NULL)
                  ans
              }
          })

setMethod("exprs", "eSet",
          function(object) getExpData(object, "exprs")
          )

##define a subset operator - needs to operate on all matrices
setMethod("[", "eSet", function(x, i, j, ..., drop=FALSE) {
    if( length(list(...)) > 0 )
        stop("extra subscripts cannot be handled")
    if( missing(j) )
	pdata <- phenoData(x)
    else
        pdata <- phenoData(x)[j,, ..., drop=FALSE]
    eList <- x@eList
    isList <- is.list(eList)

    if( isList )
        eNames <- names(eList)
    else {
        eNames <- ls(eList)
        outEnv <- new.env()
    }
    if(missing(j) ) {
        if( missing(i) ) {
            neList <- eList
        }
        else {
            for(nm in eNames) {
                if( isList )
                    eList[[nm]] <- eList[[nm]][i, ,drop=FALSE]
                else
                    assign(nm,
                           get(nm, env=eList, inherits=FALSE)[i, , drop=FALSE],
                           env=outEnv)
            }
        }
    }
    else {
        if( missing(i) ) {
            for(nm in eNames) {
                if( isList )
                    eList[[nm]] <- eList[[nm]][,j, drop=FALSE]
                else
                    assign(nm,
                           get(nm, env=eList,
                               inherits=FALSE)[, j, drop=FALSE],
                           env=outEnv)
            }
        }
        else {
            for(nm in eNames) {
                if( isList )
                    eList[[nm]] <- eList[[nm]][i, j, drop=FALSE]
                else
                    assign(nm,
                           get(nm, env=eList,
                               inherits=FALSE)[i, j, drop=FALSE],
                           env=outEnv)
            }
        }
    }
    if( isList )
        eList(x) <- eList
    else
        eList(x) <- outEnv

    phenoData(x) <- pdata
    x
})

setMethod("show", "eSet", function(object ) {
    dm <-dim(getExpData(object, "exprs"))
    ngenes <- dm[1]
    nsamples <- dm[2]
    cat("New Expression Set (eSet) with \n\t", ngenes, " genes\n\t", sep="")
    cat(nsamples, "samples\n\t")
    show(phenoData(object))
})

#
# combine generic: given an arbitrary number of arguments
# that are eSet instances, combine into a single eSet
# instance in a sensible way.
#
# current restrictions: only works for eList slots that are
# lists
#
# phenoData parts of combine are now in annotatedDataset.R

setMethod("combine", c("eSet", "eSet"), function(x, y, ...)
 {
#
# it would be nice to do a combine on the eList elements, but
# exprList is a virtual class...
#
 if (is.environment(eList(x))) stop("not currently supporting environment-valued eLists")
 if (!(any( c(is(x, class(eList(y))), is(y, class(eList(x)))) ) ) )
     stop("not currently supporting eLists of different classes")
 if (!all(names(eList(x))==names(eList(y)))) stop("eLists have different element names")
 o <- list()
 n <- names(eList(x))
 for (e in n) o[[e]] <- cbind(eList(x)[[e]], eList(y)[[e]])
 new("eSet", eList=o,
             phenoData=combine(phenoData(x), phenoData(y)))
 })

setMethod("combine", "eSet", function(x, y, ...)
 {
 return(x)
 })

