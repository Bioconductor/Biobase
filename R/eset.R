##Copyright R. Gentleman, 2003, all rights reserved

##the new, more general exprSet class
##a list/set/hash of matrices of dimension N by K and a
##single data frame of dimension K by P
##there are K labeled samples. For Affy this is the same as K arrays
##but for cDNA it is different.

if (!isClass("exprList"))
    setClassUnion("exprList", c("list", "environment"))


##just the two slots, everyone else extends this class
setClass("eSet", representation(eList="exprList",
                                phenoData="phenoData"))


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

"$.eSet" <- function(eset, val)
    (pData(eset))[[as.character(val)]]

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


##For now we duplicate code - but I would like to make both
##$ and [[ work for environments
##FIXME: this does not seem to work - should explore it
"$.environment" <- function(x, name) {
    print("howdy")
    get(as.character(name), env=x)
}

###################################################################
## Pheno data handling code
###################################################################
 if( !isGeneric("phenoData") )
    setGeneric("phenoData", function(object)
               standardGeneric("phenoData"))

  setMethod("phenoData", "eSet", function(object)
            object@phenoData)

  if( !isGeneric("phenoData<-") )
    setGeneric("phenoData<-", function(object, value)
               standardGeneric("phenoData<-"))

  setReplaceMethod("phenoData", c("eSet", "phenoData"),
                   function(object, value) {
                       object@phenoData <- value
                       object })

  ##method for pData
  setMethod("pData", "eSet",
            function(object) pData(phenoData(object)))


  ##replace method for pData
  if( !isGeneric("pData<-") )
    setGeneric("pData<-", function(object, value)
               standardGeneric("pData<-"))

  setReplaceMethod("pData", "eSet", function(object, value) {
    ph <- phenoData(object)
    pData(ph) <- value
    phenoData(object) <- ph
    object
  })



