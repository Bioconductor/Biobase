##data class for accompanying data
setClass("phenoData", representation(pData="data.frame",
                                     varLabels="list",
                                     varMetadata="data.frame"),
         prototype=list(pData=data.frame(matrix(nr=0,nc=0)),
           varLabels=list(), varMetadata=data.frame(matrix(nr=0,nc=0))),
         validity =  function(object) {
             dm <- dim(object@pData)
             if(dm[2] != length(object@varLabels) )
               return(FALSE)
             return(TRUE)
         }
         )

if( !isGeneric("pData") )
  setGeneric("pData", function(object) standardGeneric("pData"))
setMethod("pData", "phenoData",
          function(object) object@pData)


if( !isGeneric("varLabels") )
  setGeneric("varLabels", function(object)
             standardGeneric("varLabels"))

setMethod("varLabels", "phenoData",
          function(object) object@varLabels)

setMethod("[", "phenoData", function(x, i, j, ..., drop=FALSE) {
    if( missing(drop) ) drop<-FALSE
    vL <- varLabels(x)
    if( missing(j) ) {
        if( missing(i) )
          pD <- x@pData
        else
          pD <- x@pData[i, ,drop=FALSE]
    }
    else {
        vL <- vL[j]
        if(missing(i) )
          pD <- x@pData[,j,drop=drop]
        else
          pD <- x@pData[i, j,drop=FALSE]
    }
    new("phenoData", pData=pD, varLabels=vL)})

setMethod("[[", "phenoData", function(x, i, j, ...)
          x@pData[[i]])

setReplaceMethod("[[", "phenoData", function(x, i, j, ..., value) {
    x@pData[[i]] <- value
    x})

setReplaceMethod("$", "phenoData", function(x, name, value) {
    x@pData[[name]] = value
    x})

setMethod("show", "phenoData",
          function(object) {
              dm <- dim(object@pData)
              cat("\t phenoData object with ", dm[2], " variables",
                  sep="")
              cat(" and ", dm[1], " cases\n", sep="")
              vL <- object@varLabels
              cat("\t varLabels\n")
              nm <- names(vL)
              for(i in seq(along=vL) )
                cat("\t\t", nm[[i]], ": ", vL[[i]], "\n", sep="")
          })


##annotatedDataset, (virtual) Superclass of exprSet, eSet and maybe more later
setClass("annotatedDataset", representation(phenoData="phenoData","VIRTUAL"),
         prototype=list())

setReplaceMethod("$", "annotatedDataset", function(x, name, value) {
    x@phenoData[[name]] = value
    x})

##method for pData
if( !isGeneric("pData") )
  setGeneric("pData", function(object) standardGeneric("pData"))
setMethod("pData", "phenoData",
          function(object) object@pData)


##method for phenoData
if( !isGeneric("phenoData") )
  setGeneric("phenoData", function(object)
             standardGeneric("phenoData"))

setMethod("phenoData", "annotatedDataset", function(object)
          object@phenoData)

if( !isGeneric("phenoData<-") )
  setGeneric("phenoData<-", function(object, value)
             standardGeneric("phenoData<-"))

setReplaceMethod("phenoData", c("annotatedDataset", "phenoData"),
                 function(object, value) {
                     object@phenoData <- value
                     object })

##method for pData
setMethod("pData", "annotatedDataset",
          function(object) pData(phenoData(object)))


##replace method for pData
if( !isGeneric("pData<-") )
  setGeneric("pData<-", function(object, value)
             standardGeneric("pData<-"))

setReplaceMethod("pData", "annotatedDataset", function(object, value) {
    ph <- phenoData(object)
    pData(ph) <- value
    phenoData(object) <- ph
    object
})

##[[ method
setReplaceMethod("[[", "annotatedDataset", function(x, i, j, ..., value) {
    pD <- phenoData(x)
    pD@pData[[i]] <- value
    phenoData(x) <- pD
    x})

setMethod("[[", "annotatedDataset", function(x, i, j, ...)
          phenoData(x)[[i]] )


setReplaceMethod("pData", "phenoData", function(object, value){
    object@pData <- value
    object
})


setMethod("$", "annotatedDataset", function(x, name) {
    (x@phenoData)@pData[[name]]
})

setMethod("$", "phenoData", function(x, name) {
    x@pData[[name]]
})


##a varLabels method for exprSets
setMethod("varLabels", "annotatedDataset",
          function(object) phenoData(object)@varLabels)

## combine generic: given an arbitrary number of arguments
## that are phenoData instances, combine into a single phenoData
## instance in a sensible way.
##
##  used to be in eSet.R, phenoData specific combines are now here

setGeneric("combine", function(x, y, ...)
           {
               if (length(list(...)) > 0)
                 combine( x, combine( y, combine(...) ) )
               else standardGeneric("combine")
           })

setMethod("combine", c("phenoData", "phenoData"), function(x, y, ...)
          {
              ##
              ## merge here will reproduce rows
              ##
              nl <- varLabels(x)
              if (dim(pData(x))[2] == dim(pData(y))[2] && all(names(pData(x))==names(pData(y))))
                {
                    npd <- rbind(pData(x), pData(y))
                }
              else
                {
                    alln <- union(nx <- names(dx <- pData(x)), ny <- names(dy <- pData(y)))
                    if (length(xx <- setdiff(alln,nx))>0)
                      for (i in 1:length(xx)) dx[[ xx[i] ]] <- NA
                    if (length(xx <- setdiff(alln,ny))>0)
                      for (i in 1:length(xx)) dy[[ xx[i] ]] <- NA
                    npd <- rbind(dx,dy)
                    allvl <- list()
                    nvl1 <- names(varLabels(x))
                    nvl2 <- names(varLabels(y))
                    for (i in 1:length(varLabels(x))) allvl[[ nvl1[i] ]] <- varLabels(x)[[i]]
                    for (i in 1:length(varLabels(y))) allvl[[ nvl2[i] ]] <- varLabels(y)[[i]]
                    nl <- list()
                    for (i in 1:ncol(dx)) nl[[ names(dx)[i] ]] <- allvl[[ names(dx)[i] ]]
                }
              new("phenoData", pData=npd, varLabels=nl)
          })

setMethod("combine", "phenoData", function(x, y, ...)
          {
              return(x)
          })
