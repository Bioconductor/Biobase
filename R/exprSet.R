# A class for microarray data

# in this representation we think of the data set being comprised of
#  matrix slot exprs: a collection of array results organized as a
#  matrix, with genes defining rows and samples defining columns.
#  data.frame slot phenoData: in the customary organization of samples
# defining rows and variables or features defining columns.  thus
# if x is an exprSet, nrow(x@phenodata) == ncol(x@exprs)
#  * character slot description: unconstrained string with information about
# the exprSet

require(methods)

.initExprset <- function(where) {

##data class for accompanying data
  setClass("phenoData", representation(pData="data.frame",
                                       varLabels="list"),
  where=where)

  if( !isGeneric("pData") )
      setGeneric("pData", function(object) standardGeneric("pData"),
                 where=where )
 setMethod("pData", "phenoData",
           function(object) object@pData, where=where)

  if( !isGeneric("varLabels") )
      setGeneric("varLabels", function(object)
      standardGeneric("varLabels"), where=where)

  setMethod("varLabels", "phenoData",
            function(object) object@varLabels, where=where)

  setMethod("[", "phenoData", function(x, i, j, ..., drop=FALSE) {
      if( missing(drop) ) drop<-FALSE
      vL <- varLabels(x)
      if( missing(j) ) {
          if( missing(i) )
              pD <- x@pData
          else
              pD <- x@pData[i, ]
     }
      else {
          vL <- vL[j]
          if(missing(i) )
              pD <- x@pData[,j,drop=drop]
         else
             pD <- x@pData[i, j]
      }
      new("phenoData", pData=pD, varLabels=vL)}, where=where)

  setMethod("show", "phenoData",
            function(object, printTo=stdout(), oldMethods = TRUE) {
                dm <- dim(object@pData)
                cat("\t phenoData object with ", dm[2], " variables",
            sep="")
                cat(" and ", dm[1], " cases\n", sep="")
                vL <- object@varLabels
                cat("\t varLabels\n")
                nm <- names(vL)
                  for(i in 1:length(vL) )
                    cat("\t\t", nm[[i]], ": ", vL[[i]], "\n", sep="")
            }, where=where)

  validphenoData <- function(object) {
      dm <- dim(object@pData)
      if(dm[2] != length(object@pheonLabels) )
          return(FALSE)
      return(TRUE)
  }
  setValidity("phenoData", validphenoData)

##data class for expression arrays

  setClass("exprSet", representation(exprs="matrix",
                                   se.exprs = "matrix",
                                   phenoData="phenoData",
                                   description="character",
                                   notes="character") , where=where)

#define a generic for obtaining the data
 if( !isGeneric("exprs") )
     setGeneric("exprs", function(object) standardGeneric("exprs"),
 where=where )
 setMethod("exprs", "exprSet", function(object) object@exprs, where=where)

 if( !isGeneric("phenoData") )
     setGeneric("phenoData", function(object)
                standardGeneric("phenoData"), where=where)
 setMethod("phenoData", "exprSet", function(object)
         object@phenoData, where=where )

 if( !isGeneric("sampleNames") )
     setGeneric("sampleNames", function(object)
                standardGeneric("sampleNames"), where=where)
 setMethod("sampleNames", "exprSet",
           function(object) row.names(phenoData), where=where)

 if( !isGeneric("geneNames") )
     setGeneric("geneNames", function(object)
                standardGeneric("geneNames"), where=where)
 setMethod("geneNames", "exprSet", function(object)
     row.names(object@exprs), where=where )

 setMethod("[", "exprSet", function(x, i, j, ..., drop=TRUE) {

     pdata <- x@phenoData[j,, ..., drop=FALSE]
     if(missing(j) ) {
         if( missing(i) )
             nexprs <- x@exprs
         else
             nexprs <- x@exprs[i, ]
     }
     else {
         if( missing(i) )
             nexprs <- x@exprs[,j]
         else
             nexprs <- x@exprs[i, j]
     }
     new("exprSet", exprs=nexprs, phenoData = pdata,
     description=x@description,
     notes=x@notes)}, where=where)

 setMethod("show", "exprSet", function(object, printTo = stdout(),
     oldMethods = TRUE ) {
     dm <-dim(object@exprs)
     ngenes <- dm[1]
     nsamples <- dm[2]
     cat("Expression Set (exprSet) with \n\t", ngenes, " genes\n\t", sep="")
     cat(nsamples, "samples\n\t")
     show(object@phenoData)
 }, where=where)

# if( !isGeneric("plot") )
#    setGeneric("plot")

# setMethod("plot", "uarray", function(object, ...) {
#     expr <- as.matrix(uexpr(object))
#     #scale
#     expr <- sweep(expr, 1, apply(expr, 1, mean, na.rm = TRUE))
#     f <- function(v) {
#         v <- v[!is.na(v)]
#         sqrt(sum(v^2)/max(1, length(v) - 1))
#     }
#     expr <- sweep(expr, 1, apply(expr, 1, f), "/")
#     breaks <- seq(-3,3,by=.2)
#     colors<- GetColor(breaks)
#     breaks <- c(-100,breaks,100)
#     colors <- c(colors[1], colors)
#     opar<-par(mar=c(1,1,4,10))
#     on.exit(par(mar=opar))
#     image(1:ncol(expr), 1:nrow(expr), z = t(expr), axes = F,
#     col=colors, breaks=breaks, xlab="", ylab="")
#     axis(3, at=1:ncol(expr), labels=samplenames(object),tick=FALSE)
#     axis(4, at=1:nrow(expr), labels=genenames(object), tick=FALSE, las=1)
# })




}

