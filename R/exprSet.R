# A class for microarray data

# in this representation we think of the data set being comprised of
#  matrix slot exprs: a collection of array results organized as a
#  matrix, with genes defining rows and samples defining columns.
#  data.frame slot phenoData: in the customary organization of samples
# defining rows and variables or features defining columns.  thus
# if x is an exprSet, nrow(x@phenodata) == ncol(x@exprs)
#  * character slot description: unconstrained string with information about
# the exprSet

#
# it appears that the covariates slot is a list of labels
# giving details on elements of phenodata

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
              pD <- x@pData[i, ,drop=FALSE]
     }
      else {
          vL <- vL[j]
          if(missing(i) )
              pD <- x@pData[,j,drop=drop]
         else
             pD <- x@pData[i, j,drop=FALSE]
      }
      new("phenoData", pData=pD, varLabels=vL)}, where=where)

  setMethod("show", "phenoData",
            function(object) {
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
      if(dm[2] != length(object@phenoLabels) )
          return(FALSE)
      return(TRUE)
  }
  setValidity("phenoData", validphenoData)

##data class for expression arrays

  setClass("exprSet", representation(exprs="matrix",
                                   se.exprs = "matrix",
                                   phenoData="phenoData",
                                   description="character",
                                   annotation="character",
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

##a varLabels method for exprSets
  setMethod("varLabels", "exprSet",
            function(object) object@phenoData@varLabels, where=where)

  if( !isGeneric("annotation") )
      setGeneric("annotation", function(object)
                 standardGeneric("annotation"), where=where)
  setMethod("annotation", "exprSet", function(object)
            object@annotation, where=where)

#<<<<<<< exprSet.R
# if( !isGeneric("covariates") )
#     setGeneric("covariates", function(object)
#                standardGeneric("covariates"), where=where)
# setMethod("covariates", "exprSet", function(object)
#           object@covariates, where=where)
#
# setMethod("[", "exprSet", function(x, i, j, ..., drop=TRUE)
#     {
#     if (!missing(j))
#          {
#          newc <- list()
#          cn <- names(x@covariates)
#          for (cl in j)
#            newc[[cn[cl]]] <- covariates[[cl]]
#          }
#     else newc <- x@covariates
#     if (missing(j)) j <- 1:nrow(x@phenodata)
#     new("exprSet", exprs=x@exprs[i,j], phenodata = x@phenodata[j,,drop=FALSE],
#     description=x@description,
#      covariates=x@covariates)}, where=where)
#
# setMethod("print", "exprSet", function(x, ...) {
#     ngenes <- nrow(x@exprs)
#     dmp <- dim(x@phenodata)
#     nsamples <- dmp[1]
#     ncovs <- dmp[2]
#=======
 setMethod("[", "exprSet", function(x, i, j, ..., drop=TRUE) {
# why drop=TRUE? VC 12/21/01
     pdata <- phenoData(x)[j,, ..., drop=FALSE]
     if(missing(j) ) {
         if( missing(i) )
             nexprs <- exprs(x)
         else
             nexprs <- exprs(x)[i, ,drop=FALSE]
     }
     else {
         if( missing(i) )
             nexprs <- exprs(x)[,j, drop=FALSE]
         else
             nexprs <- exprs(x)[i, j, drop=FALSE]
     }
     new("exprSet", exprs=nexprs, phenoData = pdata,
     description=x@description,
     notes=x@notes)}, where=where)

 setMethod("show", "exprSet", function(object ) {
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


# this material is probably ready to go in
# but where statements need to be added
# and doc needs to be written
## these are new functions completely
#
setGeneric("iter", function(object, covlab, f) standardGeneric("iter"),
     where=where)
#
setMethod("iter", signature(object="exprSet", covlab="missing",
                            f="function"),
 function(object, covlab, f)
   apply(exprs(object), 1, f), where=where)
#
setMethod("iter", signature(object="exprSet", covlab="missing",
 f="list"),
function(object,covlab,f)
 {
 flist <- f
 out <- matrix(NA,nr=nrow(object@exprs),nc=llen <- length(flist))
 lnames <- names(flist)
 if(is.null(lnames)) lnames <- paste("l",1:llen,sep="")
 for (i in 1:llen)
   out[,i] <- apply(exprs(object),1,flist[[i]])
 dimnames(out) <- list(row.names(exprs(object)),lnames)
 out
 }, where=where)

setMethod("iter", signature(object="exprSet", covlab="character",
 f="function"),
 function(object, covlab, f) {
  # f assumed to be a function of two arguments,
  # first is a stratum identifier to be used
  # in evaluating a statistical contrast by f
  varnames <- names(object@phenoData@pData)
  if (!(any(match(covlab,varnames))))
      stop("the requested covariate is not in the exprSet")
  fc <- function(x) function(y) f(x,y)
  f2app <- fc(object@phenoData@pData[[covlab]])
  iter(object,f=f2app)
  }, where=where)

#
# the following stuff will go to genefilter
#
## this function already exists, would not need invocation of
## standardGeneric except that we are changing arg list
#
#rank.default <- get("rank", "package:base")
#setGeneric("rank", function(object,f) standardGeneric("rank"))
#setMethod("rank", c("exprSet", "function"), function(object,f) {
# rank.default(iter(object,f)) })
#
#setGeneric("tissueResamp", function(object) standardGeneric("tissueResamp"))
#setMethod("tissueResamp", "exprSet", function(object) {
# nsamp <- ncol(object@exprs)
# sel <- sample(1:nsamp, replace=T)
# object[,sel]
# })
#
#setGeneric("rankDistn",
#   function(object,f,B) standardGeneric("rankDistn"))
#setMethod("rankDistn",
# c("exprSet","function", "numeric") ,function(object,f,B) {
# ng <- nrow(object@exprs)
# out <- matrix(NA,nr=ng,nc=B)
# for (i in 1:B)
#  out[,i] <- rank(tissueResamp(object),f)
# out
# })


#


}

#with.exprSet <- function(data, expr, ...) eval(substitute(expr),
#  data@phenoData@pData, enclos=parent.frame())


esApply <- function( es, f ) {
 # assumes f is of the form f(arg1,arg2) and
 # arg2 is es
 if (class(es) != "exprSet") stop("arg1 must be of class exprSet")
 if (length(formals(f)) != 2) warning("f should be a function of two arguments...")
 apply( exprs(es), 1, f, es )
 }


