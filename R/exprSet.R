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

  setMethod("[[", "phenoData", function(x, i, j)
      x@pData[[i]], where=where)

  setReplaceMethod("[[", "phenoData", function(x, i, j, ..., value) {
      x@pData[[i]] <- value
      x}, where=where)

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
#RI: define a genric for obtaining the se's
  if( !isGeneric("se.exprs") )
     setGeneric("se.exprs", function(object) standardGeneric("se.exprs"),
 where=where )
 setMethod("se.exprs", "exprSet", function(object) object@se.exprs, where=where)
   
 if( !isGeneric("phenoData") )
     setGeneric("phenoData", function(object)
                standardGeneric("phenoData"), where=where)
 setMethod("phenoData", "exprSet", function(object)
         object@phenoData, where=where )

 setMethod("pData", "exprSet",
           function(object) pData(object@phenoData), where=where)

  if( !isGeneric("pData<-") )
      setGeneric("pData<-", function(object, value)
               standardGeneric("pData<-"), where=where)

  setReplaceMethod("pData", "exprSet", function(object, value) {
    ph<-object@phenoData
     ph@pData <- value
     object@phenoData <- ph
     object
 }, where=where)

###RI: this is a simple a pData replace for phenoData. i need it for affy.
  setReplaceMethod("pData", "phenoData", function(object, value){
    object@pData <- value
    object
  }, where=where)

  
  if( !isGeneric("sampleNames") )
     setGeneric("sampleNames", function(object)
                standardGeneric("sampleNames"), where=where)
 setMethod("sampleNames", "exprSet",
           function(object) row.names(pData(object)), where=where)

 if( !isGeneric("geneNames") )
     setGeneric("geneNames", function(object)
                standardGeneric("geneNames"), where=where)
 setMethod("geneNames", "exprSet", function(object)
     row.names(object@exprs), where=where )

  if( !isGeneric("geneNames<-") )
      setGeneric("geneNames<-", function(object, value)
          standardGeneric("geneNames<-"), where=where)

  setReplaceMethod("geneNames", "exprSet", function(object, value) {
      es <- exprs(object)
      row.names(es) <- value
      object@exprs <- es
      object
  }, where=where)

##a varLabels method for exprSets
  setMethod("varLabels", "exprSet",
            function(object) object@phenoData@varLabels, where=where)

  if( !isGeneric("annotation") )
      setGeneric("annotation", function(object)
                 standardGeneric("annotation"), where=where)
  setMethod("annotation", "exprSet", function(object)
            object@annotation, where=where)

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
            function(object,covlab,f) {
                flist <- f
                llen <- length(flist)
                out <- matrix(NA,nr=nrow(object@exprs), nc=llen )
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
             ## f assumed to be a function of two arguments,
             ## first is a stratum identifier to be used
             ## in evaluating a statistical contrast by f
                varnames <- names(object@phenoData@pData)
                if (!(any(match(covlab,varnames))))
                    stop("the requested covariate is not in the exprSet")
                fc <- function(x) function(y) f(x,y)
                f2app <- fc(object@phenoData@pData[[covlab]])
                iter(object,f=f2app)
            }, where=where)


  ##split
  if( !isGeneric("split") )
        setGeneric("split", where=where)

  setMethod("split", signature(x="exprSet", f="vector"),
            function(x, f) {
                lenf <- length(f)
                exs <- exprs(x)
                pD <- phenoData(x)
                aN <- annotation(x)
                if( (nrow(exs) %% lenf == 0 ) ) {
                    splitexprs <- lapply(split(1:nrow(exs), f),
                                         function(ind) exs[ind, , drop =
                                                           FALSE])
                    nsplit<-length(splitexprs)
                    for(i in 1:nsplit) {
                        splitexprs[[i]] <- new("exprSet",
                                               exprs=splitexprs[[i]],
            phenoData = pD, annotation= aN )
                    }
                    return(splitexprs)
                }  ##split the expressions
                if( (nrow(pData(x)) %% lenf ==0) ) {
                    npD <- split(pD, f)
                    nEx <- lapply(split(1:ncol(exs), f),
                                  function(ind) exs[,ind,drop=FALSE])
                    nsplit <- length(npD)
                    for( i in 1:nsplit)
                        npD[[i]] <- new("exprSet", exprs=nEx[[i]],
                                        phenoData=npD[[i]],
            annotation=aN)
                    return(npD)
                }
                else
                    stop("could not split")
            }, where=where)


  setMethod("split", signature(x="phenoData", f="vector"),
            function(x, f) {
                lenf <- length(f)
                pD <- pData(x)
                vL <- varLabels(x)
                if( (nrow(pD) %% lenf ==0) ) {
                    npD <- split(pD, f)
                    nsplit <- length(npD)
                    for( i in 1:nsplit)
                        npD[[i]] <- new("phenoData", pData = npD[[i]],
                                        varLabels=vL)
                    return(npD)
                }
                else
                    stop("could not split")
            }, where=where)

###write table for exprSet. makes a table with gene names in first column
###chip names in first row
###apart from quote=FALSE and sep="\t"
###everything else is the same as write.table
  if( !isGeneric("write.exprs") )
    setGeneric("write.exprs", function(x,...) standardGeneric("write.exprs"),
              where=where )
  setMethod("write.exprs", signature(x="exprSet"),
            function(x,file = "tmp.txt",
                     append = FALSE, quote = FALSE, sep = "\t",
                     eol = "\n", na = "NA", dec = ".", row.names = TRUE,
                     col.names = TRUE, qmethod = c("escape", "double")) 
            write.table(exprs(x),file = file, append = append,
                        quote = quote,
                        sep = sep,eol = eol, na = na, dec = dec,
                        row.names = row.names, col.names = col.names,
                        qmethod = qmethod),where=where)


}



##not quite the right semantics
##but it is a start

"$.exprSet" <- function(eset, val)
    (pData(eset))[[as.character(val)]]

"$.phenoData" <- function(x, val, ...)
    (pData(x))[[as.character(val)]]

#esApply <- function( es, f ) {
# # assumes f is of the form f(arg1,arg2) and
# # arg2 is es
# if (class(es) != "exprSet") stop("arg1 must be of class exprSet")
# if (length(formals(f)) != 2) warning("f should be a function of two arguments#...")
# apply( exprs(es), 1, f, es )
# }

esApply <- function(X, MARGIN, FUN, ...) {
    if (class(X) != "exprSet") stop("arg1 must be of class exprSet")
    e1 <- new.env(parent=environment(FUN))
    multiassign(names(pData(X)), pData(X), env=e1)
    environment(FUN) <- e1
    apply(exprs(X), MARGIN, FUN, ...)
  }


