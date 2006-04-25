# ==========================================================================
# exprSet Class Validator
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
validExprSet <- function(object) {
  ## add other checks here
  if (!is(object, "exprSet"))
    return(paste("cannot validate object of class", class(object)))
  if (dim(exprs(object))[2] != nrow(pData(object)))
    return("number of exprs columns different from number of pData rows")
  if (!identical(sampleNames(object), rownames(pData(object))))
    return("sampleNames different from names of phenoData rows")
##   if (!is.null(reporterInfo(object)) && dim(exprs(object))[1] != nrow(reporterInfo(object)))
##       return("number of exprs and reporterInfo rows differ")
  return(TRUE)
}
# ==========================================================================
# method to update exprSet from previous versions
setMethod("update2MIAME", "exprSet",
   function(object) {
      if(is(description(object), "MIAME"))
         cat("This object is up to date.\n")
      else {
         cat("For now we will keep old description in the experiment title.\nConsider defining an object of class MIAME with more information\n")
         description(object) <- new("MIAME",title=description(object))
      }
      object
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FIXME: do we need both assayData and exprs (next) doing the same?
# this method declaration was digged out between methods of eSet class
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("assayData", "exprSet", function(object) object@exprs)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("exprs", "exprSet", function(object) object@exprs)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setReplaceMethod("exprs", "exprSet",
   function(object, value) {
      object@exprs <- value
      return(object)
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("se.exprs", "exprSet", function(object) object@se.exprs)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setReplaceMethod("se.exprs", "exprSet",
   function(object, value) {
      object@se.exprs <- value
      return(object)
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# RI:
# added this so i can access notes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("notes", "exprSet", function(object) object@notes)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setReplaceMethod("notes", "exprSet",
   function(object, value) {
      object@notes <- value
      object
  }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("description", "exprSet", function(object) object@description)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setReplaceMethod("description", "exprSet",
   function(object, value) {
      object@description <- value
      object
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("abstract", "exprSet", function(object) abstract(description(object)))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("sampleNames", "exprSet",
   function(object) {
      if (!is.null(colnames(exprs(object))))
         colnames(exprs(object))
      else
         row.names(pData(object))
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setReplaceMethod("sampleNames", "exprSet",
   function(object, value) {
      dn <- dimnames(object@exprs)
      if( !is.character(value) )
         stop("replacement names must be strings")
      if(length(value) != length(dn[[2]]))
         stop("wrong number of names supplied")
      dn[[2]] <- value
      dimnames(object@exprs) <- dn
      row.names(pData(object)) <- value
      validObject(object)
      object
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("geneNames", "exprSet", function(object) row.names(exprs(object)))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setReplaceMethod("geneNames", "exprSet",
   function(object, value) {
      es <- exprs(object)
      row.names(es) <- value
      exprs(object) <- es
      object
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("annotation", "exprSet", definition = function(object) object@annotation)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setReplaceMethod("annotation", signature="exprSet",
   definition =  function(object, value) {
      object@annotation <- value
      return(object)
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[", "exprSet",
   function(x, i, j, ..., drop=FALSE) {
      if(missing(j))
         pdata <- phenoData(x)
      else
         pdata <- phenoData(x)[j,, ..., drop=FALSE]
      haveSES <- nrow(se.exprs(x)) > 0
      if(missing(j)) {
         if(missing(i)) {
            nexprs <- exprs(x)
            if(haveSES)
               nses <- se.exprs(x)
         }
         else {
            nexprs <- exprs(x)[i, ,drop=FALSE]
            if(haveSES)
               nses <- se.exprs(x)[i, ,drop=FALSE]
         }
      }
      else {
         if(missing(i)) {
            nexprs <- exprs(x)[,j, drop=FALSE]
            if(haveSES)
               nses <- se.exprs(x)[,j, drop=FALSE]
         }
         else {
            nexprs <- exprs(x)[i, j, drop=FALSE]
            if(haveSES)
               nses <- se.exprs(x)[i, j, drop=FALSE]
         }
      }
      if (!missing(i) &&
          !is.null(attr(x,"reporterInfo")) &&
          !is.null(reporterInfo(x)) && nrow(reporterInfo(x)) > 0)
        reporterInfo(x) <- reporterInfo(x)[i, ,drop=FALSE]
      exprs(x) <- nexprs
      if(haveSES)
         se.exprs(x) <- nses
      phenoData(x) <- pdata
      x
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("show", "exprSet",
   function(object ) {
      dm <-dim(exprs(object))
      ngenes <- dm[1]
      nsamples <- dm[2]
      cat("Expression Set (exprSet) with \n\t", ngenes, " genes\n\t", sep="")
      cat(nsamples, "samples\n\t")
      show(phenoData(object))
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("iter", signature(object="exprSet", covlab="missing", f="function"),
   function(object, covlab, f) {
      apply(exprs(object), 1, f)
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("iter", signature(object="exprSet", covlab="missing", f="list"),
   function(object,covlab,f) {
      flist <- f
      llen <- length(flist)
      out <- matrix(NA,nr=nrow(exprs(object)), nc=llen )
      lnames <- names(flist)
      if(is.null(lnames))
         lnames <- paste("l",1:llen,sep="")
      for (i in 1:llen)
         out[,i] <- apply(exprs(object),1,flist[[i]])
      dimnames(out) <- list(row.names(exprs(object)),lnames)
      out
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("iter", signature(object="exprSet", covlab="character", f="function"),
   function(object, covlab, f) {
      ## f assumed to be a function of two arguments,
      ## first is a stratum identifier to be used
      ## in evaluating a statistical contrast by f
      varnames <- names(phenoData(object)@pData)
      if (!(any(match(covlab,varnames))))
         stop("the requested covariate is not in the exprSet")
      fc <- function(x) function(y) f(x,y)
      f2app <- fc(phenoData(object)@pData[[covlab]])
      iter(object,f=f2app)
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# used below by 'split' methods!
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.splitexprSet = function(x, f) {
   lenf <- length(f)
   exs <- exprs(x)
   pD <- phenoData(x)
   aN <- annotation(x)
   ## FIXME: I have commented this out and I think it is a bad idea
   ## we should not be splitting on the rows of the exprs, only on
   ## the  rows of the pData - ie. only on cases
   #                if( (nrow(exs) %% lenf == 0 ) ) {
   #                  splitexprs <- lapply(split(1:nrow(exs), f),
   #                                         function(ind) exs[ind, , drop =
   #                                                           FALSE])
   #                  nsplit<-length(splitexprs)
   #                  for(i in 1:nsplit) {
   #                    ## Create the new exprSet with the same class as
   #                    ## the original one - SDR
   #                    tmp <- x
   #                    exprs(tmp) <- splitexprs[[i]]
   #                    phenoData(tmp) <- pD
   #                    annotation(tmp) <- aN
   #                    se.exprs(tmp) <- matrix(nr=0,nc=0)
   #                    description(tmp) <- new("MIAME")
   #                    notes(tmp) <- ""
   #                    splitexprs[[i]] <- tmp
   #                    rm(tmp)
   #                  }
   #                  return(splitexprs)
   #                }  ##split the expressions
   if( (nrow(pData(x)) %% lenf ==0) ) {
      npD <- split(pD, f)
      nEx <- split(1:ncol(exs), f)
      nsplit <- length(npD)
      for( i in 1:nsplit) {
         ## Create the new exprSet with the same class as
         ## the original one - SDR
         tmp <- x
         exprs(tmp) <- {function(ind) exs[,ind,drop=FALSE]} (nEx[[i]])
         phenoData(tmp) <- npD[[i]]
         annotation(tmp) <- aN
         npD[[i]] <- tmp
         rm(tmp)
      }
      return(npD)
   }
   else
      stop("could not split")
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FIXME: somehow factors are not vectors!!! this is going to cause
#  some grief - we will need to figure out whether this is an oversight
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("split", signature(x="exprSet", f="factor", drop="missing"),
   function(x, f) {
      .splitexprSet(x, f)
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("split", signature(x="exprSet", f="vector", drop="missing"),
   function(x, f) {
      .splitexprSet(x, f)
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("split", signature(x="phenoData", f="vector", drop="missing"),
   function(x, f) {
      lenf <- length(f)
      pD <- pData(x)
      vL <- varLabels(x)
      if( (nrow(pD) %% lenf ==0) ) {
         npD <- split(pD, f)
         nsplit <- length(npD)
         for(i in 1:nsplit)
            npD[[i]] <- new("phenoData", pData = npD[[i]], varLabels=vL)
         return(npD)
      }
      else
         stop("could not split")
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# write table for exprSet. makes a table with gene names in first column
# chip names in first row. apart from quote=FALSE and sep="\t"
# everything else is the same as in write.table
# FIXME: if this is basically the same as write.table why not to use ... instead of the full
# list of arguments (just add quote and sep)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("write.exprs", signature(x="exprSet"),
   function(x,file = "tmp.txt",
            append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double")) {
      write.table(exprs(x),file = file, append = append,
         quote = quote, sep = sep, eol = eol, na = na, dec = dec,
         row.names = row.names, col.names = col.names, qmethod = qmethod)
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FIXME: (same as above) if this is basically the same as write.table why not to use ...
# instead of the full list of arguments (just add quote and sep)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("exprs2excel", signature(x="exprSet"),
   function(x,file = "tmp.csv",
            append = FALSE, quote = FALSE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = NA, qmethod = c("escape", "double")) {
      write.table(exprs(x),file = file, append = append,
         quote = quote, sep = sep,eol = eol, na = na, dec = dec,
         row.names = row.names, col.names = col.names, qmethod = qmethod)
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FIXME: if it is an internal function, like .splitexprSet above, it would have
# sense to name it correspondingly starting with a dot
# FIXME: this is used only by the method above, exclusively (?), why not to put it there
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
doAsDataFrame <- function(x, row.names=NA, optional=NA) {
   nc.eset <- ncol(exprs(x))
   nr.eset <- nrow(exprs(x))
   gn.eset <- geneNames(x)
   if (is.null(gn.eset))
      gn.eset <- rep(NA, nr.eset)
   i.pdata <- rep(seq(1, nc.eset), rep(nr.eset, nc.eset))
   pexp <- c(exprs(x))
   rv <- do.call("data.frame", c(list(exprs=pexp, genenames=rep(gn.eset, nc.eset)),
                 lapply(pData(x), function(y, i.pdata) y[i.pdata], i.pdata)))
   return(rv)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("as.data.frame.exprSet", signature(x="exprSet"), doAsDataFrame)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FIXME: move to method for eSet, exprSet
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
esApply <- function(X, MARGIN, FUN, ...) {
   if ((!is(X, "exprSet")) && (!is(X, "eSet")))
      stop("arg1 must be of class exprSet")
   e1 <- new.env(parent=environment(FUN))
   multiassign(names(pData(X)), pData(X), env=e1)
   environment(FUN) <- e1
   apply(exprs(X), MARGIN, FUN, ...)
}
# ==========================================================================
# A function to read exprSet from text a text file. The function
# assumes that exprs and se.exprs are tab separated text files.
# The first 6 arguments are for text files to be read in for the slots
# of an exprSet object. seps is for the separators used for exprs and
# se.exprs files. length(seps) = 1 if all two files share
# the same separator or length(seps) = 2 if not. seps are ordered in
# the order as the two arguments appear in the argument list. A
# header is assumed to exist for exprs file. Only exprs is required
# and the others are optional. phenodata has to be the name of a rda
# file that is a list with a pData and a varLabels element.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
read.exprSet <- function(exprs, se.exprs, phenoData, annotation,
                         description, notes, seps = "\t" ) {
   if(missing(exprs))
      stop("exprs can not be missing!")
   # Read exprs
   geneData <- as.matrix(read.table(exprs, header = TRUE, sep = seps[1], as.is =  TRUE))
   # Read se.exprs using the right separator
   if(!missing(se.exprs))
      se.exprs <- as.matrix(read.table(se.exprs, header = FALSE,
                            sep = ifelse(length(seps == 1), seps, seps[2]), as.is = TRUE))
   # If phenoData is missing provide a default
   if(missing(phenoData))
      phenoData <- read.phenoData(NULL, colnames(geneData), FALSE)
   else
      phenoData <- read.phenoData(phenoData, colnames(geneData), FALSE)
   # Each gene should have a coVariate?
   if(nrow(pData(phenoData)) != ncol(geneData))
      warning("Gene and covariate numbers do not match")
   eSet <- new("exprSet", exprs = geneData, phenoData = phenoData)
   if(!missing(se.exprs))
      se.exprs(eSet) <- se.exprs
   if(!missing(annotation))
      annotation(eSet) <-  readLines(annotation)
   if(!missing(description))
      description(eSet) <- read.MIAME(description, FALSE)
   else
      description(eSet) <- new("MIAME")
   if(!missing(notes))
      notes(eSet) <- readLines(notes)
   return(eSet)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setAs("exprSet", "eSet",
   function(from) {
     .Deprecated('as(from,"ExpressionSet")', "Biobase")
     desc=description(from)
     if(!is(desc, "MIAME"))
       desc=new("MIAME", other=list(comment="Converted from exprSet",
                          characterDescription=desc))
      new("eSet", assayData=list(exprs=exprs(from)),
          phenoData=phenoData(from),
          description=desc,
          annotation=annotation(from),
          notes=notes(from),
          sampleNames=sampleNames(from),
          reporterNames=geneNames(from))
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

