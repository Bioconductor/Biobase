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

# A class for MIAME information is also defined.
#we try to cover all 6 MIAME entries
# 1. Experimental design 2. Array design 3. Samples 4. Hybridizations
# 5. Measurements 6. Normalization controls
# the class will contain experimenter name, laboratory, contact
# information, a single-sentence experiment title,
#an abstract describing the experiment, and URLs  and slots that are lists for
#"Samples", "Hybridizations", "Normalization controls",
#the remaing two are already covered.
# we add a slot (also a list) to keep track of pre-processing
#
#more info: http://www.mged.org/Workgroups/MIAME/miame_1.1.html

##data class for accompanying data
  setClass("phenoData", representation(pData="data.frame",
                                       varLabels="list"),
           prototype=list(pData=data.frame(matrix(nr=0,nc=0)),
             varLabels=list()),
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



##data class for MIAME information
  setClass("MIAME", representation(name="character",
                                   lab="character",
                                   contact="character",
                                   title="character",
                                   abstract="character",
                                   url="character",
                                   samples="list",
                                   hybridizations="list",
                                   normControls="list",
                                   preprocessing="list",
                                   other="list"),
           prototype=list(name="",lab="",contact="",
             title="",abstract="",url="",
             samples=list(),hybridizations=list(),normControls=list(),
             preprocessing=list(),other=list()))

  ##show method
  setMethod("show", "MIAME",
            function(object) {
              tmp <- c("samples","hybridizations","normalization controls","preprocessing")
              Index <-c(length(object@samples)>0,
                        length(object@hybridizations)>0,
                        length(object@normControls)>0,
                        length(object@preprocessing)>0)
              cat("Experimenter name:",object@name,"\n")
              cat("Laboratory:",object@lab,"\n")
              cat("Contact information:",object@contact,"\n")
              cat("Title:",object@title,"\n")
              cat("URL:",object@url,"\n")
              if(object@abstract!="")
                cat("\nA",length(strsplit(object@abstract," ")[[1]]),
                    "word abstract is available. Use 'abstract' method.\n")
              else cat("No abstract available.\n")
              if(any(Index)) cat("\nInformation is available on:",
                                 paste(tmp[Index],collapse=", "),"\n")
            })

  ##abstract method
  if( !isGeneric("abstract") )
    setGeneric("abstract", function(object)
               standardGeneric("abstract"))

  setMethod("abstract","MIAME",function(object) object@abstract)

   ##samples method
  if( !isGeneric("samples") )
    setGeneric("samples", function(object)
               standardGeneric("samples"))

  setMethod("samples","MIAME",function(object) object@samples)

   ##hybridizations method
  if( !isGeneric("hybridizations") )
    setGeneric("hybridizations", function(object)
               standardGeneric("hybridizations"))

  setMethod("hybridizations","MIAME",function(object) object@hybridizations)

   ##normControls method
  if( !isGeneric("normControls") )
    setGeneric("normControls", function(object)
               standardGeneric("normControls"))

  setMethod("normControls","MIAME",function(object) object@normControls)

   ##preproc method
  if( !isGeneric("preproc") )
    setGeneric("preproc", function(object)
               standardGeneric("preproc"))

  setMethod("preproc","MIAME",function(object) object@preprocessing)

  ##otherInfo method
  if( !isGeneric("otherInfo") )
    setGeneric("otherInfo", function(object)
               standardGeneric("otherInfo"))

  setMethod("otherInfo","MIAME",function(object) object@other)

  ##expinfo method
  if( !isGeneric("expinfo") )
    setGeneric("expinfo", function(object)
               standardGeneric("expinfo"))

  setMethod("expinfo","MIAME",function(object){
    tmp <- c(object@name,
             object@lab,
             object@contact,
             object@title,
             object@url)
    names(tmp) <- c("name","lab","contact","title","url")
    return(tmp)
  })

  ##trick so that the old exprSet and Plobs works
  setClassUnion("characterORMIAME", c("MIAME", "character"))


  ## Class union for the exprs and se.exprs slots of exprSet
  if (!isClass("exprMatrix"))
    setClassUnion("exprMatrix", c("matrix"))

  ##data class for expression arrays
  setClass("exprSet", representation(exprs="exprMatrix",
                                     se.exprs = "exprMatrix",
                                     phenoData="phenoData",
                                     description="characterORMIAME",
                                     annotation="character",
                                     notes="character") ,
           prototype=list(exprs=matrix(nr=0,nc=0),
             se.exprs = matrix(nr=0,nc=0),
             description=new("MIAME"),
             annotation="",
             notes=""))

  ##define a method to update exprsSet from previous versions
  if( !isGeneric("update2MIAME") )
    setGeneric("update2MIAME", function(object)
  standardGeneric("update2MIAME"))

  setMethod("update2MIAME", "exprSet",
            function(object){
              if(class(description(object))=="MIAME")
                cat("This object is up to date.\n")
              else{
                cat("For now we will keep old description in the experiment title.\nConsider defining an object of class MIAME with more information\n")
                description(object) <- new("MIAME",title=description(object))
              }
              object
            })

  ##define a generic for obtaining the data
  if( !isGeneric("exprs") )
    setGeneric("exprs", function(object) standardGeneric("exprs"))
  setMethod("exprs", "exprSet", function(object) object@exprs)

  ##RI: define a genric for obtaining the se's
  if( !isGeneric("se.exprs") )
    setGeneric("se.exprs", function(object) standardGeneric("se.exprs"))
  setMethod("se.exprs", "exprSet", function(object) object@se.exprs)

  if( !isGeneric("exprs<-") )
    setGeneric("exprs<-", function(object, value)
               standardGeneric("exprs<-"))

  setReplaceMethod("exprs", "exprSet",
                   function(object, value) {
                     object@exprs <- value
                     return(object)
                   })

  if( !isGeneric("se.exprs<-") )
    setGeneric("se.exprs<-", function(object, value)
               standardGeneric("se.exprs<-"))

  setReplaceMethod("se.exprs", "exprSet",
                   function(object, value) {
                     object@se.exprs <- value
                     return(object)
                   })

  ##RI: Added this so i can access notes
  ##method for notes (accessor and replacement)
  if( !isGeneric("notes") )
    setGeneric("notes", function(object)
               standardGeneric("notes"))
  setMethod("notes", "exprSet", function(object)
            object@notes)

  if( !isGeneric("notes<-") )
    setGeneric("notes<-", function(object, value)
               standardGeneric("notes<-"))

  setReplaceMethod("notes", "exprSet", function(object, value) {
    object@notes <- value
    object
  })

  ##method for MIAME description
  if( !isGeneric("description") )
    setGeneric("description", function(object)
               standardGeneric("description"))
  setMethod("description", "exprSet", function(object)
            object@description)

  ##replace method for description
  if( !isGeneric("description<-") )
    setGeneric("description<-", function(object, value)
               standardGeneric("description<-"))

  setReplaceMethod("description", "exprSet", function(object, value) {
    object@description <- value
    object
  })

  ##method for abstract
  setMethod("abstract", "exprSet",
            function(object) abstract(description(object)))

  ##method for phenoData
  if( !isGeneric("phenoData") )
    setGeneric("phenoData", function(object)
               standardGeneric("phenoData"))
  setMethod("phenoData", "exprSet", function(object)
            object@phenoData)

  if( !isGeneric("phenoData<-") )
    setGeneric("phenoData<-", function(object, value)
               standardGeneric("phenoData<-"))

  setReplaceMethod("phenoData", c("exprSet", "phenoData"),
                   function(object, value) {
                       object@phenoData <- value
                       object })

  ##method for pData
  setMethod("pData", "exprSet",
            function(object) pData(phenoData(object)))


  ##replace method for pData
  if( !isGeneric("pData<-") )
    setGeneric("pData<-", function(object, value)
               standardGeneric("pData<-"))

  setReplaceMethod("pData", "exprSet", function(object, value) {
    ph <- phenoData(object)
    pData(ph) <- value
    phenoData(object) <- ph
    object
  })

  ##[[ method
  setReplaceMethod("[[", "exprSet", function(x, i, j, ..., value) {
    pD <- phenoData(x)
    pD@pData[[i]] <- value
    phenoData(x) <- pD
    x})

  setMethod("[[", "exprSet", function(x, i, j, ...)
      phenoData(x)[[i]] )


###RI: this is a simple a pData replace for phenoData. i need it for affy.
  setReplaceMethod("pData", "phenoData", function(object, value){
    object@pData <- value
    object
  })


  if( !isGeneric("sampleNames") )
    setGeneric("sampleNames", function(object)
               standardGeneric("sampleNames"))
  setMethod("sampleNames", "exprSet",
            function(object) {
              if (! is.null(colnames(exprs(object))))
                colnames(exprs(object))
              else
                row.names(pData(object))
            })

  ##replace method for sampleNames

  if( !isGeneric("sampleNames<-") )
    setGeneric("sampleNames<-", function(object, value)
               standardGeneric("sampleNames<-"))

  setReplaceMethod("sampleNames", "exprSet", function(object, value) {
      dn <- dimnames(object@exprs)
      if( !is.character(value) )
          stop("replacement names must be strings")
      if( length(value) != length(dn[[2]]))
          stop("wrong number of names supplied")
      dn[[2]] <- value
      dimnames(object@exprs) <- dn
      row.names(object@phenoData@pData) <- value
      object})


  if( !isGeneric("geneNames") )
    setGeneric("geneNames", function(object)
               standardGeneric("geneNames"))
  setMethod("geneNames", "exprSet", function(object)
            row.names(exprs(object)))

  if( !isGeneric("geneNames<-") )
    setGeneric("geneNames<-", function(object, value)
               standardGeneric("geneNames<-"))

  setReplaceMethod("geneNames", "exprSet", function(object, value) {
    es <- exprs(object)
    row.names(es) <- value
    exprs(object) <- es
    object
  })

  ##a varLabels method for exprSets
  setMethod("varLabels", "exprSet",
            function(object) phenoData(object)@varLabels)

  ## annotation: read and replace. WH, 11 Mar 2003
  if(!isGeneric("annotation") )
    setGeneric("annotation", function(object)
               standardGeneric("annotation"))
  setMethod("annotation", "exprSet",
     definition = function(object) object@annotation)

  if(!isGeneric("annotation<-") )
    setGeneric("annotation<-", function(object, value)
               standardGeneric("annotation<-"))

  setReplaceMethod("annotation", signature="exprSet",
     definition =  function(object, value) {
                     object@annotation <- value
                     return(object)
                   })

  ## [
  setMethod("[", "exprSet", function(x, i, j, ..., drop=FALSE) {
    if( missing(j) )
	pdata <- phenoData(x)
    else
        pdata <- phenoData(x)[j,, ..., drop=FALSE]
    haveSES <- nrow(se.exprs(x)) > 0
    if(missing(j) ) {
        if( missing(i) ) {
            nexprs <- exprs(x)
            if( haveSES )
                nses <- se.exprs(x)
        }
        else {
            nexprs <- exprs(x)[i, ,drop=FALSE]
            if( haveSES )
                nses <- se.exprs(x)[i, ,drop=FALSE]
        }
    }
    else {
      if( missing(i) ) {
          nexprs <- exprs(x)[,j, drop=FALSE]
          if( haveSES )
              nses <- se.exprs(x)[,j, drop=FALSE]
      }
      else {
          nexprs <- exprs(x)[i, j, drop=FALSE]
          if( haveSES )
              nses <- se.exprs(x)[i, j, drop=FALSE]
      }
    }
    exprs(x) <- nexprs
    if( haveSES )
        se.exprs(x) <- nses
    phenoData(x) <- pdata
    x
  })

  setMethod("show", "exprSet", function(object ) {
    dm <-dim(exprs(object))
    ngenes <- dm[1]
    nsamples <- dm[2]
    cat("Expression Set (exprSet) with \n\t", ngenes, " genes\n\t", sep="")
    cat(nsamples, "samples\n\t")
    show(phenoData(object))
  })

  setGeneric("iter", function(object, covlab, f) standardGeneric("iter"))
                                        #
  setMethod("iter", signature(object="exprSet", covlab="missing",
                              f="function"),
            function(object, covlab, f)
            apply(exprs(object), 1, f))
                                        #
  setMethod("iter", signature(object="exprSet", covlab="missing",
                              f="list"),
            function(object,covlab,f) {
              flist <- f
              llen <- length(flist)
              out <- matrix(NA,nr=nrow(exprs(object)), nc=llen )
              lnames <- names(flist)
              if(is.null(lnames)) lnames <- paste("l",1:llen,sep="")
              for (i in 1:llen)
                out[,i] <- apply(exprs(object),1,flist[[i]])
              dimnames(out) <- list(row.names(exprs(object)),lnames)
              out
            })

  setMethod("iter", signature(object="exprSet", covlab="character",
                              f="function"),
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
            })


  ##split
  if( !isGeneric("split") )
    setGeneric("split")

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
                    ## Create the new exprSet with the same class as
                    ## the original one - SDR
                    tmp <- x
                    exprs(tmp) <- splitexprs[[i]]
                    phenoData(tmp) <- pD
                    annotation(tmp) <- aN
                    se.exprs(tmp) <- matrix(nr=0,nc=0)
                    description(tmp) <- new("MIAME")
                    notes(tmp) <- ""
                    splitexprs[[i]] <- tmp
                    rm(tmp)
                  }
                  return(splitexprs)
                }  ##split the expressions
              if( (nrow(pData(x)) %% lenf ==0) ) {
                npD <- split(pD, f)
                nEx <- lapply(split(1:ncol(exs), f),
                              function(ind) exs[,ind,drop=FALSE])
                nsplit <- length(npD)
                for( i in 1:nsplit) {
                  ## Create the new exprSet with the same class as
                  ## the original one - SDR
                  tmp <- x
                  exprs(tmp) <- nEx[[i]]
                  phenoData(tmp) <- npD[[i]]
                  annotation(tmp) <- aN
                  npD[[i]] <- tmp
                  rm(tmp)
                }
                return(npD)
              }
              else
                stop("could not split")
            })


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
            })

###write table for exprSet. makes a table with gene names in first column
###chip names in first row
###apart from quote=FALSE and sep="\t"
###everything else is the same as write.table
  if( !isGeneric("write.exprs") )
    setGeneric("write.exprs", function(x,...) standardGeneric("write.exprs"))
  setMethod("write.exprs", signature(x="exprSet"),
            function(x,file = "tmp.txt",
                     append = FALSE, quote = FALSE, sep = "\t",
                     eol = "\n", na = "NA", dec = ".", row.names = TRUE,
                     col.names = TRUE, qmethod = c("escape", "double"))
            write.table(exprs(x),file = file, append = append,
                        quote = quote,
                        sep = sep,eol = eol, na = na, dec = dec,
                        row.names = row.names, col.names = col.names,
                        qmethod = qmethod))


  if( !isGeneric("exprs2excel") )
    setGeneric("exprs2excel", function(x,...) standardGeneric("exprs2excel"))
  setMethod("exprs2excel", signature(x="exprSet"),
            function(x,file = "tmp.csv",
                     append = FALSE, quote = FALSE, sep = ",",
                     eol = "\n", na = "NA", dec = ".", row.names = TRUE,
                     col.names = NA, qmethod = c("escape", "double"))
            write.table(exprs(x),file = file, append = append,
                        quote = quote,
                        sep = sep,eol = eol, na = na, dec = dec,
                        row.names = row.names, col.names = col.names,
                        qmethod = qmethod))



##not quite the right semantics
##but it is a start

"$.exprSet" <- function(eset, val)
    (pData(eset))[[as.character(val)]]

"$.phenoData" <- function(x, val, ...)
    (pData(x))[[as.character(val)]]

esApply <- function(X, MARGIN, FUN, ...) {
    if (class(X) != "exprSet" && class(X) != "eSet")
        stop("arg1 must be of class exprSet")
    e1 <- new.env(parent=environment(FUN))
    multiassign(names(pData(X)), pData(X), env=e1)
    environment(FUN) <- e1
    apply(exprs(X), MARGIN, FUN, ...)
}


