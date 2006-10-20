# ==========================================================================
# Functions to operate with the environment:
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# .initContents; .buildBiobaseOpts;
# multiassign; l2e; copyEnv
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.initContents <- function() {
   makeG = TRUE
   if( isGeneric("contents", where = .GlobalEnv ) )
       if( identical(names(formals(contents)), c("object", "all.names")) )
            makeG = FALSE
   if(makeG)
     setGeneric("contents", function(object, all.names)
                         standardGeneric("contents"))


   setMethod("contents", "environment",
      function(object, all.names) {
         if (missing(all.names))
            all.names <- FALSE
         as.list(object, all.names=all.names)
     }
   )
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.buildBiobaseOpts <- function() {
    if (is.null(getOption("BioC"))) {
        BioC <- list()
        class(BioC) <- "BioCOptions"
        options("BioC"=BioC)
    }

    Base <- list()
    class(Base) <- "BioCPkg"
    Base$urls <- list( bioc = "http://www.bioconductor.org")
    ##RI: I added this to make my life easier. Should it be TRUE?
    ##AJR: NO.  I've run across a few cases when it would completely
    ##     break functionality, i.e. when tcltk isn't part of the R
    ##     package (on weird, and development-based machines
    Base$use.widgets=FALSE

    BioC <- getOption("BioC")
    BioC$Base <- Base
    options("BioC"=BioC)

}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
biocReposList <- function() {
    ## Locations of Bioconductor release repositories
    VERSION <- "2.0"
    root <- paste("http://bioconductor.org/packages", VERSION, sep="/")
    mkurl <- function(...) paste(root, ..., sep="/")
    reposList <- c(bioc=mkurl("bioc"),
                   aData=mkurl("data/annotation"),
                   eData=mkurl("data/experiment"),
                   oh=mkurl("omegahat"),
                   mo=mkurl("monograph"),
                   cran="http://cran.fhcrc.org")
    reposList
}
# ==========================================================================
# RG:
# multiput and multiget
# ideally these will be internalized at some point
# FIXME: I think Luke's Dynamic variables should be used rather than
# the on.exit kludge
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
multiassign <- function (x, value, envir = parent.frame(), inherits = FALSE) {
   if( ! is.environment(envir) )
      stop("envir argument is not an environment")
   if( missing(value) ) {
      nx <- names(x)
      if( any(nchar(nx) == 0) )
         stop("value is missing and x does not have named components")
      value <- x
      x <- nx
   }
   lenx <- length(x)
   for(i in 1:lenx) {
      i2 <- (i-1)%%lenx+1
      if( is.list(x) ) {
         if( is.list(value) )
            assign(x[[i]], value[[i2]], envir=envir, inherits=inherits)
         else
            assign(x[[i]], value[i2], envir=envir, inherits=inherits)
      }
      else {
         if( is.list(value) )
            assign(x[i], value[[i2]], envir=envir, inherits=inherits)
         else
            assign(x[i], value[i2], envir=envir, inherits=inherits)
      }
   }
}
# ==========================================================================
# RG:
# Functions for lists -> in C for efficiency
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
listLen <- function(list)
   .Call("listLen", list, PACKAGE="Biobase")
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
l2e <- function(vals, envir) {
   if(missing(envir)) envir <- new.env(hash=TRUE)
   .Call("listToEnv", vals, envir, PACKAGE="Biobase")
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
copyEnv <- function(oldEnv, newEnv=new.env(hash=TRUE,
                    parent=parent.env(oldEnv)), all.names=FALSE) {
   oldVals <- as.list(oldEnv, all.names)
   l2e(oldVals, newEnv)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



