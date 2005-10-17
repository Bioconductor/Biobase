# ==========================================================================
# Functions to operate with the environment:
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# .initContents; .buildBiobaseOpts;
# multiassign; l2e; copyEnv
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.initContents <- function() {
   if( !isGeneric("contents") && !exists("contents", mode="function") )
      setGeneric("contents", function(object, all.names) standardGeneric("contents"))
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
    ## FIXME:SF change to 1.8 once devel repos is available
    reposList <- c(bioc="http://bioconductor.org/packages/bioc/1.7",
                   aData="http://bioconductor.org/packages/data/annotation/1.7",
                   eData="http://bioconductor.org/packages/data/experiment/1.7",
                   oh="http://bioconductor.org/packages/omegahat/1.7",
                   li="http://bioconductor.org/packages/lindsey/1.7",
                   cran="http://cran.fhcrc.org")
    reposList
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
biocInstall <- function(pkgs, lib, ...) {
    if (missing(pkgs))
      stop(paste(sQuote(pkgs), "argument is missing"))
    if (is.null(pkgs) || length(pkgs) == 0)
      stop("pkgs was set to ", sQuote(pkgs),
           ".  I was expecting a non-empty character vector.")
    if (missing(lib))
      lib <- .libPaths()[1]
    args <- list(...)
    nms <- names(args)
    if (! "dependencies" %in% nms)
      args[["dependencies"]] <- c("Depends", "Imports")
    reposList <- Biobase:::biocReposList()
    args <- c(list(pkgs=pkgs, repos=reposList, lib=lib), args)
    do.call("install.packages", args)
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



