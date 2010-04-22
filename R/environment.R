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
    BioC <- getOption("BioC")
    if (is.null(BioC)) {
        BioC <- list()
        class(BioC) <- "BioCOptions"
    }
    Base <- BioC$Base
    if (is.null(Base)) {
        Base <- list()
        class(Base) <- "BioCPkg"
    }
    if (is.null(Base$urls))
        Base$urls <- list( bioc = "http://bioconductor.org")
    ##RI: I added this to make my life easier. Should it be TRUE?
    ##AJR: NO.  I've run across a few cases when it would completely
    ##     break functionality, i.e. when tcltk isn't part of the R
    ##     package (on weird, and development-based machines
    if (is.null(Base$use.widgets))
        Base$use.widgets <- FALSE
    BioC$Base <- Base
    options("BioC"=BioC)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
biocReposList <- function() {
    ## Locations of Bioconductor release repositories
    VERSION <- "2.7"
    bioc_topurl <- getOption("BioC")$Base$urls$bioc
    if (is.null(bioc_topurl))
        bioc_topurl <- "http://bioconductor.org"
    pkg_topurl <- paste(bioc_topurl, "packages", VERSION, sep="/")
    mkurl <- function(...) paste(pkg_topurl, ..., sep="/")
    reposList <- c(bioc=mkurl("bioc"),
                   aData=mkurl("data/annotation"),
                   eData=mkurl("data/experiment"),
                   extra=mkurl("extra"),
                   brainarray="http://brainarray.mbni.med.umich.edu/bioc",
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



#### Functions for lists and environments

listLen <- function(x)
   .Call("listLen", x, PACKAGE="Biobase")


.new_env_size <- function(n) {
    ## helper function returns suggested size for new environments that
    ## will be used as hashtables given expected number of elements 'n'.
    max(29L, as.integer(n * 1.20))
}

l2e <- function(vals, envir) {
   if(missing(envir))
     envir <- new.env(hash=TRUE, parent=emptyenv(),
                      size=.new_env_size(length(vals)))
   .Call("listToEnv", vals, envir, PACKAGE="Biobase")
}

copyEnv <- function(oldEnv, newEnv, all.names=FALSE) {
    if (missing(newEnv))
      newEnv <- new.env(hash=TRUE,
                        parent=parent.env(oldEnv),
                        size=.new_env_size(length(oldEnv)))
    .Call(copyEnv_sym, oldEnv, newEnv, all.names)
}
