# temporarily store this utility here

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



##we need to be more careful about the where argument. If any of these
##function calls load a library, where is wrong from there on...
.onLoad <- function(libname, pkgname) {

    ##need contents to load at library attach - not at build time
    where <- match(paste("package:", pkgname, sep=""), search())
    .initContents()
    .buildBiobaseOpts()

    cat("Welcome to Bioconductor \n")
    cat("\t Vignettes contain introductory material.  To view, \n")
    cat("\t simply type: openVignette() \n")
    ##        cat("\t to see the available vignettes\n")
    cat("\t For details on reading vignettes, see\n")
    cat("\t the openVignette help page.\n")

    ##set up menus -- windows only for now
    if( .Platform$OS.type == "windows" && .Platform$GUI == "Rgui" ) {
        addVigs2WinMenu("Biobase")
    }

}
