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

.getPDFOption <- function() {
    OS <- .Platform$OS.type
    if (OS == "unix") {
        if (setOptionPdfViewer() == FALSE) {
            note(paste("To manually set your viewer, run the",
                       "command 'setOptionPdfViewer(<path>),",
                       "where <path> is a path to a valid PDF",
                       "viewer program."))
        }
    }
}

dumpPackTxt <- function (package)
{
# stolen from "library" to get descriptive
# info out on stdout
#
    pkgpath <- .find.package(package)
#    outFile <- tempfile("Rlibrary")
#    outConn <- file(outFile, open = "w")
    docFiles <- file.path(pkgpath, c("TITLE", "DESCRIPTION",
        "INDEX"))
    headers <- c("", "Description:\n\n", "Index:\n\n")
    footers <- c("\n", "\n", "")
    for (i in which(file.exists(docFiles))) {
        writeLines(headers[i], sep = "")
        writeLines(readLines(docFiles[i]) )
        writeLines(footers[i], sep = "")
    }
#    close(outConn)
#    file.show(outFile, delete.file = TRUE)
}

##we need to be more careful about the where argument. If any of these
##function calls load a library, where is wrong from there on...
.First.lib <- function(libname, pkgname, where) {
    # I added this to add "Vignettes" to the menu bar. JZ
    if(interactive()){
        if(require(tcltk, quietly = TRUE)){ 
            addVig2Menu("vExplorer", itemAction = "vExplorer()")
            addVig2Menu("Biobase")
        }
    }

    require(methods, quietly=TRUE)
    where <- match(paste("package:", pkgname, sep=""), search())
    .initContainer(where)
    where <- match(paste("package:", pkgname, sep=""), search())
    .initAgg(where)
    where <- match(paste("package:", pkgname, sep=""), search())
    .initExprset(where)
    .buildBiobaseOpts()
    .getPDFOption()

    cat("Welcome to Bioconductor \n")
    cat("\t Vignettes contain introductory material.  To view, \n")
    cat("\t simply type: openVignette() \n")
    ##        cat("\t to see the available vignettes\n")
    cat("\t For details on reading vignettes, see\n")
    cat("\t the openVignette help page.\n")

    ##set up repository management
    if( require(reposTools, quietly=TRUE) ) {
        if (!("CRAN" %in% names(getOption("repositories2")))) {
            bioCOpt <- "http://www.bioconductor.org/CRANrepository"
            names(bioCOpt) <- "CRAN"
            options("repositories2"=c(getOption("repositories2"),bioCOpt))
        }

        if (!("BIOCRelease1.1" %in% names(getOption("repositories2")))) {
            bioROpt <- "http://www.bioconductor.org/repository/release1.1/package"
            names(bioROpt) <- "BIOCRel1.1"
            options("repositories2"=c(getOption("repositories2"),bioROpt))
        }

        if (!("BIOCDevel" %in% names(getOption("repositories")))) {
            bioDOpt <- "http://www.bioconductor.org/repository/devel/package"
            names(bioDOpt) <- "BIOCDevel"
            options("repositories2"=c(getOption("repositories2"),bioDOpt))
        }
    }

    ##set up menus -- windows only for now
    if( .Platform$OS.type == "windows" ) {
        addVig2Menu("vExplorer", itemAction = "vExplorer()")
        addVig2Menu("Biobase")
    }

}
