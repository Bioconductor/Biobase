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

.First.lib <- function(libname, pkgname, where) {
    require(methods, quietly=TRUE)
    where <- match(paste("package:", pkgname, sep=""), search())
    .initContainer(where)
    .initAgg(where)
    .initExprset(where)
    .buildBiobaseOpts()
    .getPDFOption()

    cat("Welcome to Bioconductor \n")
    cat("\t Vignettes contain introductory material.  To view, \n")
    cat("\t simply type: openVignette() \n")
    ##        cat("\t to see the available vignettes\n")
    cat("\t For details on reading vignettes, see\n")
    cat("\t the openVignette help page.")
    cacheMetaData(as.environment(where))

}
