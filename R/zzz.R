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
    ## Define the function inside of .First.lib, as other packages
    ## might be defining this function and we don't want to cause mask
    ## messages.
    checkPkgDeps <- function(pkg) {
        reqVers <- function(req)
            return(gsub("(>)|(<)|(=)|([[:space:]])","",req))

        reqOper <- function(req)
            return(gsub("[^><=]","",req))


        buildDepMtrx <- function(deps) {
            ## Takes in a vector of dependencies, separates any version
            ## requirements.  Returns a matrix, col1 is the packes, col2
            ## is any algebraic requirements
            if (is.null(deps))
                return(NULL)

            deps <- gsub("\\)","",deps)
            deps <- strsplit(deps,"\\(")
            ## Now have a list, some w/ 1 els some w/ more (those w/ reqs)
            deps <- lapply(deps, function(x){
                if (length(x) == 1)
                    return(c(x, ""))
                else
                    return(x)
            }
                           )
            pkgs <- lapply(deps,function(x){return(x[1])})
            reqs <- lapply(deps,function(x){return(x[2])})
            depMtrx <- cbind(matrix(unlist(pkgs)),matrix(unlist(reqs)))
            ## Kill any trailing whitespace on entries
            for (i in 1:2)
                depMtrx[,i] <- gsub("[[:space:]]$","",depMtrx[,i])

            if (depMtrx[1,1] == "NA")
                depMtrx <- NULL

            return(depMtrx)
        }

        if (!is.na(desc <- package.description(pkg,fields="Depends")))
            depMtrx <- buildDepMtrx(strsplit(desc,",[[:space:]]*")[[1]])
        else
            stop(paste("No DESCRIPTION file for package",pkg))

        for (i in 1:nrow(depMtrx)) {
            if (depMtrx[i,1] == "R") {
                ## Should we even check the R version?  DOesn't R do this?
            }
            else {
                depName <- depMtrx[i,1]
                depVers <- depMtrx[i,2]
                outString <- paste("You can not load package",pkg,
                                   "as\nit requires you to have package")

                if (!(depName %in% installed.packages()[,1])) {
                    ## Does not have this package
                    outString <- paste(outString,depName,
                                       "which is not currently installed.")
                    stop(outString)
                }
                if (depVers != "") {
                    ## have a package version
                    outString <- paste(outString, depName, "with a version",
                                       reqOper(depVers),
                                       reqVers(depVers),
                                       "which is not\ncurrently installed.")

                    vers <- package.description(depName,fields="Version")

                    comp <- compareVersion(vers, reqVers(depVers))
                    ## oper can currently be either '>=' or '<='
                    oper <- reqOper(depVers)

                    if ((oper == ">=")&&(comp < 0))
                        stop(outString)
                    else if ((oper == "<=")&&(comp > 0))
                        stop(outString)
                }
            }
        }
    }

    checkPkgDeps(pkgname)
    ## still need methods for 1.6.x users


    .buildBiobaseOpts()

    cat("Welcome to Bioconductor \n")
    cat("\t Vignettes contain introductory material.  To view, \n")
    cat("\t simply type: openVignette() \n")
    ##        cat("\t to see the available vignettes\n")
    cat("\t For details on reading vignettes, see\n")
    cat("\t the openVignette help page.\n")

    ##set up repository management
    require("reposTools", quietly=TRUE) || stop ("Package reposTools required")


    ##set up menus -- windows only for now
    if( .Platform$OS.type == "windows" && .Platform$GUI == "Rgui" ) {
        addPDF2Vig("Biobase")
    }

}
