# This function gets and installs the required Bioconductor libraries.
#
# libName: a character string for the name of the library to be
# installed. Valid names include "all" - all the released packages,
# "affy" - packages "affy" plus exprs, "CDNA" - packages "CDNA" plus
# exprs, and "exprs" - packages "Biobase", "annotate", "genefilter",
# "geneploter", "edd", "Roc", and "tkWidgets".
# destdir: a character string for the directory where the downloaded
# packages will be stored.
# isDevel: a boolean indicating whether the released (FALSE) and
# developer (TRUE) version will be downloaded and installed.
# verbose: a boolean indicating whether any error related to the
# downloading process will be (TRUE) printed. Error messages will
# still be returned but invisible is berbose is set to FALSE.
#
getBioC <- function (libName = "exprs", destdir = NULL, isDevel = FALSE,
                     verbose = TRUE){

    on.exit(options(show.error.messages = TRUE))

    PLATFORM <- .Platform$OS.type
    DESTDIR <- ifelse(is.null(destdir), getwd(), destdir)
    messages <- NULL

    packs <- getPackNames(libName)
    for(i in packs){
        sourceUrl <- getUrl(PLATFORM, i, isDevel)
        fileName <- getFName(PLATFORM, DESTDIR, i)

        # check the connection instead of downloading directly which
        # will write files of 0 size in the directory even when the
        # the connection is not there.
        options(show.error.messages = FALSE)
        tryMe <- try(url(sourceUrl, "r"))
        options(show.error.messages = TRUE)

        if(inherits(tryMe, "try-error")){
            if(verbose)
                print(paste("Get", i, "failed"))
            else
                messages <- paste(messages, paste("Get", i, "failed"),
                                  sep = "\n")
        }else{
# Can not use the existing functions since they are all specific to CRAN
#            download.packages(getLibName(PLATFORM, i),destdir,
#                       contriburl = getBioCUrl(PLATFORM, isDevel))
#            install.packages(getLibName(PLATFORM, i), lib = .libPaths(),
#                      contriburl = getDLUrl(PLATFORM, isDevel),
#                             destdir = destdir)

            download.file(sourceUrl, fileName, quiet = TRUE)
#           temp <- packageStatus(repositories = fileName)
#           upgrade(temp)
            installPack(PLATFORM, fileName)
        }
    }
    return(invisible(messages))
}

getPackNames <- function (libName){
    AFFY <- "affy"
    CDNA <- c("marrayInput", "marrayClasses", "marrayNorm",
           "marrayPlots")
    EXPRS <-c("Biobase", "annotate", "genefilter", "geneploter",
              "edd", "Roc", "tkWidgets")
    switch(libName,
           "all" = return(c(EXPRS, AFFY, CDNA)),
           "exprs" = return(EXPRS),
           "affy" = return(EXPRS, AFFY),
           "cdna" =,
           "CDNA" = return(c(EXPRS, CDNA)),
           stop("The library is not valid"))
}

getLibName <- function (platform, lib){
     # .Platform$file.sep returns "/" under windows. Hard code for now
     switch(platform,
            "unix" = return (paste(pack, "_", getVersion(),
            ".tar.gz", sep = "")),
            "windows" = return(paste(pack, "-snapshot.zip", sep = "")),
            stop("The OS system is not supported"))
}

getDLUrl <- function(platform, isDevel = FALSE){
    if(isDevel)
        tempUrl <-
            "http://www.bioconductor.org/packages/distrib//devel/"
    else
        tempUrl <-
            "http://www.bioconductor.org/packages/distrib//release/"
    switch(platform,
            "unix" = return (paste(tempUrl, "Source", sep = "")),
            "windows" = return(paste(tempUrl,"Win32", sep = "")),
            stop("The OS system is not supported"))
}

getUrl <- function(platform, pack, isDevel = FALSE){
     switch(platform,
            "unix" = return (paste(getDLUrl(platform, isDevel),
            "/", pack, "_", getVersion(), ".tar.gz", sep = "")),
            "windows" = return(paste(getDLUrl(platform, isDevel),
            "/", pack, "-snapshot.zip", sep = "")))
}

getFName <- function(platform, destdir, pack){
     # .Platform$file.sep returns "/" under windows. Hard code for now
     switch(platform,
            "unix" = return (paste(destdir, .Platform$file.sep,
            pack, "_", getVersion(), ".tar.gz", sep = "")),
            "windows" = return(paste(destdir, "\\",
            pack, "-snapshot.zip", sep = "")),
            stop("The OS system is not supported"))
}

getVersion <- function(isDevel = FALSE){
    if(isDevel){
        return("1.0")
    }else{
        return("1.0")
    }
}

installPack <- function(platform, fileName){
    if(platform == "unix"){
        tt <- system(paste("R CMD INSTALL ", fileName, sep = ""), TRUE)
    }else{
        if(platform == "windows"){
            tt <- system(paste("Rcmd INSTALL ", fileName, sep = ""), TRUE)
        }else{
            stop("The OS system is not supported")
        }
    }
}








