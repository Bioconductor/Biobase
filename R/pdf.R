setOptionPdfViewer <- function(viewer,verbose=FALSE) {
    if (missing(viewer)) {
        viewer <- getOption("pdfviewer")
        if (is.null(viewer)) {
            for (x in c("xpdf","acroread","acroread4")) {
                viewer<-system(paste("which",x),intern=TRUE,
                               ignore.stderr=TRUE)
                if( length(viewer) > 0 && file.exists(viewer) )
                    break
                ## It is important to break here due to an OSX problem
                viewer <- character()
            }
            if (length(viewer) == 0) {
                warning("No available PDF viewer found on system")
                return(FALSE)
            }
            if (verbose == TRUE)
                note(paste("Selecting PDF viewer",viewer))
        }
    }

    ## Probably can get away with a few less steps here
    bioOpt <- getOption("BioC")
    bioBase <- bioOpt$Base
    bioBase$pdfViewer <- viewer
    bioOpt$Base <- bioBase
    options("BioC"=bioOpt)
    return(TRUE)
}

openPDF <- function(file, bg=TRUE) {
    OST <- .Platform$OS.type
    if (OST=="windows") {
        shell.exec(file)
    }
    else if (OST == "unix") {
        bioCOpt <- getOption("BioC")
        pdf <- bioCOpt$Base$pdfViewer
        if (is.null(pdf)) {
            warning(paste("pdfViewer is set to:",pdf,
                          "which does not seem to exist.  Please",
                          "run the command setOptionPdfViewer()"))
            return(FALSE)
        }
        cmd <- paste(pdf,file)
        if( bg )
          cmd <- paste(cmd, "&")
        system(cmd)
    }
    return(TRUE)
}


