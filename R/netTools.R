testBioCConnection <- function() {
    ## Stifle the "connected to www.... garbage output
    curNetOpt <- getOption("internet.info")
    on.exit(options(internet.info=curNetOpt), add=TRUE)
    options(internet.info=3)

    ## First check to make sure they have HTTP capability.  If they do
    ## not, there is no point to this exercise.
    http <- as.logical(capabilities(what="http/ftp"))
    if (http == FALSE)
        return(FALSE)

    ## find out where we think that bioC is
    bioCoption <- getOption("BIOC")
    if (is.null(bioCoption))
        bioCoption <- "http://www.bioconductor.org"

    ## Now check to see if we can connect to the BioC website
    biocURL <- url(paste(bioCoption,"/main.html",sep=""))
    options(show.error.messages=FALSE)
    test <- try(readLines(biocURL)[1])
    options(show.error.messages=TRUE)
    if (inherits(test,"try-error"))
        return(FALSE)
    else
        close(biocURL)

    return(TRUE)
}

