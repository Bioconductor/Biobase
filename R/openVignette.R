getPkgVigs <- function(package=NULL) {
    pkgs <- .packages()

    if( !is.null(package) )
    {
        if( !is.character(package) )
            stop("package list must be a character vector")
        rows <- match(package, pkgs)
        pkgs <- pkgs[rows]
    }
    vigDirs <- file.path(.find.package(pkgs), "doc/00Index.dcf")

    vigFiles <- lapply(vigDirs, function(x){
        if (file.exists(x)) {
            vigs <- read.dcf(x)
            if (nrow(vigs) > 0) {
                vigPaths <- file.path(dirname(x),colnames(vigs))
                vigNames <- as.character(vigs)
                names(vigPaths) <- vigNames
                vigPaths
            }
            else
                NULL
        }
        else NULL
    })
    vigFiles <- unlist(vigFiles[sapply(vigFiles,function(x){
        if ((!is.null(x))&&(length(x) > 0)) TRUE else FALSE})])

    vigFiles
}

openVignette <- function(package=NULL) {
    vigFiles <- getPkgVigs(package)
    names <- names(vigFiles)
    names <- paste("",names)
    names(vigFiles) <- NULL
    index <- menu(names, title="Please select (by number) a vignette")

    if (index > 0) {
        ## Need to switch on the file extension
        ext <- strsplit(vigFiles[index],"\\.")[[1]]
        switch(ext[length(ext)],
               "pdf"=openPDF(vigFiles[index]),
               "html"=browseURL(paste("file://",vigFiles[index],sep="")),
               stop("Don't know how to handle this vignette")
               )
    }
}
