getPkgPDFs <- function(package=NULL) {
    pkgs <- installed.packages()[,"Package"]
    libs <- installed.packages()[,"LibPath"]

    if( !is.null(package) )
    {
        if( !is.character(package) )
            stop("package list must be a character vector")
        rows <- match(package, pkgs)
        pkgs <- pkgs[rows]
        libs <- libs[rows]
    }
    vigDirs <- file.path(libs, pkgs, "doc/00Index.dcf")

    pdfFiles <- lapply(vigDirs, function(x){
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
    pdfFiles <- unlist(pdfFiles[sapply(pdfFiles,function(x){
        if ((!is.null(x))&&(length(x) > 0)) TRUE else FALSE})])

    pdfFiles
}

openVignette <- function(package=NULL) {
    pdfFiles <- getPkgPDFs(package)
    names <- names(pdfFiles)
    names <- paste("",names)
    names(pdfFiles) <- NULL
    index <- menu(names, title="Please select (by number) a vignette")

    if (index > 0)
        openPDF(pdfFiles[index])
}
