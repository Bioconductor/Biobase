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
    vigDirs <- file.path(libs, pkgs, "doc")

    pdfFiles <- lapply(vigDirs, function(x) {
        if (file.exists(x)) {
            dir(x,pattern=".pdf",full.names=TRUE)
        } else NULL})
    pdfFiles <- unlist(pdfFiles[sapply(pdfFiles,function(x){
        if ((!is.null(x))&&(length(x) > 0)) TRUE else FALSE})])

    pdfs <- unlist(strsplit(basename(pdfFiles),".pdf"))
    names(pdfFiles) <- pdfs
    pdfFiles
}

openVignette <- function(package=NULL) {
    pdfFiles <- getPkgPDFs(package)
    names <- names(pdfFiles)
    names(pdfFiles) <- NULL
    index <- menu(names, title="Please select (by number) a vignette")

    if (index > 0)
        openPDF(pdfFiles[index])
}
