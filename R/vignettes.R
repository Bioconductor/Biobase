## Functions to detect and open vignettes:
## uses tools.R

getPkgVigs <- function(package=NULL) {
    require("tools", quietly=TRUE) || stop("Requires the tools package")
    pkgs <- .packages()
    if( !is.null(package) ) {
        if( !is.character(package) )
            stop("`package' must be a character vector of package names")
        rows <- match(package, pkgs)
        if( all(is.na(rows)) )
            stop("packages: ", paste(package,collapse=", "), " are not loaded")
        if( any(is.na(rows)) )
            warning("packages ", paste(package[is.na(rows)], collapse=", "), " are not loaded")
        pkgs <- pkgs[rows[!is.na(rows)]]
    }
    vigDirs <- file.path(.find.package(pkgs), "Meta", "vignette.rds")
    vigs <- lapply(vigDirs,
       function(x){
          if (file.exists(x)) {
             vigs <- .readRDS(x)
             if (nrow(vigs) > 0) {
                vigPaths <- file.path(dirname(x),"..", "doc", vigs[,"PDF"])
                names(vigPaths) <- vigs[,"Title"]
                vigPaths
             } # else NULL
          } # else NULL
       }
    )
    unlist(vigs[sapply(vigs, function(x) !is.null(x) && (length(x) > 0))])
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
openVignette <- function(package=NULL) {
    vigFiles <- getPkgVigs(package)
    if(is.null(vigFiles)) {
      warning(package, " contains no vignettes")
    } else {
      names <- names(vigFiles)
      ##indent a little
      names <- paste("",names)
      ##FIXME: why set names to NULL?
      names(vigFiles) <- NULL
      index <- menu(names, title="Please select (by number) a vignette")

      if (index > 0) {
        ## Need to switch on the file extension
        ext <- strsplit(vigFiles[index],"\\.")[[1]]
        switch(ext[length(ext)],
           "pdf" = openPDF(vigFiles[index]),
           "html"= browseURL(paste("file://",vigFiles[index],sep="")),
           stop("Don't know how to handle this vignette")
        )
      }
    }
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# add vignetts or other elements to the menu bar of a window (2002 J. Zhang)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
addVigs2WinMenu <- function(pkgName) {
    if ((.Platform$OS.type == "windows") && (.Platform$GUI == "Rgui")
        && interactive()) {
        vigFile <- system.file("Meta", "vignette.rds", package=pkgName)
        if (!file.exists(vigFile)) {
            warning(sprintf("%s contains no vignette, nothing is added to the menu bar", pkgName))
        } else {
            vigMtrx <- .readRDS(vigFile)
            vigs <- file.path(.find.package(pkgName), "doc", vigMtrx[,"PDF"])
            names(vigs) <- vigMtrx[,"Title"]

            if (!"Vignettes" %in% winMenuNames())
              winMenuAdd("Vignettes")
            pkgMenu <- paste("Vignettes", pkgName, sep="/")
            winMenuAdd(pkgMenu)
            for (i in vigs) {
                item <- sub(".pdf", "", basename(i))
                winMenuAddItem(pkgMenu, item, paste("shell.exec(\"", as.character(i), "\")", sep = ""))
            }
        } ## else
    }
}

