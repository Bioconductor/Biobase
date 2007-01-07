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
                pdfname = vigs[, "PDF"]
                tit     = vigs[, "Title"]
                vigPaths = ifelse(pdfname=="", as.character(NA),
                             file.path(dirname(x), "..", "doc", pdfname))
                names(vigPaths) = ifelse(tit=="", "(no title)", tit)
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
      warning(paste(sep="", "No vignettes found",
       ifelse(is.null(package), ".\n", sprintf(" for package%s %s.\n",
          ifelse(length(package)==1, "", "s"), paste(package, collapse=", ")))))
    } else {
      nm <- paste("",names(vigFiles))  ##indent a little
      
      ##FIXME: why set names to NULL?
      names(vigFiles) <- NULL
      index <- menu(nm, title="Please select (by number) a vignette")

      if (index > 0) {
        vi = vigFiles[index]
        if(!is.na(vi)) {
          openPDF(vi)
          cat("Opening", vi, "\n")
          ## browseURL(paste("file://", vigFiles[index],sep=""))
        } else {
          stop("Sorry, this vignette seems to be corrupted on your system, cannot open it.\n",
               "You might try to install the package.")
        }
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
        ans <- TRUE
    } else {
        ans <- FALSE
    }
    ans
}

