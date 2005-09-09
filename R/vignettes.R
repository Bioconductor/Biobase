# ==========================================================================
# Functions to detect and open vignettes:
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# getPkgVigs; openVignette; addVigs2WinMenu
# deprecated: addVig2Menu; addVig4Win; addVig4Unix; addNonExisting; addPDF2Vig
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# uses tools.R
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
    vigFile <- system.file("Meta", "vignette.rds", package=pkgName)
    if (file.exists(vigFile)) {
        vigMtrx <- .readRDS(vigFile)
        vigs <- file.path(.find.package(pkgName), "doc", vigMtrx[,"PDF"])
        names(vigs) <- vigMtrx[,"Title"]
    }
    if (!"Vignettes" %in% winMenuNames())
        winMenuAdd("Vignettes")
    pkgMenu <- paste("Vignettes", pkgName, sep="/")
    winMenuAdd(pkgMenu)
    for (i in vigs) {
        item <- sub(".pdf", "", basename(i))
        winMenuAddItem(pkgMenu, item, paste("shell.exec(\"", as.character(i), "\")", sep = ""))
    }
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# DEPRECATED (all below)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
addVig2Menu <- function(itemName, menuName = "Vignettes", itemAction = ""){
    .Deprecated("addVigs2WinMenu")
    os <- .Platform$OS.type
    switch(os,
           "windows" = addVig4Win(menuName, itemName,itemAction ),
           "unix" = addVig4Unix(menuName, itemName, itemAction),
           stop("Unknown operating system"))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Add menu for windows
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
addVig4Win <- function(menuName, itemName, itemAction){
    .Deprecated("addVigs2WinMenu")
    options(show.error.messages = FALSE)
    tryMe <- try(winMenuAddItem(menuName, itemName, itemAction))
    options(show.error.messages = TRUE)
    if(inherits(tryMe, "try-error")){
        # Menu does not exist for the item. Add menus
        addNonExisting(menuName)
        winMenuAddItem(menuName, itemName, itemAction)
    }
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Add menu for a window in Unix
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
addVig4Unix <- function(menuName, itemName, itemAction){
   .Deprecated("addVigs2WinMenu")
   # "Do not know what to do yet"
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Find and add all the non-existing menu elelments
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
addNonExisting <- function(menuName){
    .Deprecated("addVigs2WinMenu")
    temp <- menuName
    menus <- unlist(strsplit(menuName, "/"))
    counter <- 1
    # Find and add the first missing menu along the menu tree
    repeat{
        options(show.error.messages = FALSE)
        tryMe <- try(winMenuAdd(temp))
        options(show.error.messages = TRUE)
        if(inherits(tryMe, "try-error")){
            temp <- paste(menus[1:(length(menus) - counter)], sep = "",
                          collapse = "/")
            counter <- counter + 1
        }else{
            break
        }
    }
    # Add the rest menus
    if(counter > 1){
        for(i in ((length(menus) - counter + 2):length(menus))){
            temp <-  paste(menus[1:i], sep = "", collapse = "/")
            winMenuAdd(temp)
        }
    }
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Add click-able menu items to view the pdf files of a package
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
addPDF2Vig <- function(pkgName){
    .Deprecated("addVigs2WinMenu")
    addVigs2WinMenu(pkgName)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

