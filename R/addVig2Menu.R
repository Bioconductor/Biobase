# Functions that add vignetts or other elements to the menu bar of a window.
#
# Copyright 2002 J. Zhang, all rights reserved
#

addVig2Menu <- function(itemName, menuName = "Vignettes", itemAction = ""){
    .Deprecated("addVigs2WinMenu")
    os <- .Platform$OS.type
    switch(os,
           "windows" = addVig4Win(menuName, itemName,itemAction ),
           "unix" = addVig4Unix(menuName, itemName, itemAction),
           stop("Unknown operating system"))

}
# Add menu for windows
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

# Add menu for a window in Unix
addVig4Unix <- function(menuName, itemName, itemAction){
    .Deprecated("addVigs2WinMenu")

                                        # "Do not know what to do yet"
}

# Find and add all the non-existing menu elelments
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

# Add click-able menu items to view the pdf files of a package
addPDF2Vig <- function(pkgName){
    .Deprecated("addVigs2WinMenu")

    addVigs2WinMenu(pkgName)
}

addVigs2WinMenu <- function(pkgName) {
    vigFile <- system.file("Meta", "vignette.rds", package=pkgName)
    if (file.exists(vigFile)) {
        vigMtrx <- .readRDS(vigFile)
        vigs <- file.path(.find.package(pkgName),
                                  pkgName, "doc", vigMtrx[,"PDF"])
        names(vigs) <- vigMtrx[,"Title"]
    }

    if (! "Vignettes" %in% winMenuNames())
        winMenuAdd("Vignettes")

    pkgMenu <- paste("Vignettes", pkgName, sep="/")
    winMenuAdd(pkgMenu)

    for (i in vigs) {
        item <- sub(".pdf", "", basename(i))
        winMenuAddItem(pkgMenu, item, paste("shell.exec(\"",
                                            as.character(i),
                                            "\")", sep = ""))
    }
}

