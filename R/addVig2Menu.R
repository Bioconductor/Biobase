# Functions that add vignetts to the menu bar of a window.
#
# Copyright 2002 J. Zhang, all rights reserved
#

addVig2Menu <- function(itemName, menuName = "Vignettes",
                         doThis = paste("vExplorer(pkgName = \"",
                               itemName, "\")", sep = "")){
    require(tkWidgets) || stop("here")
    os <- .Platform$OS.type
    switch(os,
           "windows" = addVig4Win(menuName, itemName, doThis),
           "unix" = addVigsUnix(menuName, itemName, doThis),
           stop("Unknown operating system"))

}
# Add menu for windows
addVig4Win <- function(menuName, itemName, doThis){
    # tkWidgets will be loaded to make vExplorer available
    if(require(tkWidgets)){
        # Try to see if the menu already exists
        options(show.error.messages = FALSE)
        tryMe <- try(winMenuAddItem(menuName, itemName, doThis))
        options(show.error.messages = TRUE)
        # No existing menu yet, add one
        if(inherits(tryMe, "try-error")){
            winMenuAdd(menuName)
            winMenuAddItem(menuName, itemName, doThis)
        }
    }
}
# Add menu for a window in Unix
addVig4Unix <- function(menuName, itemName, doThis){
    # Do not know what to do yet
}
