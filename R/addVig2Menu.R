# Functions that add vignetts to the menu bar of a window.
#
# Copyright 2002 J. Zhang, all rights reserved
#

addVig2Menu <- function(itemName, menuName = "Vignettes",
                         itemAction = paste("vExplorer(pkgName = \"",
                               itemName, "\")", sep = "")){
    os <- .Platform$OS.type
    switch(os,
           "windows" = addVig4Win(menuName, itemName,itemAction ),
           "unix" = addVig4Unix(menuName, itemName, itemAction),
           stop("Unknown operating system"))

}
# Add menu for windows
addVig4Win <- function(menuName, itemName, itemAction){
    # tkWidgets will be loaded to make vExplorer available
    if(require(tkWidgets)){
        # Try to see if the menu already exists
        options(show.error.messages = FALSE)
        tryMe <- try(winMenuAddItem(menuName, itemName, itemAction))
        options(show.error.messages = TRUE)
        # No existing menu yet, add one
        if(inherits(tryMe, "try-error")){
            winMenuAdd(menuName)
            winMenuAddItem(menuName, itemName, itemAction)
        }
    }
}
# Add menu for a window in Unix
addVig4Unix <- function(menuName, itemName, itemAction){
    warning("Do not know what to do yet")
}
