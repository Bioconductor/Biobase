# Functions that add vignetts or other elements to the menu bar of a window.
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
        # First try to add the menu item
        options(show.error.messages = FALSE)
        tryMe <- try(winMenuAddItem(menuName, itemName, itemAction))
        options(show.error.messages = TRUE)
        if(inherits(tryMe, "try-error")){
            # Menu does not exist for the item. Add menus
            addNonExisting(menuName)
            winMenuAddItem(menuName, itemName, itemAction)
        }
    }
}

# Add menu for a window in Unix
addVig4Unix <- function(menuName, itemName, itemAction){
    # "Do not know what to do yet"
}

# Find and add all the non-existing menu elelments
addNonExisting <- function(menuName){
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
    path <- .path.package(pkgName)
    pdfs <- dir(file.path(path, "doc"), pattern = ".pdf")
    for(i in pdfs){
        item <- sub(".pdf", "", i)
        pdfPath <- file.path(path, "doc", i)
        addVig2Menu(item, menuName = paste("Vignettes/", pkgName, sep = ""),
                    itemAction = paste("shell.exec(\"", pdfPath, "\")",
                    sep = ""))
    }
}
