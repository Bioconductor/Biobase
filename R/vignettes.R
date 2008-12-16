## Functions to detect and open vignettes:
## uses tools.R

getPkgVigs = function(package=NULL) {
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
  vigrds = file.path(.find.package(pkgs), "Meta", "vignette.rds")
  
  ## construct data frame with: package, path, title
  pkgVigs = vector(mode="list", length=length(vigrds))
  for(j in seq(along=vigrds)) {
    if (file.exists(vigrds[j])) {
      v = .readRDS(vigrds[j])
      f = v[, "PDF"]
      f = ifelse(f=="",
        as.character(NA),
        file.path(dirname(dirname(vigrds[j])), "doc", f))
      tit = v[, "Title"]
      tit[tit==""] = as.character(NA)
      rv= data.frame(package  = pkgs[j],
                     filename = f,
                     title    = tit,
                     stringsAsFactors=FALSE)
      pkgVigs[[j]] = rv
    }
  } ## for j
  do.call(rbind, args=pkgVigs)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
openVignette = function(package=NULL) {
  vig = getPkgVigs(package)
  if(is.null(vig)) {
    warning(paste(sep="", "No vignettes found",
       ifelse(is.null(package), ".\n", sprintf(" for package%s %s.\n",
              ifelse(length(package)==1, "", "s"), paste(package, collapse=", ")))))
  } else {
    hasnofile = is.na(vig$filename)
    vig$title[hasnofile] = paste(vig$title[hasnofile], "[-]")
    vig = vig[order(hasnofile, tolower(vig$package), tolower(vig$title)), ]
    index = menu(paste(vig$package, "-", vig$tit),
      title = paste("Please select a vignette:",
        if(any(hasnofile))
          "(entries marked by '[-]' have no PDF file)"
        else
          ""
        ))

    if (index>0) {
      vif = vig$filename[index]
      if(!is.na(vif)) {
        openPDF(vif)
        cat("Opening", vif, "\n")
        ## browseURL(paste("file://", vig[index],sep=""))
      } else {
        stop("Sorry, no PDF file could be found for this vignette.\n",
             "Please reinstall the package with built vignettes.")
      }
    }
  }
}

#-----------------------------------------------------------
# add package vignettes to the menu bar of the Windows Rgui
##----------------------------------------------------------
addVigs2WinMenu = function(pkgName) {
  if ((.Platform$OS.type == "windows") &&
      (.Platform$GUI == "Rgui") &&
      interactive()) {
      
    vigFile = system.file("Meta", "vignette.rds", package=pkgName)
    if (!file.exists(vigFile)) {
      warning(sprintf("%s contains no vignette, nothing is added to the menu bar", pkgName))
    } else {
      vigMtrx = .readRDS(vigFile)
      vigs = file.path(.find.package(pkgName), "doc", vigMtrx[, "PDF"])
      names(vigs) = vigMtrx[,"Title"]

      if (!"Vignettes" %in% winMenuNames())
        winMenuAdd("Vignettes")
      
      pkgMenu = paste("Vignettes", pkgName, sep="/")
      winMenuAdd(pkgMenu)

      for (i in seq(along=vigs))
        winMenuAddItem(pkgMenu, names(vigs)[i],
                       paste("shell.exec(\"", vigs[i], "\")", sep=""))

    } ## else
    ans = TRUE

  } else {
    ans = FALSE
  } ## else
  ans
}

