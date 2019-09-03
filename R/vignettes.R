## Functions to detect and open vignettes:
## uses tools.R

getPkgVigs = function(package=NULL) {
  pkgs <- .packages()
  if( !is.null(package) ) {
    if( !is.character(package) )
      stop("`package' must be a character vector of package names")
    rows <- match(package, pkgs)
    if( all(is.na(rows)) )
        stop("packages '", paste(package, collapse="', '"),
             "' are not loaded")
    if( any(is.na(rows)) )
        warning("packages '",
                paste(package[is.na(rows)], collapse="', '"),
                "' are not loaded")
    pkgs <- pkgs[rows[!is.na(rows)]]
  }
  vigrds = file.path(find.package(pkgs), "Meta", "vignette.rds")
  
  ## construct data frame with: package, path, title
  pkgVigs = vector(mode="list", length=length(vigrds))
  for(j in seq(along.with=vigrds)) {
    if (file.exists(vigrds[j])) {
      v = readRDS(vigrds[j])
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
  if (is.null(vig)) {
      txt <- ""
      if (!is.null(package)) {
          ss <- if (length(package)==1) "" else "s"
          pkgs <- paste(package, collapse=", ")
          txt <- sprintf(" for package%s %s", ss, pkgs)
      }
      warning("no vignettes found", txt)
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
        ## browseURL(paste0("file://", vig[index]))
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
      vigMtrx = readRDS(vigFile)
      vigs = file.path(find.package(pkgName), "doc", vigMtrx[, "PDF"])
      names(vigs) = vigMtrx[,"Title"]

      if (!"Vignettes" %in% winMenuNames())
        winMenuAdd("Vignettes")
      
      pkgMenu = paste("Vignettes", pkgName, sep="/")
      winMenuAdd(pkgMenu)

      for (i in seq(along.with=vigs))
        winMenuAddItem(pkgMenu, names(vigs)[i],
                       paste0("shell.exec(\"", vigs[i], "\")"))

    } ## else
    ans = TRUE

  } else {
    ans = FALSE
  } ## else
  ans
}

