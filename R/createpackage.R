createPackage <- function(pkgname, destinationDir, originDir, symbolValues,
                          force=FALSE, quiet=FALSE)
{
  if(any(!is.character(c(pkgname, destinationDir, originDir))))
    stop(paste("Arguments pkgname, destinationDir, originDir must be character strings."))
  
  ## check whether destinationDir and originDir exist and are directories
  fi <- file.info(c(destinationDir, originDir))
  if(!all(fi$isdir))
    stop(paste("destinationDir:", destinationDir, "and originDir:", originDir,
               "must be directories."))

  ## create the package directories
  pkgdir = file.path(destinationDir, pkgname)
  if (!quiet)
    cat("Creating package in", pkgdir, "\n")
  if (file.exists(pkgdir)) {
    if (force) {
      unlink(pkgdir, recursive=TRUE)
      if (file.exists(pkgdir)) {
        stop(paste("Directory", pkgdir, "exists and could not be removed.",
                   "Please remove it manually or choose another destination directory."))
      } else {
        if(!quiet)
          cat(paste("Existing", pkgdir, "was removed.\n"))
      }
    } else {
      stop(paste("Directory", pkgdir, "exists. Please use force=TRUE to remove it",
                 "or choose another destination directory."))
    } ## if(force)else 
  }  ## if (file.exists)
  
  for (d in c("", "man", "src", "R", "data")) {
    res = dir.create(file.path(pkgdir, d))
    if(!res)
      stop(paste("Failed to create directory", file.path(pkgdir, d)))
  }

  ## predefined symbols
  symbolValues = append(symbolValues, list(TODAY=date(), PKGNAME=pkgname))
  
  ## DESCRIPTION
  copySubstitute(file.path(originDir, "DESCRIPTION"),
                 pkgdir, symbolValues, recursive=TRUE)
    
  ## R files
  for (fn in list.files(originDir, pattern="*\\.R$"))
    copySubstitute(file.path(originDir, fn), file.path(pkgdir, "R"), symbolValues, recursive=TRUE)
  
  ## C files
  for (fn in list.files(originDir, pattern="*\\.c$"))
    copySubstitute(file.path(originDir, fn), file.path(pkgdir, "src"), symbolValues, recursive=TRUE)
  
  ## man files
  for (fn in list.files(originDir, pattern="*\\.Rd$"))
    copySubstitute(file.path(originDir, fn), file.path(pkgdir, "man"), symbolValues, recursive=TRUE)
  
  return(list(pkgdir=pkgdir))
}

