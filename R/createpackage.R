createPackage <- function(pkgname, destinationDir, originDir, symbolValues,
                          unlink=FALSE, quiet=FALSE)
{
  ## check arguments 
  for (a in c("pkgname", "destinationDir", "originDir"))
    if (!is.character(get(a)) || length(get(a))!=1)
      stop(paste("'", a, "' must be a character vector of length 1.", sep=""))
  
  ## check whether destinationDir, originDir exist and are directories
  for (a in c("destinationDir", "originDir"))
    if(!file.info(get(a))$isdir)
      stop(paste("'", a, "' must be a directory (", get(a), ")\n.", sep=""))

  ## locate / remove / create destination directory
  pkgdir = file.path(destinationDir, pkgname)
  if (!quiet)
    cat("Creating package in", pkgdir, "\n")
  
  if (file.exists(pkgdir)) {
    if (unlink) {
      unlink(pkgdir, recursive=TRUE)
      if (file.exists(pkgdir)) {
        stop(paste("Directory", pkgdir, "exists and could not be removed.",
                   "Please remove it manually or choose another destination directory."))
      } else {
        if(!quiet)
          cat(paste("Existing", pkgdir, "was removed.\n"))
      }
    } else {
      stop(paste("Directory", pkgdir, "exists. Please use unlink=TRUE to remove it",
                 "or choose another destination directory."))
    } ## if (unlink) else 
  }  ## if (file.exists)

  ## predefined symbols
  symbolValues = append(symbolValues, list(TODAY=date(), PKGNAME=pkgname))

  ## copy
  copySubstitute(dir(originDir, full.names=TRUE), pkgdir, symbolValues, recursive=TRUE)
    
  return(list(pkgdir=pkgdir))
}

