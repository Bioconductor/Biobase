# ==========================================================================
# Functions to operate with packages:
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# createPackage; package.version; dumpPackTxt
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# uses strings.R
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
createPackage <- function(pkgname, destinationDir, originDir, symbolValues,
                          unlink=FALSE, quiet=FALSE) {
   ## check arguments
   for (a in c("pkgname", "destinationDir", "originDir"))
      if (!is.character(get(a)) || length(get(a))!=1)
         stop("'", a, "' must be character(1)")
   ## check whether destinationDir, originDir exist and are directories
   for (a in c("destinationDir", "originDir"))
      if(!file.exists(get(a)) || !file.info(get(a))$isdir)
         stop("'", a, "' must be a directory (", get(a), ")")
   ## locate / remove / create destination directory
   pkgdir = file.path(destinationDir, pkgname)
   if (!quiet)
      cat("Creating package in", pkgdir, "\n")
   if (file.exists(pkgdir)) {
      if (unlink) {
         unlink(pkgdir, recursive=TRUE)
         if (file.exists(pkgdir)) {
            stop("directory '", pkgdir, "' exists and could not be removed; ",
                 "remove it manually or choose another destination directory")
         }
         else {
            if(!quiet)
               cat("existing", pkgdir, "was removed.\n")
         }
      }
      else
         stop("directory '", pkgdir, "' exists; use unlink=TRUE ",
              "to remove it, or choose another destination directory")
   } ## if (file.exists)
   ## predefined symbols
   symbolValues = append(symbolValues, list(TODAY=date(), PKGNAME=pkgname))
   ## copy (from strings.R)
   copySubstitute(dir(originDir, full.names=TRUE), pkgdir, symbolValues, recursive=TRUE)
   return(list(pkgdir=pkgdir))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
package.version <- function(pkg, lib.loc = NULL) {
   curWarn <- getOption("warn")
   on.exit(options(warn=curWarn),add=TRUE)
   options(warn=-1)
   desc <- packageDescription(pkg, lib.loc, "Version")
   if (is.na(desc))
      stop("package '", pkg, "' does not exist")
   desc
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
dumpPackTxt <- function (package) {
   # stolen from "library" to get descriptive info out on stdout
   pkgpath <- find.package(package)
#   outFile <- tempfile("Rlibrary")
#   outConn <- file(outFile, open = "w")
   docFiles <- file.path(pkgpath, c("TITLE", "DESCRIPTION", "INDEX"))
   headers <- c("", "Description:\n\n", "Index:\n\n")
   footers <- c("\n", "\n", "")
   for (i in which(file.exists(docFiles))) {
      writeLines(headers[i], sep = "")
      writeLines(readLines(docFiles[i]) )
      writeLines(footers[i], sep = "")
   }
#   close(outConn)
#   file.show(outFile, delete.file = TRUE)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

 ##strictly internal, not for export
 ##take an AnnotatedDataFrame object and create a valid format
 ## section for a man page
 pD2Rd <- function(pD) {
   if(!inherits(pD, "AnnotatedDataFrame") )
     stop("only works for AnnotatedDataFrames")

   fmt = "\\format{\n  The format is:\n  An \\code{ExpressionSetObject} with covariates:\n"
   covs = "\\itemize{"
   vMD = varMetadata(pD)
   vL = varLabels(pD)
   for(i in 1:length(vL) ) {
     item = paste0("\\item \\code{", vL[i], "}: ", vMD[i,1])
     covs = paste(covs, item, sep="\n")
   }
   paste0(fmt, covs, "\n}\n}\n")
 }
