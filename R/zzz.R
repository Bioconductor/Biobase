# temporarily store this utility here

dumpPackTxt <- function (package)
{
# stolen from "library" to get descriptive
# info out on stdout
#
    pkgpath <- .find.package(package)
#    outFile <- tempfile("Rlibrary")
#    outConn <- file(outFile, open = "w")
    docFiles <- file.path(pkgpath, c("TITLE", "DESCRIPTION",
        "INDEX"))
    headers <- c("", "Description:\n\n", "Index:\n\n")
    footers <- c("\n", "\n", "")
    for (i in which(file.exists(docFiles))) {
        writeLines(headers[i], sep = "")
        writeLines(readLines(docFiles[i]) )
        writeLines(footers[i], sep = "")
    }
#    close(outConn)
#    file.show(outFile, delete.file = TRUE)
}

.First.lib <- function(libname, pkgname, where) {
    require(methods)
    where <- match(paste("package:", pkgname, sep=""), search())
    .initContainer(where)
    .initAgg(where)
    .initExprset(where)
    cacheMetaData(as.environment(where))
}
