.First.lib <- function(libname, pkgname, where)
 { require(methods)
    where <- match(paste("package:", pkgname, sep=""), search())
    cacheMetaData(as.environment(where))
}
