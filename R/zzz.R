.First.lib <- function(libname, pkgname, where) {
    require(methods)
    where <- match(paste("package:", pkgname, sep=""), search())
    .initContainer(where)
    .initAgg(where)
    .initExprset(where)
    cacheMetaData(as.environment(where))
}
