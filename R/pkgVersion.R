package.version <- function(pkg, lib.loc = NULL) {

    curWarn <- getOption("warn")
    on.exit(options(warn=curWarn),add=TRUE)
    options(warn=-1)

    desc <- package.description(pkg, lib.loc, "Version")
    if (is.na(desc))
        stop(paste("Package",pkg,"does not exist"))
    desc

}
