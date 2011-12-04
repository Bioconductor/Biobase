# ==========================================================================
# Biobase package initialization
# ==========================================================================
.onLoad <- function(libname, pkgname) {
    ## need contents to load at library attach - not at build time
    .initContents() ## in environment.R
    .buildBiobaseOpts() ## in environment.R
}

.onAttach <- function(libname, pkgname) {
    msg0 <- "Vignettes contain introductory material; view with
             'browseVignettes()'. To cite Bioconductor, see
             'citation(\"Biobase\")', and for packages
             'citation(\"pkgname\")'."
    msg <- strwrap(paste(msg0, collapse=""), exdent=4, indent=4)
    packageStartupMessage(paste(c("Welcome to Bioconductor\n", msg),
                                collapse="\n"), "\n")
    addVigs2WinMenu("Biobase") 
}

.onUnload <- function( libpath ) {
  library.dynam.unload( "Biobase", libpath )
}
