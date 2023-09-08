# ==========================================================================
# Biobase package initialization
# ==========================================================================
.onLoad <- function(libname, pkgname) {
    ## need contents to load at library attach - not at build time
    .initContents() ## in environment.R
    .buildBiobaseOpts() ## in environment.R
}

.onUnload <- function( libpath ) {
  library.dynam.unload( "Biobase", libpath )
}
