# ==========================================================================
# Biobase package initialization
# ==========================================================================
.onLoad <- function(libname, pkgname) {
    require("methods")
    ## need contents to load at library attach - not at build time
    .initContents() ## in environment.R
    .buildBiobaseOpts() ## in environment.R
}

.onAttach <- function(libname, pkgname) {
   message("\nWelcome to Bioconductor\n")
   message("    Vignettes contain introductory material. To view, type")
   message("    'openVignette()' or start with 'help(Biobase)'. For details")
   message("    on reading vignettes, see the openVignette help page.\n")
   ##set up menus -- windows only for now
   if( .Platform$OS.type == "windows" && .Platform$GUI == "Rgui" )
      addVigs2WinMenu("Biobase") # in vignettes.R
}

.onUnload <- function( libpath ) {
  library.dynam.unload( "Biobase", libpath )
}
