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
   cat("\nWelcome to Bioconductor \n\n")
   cat("\tVignettes contain introductory material.\n")
   cat("\tTo view, simply type 'openVignette()' or start with 'help(Biobase)'. \n")
   cat("\tFor details on reading vignettes, see the openVignette help page.\n\n")
   ##set up menus -- windows only for now
   if( .Platform$OS.type == "windows" && .Platform$GUI == "Rgui" )
      addVigs2WinMenu("Biobase") # in vignettes.R
}
