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
    packageStartupMessage(
        paste("\nWelcome to Bioconductor\n",
                "Vignettes contain introductory material. To view, type",
                "'openVignette()'. To cite Bioconductor, see",
                "'citation(\"Biobase\")' and for packages 'citation(pkgname)'.\n", sep="\n  "))
   addVigs2WinMenu("Biobase") 
}

.onUnload <- function( libpath ) {
  library.dynam.unload( "Biobase", libpath )
}
