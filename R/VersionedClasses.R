## Versions previously defined
setClass("VersionsNull", contains="Versions")
setClass("Versioned", representation(.__classVersion__ = "Versions"))
setClass("VersionedBiobase",
         contains="Versioned",
         prototype=prototype(
           .__classVersion__ = new("Versions", 
             R = getRversion(),
             Biobase = package_version(packageDescription("Biobase", NULL, "Version")))))
