## Versions previously defined
.VersionsNull <- setClass("VersionsNull", contains="Versions")
.Versioned <- setClass("Versioned",
    representation(.__classVersion__ = "Versions"))
.VersionedBiobase <- setClass("VersionedBiobase",
    contains="Versioned",
    prototype=prototype(
      .__classVersion__ = .Versions(
        R = getRversion(),
        Biobase = package_version(packageDescription("Biobase", NULL, "Version")))))
