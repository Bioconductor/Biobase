require("Biobase") || stop("unable to load Biobase")
BiocGenerics:::testPackage("Biobase", "UnitTests", ".*_test\\.R$")
