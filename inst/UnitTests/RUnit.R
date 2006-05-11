library(Biobase)
library(RUnit)

## instantiate here to satisfy RUnit

setClass("SwirlSet", contains="eSet")
setMethod("initialize", "SwirlSet",
          function(.Object,
                   phenoData = new("AnnotatedDataFrame"),
                   experimentData = new("MIAME"),
                   annotation = character(),
                   R = new("matrix"),
                   G = new("matrix"),
                   Rb = new("matrix"),
                   Gb = new("matrix"),
                   ... ) {
            callNextMethod(.Object,
                           assayData = assayDataNew(
                             R=R, G=G, Rb=Rb, Gb=Gb,
                             ...),
                           phenoData = phenoData,
                           experimentData = experimentData,
                           annotation = annotation)
          })
setValidity("SwirlSet", function(object) {
  assayDataValidMembers(assayData(object), c("R", "G", "Rb", "Gb"))
})

eSetSubclassesSuite <- defineTestSuite( "eSetSubclasses",
                                       system.file("UnitTests", package="Biobase"),
                                       "^runitEset.+\.R$")

setClass("A",
         representation(x="numeric"),
         contains=("VersionedBiobase"),
         prototype=prototype(new("VersionedBiobase", versions=c(C="1.0.1"))))

setMethod("initialize", signature(.Object = "A"),
          function(.Object, ...) {
            .Object <- callNextMethod()
            args <- list(...)
            if ("x" %in% names(args)) .Object@x <- args[["x"]]
            .Object
          })



allSuite <- defineTestSuite( "allSuite",
                            system.file("UnitTests", package="Biobase"),
                            "^runit.+\.R$")

testData <- runTestSuite(allSuite)

printTextProtocol(testData, showDetails=TRUE)

