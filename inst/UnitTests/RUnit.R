library("Biobase")
library("RUnit")


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

## RUnit Test Suites

testFilePat <- ".*_test\.R$"

allSuite <- defineTestSuite(name="allSuite",
                            dirs=system.file("UnitTests", package="Biobase"),
                            testFileRegexp=testFilePat,
                            rngKind="default",
                            rngNormalKind="default")

testData <- runTestSuite(allSuite)
printTextProtocol(testData, showDetails=FALSE)

