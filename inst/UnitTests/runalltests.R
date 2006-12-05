library("Biobase")
library("RUnit")

options(warn=1)

## instantiate here to satisfy RUnit

setClass("SwirlSet", contains="eSet")

setMethod("initialize", "SwirlSet",
          function(.Object,
                   assayData = assayDataNew(
                     R=R, G=G, Rb=Rb, Gb=Gb, ...),
                   phenoData = annotatedDataFrameFrom(assayData, byrow=FALSE),
                   featureData = annotatedDataFrameFrom(assayData, byrow=TRUE),
                   experimentData = new("MIAME"),
                   annotation = character(),
                   R = new("matrix"),
                   G = new("matrix"),
                   Rb = new("matrix"),
                   Gb = new("matrix"),
                   ... ) {
            callNextMethod(.Object,
                           assayData = assayData,
                           phenoData = phenoData,
                           featureData = featureData,
                           experimentData = experimentData,
                           annotation = annotation)
          })

setValidity("SwirlSet", function(object) {
  assayDataValidMembers(assayData(object), c("R", "G", "Rb", "Gb"))
})

setClass("ExtraSlotSet", contains="eSet",
         representation=representation(
           extraSlot="character"))


## RUnit Test Suites

dirs <- '.'
## dirs <- system.file("UnitTests", package="Biobase"),
testFilePat <- ".*_test\\.R$"

allSuite <- defineTestSuite(name="allSuite",
                            dirs=dirs,
                            testFileRegexp=testFilePat,
                            rngKind="default",
                            rngNormalKind="default")

testData <- runTestSuite(allSuite)
printTextProtocol(testData, showDetails=FALSE)

