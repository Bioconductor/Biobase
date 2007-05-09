library("Biobase")
library("RUnit")

options(warn=1)

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

