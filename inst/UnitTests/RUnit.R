library(Biobase)
library(RUnit)

eSetSubclassesSuite <- defineTestSuite( "eSetSubclasses",
                                       system.file("UnitTests", package="Biobase"),
                                       "^runitEset.+\.R$")
testData <- runTestSuite( eSetSubclassesSuite )

printTextProtocol(testData, showDetails=TRUE)

