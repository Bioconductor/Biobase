setMethod("initialize", "phenoData",
          function(.Object, ...) {
              .Defunct(msg="The phenoData class is defunct, use AnnotatedDataFrame (with ExpressionSet) instead")
          })

df2pD <- function(x, varLabels, varMetadata) {
   .Defunct()
}

read.pD <- function(filename = NULL, ...) {
   .Defunct()
}

read.phenoData <- function(filename = NULL, sampleNames = NULL,
                           widget = getOption("BioC")$Base$use.widgets,...) {

    .Defunct(msg="read.phenoData is defunct, use read.AnnotatedDataFrame instead")
}
