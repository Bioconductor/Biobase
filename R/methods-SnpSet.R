setMethod("initialize", "SnpSet",
          function(.Object,
                   assayData = assayDataNew(call = call,
                                            callProbability = callProbability, ...),
                   phenoData = annotatedDataFrameFrom(assayData, byrow=FALSE),
                   featureData = annotatedDataFrameFrom(assayData, byrow=TRUE),
                   experimentData = new("MIAME"),
                   annotation = character(),
                   scanDates = character(),
                   call = new("matrix"),
                   callProbability = matrix(numeric(),
                                            nrow=nrow(call), ncol=ncol(call),
                                            dimnames=dimnames(call)),
                   ...) {
            callNextMethod(.Object,
                           assayData = assayData,
                           phenoData = phenoData,
                           featureData = featureData,
                           experimentData = experimentData,
                           annotation = annotation,
                           scanDates = scanDates)
          })

setValidity("SnpSet", function(object) {
  assayDataValidMembers(assayData(object), c("call", "callProbability"))
})

setMethod("exprs", c("SnpSet"), function(object) assayDataElement(object, "call"))

setReplaceMethod("exprs", c("SnpSet", "matrix"), function(object, value) {
  assayDataElementReplace(object, "call", value)
})
