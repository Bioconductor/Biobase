setMethod("initialize", "SnpSet",
          function(.Object,
                   assayData = assayDataNew(call = call,
                                            callProbability = callProbability, ...),
                   phenoData = annotatedDataFrameFrom(assayData, byrow=FALSE),
                   featureData = annotatedDataFrameFrom(assayData, byrow=TRUE),
                   experimentData = MIAME(),
                   annotation = character(),
                   protocolData = phenoData[,integer(0)],
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
                           protocolData = protocolData)
          })

setValidity("SnpSet", function(object) {
  assayDataValidMembers(assayData(object), c("call", "callProbability"))
})

setMethod("exprs", c("SnpSet"), function(object) assayDataElement(object, "call"))

setReplaceMethod("exprs", c("SnpSet", "matrix"), function(object, value) {
  assayDataElementReplace(object, "call", value)
})

setMethod(snpCall, "SnpSet", function(object, ...) {
    assayDataElement(object, "call")
})

setMethod(snpCallProbability, "SnpSet", function(object, ...) {
    assayDataElement(object, "callProbability")
})

setReplaceMethod("snpCall", c("SnpSet", "matrix"),
                 function(object, ..., value)
{
    assayDataElementReplace(object, "call", value)
})

setReplaceMethod("snpCallProbability", c("SnpSet", "matrix"),
                 function(object, ..., value)
{
    assayDataElementReplace(object, "callProbability", value)
})
