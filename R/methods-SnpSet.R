setMethod("initialize", "SnpSet",
          function(.Object,
                   phenoData = new("AnnotatedDataFrame"),
                   experimentData = new("MIAME"),
                   annotation = character(),
                   call = new("matrix"),
                   callProbability = new("matrix"),
                   ... ) {
            callNextMethod(.Object,
                           assayData = assayDataNew(
                             call = call,
                             callProbability = callProbability,
                             ...),
                           phenoData = phenoData,
                           experimentData = experimentData,
                           annotation = annotation)
          })

setValidity("SnpSet", function(object) {
  assayDataValidMembers(assayData(object), c("call", "callProbability"))
})

setMethod("exprs", c("SnpSet"), function(object) assayDataElement(object, "call"))

setReplaceMethod("exprs", c("SnpSet", "matrix"), function(object, value) {
  assayDataElementReplace(object, "call", value)
})
