setMethod("initialize", "ExpressionSet",
          function(.Object,
                   phenoData = new("AnnotatedDataFrame"),
                   experimentData = new("MIAME"),
                   annotation = character(),
                   exprs = new("matrix"),
                   ... ) {
            callNextMethod(.Object,
                           assayData = assayDataNew(
                             exprs=exprs,
                             ...),
                           phenoData = phenoData,
                           experimentData = experimentData,
                           annotation = annotation)
          })

setAs("exprSet", "ExpressionSet", function(from) {
  desc <- description(from)
  if (class(desc)!="MIAME") {
    warning("missing or mis-formed MIAME 'description' in original object; creating new, empty description")
    desc <- new("MIAME")
  } else if (!isCurrent(desc)) desc <- updateObject(desc)
  exprs <- assayData(from)
  dims <- dim(exprs)
  if (all(dim(from@se.exprs) == dims)) {
    se.exprs <- from@se.exprs
    colnames(se.exprs) <- colnames(exprs)
    new("ExpressionSet",
        phenoData=as(phenoData(from), "AnnotatedDataFrame"),
        experimentData=desc,
        annotation=annotation(from),
        exprs=exprs,
        se.exprs=se.exprs)
  } else {
    warning("missing or mis-shaped 'se.exprs' in original object; creating ExpressionSet without se.exprs")
    new("ExpressionSet",
        phenoData=as(phenoData(from), "AnnotatedDataFrame"),
        experimentData=desc,
        annotation=annotation(from),
        exprs=exprs)
  }
})

setValidity("ExpressionSet", function(object) {
  assayDataValidMembers(assayData(object), c("exprs"))
})

setMethod("exprs", signature(object="ExpressionSet"),
          function(object) assayDataElement(object,"exprs"))

setReplaceMethod("exprs", signature(object="ExpressionSet",value="matrix"),
                 function(object, value) assayDataElementReplace(object, "exprs", value))
