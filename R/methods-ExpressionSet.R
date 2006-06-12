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
  desc <- 
    if (class(desc)!="MIAME") {
        warning("missing or mis-formed MIAME 'description' in original object; creating new, empty description")
        new("MIAME")
    } else updateObject(desc)
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
    msg <- validMsg(NULL, isValidVersion(object, "ExpressionSet"))
    msg <- validMsg(msg, assayDataValidMembers(assayData(object), c("exprs")))
    if (is.null(msg)) TRUE else msg
})

setMethod("updateObject", signature(object="ExpressionSet"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'ExpressionSet')")
              if (isVersioned(object) && all(isCurrent(object)[c("eSet","ExpressionSet")])) {
                  object
              } else {
                  updt <- new("ExpressionSet")
                  phenoData(updt) <- updateObject(phenoData(object),verbose=verbose)
                  experimentData(updt) <- updateObject(experimentData(object),verbose=verbose)
                  annotation(updt) <- updateObject(annotation(object),verbose=verbose)
                  assayData(updt) <- updateObject(assayData(object),verbose=verbose)
                  updt
              }
          })

setAs("ExpressionSet", "data.frame",
      function (from) data.frame(t(exprs(from)), pData(from)))

as.data.frame.ExpressionSet <- function(x, row.names=NULL, optional=FALSE, ...)
  as(x, "data.frame")

setMethod("exprs", signature(object="ExpressionSet"),
          function(object) assayDataElement(object,"exprs"))

setReplaceMethod("exprs", signature(object="ExpressionSet",value="matrix"),
                 function(object, value) assayDataElementReplace(object, "exprs", value))


setMethod("geneNames", signature(object="ExpressionSet"),
          function(object) {
              .Deprecated("featureNames")
              featureNames(object)
          })


setReplaceMethod("geneNames", signature(object="ExpressionSet",
                                        value="character"),
          function(object, value) {
              .Deprecated("featureNames")
              ## FIXME: check length and uniqueness?
              ##        call validObject?
              featureNames(object) <- value
          })
