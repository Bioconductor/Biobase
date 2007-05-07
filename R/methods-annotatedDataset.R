# ==========================================================================
setReplaceMethod("$", "annotatedDataset",
   function(x, name, value) {
      x@phenoData[[name]] = value
      x
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("$", "annotatedDataset",
   function(x, name) {
      "$"((x@phenoData)@pData, name)
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("phenoData", "annotatedDataset", function(object) object@phenoData)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setReplaceMethod("phenoData", c("annotatedDataset", "phenoData"),
   function(object, value) {
      object@phenoData <- value
      object
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("reporterInfo", "annotatedDataset",
          function(object) {
              object@reporterInfo
          })
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setReplaceMethod("reporterInfo", c("annotatedDataset", "data.frame"),
   function(object, value) {
      object@reporterInfo <- value
      object
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("pData", "annotatedDataset", function(object) pData(phenoData(object)))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setReplaceMethod("pData", "annotatedDataset",
   function(object, value) {
      ph <- phenoData(object)
      pData(ph) <- value
      phenoData(object) <- ph
      object
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setReplaceMethod("[[", "annotatedDataset",
   function(x, i, j, ..., value) {
      pD <- phenoData(x)
      pD@pData[[i]] <- value
      phenoData(x) <- pD
      x
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("[[", "annotatedDataset", function(x, i, j, ...) phenoData(x)[[i]] )
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("varLabels", "annotatedDataset", function(object) phenoData(object)@varLabels)
