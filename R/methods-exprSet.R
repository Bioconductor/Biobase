setMethod("initialize", "exprSet",
          function(.Object, ...) {
              .Defunct(msg="The exprSet class is defunct, use ExpressionSet instead")
          })

read.exprSet <- function(exprs, se.exprs, phenoData, annotation,
                         description, notes, seps = "\t" ) {
    .Defunct("readExpressionSet", "Biobase")
}
