eset <- function() {
    if ("Biobase" %in% loadedNamespaces())
      .Deprecated("sample.exprSet.1")
}
eset()
rm("eset")
load("eset.rda")
