testNew <- function() {
    ## create instances of non-virtual objects with simple call to "new"
    nms <- ls(getNamespace("Biobase"),all=TRUE)
    classes <- gsub(".__C__", "", nms[grep(".__C__", nms)])
    isVirtual <- sapply(classes, function(nm) getClass(nm)@virtual)
    isDefunct <- classes %in% c("exprSet", "phenoData")
    res <- lapply(classes[!isVirtual & !isDefunct],
                  function(x) suppressWarnings(new(x)))
}
