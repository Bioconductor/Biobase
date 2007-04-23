deprecationWarning <- function() {
    if (interactive() && "Biobase" %in% loadedNamespaces())
        .Deprecated(msg="This data set is deprecated. Use 'as(sample.eSet, \"MultiSet\")' to update")
}
deprecationWarning()
rm("deprecationWarning")
load("sample.eSet.rda")
