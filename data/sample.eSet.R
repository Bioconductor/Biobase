deprecationWarning <- function() {
    if (interactive() && "Biobase" %in% loadNamespace())
        .Deprecated(msg="This data set is deprecated. Use 'as(sample.eSet, \"MultiSet\")' to update")
}
deprecationWarning()
rm("deprecationWarning")
load("sample.eSet.rda")
