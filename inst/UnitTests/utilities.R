## these assume we're in Biobase/inst/UnitTests

createCurrentInstances <- function(instanceDir = "VersionedClass_data") {
    nms <- ls(getNamespace("Biobase"),all=TRUE)
    classes <- gsub(".__C__", "", nms[grep(".__C__", nms)])
    isVirtual <- sapply(classes, function(nm) getClass(nm)@virtual)
    nonvirtualClasses <- classes[!isVirtual]

    instances <- sub(".Rda", "",
                     list.files(path=instanceDir, "devel", pattern=".*.Rda"))

    need <- nonvirtualClasses[!nonvirtualClasses %in% instances]
    if (length(need)!=0) {
        cat("need:", need, "\n")
        lapply(need, function(cls) {
            cat("creating", cls, "\n")
            assign(cls,  new(cls))
            save(list=cls,
                 file=file.path(instanceDir, "devel", paste(cls, ".Rda", sep="")))
        })
    } else cat("no instances need creating\n")
}

createComponentClasses <- function(exprSet, ExpressionSet, vers="devel", instanceDir = "VersionedClass_data") {
    MIAME <- experimentData(ExpressionSet)
    AnnotatedDataFrame <- phenoData(ExpressionSet)
    phenoData <- phenoData(exprSet)
    cat("creating MIAME\n")
    save(MIAME, file=file.path(instanceDir, vers, "MIAME.Rda"))
    cat("creating AnnotatedDataFrame\n")
    save(AnnotatedDataFrame, file=file.path(instanceDir, vers, "AnnotatedDataFrame.Rda"))
    cat("creating phenoData\n")
    save(phenoData, file=file.path(instanceDir, vers, "phenoData.Rda"))
}
