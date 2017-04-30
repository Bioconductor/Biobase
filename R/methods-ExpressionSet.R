setMethod("initialize", "ExpressionSet",
    function(.Object, assayData, phenoData, featureData,
             exprs=new("matrix"), ... )
{
    if (missing(assayData)) {
        if (missing(phenoData))
            phenoData <- annotatedDataFrameFrom(exprs, byrow=FALSE)
        if (missing(featureData))
            featureData <- annotatedDataFrameFrom(exprs, byrow=TRUE)
        .Object <- callNextMethod(.Object,
                                  phenoData = phenoData,
                                  featureData = featureData,
                                  exprs = exprs,
                                  ...)
    } else if (missing(exprs)) {
        if (missing(phenoData))
            phenoData <- annotatedDataFrameFrom(assayData, byrow=FALSE)
        if (missing(featureData))
            featureData <- annotatedDataFrameFrom(assayData, byrow=TRUE)
        .Object <- callNextMethod(.Object,
                                  assayData = assayData,
                                  phenoData = phenoData,
                                  featureData = featureData,
                                  ...)
    } else stop("provide at most one of 'assayData' or 'exprs' to initialize ExpressionSet",
                call.=FALSE)
    .harmonizeDimnames(.Object)
})

.harmonizeDimnames <- function(object) {
    err <- function(conflicts)
        stop("assayData element dimnames conflict: ",
             paste(names(conflicts), collapse=", "))
    okNames <- list(featureNames(featureData(object)),
                    sampleNames(phenoData(object)))
    dimNames <- .assayDataDimnames(assayData(object))
    dimConflict <- function(dimNames, okNames, dim) {
        nm <- lapply(dimNames, "[[", dim)
        isConflict <- !sapply(nm, function(x, y) {
            is.null(x) || isTRUE(all.equal(x, y, check.attr=FALSE))
        }, okNames[[dim]])
        isNamed <- sapply(lapply(nm, names), length) > 0
        isNull <- sapply(nm, is.null)
        if (all(!isConflict & !isNamed & !isNull))
            return (FALSE)
        if (any(isConflict & !isNull))
            err(isConflict[!isNull])
        TRUE
    }
    if (dimConflict(dimNames, okNames, 1))
        featureNames(assayData(object)) <- okNames[[1]]
    if (dimConflict(dimNames, okNames, 2))
        sampleNames(assayData(object)) <- okNames[[2]]
    object
}

setAs("exprSet", "ExpressionSet", function(from) {
  from <- asS4(from)
  desc <- from@description
  desc <- 
    if (class(desc)!="MIAME") {
        txt <- "missing or mis-formed MIAME 'description' in original object;
                creating new, empty description"
        warning(paste0(strwrap(txt, indent=2), collapse="\n  "))
        MIAME()
    } else updateObject(desc)
  exprs <- from@exprs
  phenoData <- as(from@phenoData, "AnnotatedDataFrame")
  annotation <- from@annotation
  dims <- dim(exprs)
  if (all(dim(from@se.exprs) == dims)) {
    se.exprs <- from@se.exprs
    colnames(se.exprs) <- colnames(exprs)
    .ExpressionSet(phenoData=phenoData, experimentData=desc,
        annotation=annotation, exprs=exprs, se.exprs=se.exprs)
  } else {
    txt <- "missing or mis-formed 'se.exprs' in original object;
            creating ExpressionSet without se.exprs"
    warning(paste0(strwrap(txt, indent=2), collapse="\n  "))
    .ExpressionSet(phenoData=phenoData, experimentData=desc,
                   annotation=annotation, exprs=exprs)
  }
})

setValidity("ExpressionSet", function(object) {
    msg <- validMsg(NULL, isValidVersion(object, "ExpressionSet"))
    msg <- validMsg(msg, assayDataValidMembers(assayData(object), c("exprs")))
    if(class(experimentData(object)) != "MIAME")
        msg <- validMsg(msg, 
               "experimentData slot in ExpressionSet must be 'MIAME' object")
    if (is.null(msg)) TRUE else msg
})

setAs("ExpressionSet", "data.frame",
      function (from) data.frame(t(exprs(from)), pData(from)))

as.data.frame.ExpressionSet <- function(x, row.names=NULL, optional=FALSE, ...)
  as(x, "data.frame")

setMethod("exprs", signature(object="ExpressionSet"),
          function(object) assayDataElement(object,"exprs"))

setReplaceMethod("exprs", signature(object="ExpressionSet",value="matrix"),
                 function(object, value) assayDataElementReplace(object, "exprs", value))


.esApply <- function(X, MARGIN, FUN, ...) {
    parent <- environment(FUN)
    if (is.null(parent))
        parent <- emptyenv()
    e1 <- new.env(parent=parent)
    multiassign(names(pData(X)), pData(X), envir=e1)
    environment(FUN) <- e1
    apply(exprs(X), MARGIN, FUN, ...)
}

setMethod("esApply",
          signature=signature(X="ExpressionSet"),
          .esApply)

setMethod("makeDataPackage",
          signature(object="ExpressionSet"),
          function(object, author, email,
                   packageName, packageVersion, license, biocViews, filePath,
                   description=paste(abstract(object), collapse="\n\n"), ...) {
              if( missing(email) || !(is.character(email) && (length(email) == 1)
                                      && grep("@", email) == 1 ) )
                stop("invalid email address")

              sym = list(
                AUTHOR = author,
                VERSION=as.character(package_version(packageVersion)),
                LICENSE=license,
                TITLE = paste("Experimental Data Package:",packageName),
                MAINTAINER = paste0(author, ", <", email, ">"),
                BVIEWS = biocViews,
                DESCRIPTION = description,
                FORMAT = pD2Rd(phenoData(object)))

              res = createPackage(packageName, destinationDir=filePath,
                originDir = system.file("ExpressionSet", package="Biobase"),
                symbolValues = sym, unlink=TRUE)

              ##save the data file
              datadir = file.path(res$pkgdir, "data")
              dir.create(datadir, showWarnings=FALSE)
              outfile = file.path(datadir, paste0(packageName, ".rda"))
              assign(packageName, object)
              save(list=packageName, file = outfile)

              return(res)
          })

setMethod("write.exprs",
          signature(x="ExpressionSet"),
          function(x, file="tmp.txt", quote=FALSE,
                   sep="\t", col.names=NA, ...){
            write.table(exprs(x), file=file, quote=quote, sep=sep,
                        col.names=col.names, ...)
          })

readExpressionSet <- function(exprsFile,
                              phenoDataFile,
                              experimentDataFile,
                              notesFile,
                              path,
                              annotation,
                              ## arguments to read.* methods 
                              exprsArgs=list(sep=sep, header=header, row.names=row.names, quote=quote, ...),
                              phenoDataArgs=list(sep=sep, header=header, row.names=row.names, quote=quote, stringsAsFactors=stringsAsFactors, ...),
                              experimentDataArgs=list(sep=sep, header=header, row.names=row.names, quote=quote, stringsAsFactors=stringsAsFactors, ...),
                              sep = "\t", header = TRUE, quote = "", stringsAsFactors = FALSE, row.names = 1L,
                              ## widget
                              widget = getOption("BioC")$Base$use.widgets,
                              ...) {
    if (!missing(widget) && widget != FALSE)
        stop("sorry, widgets not yet available")

    ## exprs
    if (missing(exprsFile))
        stop("exprs can not be missing!")
    exprsArgs$file=exprsFile
    ex = as.matrix(do.call(read.table, exprsArgs))

    ## phenoData
    if (!missing(phenoDataFile)) {
        phenoDataArgs$file=phenoDataFile
        pd = do.call(read.AnnotatedDataFrame, phenoDataArgs)
        if (!identical(sampleNames(pd), colnames(ex)))
            stop("Column names of expression matrix must be identical to\n",
                 "the sample names of the phenodata table.\n",
                 "You could use 'options(error=recover)' to compare the",
                 "values of 'sampleNames(pd)' and 'colnames(ex)'.\n")
    } else {
        pd = annotatedDataFrameFrom(ex, byrow=FALSE)
    }

    obj = ExpressionSet(ex, phenoData=pd)


    ## FIXME: these should probably added to obj before, or simultaneously to, exprs;
    ##   as is this can provoke a lot of copying
    ## experimentData
    if (!missing(experimentDataFile))
        experimentDataArgs$file=experimentDataFile
    if (!is.null(experimentDataArgs$file))
        experimentData(obj) <- do.call(read.MIAME, experimentDataArgs)
    ## annotation
    if (!missing(annotation))
        annotation(obj) <- annotation
    ## notes
    if (!missing(notesFile))
        notes(obj) <- readLines(notesFile)

    validObject(obj)
    obj
}

setMethod(ExpressionSet, "missing",
    function(assayData,
             phenoData=AnnotatedDataFrame(),
             featureData=AnnotatedDataFrame(),
             experimentData=MIAME(), annotation=character(),
             protocolData=AnnotatedDataFrame(),
             ...)
{
    .ExpressionSet(
        assayData=assayDataNew(exprs=new("matrix")),
        phenoData=phenoData,
        featureData=featureData, experimentData=experimentData,
        annotation=annotation, protocolData=protocolData, ...)
})

setMethod(ExpressionSet, "environment",
    function(assayData,
             phenoData=annotatedDataFrameFrom(assayData, byrow=FALSE),
             featureData=annotatedDataFrameFrom(assayData, byrow=TRUE),
             experimentData=MIAME(), annotation=character(),
             protocolData=annotatedDataFrameFrom(assayData, byrow=FALSE),
             ...)
{
    .ExpressionSet(assayData=assayData, phenoData=phenoData,
        featureData=featureData, experimentData=experimentData,
        annotation=annotation, protocolData=protocolData, ...)
})


setMethod(ExpressionSet, "matrix",
    function(assayData,
             phenoData=annotatedDataFrameFrom(assayData, byrow=FALSE),
             featureData=annotatedDataFrameFrom(assayData, byrow=TRUE),
             experimentData=MIAME(), annotation=character(),
             protocolData=annotatedDataFrameFrom(assayData, byrow=FALSE),
             ...)
{
    assayData <- assayDataNew(exprs=assayData)
    .ExpressionSet(assayData=assayData, phenoData=phenoData,
        featureData=featureData, experimentData=experimentData,
        annotation=annotation, protocolData=protocolData, ...)
})

.DollarNames.ExpressionSet <- .DollarNames.eSet
