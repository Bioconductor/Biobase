setMethod("initialize", "ExpressionSet",
          function(.Object,
                   assayData,
                   phenoData = annotatedDataFrameFrom(assayData, byrow=FALSE),
                   featureData = annotatedDataFrameFrom(assayData, byrow=TRUE),
                   experimentData = new("MIAME"),
                   annotation = character(),
                   exprs = new("matrix"),
                   ... ) {
              if (missing(assayData)) {
                  if (missing(phenoData))
                    phenoData <- annotatedDataFrameFrom(exprs, byrow=FALSE)
                  if (missing(featureData))
                    featureData <- annotatedDataFrameFrom(exprs, byrow=TRUE)
                  callNextMethod(.Object,
                                 phenoData=phenoData,
                                 featureData=featureData,
                                 experimentData=experimentData,
                                 annotation=annotation,
                                 exprs=exprs,
                                 ...)
              } else if (missing(exprs)) {
                  callNextMethod(.Object,
                                 assayData = assayData,
                                 phenoData = phenoData,
                                 featureData = featureData,
                                 experimentData = experimentData,
                                 annotation = annotation,
                                 ...)
              } else stop("provide at most one of 'assayData' or 'exprs' to initialize ExpressionSet",
                          call.=FALSE)
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

setMethod("makeDataPackage",
          signature(object="ExpressionSet"),
          function(object, author, email,
                   packageName, packageVersion, license, biocViews, filePath, ...) {
              if( missing(email) || !(is.character(email) && (length(email) == 1)
                                      && grep("@", email) == 1 ) )
                stop("invalid email address")

              sym = list(
                AUTHOR = author,
                VERSION=as.character(package_version(packageVersion)),
                LICENSE=license,
                TITLE = paste("Experimental Data Package:",packageName),
                MAINTAINER = paste(author, ", <", email, ">", sep = ""),
                BVIEWS = biocViews,
                DESCRIPTION = "place holder 1",
                FORMAT = pD2Rd(phenoData(object)))

              res = createPackage(packageName, destinationDir=filePath,
                originDir = system.file("ExpressionSet", package="Biobase"),
                symbolValues = sym, unlink=TRUE)

              ##save the data file
              datadir = file.path(res$pkgdir, "data")
              dir.create(datadir, showWarnings=FALSE)
              outfile = file.path(datadir, paste(packageName, ".rda", sep=""))
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
