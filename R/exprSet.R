# A class for microarray data

# in this representation we think of the data set being comprised of
#  * matrix slot exprs: a collection of array results organized as a matrix, with
# genes defining rows and samples defining columns.
#  * data.frame slot phenodata: in the customary organization of samples
# defining rows and variables or features defining columns.  thus
# if x is an exprSet, nrow(x@phenodata) == ncol(x@exprs)
#  * character slot description: unconstrained string with information about
# the exprSet

require(methods)

.initExprset <- function(where) {
 setClass("exprSet", representation(exprs="matrix",
                                   phenodata="data.frame",
                                    description="character") , where=where)

#define a generic for obtaining the data
 if( !isGeneric("exprs") )
     setGeneric("exprs", function(object) standardGeneric("exprs"),
 where=where )
 setMethod("exprs", "exprSet", function(object) object@exprs, where=where)

 if( !isGeneric("phenodata") )
     setGeneric("phenodata", function(object)
                standardGeneric("phenodata"), where=where)
 setMethod("phenodata", "exprSet", function(object)
         object@phenodata, where=where )

 if( !isGeneric("sampleNames") )
     setGeneric("sampleNames", function(object)
                standardGeneric("sampleNames"), where=where)
 setMethod("sampleNames", "exprSet",
           function(object) row.names(phenodata), where=where)

 if( !isGeneric("geneNames") )
     setGeneric("geneNames", function(object)
                standardGeneric("geneNames"), where=where)
 setMethod("geneNames", "exprSet", function(object)
     row.names(object@exprs), where=where )



 setMethod("[", "exprSet", function(x, i, j, ..., drop=TRUE)
     new("exprSet", exprs=x@exprs[i,j], phenodata = x@phenodata[j,,drop=FALSE],
     description=x@description), where=where)

 setMethod("print", "exprSet", function(x, ...) {
     ngenes <- nrow(x@exprs)
     nsamples <- ncol(x@exprs)
     cat("Expression Set (exprSet) with \n\t", ngenes, " genes\n\t", sep="")
     cat(nsamples, "samples\n")
     cat("\tCovariates\n")
     cat("\t\t", names(x@phenodata), "\n", sep="")
 }, where=where)

# if( !isGeneric("plot") )
#    setGeneric("plot")

# setMethod("plot", "uarray", function(object, ...) {
#     expr <- as.matrix(uexpr(object))
#     #scale
#     expr <- sweep(expr, 1, apply(expr, 1, mean, na.rm = TRUE))
#     f <- function(v) {
#         v <- v[!is.na(v)]
#         sqrt(sum(v^2)/max(1, length(v) - 1))
#     }
#     expr <- sweep(expr, 1, apply(expr, 1, f), "/")
#     breaks <- seq(-3,3,by=.2)
#     colors<- GetColor(breaks)
#     breaks <- c(-100,breaks,100)
#     colors <- c(colors[1], colors)
#     opar<-par(mar=c(1,1,4,10))
#     on.exit(par(mar=opar))
#     image(1:ncol(expr), 1:nrow(expr), z = t(expr), axes = F,
#     col=colors, breaks=breaks, xlab="", ylab="")
#     axis(3, at=1:ncol(expr), labels=samplenames(object),tick=FALSE)
#     axis(4, at=1:nrow(expr), labels=genenames(object), tick=FALSE, las=1)
# })




}

