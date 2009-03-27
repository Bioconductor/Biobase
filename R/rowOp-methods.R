setGeneric("rowMedians", function(imat, na.rm=FALSE) {
  standardGeneric("rowMedians")
})


setMethod("rowMedians", signature(imat="matrix"), function(imat, na.rm=FALSE) {
  na.rm <- as.logical(na.rm);
  hasNAs <- TRUE;  # Add as an argument? /2007-08-24
  .Call("rowMedians", imat, na.rm, hasNAs, PACKAGE="Biobase");
})


setMethod("rowMedians", signature(imat="ExpressionSet"), function(imat, na.rm=FALSE) {
  rowMedians(exprs(imat), na.rm=na.rm);
})


setGeneric("rowQ", function(imat, which) standardGeneric("rowQ"))


setMethod("rowQ", signature(imat="matrix", which="numeric"),
          function(imat, which) {
              if (any(is.na(imat)))
                stop("cannot handle missing values.")
              if (length(which) != 1 || !is.finite(which))
                stop("'which' must be length one and finite numeric")
              .Call("rowQ", imat, which, PACKAGE="Biobase")
          })


setMethod("rowQ", signature(imat="ExpressionSet", which="numeric"),
          function(imat, which) rowQ(exprs(imat), which))


rowMin <- function(imat)
   rowQ(imat, 1)


rowMax <- function(imat)
   rowQ(imat, ncol(imat))
