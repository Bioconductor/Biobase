setGeneric("rowQ", function(imat, which) standardGeneric("rowQ"))


setGeneric("rowMedians", function(imat) standardGeneric("rowMedians"))


setMethod("rowQ", signature(imat="matrix", which="numeric"),
          function(imat, which) {
              if (any(is.na(imat)))
                stop("cannot handle missing values.")
              if (length(which) != 1 || !is.finite(which))
                stop("which must be length one and finite numeric")
              .Call("rowQ", imat, which, PACKAGE="Biobase")
          })


setMethod("rowQ", signature(imat="ExpressionSet", which="numeric"),
          function(imat, which) rowQ(exprs(imat), which))


setMethod("rowQ", signature(imat="exprSet", which="numeric"),
          function(imat, which) rowQ(exprs(imat), which))


setMethod("rowMedians", signature(imat="matrix"),
          function(imat) {
              nr <- ncol(imat)
              half <- (nr + 1)/2
              if (nr%%2 == 1)
                return(rowQ(imat, half))
              else
                return((rowQ(imat, half) + rowQ(imat, half+1))/2)
          })


setMethod("rowMedians", signature(imat="ExpressionSet"),
          function(imat) rowMedians(exprs(imat)))


setMethod("rowMedians", signature(imat="exprSet"),
          function(imat) rowMedians(exprs(imat)))


rowMin <- function(imat)
   rowQ(imat, 1)


rowMax <- function(imat)
   rowQ(imat, ncol(imat))
