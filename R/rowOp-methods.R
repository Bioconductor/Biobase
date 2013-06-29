setGeneric("rowMedians", function(x, na.rm=FALSE, ...) {
  # Backward compatibility for rowMedians(imat=...)
  if (missing(x)) {
    args <- list(...);
    if (is.element("imat", names(args))) {
      x <- args$imat;
      .Deprecated(msg="The name of the matrix argument for Biobase::rowMedians() has been changed from 'imat' to 'x'.");
    }
  }
  standardGeneric("rowMedians")
})


setMethod("rowMedians", signature(x="matrix"), function(x, na.rm=FALSE, ...) {
  na.rm <- as.logical(na.rm);
  hasNAs <- TRUE;  # Add as an argument? /2007-08-24
  .Call("rowMedians", x, na.rm, hasNAs, TRUE, PACKAGE="Biobase");
})


setMethod("rowMedians", signature(x="ExpressionSet"), function(x, na.rm=FALSE, ...) {
  rowMedians(exprs(x), na.rm=na.rm, ...);
})


setGeneric("rowQ", function(imat, which) standardGeneric("rowQ"))


setMethod("rowQ", signature(imat="matrix", which="numeric"),
          function(imat, which) {
              if (any(is.na(imat)))
                stop("cannot handle missing values.")
              if (length(which) != 1 || !is.finite(which))
                stop("'which' must be length one and finite numeric")
              if(is.integer(imat))
                  imat <- structure(as.numeric(imat), dim=dim(imat))
              .Call("rowQ", imat, which, PACKAGE="Biobase")
          })


setMethod("rowQ", signature(imat="ExpressionSet", which="numeric"),
          function(imat, which) rowQ(exprs(imat), which))


rowMin <- function(imat)
   rowQ(imat, 1)


rowMax <- function(imat)
   rowQ(imat, ncol(imat))
