as.data.frame.exprSet <- function(x, row.names=NA, optional=NA) {
  nc.eset <- ncol(exprs(x))
  nr.eset <- nrow(exprs(x))
  gn.eset <- geneNames(x)
  
  i.pdata <- rep(seq(1, nc.eset), rep(nr.eset, nc.eset))

  pexp <- c(exprs(x))
  
  
  rv <- do.call("data.frame", c(list(exprs=pexp, genenames=rep(gn.eset, nc.eset)),
                                lapply(pData(x), function(y, i.pdata) y[i.pdata], i.pdata))
                )
  
  return(rv)
}
