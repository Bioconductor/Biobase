\name{esApply}
\alias{esApply}
\title{ iterate over an exprSet }
\description{
esApply is a wrapper to apply for use with exprSets.
Given a function f of two arguments with a specific structure
(see details), evaluate
f, which may involve use of phenotypic data, on all
gene expression vectors in an exprSet.  The value returned
is that of a call to apply(exprs(es),1,f).
}
\usage{
esApply(es, f)
}
\arguments{
  \item{es}{ an instance of the exprSet class of the Biobase package.
In the sequel it is assumed that \code{es} has information
on gene expression for G genes in N tissue samples. }
  \item{f}{ an R function of two arguments: the first is assumed to
be an N-vector of expression values for a single gene, the
second is the exprSet \code{es} }
}
\details{
See the comments of the examples for further explanation.
}
\value{
The result of apply(exprs(es),1,f,es).
}
\author{ VJ Carey <stvjc@channing.harvard.edu> }

\seealso{ \code{\link{apply}} }

\examples{
require(Biobase)
data(eset)

# we know that eset has covariates in
# the pData called "cov1" and "cov2"
#
# here are three functions conforming to the esApply
# protocol

mytt.demo <- function( y, x ) {
# obtain the p value of t-test across strata
# defined by cov1
 myd <- pData(x)[["cov1"]]
 ys <- split( y, myd )
 t.test( ys[[1]], ys[[2]] )$p.value
 }

mytt.alt <- function(y, x) {
# here we allow the user to refer directly
# to variables in the pData dataframe
# but you need to do a cleanup!
 att <- attach(pData(x))
 attnm <- attr(att,"name")
# cleanup phase
 rpos <- match(attnm,search())
 on.exit(detach(pos=rpos))
# actual program
 ys <- split( y, cov1 )
 t.test( ys[[1]], ys[[2]] )$p.value
 }

myreg.demo <- function( y, x ) {
# obtain the p value of the slope
# associated with cov2, adjusting for cov1
# (if we were concerned with sign we could
# save the z statistic instead at coef[3,3]
#
 myx1 <- pData(x)[["cov1"]]
 myx2 <- pData(x)[["cov2"]]
 summary(lm(y~myx1+myx2))$coef[3,4]
 }

# let's try it out

newt <- esApply( eset, mytt.demo )

# here's a resampling method 

resamp <- function( eset ) {
 ntiss <- ncol(exprs(eset))
 newind <- sample(1:ntiss,size=ntiss,replace=T)
 eset[newind,]
 }

# here's a filter

q3g100filt <- function( eset ) {
 apply( exprs(eset), 1, function(x)quantile(x,.75)>100 )
 }

# want to filter after resampling and then apply

set.seed(123)
rest <- esApply( { bool <- q3g100filt(resamp(eset)); eset[bool,] }, mytt.demo )
set.seed(123)
resta <- esApply( { bool <- q3g100filt(resamp(eset)); eset[bool,] }, mytt.alt )
}
\keyword{ models }
