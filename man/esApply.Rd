\name{esApply}
\alias{esApply}
\title{ Apply for the expression data in an exprSet }
\description{
esApply is a wrapper to apply for use with exprSets.
Because the application of a function to the rows of the expression
array usually involves variables in the \code{phenoData} slot we have
used a special evaluation paradigm here.
The function \code{FUN} may reference any data in phenoData by name.
}
\usage{
esApply(X, MARGIN, FUN, ...)
}
\arguments{
  \item{X}{ an instance of the exprSet class of the Biobase package.
It is assumed that \code{X} has information
on gene expression for G genes in N tissue samples. }
  \item{MARGIN}{ The margin to apply to, either 1 for rows or 2 for columns.}
  \item{FUN}{ any function }
  \item{...}{additional parameters for FUN.}
}
\details{
 The \code{phenoData} from \code{X} is installed in an
 environment. This environment is installed as the environment of
 \code{FUN}. This will then provide bindings for any symbols in \code{FUN}
 that are the same as the names of the \code{phenoData} of \code{X}.
 If \code{FUN} has an environment already it is retained but placed after
 the newly created environment. Some variable shadowing could occur
 under these circumstances.
}
\value{
The result of \code{apply(exprs(X),MARGIN, FUN, ...)}.
}
\author{ VJ Carey <stvjc@channing.harvard.edu>, R. Gentleman }

\seealso{ \code{\link{apply}} }

\examples{
require(Biobase)
data(eset)

# we know that eset has covariates in
# the pData called "cov1" and "cov2"
#
# here are two functions conforming to the esApply
# protocol

##here cov1 is an unbound value, it will be resolved
##by using the pData

mytt.demo <- function(y) {
 ys <- split( y, cov1 )
 t.test( ys[[1]], ys[[2]] )$p.value
 }

myreg.demo <- function( y ) {
# obtain the p value of the slope
# associated with cov2, adjusting for cov1
# (if we were concerned with sign we could
# save the z statistic instead at coef[3,3]
#
 summary(lm(y~cov1+cov2))$coef[3,4]
 }

# let's try it out

newt <- esApply( eset, 1, mytt.demo )

# here's a resampling method 

resamp <- function( ESET ) {
 ntiss <- ncol(exprs(ESET))
 newind <- sample(1:ntiss, size=ntiss, replace=TRUE)
 ESET[newind,]
 }

# here's a filter

q3g100filt <- function( eset ) {
 apply( exprs(eset), 1, function(x)quantile(x,.75)>100 )
 }

# want to filter after resampling and then apply

set.seed(123)
rest <- esApply( { bool <- q3g100filt(resamp(eset)); eset[bool,] }, 1,
mytt.demo ) 
 
}
\keyword{ models }
