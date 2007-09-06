\name{esApply}
\alias{esApply}

\title{
  An apply-like function for ExpressionSet and related structures.
}

\description{
   \code{esApply} is a wrapper to \code{apply} for use with \code{ExpressionSet}s and
   (deprecated) \code{exprSet}s. The application of a function to rows
   of an expression array usually involves variables in
   \code{pData}. \code{esApply} uses a special evaluation paradigm
   to make this easy. The function \code{FUN} may reference any data in
   \code{pData} by name.
}
\usage{
esApply(X, MARGIN, FUN, ...)
}

\arguments{
   \item{X}{An instance of class \code{\link{ExpressionSet}} or
     (deprecated) \code{exprSet}.}
   \item{MARGIN}{The margin to apply to, either 1 for rows (samples) or
     2 for columns (features).}
   \item{FUN}{Any function }
   \item{...}{Additional parameters for \code{FUN}.}
}

\details{
  The \code{pData} from \code{X} is installed in an
  environment. This environment is installed as the environment of
  \code{FUN}. This will then provide bindings for any symbols in
  \code{FUN} that are the same as the names of the \code{pData} of
  \code{X}. If \code{FUN} has an environment already it is retained but
  placed after the newly created environment. Some variable shadowing
  could occur under these circumstances.
}

\value{
  The result of \code{with(pData(x), apply(exprs(X), MARGIN, FUN, ...))}.
}

\author{V.J. Carey <stvjc@channing.harvard.edu>, R. Gentleman }

\seealso{
  \code{\link{apply}}, \code{\link{ExpressionSet}}
}

\examples{
data(sample.ExpressionSet)
## sum columns of exprs
res <- esApply(sample.ExpressionSet, 1, sum)

## t-test, spliting samples by 'sex'
f <- function(x) {
    xx <- split(x, sex)
    t.test(xx[[1]], xx[[2]])$p.value
}
res <- esApply(sample.ExpressionSet, 1, f)
    
## same, but using a variable passed in the function call

f <- function(x, s) {
    xx <- split(x, s)
    mean(xx[[1]])-mean(xx[[2]])
}
sex=sample.ExpressionSet[["sex"]]
res <- esApply(sample.ExpressionSet, 1, f, s=sex)

## Earlier examples, with (deprecated) sample.exprSet.1

data(sample.exprSet.1)

# we know that eset has covariates in the pData called "cov1" and "cov2"
# here cov1 is an unbound value, it will be resolved by using the pData
# here are two functions conforming to the esApply protocol

mytt.demo <- function(y) {
 ys <- split( y, cov1 )
 t.test( ys[[1]], ys[[2]] )$p.value
 }

# obtain the p value of the slope associated with cov2, adjusting for cov1
# (if we were concerned with sign we could save the z statistic instead at coef[3,3]
myreg.demo <- function( y ) {
   summary(lm(y~cov1+cov2))$coef[3,4]
}

newt <- esApply( sample.exprSet.1, 1, mytt.demo )

# a resampling method
resamp <- function( ESET ) {
 ntiss <- ncol(exprs(ESET))
 newind <- sample(1:ntiss, size=ntiss, replace=TRUE)
 ESET[newind,]
 }

# a filter
q3g100filt <- function( eset ) {
 apply( exprs(eset), 1, function(x)quantile(x,.75)>100 )
 }

# filter after resampling and then apply
set.seed(123)
rest <- esApply( { bool <- q3g100filt(resamp(sample.exprSet.1)); sample.exprSet.1[bool,] }, 1,
mytt.demo )

}
\keyword{models}
\keyword{methods}
