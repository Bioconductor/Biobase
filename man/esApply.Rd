\name{esApply}
\alias{esApply}

\title{
  An apply-like function for ExpressionSet and related structures.
}

\description{
   \code{esApply} is a wrapper to \code{apply} for use with \code{ExpressionSet}s.
   The application of a function to rows of an expression array usually involves
   variables in \code{pData}. \code{esApply} uses a special evaluation paradigm
   to make this easy. The function \code{FUN} may reference any data in
   \code{pData} by name.
}
\usage{
esApply(X, MARGIN, FUN, \dots)
}

\arguments{
   \item{X}{An instance of class \code{\link{ExpressionSet}}.}
   \item{MARGIN}{The margin to apply to, either 1 for rows (samples) or
     2 for columns (features).}
   \item{FUN}{Any function }
   \item{\dots}{Additional parameters for \code{FUN}.}
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
    mean(xx[[1]]) - mean(xx[[2]])
}
sex <- sample.ExpressionSet[["sex"]]
res <- esApply(sample.ExpressionSet, 1, f, s = sex)

# obtain the p-value of the t-test for sex difference
mytt.demo <- function(y) {
 ys <- split(y, sex)
 t.test(ys[[1]], ys[[2]])$p.value
}
sexPValue <- esApply(sample.ExpressionSet, 1, mytt.demo)

# obtain the p-value of the slope associated with score, adjusting for sex
# (if we were concerned with sign we could save the z statistic instead at coef[3,3]
myreg.demo <- function(y) {
   summary(lm(y ~ sex + score))$coef[3,4]
}
scorePValue <- esApply(sample.ExpressionSet, 1, myreg.demo)

# a resampling method
resamp <- function(ESET) {
 ntiss <- ncol(exprs(ESET))
 newind <- sample(1:ntiss, size = ntiss, replace = TRUE)
 ESET[newind,]
}

# a filter
q3g100filt <- function(eset) {
 apply(exprs(eset), 1, function(x) quantile(x,.75) > 100)
}

# filter after resampling and then apply
set.seed(123)
rest <- esApply({bool <- q3g100filt(resamp(sample.ExpressionSet)); sample.ExpressionSet[bool,]},
                1, mytt.demo)
}
\keyword{models}
\keyword{methods}
