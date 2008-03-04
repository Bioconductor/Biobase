\name{rowMedians}
\alias{rowMedians}
\alias{rowMedians,matrix-method}
\alias{rowMedians,ExpressionSet-method}

\title{Calculates the median for each row in a matrix}

\description{
  Calculates the median for each row in a matrix.
}

\usage{rowMedians(imat, na.rm=FALSE)}

\arguments{
 \item{imat}{A \code{\link[base]{numeric}} \code{\link[base]{matrix}}.}
 \item{na.rm}{If \code{\link[base:logical]{TRUE}}, \code{\link[base]{NA}}s are excluded before calculating the medians, otherwise not.}
 \item{...}{Not use.}
}

\value{
  Returns a \code{\link[base]{double}} \code{\link[base]{vector}} of length equal to number of rows in \code{x}.
}

\section{Missing values}{
  Missing values are excluded before calculating the medians.
}

\section{Benchmarking}{
  This implementation is optimized for speed and memory to calculate.
  As the example shows, this implementation is roughly 3-10 times faster
  than using \code{apply(x, MARGIN=1, FUN=medians)}.
  As the example might show, the \code{\link{rowQ}()} does not (have to)
  handle missing values, and is therefore in some cases faster.
}

\author{Henrik Bengtsson}

\seealso{
  See \code{rowMeans()} in \code{\link[base]{colSums}}().
}

\examples{
set.seed(1)
x <- rnorm(n=234*543)
x[sample(1:length(x), size=0.1*length(x))] <- NA
dim(x) <- c(234,543)
y1 <- rowMedians(x, na.rm=TRUE)
y2 <- apply(x, MARGIN=1, FUN=median, na.rm=TRUE)
stopifnot(all.equal(y1, y2))

x <- cbind(x1=3, x2=c(4:1, 2:5))
stopifnot(all.equal(rowMeans(x), rowMedians(x)))
}

\keyword{manip}
