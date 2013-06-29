\name{rowMedians}
\alias{rowMedians}
\alias{rowMedians,matrix-method}
\alias{rowMedians,ExpressionSet-method}

\title{Calculates the median for each row in a matrix}

\description{
  Calculates the median for each row in a matrix.
}

\usage{rowMedians(x, na.rm=FALSE, ...)}

\arguments{
 \item{x}{A \code{\link[base]{numeric}} NxK \code{\link[base]{matrix}}.}
 \item{na.rm}{If \code{\link[base:logical]{TRUE}},
   \code{\link[base]{NA}}s are excluded first, otherwise not.}
 \item{...}{Not use.}
}

\value{
  Returns a \code{\link[base]{numeric}} \code{\link[base]{vector}} of
  length N.
}

\section{Missing values}{
  Missing values are excluded before calculating the medians.
}

\details{
  The implementation of \code{rowMedians()}
  is optimized for both speed and memory.
  To avoid coercing to \code{\link[base]{double}}s (and hence memory
  allocation), there is a special implementation for
  \code{\link[base]{integer}} matrices.  That is, if \code{x} is an
  \code{\link[base]{integer}} \code{\link[base]{matrix}}, then 
  \code{rowMedians(as.double(x))} would require three times the memory
  of \code{rowMedians(x)}, but all this is avoided.
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
