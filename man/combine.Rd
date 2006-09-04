\name{combine}
\alias{combine}
\alias{combine,data.frame,data.frame-method}
\concept{combine phenoData}

\title{Methods for Function combine in Package `Biobase'}
\description{
  This generic function handles methods for combining or merging different
  Bioconductor data structures. It should, given an arbitrary number of
  arguments of the same class (possibly by inheritance), combine them into
  a single instance in a sensible way (some methods may only combine 2
  objects, ignoring \code{...} in the argument list; because Bioconductor
  data structures are complicated, check carefully that \code{combine}
  does as you intend).
}
\usage{
combine(x, y, ...)
}
\arguments{
  \item{x}{One of the values. }
  \item{y}{A second value. }
  \item{\dots}{Any other objects of the same class as \code{x} and \code{y}. }
}
\details{
  There are two basic combine strategies. One is an intersection
  strategy. The returned value should only have rows (or columns) that
  are found in all input data objects. The union strategy says that the
  return value will have all rows (or columns) found in any one of the
  input data objects (in which case some indication of what to use for
  missing values will need to be provided).

  These functions and methods are currently under construction. Please
  let us know if there are features that you require.
}
\section{Methods}{
  \describe{
    \item{\code{combine(data.frame, data.frame)}}{Combines two
      \code{data.frame} objects so that the resulting \code{data.frame}
      contains all rows and columns of the original objects. Rows and
      columns in the returned value are unique, that is, a row or column
      represented in both arguments is represented only once in the
      result. To perform this operation, \code{combine} makes sure that data
      in shared rows and columns is identical in the two
      data.frames. Data diffrences in shared rows and columns cause an
      error. \code{combine} issues a warning when a column is a
      \code{\link{factor}} and the levels of the factor in the two
      \code{data.frame}s are different; the returned value may be recoded.}
  }
  Additional \code{combine} methods are defined for
  \code{\link{AnnotatedDataFrame}},
  \code{\link{AssayData}}, \code{\link{MIAME}}, and \code{\link{eSet}}
  classes and subclasses.
}}
\value{
  A single value of the same class as the most specific common ancestor
  (in class terms) of the input values. This will contain the
  appropriate combination of the data in the input values.
}
\author{Biocore}

\seealso{\code{\link{merge}}}

\examples{
  x <- data.frame(x=1:5,
          y=factor(letters[1:5], levels=letters[1:7]),
          row.names=letters[1:5])
  y <- data.frame(z=3:7,
          y=factor(letters[3:7], levels=letters[1:7]),
          row.names=letters[3:7])
  combine(x,y)

  # y is converted to 'factor' with different levels
  x <- data.frame(x=1:5,y=letters[1:5], row.names=letters[1:5])
  y <- data.frame(z=3:7,y=letters[3:7], row.names=letters[3:7])
  try(combine(x,y))
}

\keyword{manip}
