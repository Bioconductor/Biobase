\name{lcSuffix}
\alias{lcSuffix}
\title{ Compute the longest common suffix. }
\description{
The function takes an input character vector and finds the longest
common suffix.
}
\usage{
lcSuffix(x, ignore.case=FALSE)
}
\arguments{
  \item{x}{ A character vector. }
  \item{ignore.case}{ A logical value indicating whether or
    not to ignore the case in making comparisons. }
}
\details{
Computing the longest common suffix is helpful for truncating names of
objects, like microarrays, that often have a common suffix, such as .CEL.

There are some potential problems with the approach used if
multibyte character encodings are being used.
}
\value{
The common suffix.
}
\author{ R. Gentleman }
}
\seealso{ \code{\link{nchar}}, \code{\link{nchar}}}
\examples{
  s1 = c("ABC.CEL", "DEF.CEL")
  lcSuffix(s1)


CHK <- stopifnot
CHK(".CEL" == lcSuffix(s1))
CHK("bc" == lcSuffix(c("abc", "abc", "333abc", "bc")))
CHK("c" == lcSuffix(c("c", "abc", "xxxc")))
CHK("" == lcSuffix(c("c", "abc", "xxx")))
}
\keyword{ manip }
