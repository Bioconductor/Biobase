\name{lcSuffix}

\alias{lcSuffix}
\alias{lcPrefix}
\alias{lcPrefixC}

\title{Compute the longest common prefix or suffix of a string}

\description{
These functions find the longest common prefix or suffix among the
strings in a character vector.
}

\usage{
lcPrefix(x, ignore.case=FALSE)
lcPrefixC(x, ignore.case=FALSE)
lcSuffix(x, ignore.case=FALSE)
}
\arguments{
  \item{x}{ a character vector. }
  \item{ignore.case}{ A logical value indicating whether or
    not to ignore the case in making comparisons. }
}
\details{
Computing the longest common suffix is helpful for truncating names of
objects, like microarrays, that often have a common suffix, such as .CEL.

There are some potential problems with the approach used if
multibyte character encodings are being used.

\code{lcPrefixC} is a faster implementation in C.  It only handles
ascii characters.

}
\value{
The common prefix or suffix.
}
\author{ R. Gentleman }

\seealso{ \code{\link{nchar}}, \code{\link{nchar}}}
\examples{
s1 <- c("ABC.CEL", "DEF.CEL")
lcSuffix(s1)

s2 <- c("ABC.123", "ABC.456")
lcPrefix(s2)


CHK <- stopifnot

CHK(".CEL" == lcSuffix(s1))
CHK("bc" == lcSuffix(c("abc", "333abc", "bc")))
CHK("c" == lcSuffix(c("c", "abc", "xxxc")))
CHK("" == lcSuffix(c("c", "abc", "xxx")))

CHK("ABC." == lcPrefix(s2))
CHK("ab" == lcPrefix(c("abcd", "abcd123", "ab", "abc", "abc333333")))
CHK("a" == lcPrefix(c("abcd", "abcd123", "ax")))
CHK("a" == lcPrefix(c("a", "abcd123", "ax")))
CHK("" == lcPrefix(c("a", "abc", "xxx")))

CHK("ab" == lcPrefixC(c("abcd", "abcd123", "ab", "abc", "abc333333")))
CHK("a" == lcPrefixC(c("abcd", "abcd123", "ax")))
CHK("a" == lcPrefixC(c("a", "abcd123", "ax")))
CHK("" == lcPrefixC(c("a", "abc", "xxx")))

}
\keyword{ manip }
