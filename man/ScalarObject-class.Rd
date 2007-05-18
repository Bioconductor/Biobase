\name{ScalarObject-class}
\docType{class}
\alias{ScalarObject-class}
\alias{ScalarCharacter-class}
\alias{ScalarInteger-class}
\alias{ScalarNumeric-class}
\alias{mkScalar}

\alias{show,ScalarObject-method}
\alias{show,ScalarCharacter-method}

\title{Utility classes for length one (scalar) objects}

\usage{
mkScalar(obj)
}

\arguments{
  \item{obj}{An object of type \code{character}, \code{integer}, or
    \code{double}}
}

\description{
  These classes represent scalar quantities, such as a string or a
  number and are useful because they provide their own validity
  checking.  The classes \code{ScalarCharacter}, \code{ScalarInteger},
  and \code{ScalarNumeric} all extend their respective base vector types
  and can be used interchangeably (except they should always have length
  one).

  The \code{mkScalar} factory function provides a convenient way of
  creating \code{Scalar<type>} objects (see the examples section below).

}

\author{Seth Falcon}
\examples{
v <- list(mkScalar("a single string"),
          mkScalar(1),
          mkScalar(1L))
sapply(v, class)
sapply(v, length)
}
\keyword{classes}
