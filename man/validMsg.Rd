\name{validMsg}
\alias{validMsg}
\title{Conditionally append result to validity message}
\description{
This function facilitates constructing messages during S4 class
validation, and is meant for developer rather than end-user use.
}
\usage{
validMsg(msg, result)
}
\arguments{
  \item{msg}{A character vector or NULL.}
  \item{result}{Any vector.}
}
\details{
This function appends \code{result} to \code{msg}, but only if
\code{result} is a character vector.
}
\author{Martin Morgan <mtmorgan@fhcrc.org>}

\examples{
msg <- NULL
validMsg(msg, FALSE) # still NULL
msg <- validMsg(msg, "one")
validMsg(msg, "two")
}
\keyword{utilities}
