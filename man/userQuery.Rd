\name{userQuery}
\alias{userQuery}
\title{A function to query the user for input}
\description{
  This function will output a given message and seek a response from the
  user, repeating the message until the input is from a valid set
  provided by the code.
}
\usage{
userQuery(msg, allowed = c("y", "n"), default = "n", case.sensitive = FALSE)
}
\arguments{
  \item{msg}{The output message}
  \item{allowed}{Allowed input from the user}
  \item{default}{Default response if called in batch mode}
  \item{case.sensitive}{Is the response case sensitive? Defaults to
    \code{FALSE}}
}
\value{
  The input from the user
}
\author{Jeff Gentry}
\keyword{utilities}
