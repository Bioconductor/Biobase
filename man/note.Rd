\name{note}
\alias{note}
\title{Informational Messages}
\description{
  Generates  an informational message that corresponds to its
  argument(s).  Similar to warning() except prefaced by "Note:"
  instead of "Warning message:".
}
\usage{
note(...)
}
\arguments{
  \item{\dots}{character vectors (which are pasted together) or NULL}
}
\details{
  This function essentially cat()'s the created string to the screen.
  It is intended for messages to the user that are deemed to be
  'informational', as opposed to warnings, etc.
}
\author{Jeff Gentry}
\seealso{\code{\link{warning}},\code{\link{stop}}}
\examples{
   note("This is an example of a note")
}
\keyword{utilities}
