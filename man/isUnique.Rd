\name{isUnique}
\alias{isUnique}
\title{Determine Unique Elements}
\description{
  Determines which elements of a vector occur exactly once.
}

\usage{
isUnique(x)
}

\arguments{
\item{x}{a vector}
}

\value{A logical vector of the same length as \code{x}, in which
\code{TRUE} indicates uniqueness.}
 
\seealso{\code{\link[base:unique]{unique}},\code{\link[base:duplicated]{duplicated}}.}
\author{Wolfgang Huber}

\examples{
x <- c(9:20, 1:5, 3:7, 0:8)
isUnique(x)
}
\keyword{logic}
\keyword{manip}
