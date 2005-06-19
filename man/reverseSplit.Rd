\name{reverseSplit}
\alias{reverseSplit}
\title{ A function to reverse the role of names and values in a list. }
\description{
Given a list with names \code{x} and values in a set \code{y} this function
returns a list with names in \code{y} and values in \code{x}.
}
\usage{
reverseSplit(inList)
}
\arguments{
  \item{inList}{ A named list with values that are vectors. }
}
\details{
 First the list is unrolled to provide a two long vectors, names are repeated,
 once for each of their values. Then the names are \code{split} by the values.
 
 This turns out to be useful for inverting mappings between one set of 
identifiers and an other.
}
\value{
 A list with length equal to the number of distinct values in the input list
and values from the names of the input list.
}
\author{ R. Gentleman }

\seealso{ \code{\link{split}}}
\examples{
 l1 = list(a=1:4, b=c(2,3), d=c(4,5))
 reverseSplit(l1)

}
\keyword{ manip } 
