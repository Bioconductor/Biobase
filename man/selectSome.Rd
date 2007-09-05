\name{selectSome}
\alias{selectSome}
\title{extract aspects of a vector for concise rendering  }
\description{
extract aspects of a vector for concise rendering  
}
\usage{
selectSome(obj, maxToShow=5)
}
\arguments{
  \item{obj}{ vector }
  \item{maxToShow}{ number of elements to render}
}
\details{This function can be used in 'show' methods
to give users exemplars of the tokens used in a vector.
For example, an \code{\link{ExpressionSet}} built from
a yeast experiment might have features enumerated using
systematic gene names (e.g., YPR181C) or standard gene names (e.g., SEC23).
The \code{\link{show}} method for \code{\link{ExpressionSet}} uses
\code{selectSome} to alert the user to the tokens used, and thereby
to indicate what vocabulary must be understood to work with the feature names.
}
\value{
A string vector with at most \code{maxToShow} plus 1 elements,
where an ellipsis ("...") is included to indicate incompleteness
of the excerpt.
}
%\references{  }
\author{Martin Morgan <mtmorgan@fhcrc.org>}
%\note{  }


%\seealso{  }

\examples{
selectSome(1:20)
}
\keyword{ utilities }
