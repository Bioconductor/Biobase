\name{snpCall}
\alias{snpCall}
\alias{snpCall<-}
\alias{snpCallProbability}
\alias{snpCallProbability<-}

\title{Get and retrieve SNP call and call probability data.}
\description{
  These generic functions access the calls and call probabilities
  stored in objects.
}
\usage{
snpCall(object, ...)
snpCall(object, ...) <- value
snpCallProbability(object, ...)
snpCallProbability(object, ...) <- value
}
\arguments{
  \item{object}{Object, possibly derived from class \code{SnpSet}.}
  \item{value}{Matrix with rows representing SNP calls or call
	probabilities and columns samples.}
  \item{...}{Additional arguments available to methods.}
}
\value{
  \code{snpCall} returns a matrix of SNP calls;
  \code{snpCallProbability} returns the corresponding matrix of standard errors,
  when available.
}
\author{Biocore}

\seealso{\code{\link{SnpSet-class}}}

\keyword{manip}
