\name{as.data.frame.exprSet}
\alias{as.data.frame.exprSet}
\title{ converts an exprSet to a dataframe }
\description{
  
}
\usage{
as.data.frame.exprSet(x, row.names = NA, optional = NA)
}
\arguments{
  \item{x}{ \code{\link[Biobase]{exprSet}}}
  \item{row.names}{ see \code{as.data.frame}}
  \item{optional}{ see \code{as.data.frame}}
}
\details{

}
\value{
  A \code{data.frame}. The first column will be \code{exprs}.
  The other column will depend on the contents of the
  \code{\link[Biobase]{phenoData}} slot.
}
\author{ laurent@cbs.dtu.dk }
\examples{
}
\keyword{ manip }

