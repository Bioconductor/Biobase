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
  A \code{data.frame}. The first column is called \code{exprs} and
  contains the values returned by the method \code{exprs()}.
  The second column is called genenames and contains the values
  returned by the method \code{geneNames()}.
  The other columns will depend on the contents of the
  \code{\link[Biobase]{phenoData}} slot.
}
\author{ laurent@cbs.dtu.dk }
\examples{
data(eset)

sd.genes <- esApply(eset, 1, sd)

dataf <- as.data.frame(eset)

dataf <- cbind(dataf, sd.genes=rep(unname(sd.genes), length=nrow(dataf)))

coplot(sd.genes ~ exprs | cov1+cov2, data=dataf)

}
\keyword{ manip }

