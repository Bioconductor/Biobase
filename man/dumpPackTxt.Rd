\name{dumpPackTxt}
\alias{dumpPackTxt}
\title{dump textual description of a package  }
\description{dump textual description of a package
}
\usage{
dumpPackTxt(package)
}
\arguments{
  \item{package}{ character string naming an R package}
}
\details{
dumps DESCRIPTION and INDEX files from package sources
}
\value{
writes to stdout
}
\references{  }
\author{<stvjc@channing.harvard.edu>}
\note{ Other approaches using formatDL are feasible }


\seealso{  }

\examples{
dumpPackTxt("ctest")
}
\keyword{ models }
