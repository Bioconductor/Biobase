\name{dumpPackTxt}
\alias{dumpPackTxt}

\title{Dump Textual Description of a Package  }

\description{
   Dump textual description of a package
}
\usage{
dumpPackTxt(package)
}
\arguments{
  \item{package}{Character string naming an R package}
}
\details{
dumps DESCRIPTION and INDEX files from package sources
}
\value{
   \code{stdout} output
}
\author{<stvjc@channing.harvard.edu>}

\note{ Other approaches using formatDL are feasible }

\examples{
dumpPackTxt("stats")
}
\keyword{ models }
