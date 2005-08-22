\name{eSet2-class}
\docType{class}
\alias{eSet2-class}
\alias{[,eSet2-method}
\alias{dim,eSet2-method}
\alias{exprs,eSet2-method}
\alias{show,eSet2-method}

\title{Class "eSet2" ~~~ }
\description{  ~~ A concise (1-5 lines) description of what the class is.  ~~}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("eSet2", ...)}.
    ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{exprs}:}{Object of class \code{"listOrEnv"} ~~ }
    \item{\code{phenoData}:}{Object of class \code{"phenoData"} ~~ }
  }
}
\section{Extends}{
Class \code{"annotatedDataset"}, directly.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "eSet2")}: ... }
    \item{dim}{\code{signature(x = "eSet2")}: ... }
    \item{exprs}{\code{signature(object = "eSet2")}: ... }
    \item{show}{\code{signature(object = "eSet2")}: ... }
  }
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ }

 ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
  or \code{\link{CLASSNAME-class}} for links to other classes
}
\examples{
data(eset)
pd <- phenoData(eset)
xl <- list(e1=exprs(eset))
es2 <- new("eSet2", phenoData=pd, exprs=xl)
es2[1:4,1:4]
eenv <- new.env()
assign("e1", exprs(eset), eenv)
es3 <- new("eSet2", phenoData=pd, exprs=eenv)
es3
es3[1:5,1:5]
}
\keyword{classes}
