\name{exprList-class}
\docType{class}
\alias{exprList-class}
\alias{eList<-,eSet,exprList-method}

\title{Class "exprList" a class union of a list and an environment. }
\description{
The \code{exprList} class defines a \code{classUnion} for \code{list}s
and \code{environment}s. This allows programmers to choose their
implementation. Environments will be more efficient if there are a
number of objects or if there are large static objects that will be
passed to functions. Be very, very careful of using environments to pass
around objects that might get altered.

}
\section{Objects from the Class}{A virtual Class: No objects may be
  created from it.} 
\section{Methods}{
  \describe{
    \item{eList<-}{\code{signature(object = "eSet", value =
	"exprList")}:a replacement method for \code{eSet}s. }
  } 
}
\author{Biocore }

\seealso{
\code{\link{list}},\code{\link{environment}},\code{\link{eSet-class}} }
\examples{
}
\keyword{classes}
