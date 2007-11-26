\name{class:exprList}
\docType{class}
\alias{class:exprList}
\concept{exprList}
\concept{eSet methods}
\concept{exprSet methods}

\title{Class Union of a List and an Environment}

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
    \item{\code{eList(eSet, exprList)<-}:}{A replacement method for \code{eSet}s. }
  }
}
\author{Biocore }

\seealso{
\code{\link{list}},\code{\link{environment}},\code{\link{eSet}} }
\keyword{classes}
