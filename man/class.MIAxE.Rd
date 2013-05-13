\name{MIAxE}
\docType{class}
\alias{MIAxE}
\alias{class:MIAxE}
\alias{MIAxE-class}
\alias{show,MIAxE-method}

\title{MIAxE objects}

\description{
  The \code{MIAxE} virtual class is a general container for storing
  experiment metadata. Information such as experimental design, samples,
  normalization methods and pre-processing information can be stored in 
  these objets. 

  The \code{MIAxE} class is virtual and \code{MIAxE} 
  objects cannot be instantiated directly. The following classes
  derive directly from the \code{MIAxE} class: MIAME. 
}

\section{Slots}{
   Introduced in \code{MIAxE}:
   \describe{
      \item{\code{.__classVersion__}:}{A \code{Versions} object describing
    the \code{MIAxE} version number. Intended for developer use.}
  }
}


\section{Methods}{
   Standard generic methods:
   \describe{
      \item{\code{show(object)}}{Informatively display object contents.}
   }
}


\author{Biocore team}

\seealso{
  Related  classes
  \code{\link{MIAME-class}}, \code{\link{ExpressionSet-class}}.
  Derived classes
  \code{\link{MIAME-class}}.
}

\examples{

# Create a new class
MyData <- setClass("MyData", contains="MIAxE")
MyData()

# MIAxE objects cannot be instantiated directly
try(new("MIAxE"))

}

\keyword{classes}
