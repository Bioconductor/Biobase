\name{MIAxE}
\docType{class}
\alias{class:MIAxE}
\alias{MIAxE}

\alias{MIAxE-class}

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

\section{Methods}{
  Currently no methods exist for the \code{MIAxE} objects. 
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
setClass("MyData", contains="MIAxE")
new("MyData")

# MIAxE objects cannot be instantiated directly
try(new("MIAxE"))

}

\keyword{classes}
