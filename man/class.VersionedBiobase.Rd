\name{VersionedBiobase}
\docType{class}
\alias{VersionedBiobase-class}
\alias{VersionedBiobase}

\title{Class "VersionedBiobase"}

\description{

  Use this class as a `superclass' for classes requiring information
  about versions. By default, the class contains versions for R and
  Biobase. See \code{\link{Versioned-class}} for additional details.

}

\section{Methods}{

  set \code{\link{Versioned-class}} for methods.

}

\author{Biocore}

\seealso{
  \code{\link{Versioned-class}}
}

\examples{

obj <- new("VersionedBiobase")
classVersion(obj)

obj <- new("VersionedBiobase", versions=list(A="1.0.0"))
classVersion(obj)

A <- setClass("A", contains="VersionedBiobase")

classVersion("A")
a <- A()
classVersion(a)

obj <- new("VersionedBiobase", versions=c(MyVersion="1.0.0"))
classVersion(obj)

B <- setClass("B", contains="VersionedBiobase",
         prototype=prototype(new("VersionedBiobase",versions=list(B="1.0.0"))))

classVersion("B")
b <- B()
classVersion(b)

removeClass("A")
removeClass("B")

}

\keyword{classes}
