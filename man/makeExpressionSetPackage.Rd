\name{makeExpressionSetPackage}
\alias{makeExpressionSetPackage}
\title{ Make an R package from an ExpressionSet }
\description{
 This function creates a valid R package from an Expression set.
}
\usage{
makeExpressionSetPackage(expS, author, filePath = tempdir(), version = "1.0.0", license, email, biocViews = "ExperimentData", packageName)
}
\arguments{
  \item{expS}{ An instance of an ExpressionSet. }
  \item{author}{ The author, as a character string. }
  \item{filePath}{ The location to create the package. }
  \item{version}{ The version number, as a character string. }
  \item{license}{ The license, as a character string. }
  \item{email}{ A valid email address for the maintainer, as a character string. }
  \item{biocViews}{ A character vector of valid biocViews views. }
  \item{packageName}{ The name of the package, defaults to
     the name of the ExpressionSet instance. }
}
\details{
  The function makes use of various tools in R and Bioconductor to automatically
generate the source files for a valid R package.
}
\value{
 The return value is that from a call to \code{link{createPackage}} which
is invoked once the default arguments are set up. The ExpressionSet instance
is stored in the data directory with a name the same as that of the
resulting package.
}
\author{ R. Gentleman }
\seealso{ \code{\link{createPackage}}}
\examples{
 data(sample.ExpressionSet)
 s1 = makeExpressionSetPackage(sample.ExpressionSet, "Foo Bar", 
    version = "1.0.0",  email="foo@bar", packageName = "FooBarPkg")

 makeEx
}
\keyword{ manip }
