\name{makeDataPackage}
\alias{makeDataPackage}
\alias{makeDataPackage,ANY-method}
\title{Make an R package from a data object}
\description{
  This generic creates a valid R package from an R data object.
}
\usage{
makeDataPackage(object, author, email,
                packageName=deparse(substitute(object)),
                packageVersion=package_version("1.0.0"),
                license="Artistic-2.0",
                biocViews="ExperimentData",
                filePath=tempdir(),
                \dots)
}
\arguments{
  \item{object}{An instance of an R data object.}
  \item{author}{The author, as a character string.}
  \item{email}{A valid email address for the maintainer, as a character string.}
  \item{packageName}{The name of the package, defaults to the name of the object instance.}
  \item{packageVersion}{The version number, as a character string.}
  \item{license}{The license, as a character string.}
  \item{biocViews}{A character vector of valid biocViews views.}
  \item{filePath}{The location to create the package.}
  \item{\dots}{Additional arguments to specific methods.}
}
\details{
  The function makes use of various tools in R and Bioconductor to automatically
  generate the source files for a valid R package.
}
\value{
 The return value is that from a call to \code{link{createPackage}} which
is invoked once the default arguments are set up. The data instance
is stored in the data directory with a name the same as that of the
resulting package.
}
\note{Developers implementing derived methods might force correct
      package name evaluation by including 'packageName' in any
      \code{callNextMethod()}.}
\author{R. Gentleman}
\seealso{\code{\link{createPackage}}}
\examples{
 data(sample.ExpressionSet)
 ## package created in tempdir()
 s1 <- makeDataPackage(sample.ExpressionSet,
                       author = "Foo Author",
                       email = "foo@bar",
                       packageName = "FooBarPkg",
                       packageVersion = "1.0.0")
}
\keyword{manip}
