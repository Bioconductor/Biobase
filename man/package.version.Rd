\name{package.version}
\alias{package.version}
\title{Report Version of a Package}
\description{
  Will report the version number of a requested installed package
}
\usage{
package.version(pkg, lib.loc = NULL)
}
\arguments{
  \item{pkg}{The name of the package}
  \item{lib.loc}{a character vector describing the location of R library trees
          to search through, or `NULL'.  The default value of `NULL'
          corresponds to all libraries currently known.}
}
\details{
  This function is a convenience wrapper around
  \code{package.description}, and will report simply the version number
  of the requested package.  If the package does not exist or if the
  DESCRIPTION file can not be read, then an error will be thrown.
}
\value{
  A character string reporting the version number.
}
\author{Jeff Gentry}
\seealso{\code{\link{package.description}}}
\examples{
  package.version("Biobase")
}
\keyword{utilities}
