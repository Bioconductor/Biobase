\name{openVignette}
\alias{openVignette}
\title{ Open a selected Vignette }
\description{
  Using the data returned by \code{vignette} this function provides a
  simple easy to use interface for opening vignettes.
}
\usage{
openVignette(x, index)
}
\arguments{
  \item{x}{Either a list or an object of class \code{pkgFileList}. }
  \item{index}{An index into \code{x}, it should only be used if
  \code{x} is an object of class \code{pkgFileList}.}
}
\details{
  The vignette is opened in the users PDF viewer.
}
\value{
 No value is returned this function is run entirely for the side effect
 of opening the pdf document in the PDF viewer.
}
\author{R. Gentleman }

\seealso{\code{\link{vignette}},\code{\link{openPDF}} }

\examples{
  w <- vignette()
  if( interactive() )
    openVignette(w[[1]])
}
\keyword{ utilities }
