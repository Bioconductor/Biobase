\name{openVignette}
\alias{openVignette}
\title{ Open a selected Vignette }
\description{
  Using the data returned by \code{vignette} this function provides a
  simple easy to use interface for opening vignettes.
}
\usage{
openVignette(package)
}
\arguments{
  \item{package}{A character string indicating the package to be used.}
 }
 \details{
   If no \code{package} is specified then all packages are scanned for
   vignettes. The list of vignettes is presented to the user via the
   \code{menu} command. The user may select one of the vignettes to be
   opened in a PDF viewer.
}
\value{
 No value is returned this function is run entirely for the side effect
 of opening the pdf document in the PDF viewer.
}
\author{R. Gentleman }

\seealso{\code{\link{vignette}},\code{\link{openPDF}}, \code{\link{menu}} }

\examples{
  if( interactive() )
    openVignette("Biobase")
}
\keyword{ utilities }
