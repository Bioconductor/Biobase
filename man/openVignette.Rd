\name{openVignette}
\alias{openVignette}
\title{ Open a Vignette or Show Vignette Selection Menu}
\description{
  Using the data returned by \code{vignette} this function provides a
  simple easy to use interface for opening vignettes.
}
\usage{
openVignette(package=NULL)
}
\arguments{
  \item{package}{character string indicating the package to be used.}
 }
 \details{
   If \code{package} is NULL then all packages are scanned for
   vignettes.  The list of vignettes is presented to the user via the
   \code{menu} command.  The user may select one of the vignettes to be
   opened in a PDF viewer.
}
\value{
 No value is returned; this function is run entirely for the side effect
 of opening the pdf document in the PDF viewer.
}
\author{R. Gentleman }

\seealso{\code{\link{vignette}}, \code{\link{openPDF}},
  \code{\link{menu}}, \code{\link{getPkgVigs}}}

\examples{
  if( interactive() )
    openVignette("Biobase")
}
\keyword{ utilities }
