\name{addVigs2WinMenu}
\alias{addVigs2WinMenu}
\alias{addVig2Menu}
\alias{addVig4Win}
\alias{addVig4Unix}
\alias{addNonExisting}
\alias{addPDF2Vig}
\title{Add menu items to an existing/new menu of a window}
\description{
  This function adds a menu item for a package's vignettes.
}
\usage{
addVigs2WinMenu(pkgName)
}

\arguments{
  \item{pkgName}{\code{pkgName} - a character string for the name of an
    R package}
}
\details{
  The original functionality, \code{addVig2Menu} and related functions
  have been replaced by \code{addVigs2WinMenu}, please use those
  instead.
}
\value{
  The functions do not return any value.
}

\author{Jianhua Zhang and Jeff Gentry}
\note{The functions are part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R}

\examples{
    # Only works for windows now
    if(interactive() && .Platform$OS.type == "windows" &&
             .Platform.GUI == "Rgui"){
        addVigs2WinMenu("Biobase")
    }
}
\keyword{interface}

