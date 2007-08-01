\name{addVigs2WinMenu}
\alias{addVigs2WinMenu}

\concept{addVig2Menu}
\concept{addVig4Win}
\concept{addVig4Unix}
\concept{addNonExisting}
\concept{addPDF2Vig}

\title{Add Menu Items to an Existing/New Menu of Window}
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
   The original functions \code{addVig2Menu}, \code{addVig4Win}, \code{addVig4Unix}, \code{addNonExisting}, \code{addPDF2Vig} have been replaced by \code{addVigs2WinMenu}, please use those instead.
}
\value{
  The functions do not return any value.
}

\author{Jianhua Zhang and Jeff Gentry}

\examples{
    # Only works for windows now
    if(interactive() && .Platform$OS.type == "windows" &&
             .Platform$GUI == "Rgui"){
        addVigs2WinMenu("Biobase")
    }
}
\keyword{interface}

