\name{addVig2Menu}
\alias{addVig2Menu}
\alias{addVig4Win}
\alias{addVig4Unix}

\title{Add menu items to an existing/new menu of a window}
\description{
  These functions add menu items dynamically to an existing or new menu
  of a window for an R session. 
}
\usage{
addVig2Menu(itemName, menuName = "Vignettes", doThis = "vExplorer()")
addVig4Win(menuName, itemName, doThis)
addVig4Unix(menuName, itemName, doThis)
}

\arguments{
  \item{menuName}{\code{menuName} - a character string for the name of
    menu that appears in the menu bar of the window of an R session}
  \item{itemName}{\code{itemName} - a character string for the name of
    the item in the menu defined by \code{menuName}}
  \item{doThis}{\code{doThis} a character string for the action(s) to be
  taken when menu item \code{itemName} is selected}
}
\details{
  The code checks to see if a menu already exists and adds an item to
  the menu if so. Otherwise, a new memu will be created and with the
  item add.
}
\value{
  The functions do not return any value
}

\author{Jianhua Zhang}
\note{The functions are part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R}

\seealso{\code{\link{vExplorer}}}
\examples{
    # Only works for windows now
    if(interactive()){
        addVig2Menu("Biobase")
    }
}
\keyword{interface}

