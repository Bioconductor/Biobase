\name{multiassign}
\alias{multiassign}

\title{Assign Values to a Names}

\description{
  Assign values to names in an environment.
}
\usage{
multiassign(x, value, envir = parent.frame(), inherits=FALSE)
}
\arguments{
  \item{x}{A vector or list of names, represented by strings.}
  \item{value}{a vector or list of values to be assigned.}
  \item{envir}{the \code{\link{environment}} to use. See the details section.}
  \item{inherits}{should the enclosing frames of the environment be
    inspected?}
}
\details{
  The \code{pos} argument can specify the  environment in which to assign
  the object in any of several ways:
  as an integer (the position in the \code{\link{search}} list); as
  the character string name of an element in the search list; or as an
  \code{\link{environment}} (including using \code{\link{sys.frame}} to
  access the currently active function calls).
  The \code{envir} argument is an alternative way to specify an
  environment, but is primarily there for back compatibility.

  If \code{value} is missing and \code{x} has names then the values in
  each element of \code{x} are assigned to the names of \code{x}.
}
\value{
  This function is invoked for its side effect, which is assigning
  the \code{values} to the variables in \code{x}.  If no \code{envir} is
  specified, then the assignment takes place in the currently active
  environment.

  If \code{inherits} is \code{TRUE}, enclosing environments of the supplied
  environment are searched until the variable \code{x} is encountered.
  The value is then assigned in the environment in which the variable is
  encountered.  If the symbol is not encountered then assignment takes
  place in the user's workspace (the global environment).

  If \code{inherits} is \code{FALSE}, assignment takes place in the
  initial frame of \code{envir}.
}

\examples{
#-- Create objects  'r1', 'r2', ... 'r6' --
nam <- paste("r",1:6, sep=".")

multiassign(nam, 11:16)
ls(pat="^r..$")

#assign the values in y to variables with the names from y

y<-list(a=4,d=mean,c="aaa")
multiassign(y)


}
\keyword{data}












