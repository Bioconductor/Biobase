\name{container}
\docType{class}
\alias{class:container}
\alias{container}

\alias{container-class}
\alias{[,container-method}
\alias{[[<-,container-method}
\alias{[[,container-method}
\alias{content,container-method}
\alias{length,container-method}
\alias{locked,container-method}
\alias{show,container-method}

\concept{content}
\concept{locked}

\title{A Lockable List Structure with Constraints on Content}

\description{Container class that specializes the list construct of R to provide content and access control }

\section{Creating Objects}{
   \code{new('container', x  = [list], content = [character], locked = [logical])}
}

\section{Slots}{
   \describe{
      \item{x}{list of entities that are guaranteed to share a certain property}
      \item{content}{tag describing container contents}
      \item{locked}{boolean indicator of locked status. Value of TRUE implies assignments into the container are not permitted}
   }
}

\section{Methods}{
   Class-specific methods:
   \describe{
      \item{\code{content(container)}}{returns \code{content} slot of argument}
      \item{\code{locked(container)}}{returns \code{locked} slot of argument}
   }
   Standard methods defined for 'container':
   \describe{
      \item{\code{show(container)}}{prints container}
      \item{\code{length(container)}}{returns number of elements in the container}
      \item{\code{[[(index)} and \code{[[(index, value)}}{access and replace elements in the container }
      \item{\code{[(index)}}{make a subset of a container (which will itself be a container)}
   }
}
\examples{
  x1 <- new("container", x=vector("list", length=3), content="lm")
  lm1 <- lm(rnorm(10)~runif(10))
  x1[[1]] <- lm1
 }
\keyword{methods}
\keyword{classes}
