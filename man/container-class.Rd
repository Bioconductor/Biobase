\name{container-class}
\docType{class}
\alias{container-class}
\alias{[,container-method}
\alias{length,container-method}
\alias{content,container-method}
\alias{content}
\alias{locked}
\alias{locked,container-method}
\alias{[[<-,container-method}
\alias{[[,container-method}
\alias{show,container-method}

 \title{container: a class that specializes the list construct of R
   to provide content and access control }
 \description{ A lockable list structure with constraints on content. }
 \usage{ #new('container', x=[list], content=[character], locked=[logical] )} 
 \section{Slots}{ 
   \item{x}{[class: list] list of entities that are guaranteed to share a
     certain property}
  \item{content}{[class: character] tag describing container contents }
  \item{locked}{[class: logical] boolean indicator of locked status.
    locked==TRUE implies assignments into the container are not permitted }
 }

\section{Methods}{
 \item{\code{content}}{returns \code{content} slot of argument}
 \item{\code{locked}}{returns \code{locked} slot of argument}
 \item{\code{show}}{prints container}
 \item{\code{[[}(replace)}{replace elements in the container }
 \item{\code{[[}(access)}{access elements in a container}
 \item{\code{[}(access)}{make a subset of a container (which will itself
 be a container)}
 }
\examples{
  x1 <- new("container", x=vector("list", length=3), content="lm")
  lm1 <- lm(rnorm(10)~runif(10))
  x1[[1]] <- lm1
 }
\keyword{methods}
