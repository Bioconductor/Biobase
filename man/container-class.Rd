\name{container-class}
\docType{class}
\alias{container-class}
\alias{[,container-method}
\alias{content}
\alias{locked}

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
 \item{\code{Print}}{prints container}
 \item{\code{[[}(replace)}{container element op }
 \item{\code{[[}(access)}{ container element op}
 \item{\code{[}(access)}{ container element op}
 }
\examples{
  x1 <- new("container", x=vector("list", length=3), content="lm")
  lm1 <- lm(rnorm(10)~runif(10))
  x1[[1]] <- lm1
 }
\keyword{methods}
