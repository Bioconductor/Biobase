\name{MIAME-class}
\docType{class}
\alias{MIAME-class}
\alias{MIAME}
\alias{abstract}
\alias{expinfo}
\alias{hybridizations}
\alias{normControls}
\alias{preproc}
\alias{samples}
\alias{otherInfo}


\title{Class MIAME, a class for storing microarray experiment information}
\description{We try to cover the MIAME entries that are not covered by
  other classes in Bioconductor. Namely, experimental design, samples,
  hybridizations, normalization controls, and pre-processing information.}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"} containing
      the experimenter name}
    \item{\code{lab}:}{Object of class \code{"character"} containing the
      laboratory where the experiment was conducted}
    \item{\code{contact}:}{Object of class \code{"character"} containing
      contacct information for lab and/or experimenter}
  \item{\code{title}:}{Object of class \code{"character"} containing a single-sentence experiment title }
    \item{\code{abstract}:}{Object of class \code{"character"}
      containing an abstract describing the experiment}
    \item{\code{url}:}{Object of class \code{"character"} containing
      a URL for the experiment}
    \item{\code{samples}:}{Object of class \code{"list"} containg
      information about the samples}
    \item{\code{hybridizations}:}{Object of class \code{"list"}
      containing information about the hybridizations}
    \item{\code{normControls}:}{Object of class \code{"list"}
      containing information about the controls such as house keeping genes}
    \item{\code{preprocessing}:}{Object of class \code{"list"} containing
      information about the pre-processing steps used on the raw data
      from this experiment}
    \item{\code{other}:}{Object of class \code{"list"} containing other 
      information for which none of the above slots does not applies}
  }
}

\section{Methods}{
  \describe{
    \item{abstract}{\code{signature(object = "MIAME")}: An accesor
      funciton for \code{abstract}}
    \item{expinfo}{\code{signature(object = "MIAME")}: An accesor
      funciton for \code{name}, \code{lab}, \code{contact}, \code{title},
      and \code{url},}
    \item{hybridizations}{\code{signature(object = "MIAME")}: An accesor
      funciton for \code{hybridizations} }
    \item{normControls}{\code{signature(object = "MIAME")}: An accesor
      funciton for \code{normControls} }
    \item{otherInfo}{\code{signature(object = "MIAME")}: An accesor
      funciton for \code{other} }
    \item{samples}{\code{signature(object = "MIAME")}: An accesor
      funciton for \code{samples} }
    \item{show}{\code{signature(object = "MIAME")}: renders information
      about the MIAME information }
  }
}
\references{\url{http://www.mged.org/Workgroups/MIAME/miame_1.1.html}}
\author{Rafael A. Irizarry}
\keyword{classes}

