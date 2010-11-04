\name{MIAME}
\docType{class}
\alias{class:MIAME}
\alias{MIAME}

\alias{MIAME-class}
\alias{abstract,MIAME-method}
\alias{combine,MIAME,MIAME-method}
\alias{expinfo,MIAME-method}
\alias{hybridizations,MIAME-method}
\alias{normControls,MIAME-method}
\alias{otherInfo,MIAME-method}
\alias{preproc}
\alias{preproc,MIAME-method}
\alias{preproc<-,MIAME-method}
\alias{preproc<-}
\alias{notes,MIAME-method}
\alias{notes<-,MIAME,list-method}
\alias{notes<-,MIAME,character-method}
\alias{pubMedIds,MIAME-method}
\alias{pubMedIds<-,MIAME,ANY-method}
\alias{samples,MIAME-method}
\alias{show,MIAME-method}
\alias{updateObject,MIAME-method}
\alias{isCurrent,MIAME,missing-method}
\alias{samples}

\concept{abstract}
\concept{expinfo}
\concept{hybridizations}
\concept{normControls}
\concept{preproc}

\title{
   Class for Storing Microarray Experiment Information
}

\description{
   Class \code{MIAME} covers MIAME entries that are not covered by other classes
in Bioconductor. Namely, experimental design, samples, hybridizations,
normalization controls, and pre-processing information. The \code{MIAME} class 
is derived from \code{\link{MIAxE}}.}

\section{Slots}{
   \describe{
      \item{\code{name}:}{Object of class \code{character} containing the experimenter name}
      \item{\code{lab}:}{Object of class \code{character} containing the laboratory where the experiment was conducted}
      \item{\code{contact}:}{Object of class \code{character} containing contact information for lab and/or experimenter}
      \item{\code{title}:}{Object of class \code{character} containing a single-sentence experiment title }
      \item{\code{abstract}:}{Object of class \code{character} containing an abstract describing the experiment}
      \item{\code{url}:}{Object of class \code{character} containing a URL for the experiment}
      \item{\code{samples}:}{Object of class \code{list} containing information about the samples}
      \item{\code{hybridizations}:}{Object of class \code{list} containing information about the hybridizations}
      \item{\code{normControls}:}{Object of class \code{list} containing information about the controls such as house keeping genes}
      \item{\code{preprocessing}:}{Object of class \code{list} containing information about the pre-processing steps used on the raw data from this experiment}
      \item{\code{pubMedIds}:}{Object of class \code{character} listing strings of PubMed identifiers of papers relevant to the dataset}
      \item{\code{other}:}{Object of class \code{list} containing other information for which none of the above slots does not applies}
  }
}

\section{Methods}{
   Constructor methods:
   \describe{
      \item{\code{MIAME()}:}{
      \code{MIAME(name = "", lab = "", contact = "",
                  title = "", abstract = "", url = "",
                  pubMedIds = "" , samples = "",
                  hybridizations = list(), normControls = list(),
                  preprocessing = list(), other = list())}:
      Creates a new \code{MIAME} object with slots as defined above.}
    }

   Class-specific methods:
   \describe{
      \item{\code{abstract(MIAME)}:}{An accessor function for \code{abstract}.}
      \item{\code{combine(MIAME,MIAME)}:}{Combine two objects of \code{MIAME-class}, issuing warnings when ambiguities encountered.}
      \item{\code{expinfo(MIAME)}:}{An accessor function for \code{name}, \code{lab}, \code{contact}, \code{title}, and \code{url}.}
      \item{\code{hybridizations(MIAME)}:}{An accessor function for \code{hybridizations}.}
      \item{\code{normControls(MIAME)}:}{An accessor function for \code{normControls}.}
      \item{\code{notes(MIAME), notes(MIAME) <- value}:}{Accessor functions for \code{other}. \code{notes(MIAME) <- character}
           \emph{appends} character to notes; use \code{notes(MIAME) <- list} to replace the notes entirely.}
      \item{\code{otherInfo(MIAME)}:}{An accessor function for \code{other}.}
      \item{\code{preproc(MIAME)}:}{An accessor function for \code{preprocessing}.}
      \item{\code{pubMedIds(MIAME), pubMedIds(MIAME) <- value}:}{Accessor function for \code{pubMedIds}.}
      \item{\code{samples(MIAME)}:}{An accessor function for \code{samples}.}
   }
   Standard generic methods:
   \describe{
     \item{\code{updateObject(object, \dots, verbose=FALSE)}}{Update instance to current version, if necessary. See \code{\link{updateObject}}}
     \item{\code{isCurrent(object)}}{Determine whether version of object is current. See \code{\link{isCurrent}}}
     \item{\code{isVersioned(object)}}{Determine whether object contains a 'version' string describing its structure . See \code{\link{isVersioned}}}
      \item{\code{show(MIAME)}:}{Renders information about the MIAME information }
   }
}

\references{
   \url{http://www.mged.org/Workgroups/MIAME/miame_1.1.html}
}

\seealso{
   \code{\link{class:characterORMIAME}}, \code{\link{read.MIAME}}
}

\author{
   Rafael A. Irizarry
}

\keyword{classes}

