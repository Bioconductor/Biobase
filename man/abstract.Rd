\name{abstract}
\alias{abstract}
\alias{pubMedIds}
\alias{pubMedIds<-}
\alias{experimentData}
\alias{experimentData<-}

\title{Retrieve Meta-data from eSets and ExpressionSets.}

\description{
  These generic functions access generic data, abstracts, PubMed IDs
 and experiment data from instances of the 
  \code{\link{eSet-class}} or \code{\link{ExpressionSet-class}}.
}

\usage{
abstract(object)
pubMedIds(object)
pubMedIds(object) <- value
experimentData(object)
experimentData(object) <- value
}
\arguments{
  \item{object}{Object, possibly derived from \code{eSet-class} or 
    \code{MIAME-class}}
  \item{value}{Value to be assigned; see class of \code{object} (e.g.,
    \code{\link{eSet-class}}) for specifics.}
}
\value{
  \code{abstract} returns a character vector containing the abstract
  (as in a published paper) associated with \code{object}.

  \code{pubMedIds} returns a character vector of PUBMED IDs associated
  with the experiment.

  \code{experimentData} returns an object representing the description
  of an experiment, e.g., an object of \code{\link{MIAME-class}}
}
\author{Biocore}

\seealso{\code{\link{ExpressionSet-class}}, \code{\link{eSet-class}}, 
   \code{\link{MIAME-class}}}

\keyword{manip}
