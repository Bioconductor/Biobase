\name{phenoData}
\docType{class}
\alias{class:phenoData}

\alias{phenoData-class}
\alias{[,phenoData-method}
\alias{[[<-,phenoData-method}
\alias{[[,phenoData-method}
\alias{$<-,phenoData-method}
\alias{$,phenoData-method}
\alias{coerce,data.frame,phenoData-method}
\alias{combine,phenoData,phenoData-method}
\alias{combine,phenoData,ANY-method}
\alias{convertVarLabels,phenoData-method}
\alias{getVarMetadata}
\alias{getVarMetadata<-}
\alias{getVarMetadata,phenoData,character,character-method}
\alias{getVarMetadata,phenoData,character,missing-method}
\alias{initialize,phenoData-method}
\alias{pData<-,phenoData,ANY-method}
\alias{pData,phenoData-method}
\alias{phenoData<-,annotatedDataset,phenoData-method}
\alias{show,phenoData-method}
\alias{split,phenoData,vector,missing-method}
\alias{updateObject,phenoData-method}
\alias{varLabels,phenoData-method}
\alias{varMetadata,phenoData-method}

\concept{varLabels}
\concept{convertVarLabels}
\concept{getUnits}
\concept{getVarMetadata}
\concept{addVarMetadataEntry}

\title{Class For Patient or Experiment Level Data}

\description{The class consists of a \code{data.frame} and some
  accompanying methods suited to handle patient level data for microarrays.}

\section{Creating Objects}{
   \code{new('phenoData', pData = [data.frame], varLabels = [list], varMetadata = [data.frame])}
}

\section{Slots}{
  \describe{
    \item{\code{pData}:}{Object of class 'data.frame'. The data. }
    \item{\code{varLabels}:}{Object of class 'list'. Optional long labels for the different variables. }
    \item{\code{varMetadata}:}{Object of class 'data.frame'. A data frame with a column named varNames and arbitrarily many additional columns. }
  }
}

\section{Methods}{
   Class-specific methods:
   \describe{
      \item{\code{combine(phenoData, phenoData)}}{Combines several instances into a single 'phenoData' instance. See \code{\link{combine}}}
      \item{\code{convertVarLabels(phenoData)}}{Not documented}
      \item{\code{getVarMetadata(phenoData, character, character)} or \code{getVarMetadata(phenoData, character, missing)}}{Not documented}
      \item{\code{pData(phenoData), pData(phenoData)<-}}{Get/set methods for slot \code{pData}}
      \item{\code{phenoData(exprSet)<-}}{A replacement method for the \code{phenoData} slot}
      \item{\code{split(phenoData, vector)}}{FIXME: there is no implementation!!! A method to \code{split} \code{pData} and return a list of \code{phenoData} objects. Unlike \code{split} this function requires the number of rows of \code{pData} to be a multiple of the length of \code{vector}}
      \item{\code{varLabels(phenoData)}}{A method to display the variable labels. }
      \item{\code{varMetadata(phenoData)}}{Not documented}
   }
   Standard methods defined for 'phenoData':
   \describe{
      \item{\code{$(name), $(name, value)<-}}{An old-style method to get and set. It is \code{pData(object)[[as.character(val)]]} which does not quite have the right semantics but it is close. This operator extracts the named component of the \code{pData} slot in \code{phenoData}.}
      \item{\code{[(phenoData)}}{A subset operator}
      \item{\code{[[(phenoData), [[(phenoData)<-}}{A get/set subset operator}
      \item{\code{show(phenoData)}}{Displays the data in a readable manner}
   }
   Functions defined in Biobase to operate with 'phenoData' instances include:
   \describe{
     \item{\code{updateObject(object, ..., verbose=FALSE)}}{Update instance to current version, if necessary. See \code{\link{updateObject}}}
     \item{\code{isCurrent(object)}}{Determine whether version of object is current. See \code{\link{isCurrent}}}
     \item{\code{isVersioned(object)}}{Determine whether object contains a 'version' string describing its structure . See \code{\link{isVersioned}}}
      \item{\code{df2pD(x, varLabels, varMetadata)}}{Function to coerce from a \code{data.frame} to a \code{phenoData} object. Here \code{x} is the \code{data.frame} to be coerced; \code{varLabels} is a character describing each of the columns in \code{x} and \code{varMetadata} is a more extensive descriptions of the variables in \code{x}, including units of measurement etc}
      \item{\code{read.phenoData}}{See help on \code{\link{read.phenoData}}}
   }
}
\seealso{
   \code{\link{read.phenoData}}, \code{\link{combine}}
}
\examples{
data(sample.exprSet)
pes <- phenoData(sample.exprSet)
pes <- convertVarLabels(pes)
varMetadata(pes)

# example for 'df2pD'
data(cars)
d1 = df2pD(cars)

# update phenoData to current class description
updateObject(phenoData(sample.exprSet))

}
\keyword{methods}
\keyword{manip}
\keyword{classes}
