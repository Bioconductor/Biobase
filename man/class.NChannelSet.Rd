\name{NChannelSet-class}
\docType{class}
\alias{NChannelSet-class}
\alias{NChannelSet}
\alias{class.NChannelSet}
\alias{assayData<-,NChannelSet,environment-method}
\alias{assayData<-,NChannelSet,list-method}
\alias{channelNames,NChannelSet-method}
\alias{channelNames<-,NChannelSet,character-method}
\alias{channelNames<-,NChannelSet,list-method}
\alias{channel,NChannelSet,character-method}
\alias{sampleNames,NChannelSet-method}
\alias{sampleNames<-,NChannelSet,list-method}
\alias{selectChannels,NChannelSet,character-method}
\alias{initialize,NChannelSet-method}

\title{Class to contain data from multiple channel array technologies}

\description{
  Container for high-throughput assays and experimental meta-data. Data
  are from experiments where a single \sQuote{chip} contains several
  (more than 1) different \sQuote{channels}. All channels on a chip have
  the same set of\sQuote{features}. An experiment consists of a
  collection of several N-channel chips; each chip is a \sQuote{sample}.

  An \code{NChannelSet} provides a way to coordinate assay data
  (expression values) with phenotype information and references to chip
  annotation data; it extends the \code{\link{eSet}} class.

  An \code{NChannelSet} allows channels to be extracted (using the
  \code{channels} method, mentioned below), and subsets of features or
  samples to be selected (using \code{[<features>,
    <samples>]}). Selection and subsetting occur so that relevant
  phenotypic data is maintained by the selection or subset.}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{NChannelSet(
    assayData, phenoData, ...)}. See the examples below.
}

\section{Slots}{
  \describe{
    \item{\code{assayData}:}{
      Object of class \code{\link{AssayData}}, usually
      an environment containing matrices of identical size. Each matrix
      represents a single channel. Columns in each matrix correspond to
      samples, rows to features. Once created, \code{NChannelSet} manages
      coordination of samples and channels.
    }

    \item{\code{phenoData}:}{
      Object of class \code{\link{AnnotatedDataFrame}}.

      The \code{data} component of the \code{AnnotatedDataFrame} is
      \code{data.frame} with number of rows equal to the number of
      samples. Columns of the \code{data} component correspond to measured
      covariates.

      The \code{varMetadata} component consists of mandatory columns
      \code{labelDescription} (providing a textual description of each column
      label in the \code{data} component) and \code{channel}. The \code{channel}
      of \code{varMetadata} is a \code{factor}, with levels equal to the names
      of the \code{assayData} channels, plus the special symbol
      \code{_ALL_}. The \code{channel} column is used to indicate which
      channel(s) the corresponding column in the \code{data} component
      of \code{AnnotatedDataFrame} correspond;  the \code{_ALL_} symbol
      indicates that the \code{data} column is applicable to all
      channels. \code{varMetadata} may contain additional columns with
      arbitrary information.

      Once created, \code{NChannelSet} coordinates selection and subsetting
      of channels in \code{phenoData}.
    }

    \item{\code{featureData}:}{
      Object of class \code{\link{AnnotatedDataFrame}}, used to contain
      feature data that is unique to this experiment; feature-level
      descriptions common to a particular chip are usually referenced
      through the \code{annotation} slot.
    }
    \item{\code{experimentData}:}{
      Object of class \code{\link{MIAME}} containing descriptions of the
      experiment.
    }

    \item{\code{annotation}:}{
      Object of class \code{"character"}. Usually a length-1 character
      string identifying the chip technology used during the
      experiment. The annotation string is used to retrieve information 
      about features, e.g., using the \code{annotation} package.}

    \item{\code{protocolData}:}{
      Object of class \code{"character"}. A character vector
      identifying the dates the samples were scanned during the
      experiment.}

    \item{\code{.__classVersion__}:}{
      Object of class \code{\link{Versions}}, containing
      automatically created information about the class definition
      Biobase package version, and other information about the user
      system at the time the instance was created. See
      \code{\link{classVersion}} and \code{\link{updateObject}} for
      examples of use.
    }
  }
}

\section{Extends}{
  Class \code{"\linkS4class{eSet}"}, directly.
  Class \code{"\linkS4class{VersionedBiobase}"}, by class "eSet", distance 2.
  Class \code{"\linkS4class{Versioned}"}, by class "eSet", distance 3.
}

\section{Methods}{
  Methods with class-specific functionality:
  \describe{
    \item{\code{channel(object, name, ...)}}{
      \code{signature(object="NChannelSet", name="character")}.
      Return an \code{ExperessionSet} created from the channel and
      corresponding phenotype of argument \code{name}. \code{name} must
      have length 1. Arguments \code{...} are rarely used, but are
      passed to the \code{ExpressionSet} constructor, for instance
      to influence \code{storage.mode}.
    }

    \item{\code{channelNames(object)}, \code{channelNames(object) <- value}}{
      \code{signature(object = "NChannelSet")}.
      Obtain, reorder, or rename channels contained in
      \code{object}. See \code{\link{channelNames}}.
    }

    \item{\code{selectChannels(object, names, ...}}{
      \code{signature(object = "NChannelSet", names = "character")}.
      Create a new \code{NChannelSet} from \code{object}, containing
      only channels in \code{names}. The \code{...} is not used by this
      method.
    }
    \item{\code{object[features, samples]}}{
      \code{signature(object = "NChannelSet",
        features = "ANY", samples = "ANY")}.
      Create a new \code{NChannelSet} from \code{object}, containing
      only elements matching \code{features} and \code{samples}; either
      index may be missing, or a character, numeric, or logical vector.
    }
    \item{\code{sampleNames(object) <- value}}{
      \code{signature(object = "NChannelSet", value = "list")} assign
      each (named) element of \code{value} to the \code{sampleNames} of
      the correspondingly named elements of \code{assayData} in
      \code{object}.
    }
      
  }

  Methods with functionality derived from \code{\link{eSet}}:
  \code{annotation}, \code{annotation<-}, \code{assayData},
  \code{assayData<-}, \code{classVersion}, \code{classVersion<-},
  \code{dim}, \code{dims}, \code{experimentData}, \code{experimentData<-},
  \code{featureData}, \code{featureData<-}, \code{phenoData},
  \code{phenoData<-}, \code{protocolData}, \code{protocolData<-},
  \code{pubMedIds}, \code{pubMedIds<-}, \code{sampleNames},
  \code{sampleNames<-}, \code{storageMode}, \code{storageMode<-},
  \code{varMetadata}, \code{varMetadata<-}, \code{isCurrent},
  \code{isVersioned}, \code{updateObject}.

  Additional methods: \code{coerce} (\sQuote{as}, to convert between
  objects, if possible), \code{initialize} (used internally for
  creating objects), \code{show} (invoked automatically when the
  object is displayed to the screen)
}

\author{Martin Morgan <mtmorgan@fhcrc.org>}

\seealso{
  \code{\linkS4class{eSet}}, \code{\linkS4class{ExpressionSet}}. 
}
\examples{
## An empty NChannelSet
obj <- NChannelSet()

## An NChannelSet with two channels (R, G) and no phenotypic data
obj <- NChannelSet(R=matrix(0,10,5), G=matrix(0,10,5))

## An NChannelSet with two channels and channel-specific phenoData
R <- matrix(0, 10, 3, dimnames=list(NULL, LETTERS[1:3]))
G <- matrix(1, 10, 3, dimnames=list(NULL, LETTERS[1:3]))
assayData <- assayDataNew(R=R, G=G)
data <- data.frame(ChannelRData=numeric(ncol(R)),
                   ChannelGData=numeric(ncol(R)),
                   ChannelRAndG=numeric(ncol(R)))
varMetadata <- data.frame(labelDescription=c(
                            "R-specific phenoData",
                            "G-specific phenoData",
                            "Both channel phenoData"),
                          channel=factor(c("R", "G", "_ALL_")))
phenoData <- AnnotatedDataFrame(data=data, varMetadata=varMetadata)
obj <- NChannelSet(assayData=assayData, phenoData=phenoData)
obj

## G channel as NChannelSet
selectChannels(obj, "G")

## G channel as ExpressionSet
channel(obj, "G")

## Samples "A" and "C"
obj[,c("A", "C")]
}
\keyword{classes}
