\name{eSet}
\docType{class}
\alias{class:eSet}
\alias{eSet}

\alias{eSet-class}
\alias{[,eSet-method}
\alias{$,eSet-method}
\alias{$<-,eSet-method}
\alias{abstract,eSet-method}
\alias{annotation,eSet-method}
\alias{annotation<-,eSet,character-method}
\alias{assayData,eSet-method}
\alias{assayData<-,eSet,AssayData-method}
\alias{coerce,exprSet,eSet-method}
\alias{combine,eSet,ANY-method}
\alias{combine,eSet,eSet-method}
\alias{description,eSet-method}
\alias{description<-,eSet,MIAME-method}
\alias{dim,eSet-method}
\alias{dims,eSet-method}
\alias{experimentData,eSet-method}
\alias{experimentData<-,eSet,MIAME-method}
\alias{exprs,eSet-method}
\alias{exprs<-,eSet,AssayData-method}
\alias{featureNames,eSet-method}
\alias{featureNames<-,eSet-method}
\alias{initialize,eSet-method}
\alias{getExpData,eSet,character-method}
\alias{ncol,eSet-method}
\alias{notes,eSet-method}
\alias{notes<-,eSet,list-method}
\alias{pData,eSet-method}
\alias{pData<-,eSet,data.frame-method}
\alias{phenoData,eSet-method}
\alias{phenoData<-,eSet,AnnotatedDataFrame-method}
\alias{pubMedIds,eSet-method}
\alias{pubMedIds<-,eSet,character-method}
\alias{reporterNames,eSet-method}
\alias{sampleNames,eSet-method}
\alias{sampleNames<-,eSet-method}
\alias{show,eSet-method}
\alias{storageMode,eSet-method}
\alias{storageMode<-,eSet,character-method}
\alias{varLabels,eSet-method}
\alias{varLabels<-,eSet-method}
\alias{varMetadata,eSet-method}
\alias{varMetadata<-,eSet,data.frame-method}
\alias{storageMode,eSet-method}
\alias{storageMode<-,eSet,character-method}

\alias{listOrEnv}
\alias{SW}
\alias{SWPD}
\alias{swrep}
\alias{golubMergeSub}

\concept{exprs}
\concept{annotation}
\concept{combine}
\concept{description}
\concept{ncol}
\concept{notes}
\concept{featureNames}
\concept{sampleNames}

\title{
  Class to Contain High-Throughput Assays and Experimental Metadata
}

\description{

  Container for high-throughput assays and experimental
  metadata. Classes derived from \code{eSet} contain one or more
  identical-sized matricies as \code{assayData} elements. Derived
  classes (e.g., \code{\link{ExpressionSet-class}}, \code{\link{SnpSet-class}})
  specify which elements must be present in the \code{assayData} slot.

  \code{eSet} object cannot be instantiated directly; see the examples
  for useage.

}

% \section{Extends}{
%    Directly extends class \code{\link{annotatedDataset}}.
% }
\section{Creating Objects}{

  \code{eSet} is a virtual class, so instances cannot be created.

  Objects created under previous definitions of \code{eSet-class} can be
  coerced to classes derived  based on \code{eSet} using \code{updateOldESet}.
}
\section{Slots}{
   Introduced in \code{eSet}:
   \describe{
      \item{\code{assayData}:}{Contains matrices with equal
	dimensions, and with column number equal to
	\code{nrow(phenoData)}. Class:\code{\link{AssayData-class}}}
      \item{\code{phenoData}:}{Contains variables describing sample
	(i.e., columns in \code{assayData})
	phenotypes.Class: \code{\link{AnnotatedDataFrame-class}}}
      \item{\code{experimentData}:}{Contains details of experimental
	methods. Class: \code{\link{MIAME-class}}}
      \item{\code{annotation}:}{Label associated with the annotation
	package used in the experiment. Class: \code{character}}
      \item{\code{.classVersion}:}{Character vector describing the R and
	Biobase version number used to created the instance. Intended
	for developer use. Class: \code{character}} 
  }
}
\section{Methods}{
  Methods defined in derived classes (e.g., \code{\link{ExpressionSet-class}},
  \code{\link{SnpSet-class}}) may override the methods described here.

   Class-specific methods:
   \describe{
     \item{\code{sampleNames(object)} and
       \code{sampleNames(object)<-value}:}{Coordinate accessing and setting
       sample names in \code{assayData} and \code{phenoData}}
     \item{\code{featureNames(object)},
       \code{featureNames(object) <- value}:}{Coordinate accessing and setting of
       feature names (e.g, genes, probes) in \code{assayData}.}
     \item{\code{dims(object)}:}{Access the common dimensions (\code{dim}) or
       column numbers (\code{ncol}), or dimensions of all memebers
       (\code{dims}) of \code{assayData}.}
     \item{\code{phenoData(object)},
       \code{phenoData(object) <- value}:}{Access and set \code{phenoData}.}
     \item{\code{varLabels(object)}, \code{varLabels(eSet,
	 value)<-}:}{Access and set variable labels in
       \code{phenoData}.}
     \item{\code{varMetadata(object)},
       \code{varMetadata(object) <- value}:}{Access and set descriptions of
       variables in \code{phenoData}.}
     \item{\code{pData(object)}, \code{pData(object) <- value}:}{Access and set
       sample data information}
     \item{\code{varMetadata(object)},
       \code{varMetadata(eSet,value)}}{Access and set metadata describing
       variables reported in \code{pData}}
     \item{\code{experimentData(object)},\code{experimentData(object) <- value}:}{Access
       and set details of experimental methods}
     \item{\code{description(object)},\code{description(object) <- value}:}{Synonymous with experimentData; included for compatibility with \code{exprSet}. }
     \item{\code{notes(object)},\code{notes(object) <- value}:}{\code{signature(object="eSet", value="list")} Retrieve and set unstructured notes associated with \code{eSet}.}
     \item{\code{pubMedIds(object)}, \code{pubMedIds(eSet,value)}}{Access
       and set PMIDs in \code{experimentData}.}
     \item{\code{abstract(object)}:}{Access abstract in
       \code{experimentData}.}
     \item{\code{annotation(object)},
       \code{annotation(object) <- value}}{Access and set annotation label
       indicating package used in the experiment.}
     \item{\code{combine(eSet,eSet)}:}{Combine two \code{eSet}
       objects. To be combined, eSets must have identical numbers of
       \code{featureNames}, distinct \code{sampleNames}, and identical \code{annotation}.}
     \item{\code{storageMode(object)}, \code{storageMode(eSet,character)<-}:}{Change storage mode
       of \code{\link{assayData}}. Can be used to 'unlock' environments,
       or to change between \code{list} and \code{environment} modes of
       storing \code{assayData}.}
     \item{\code{reporterNames(object)}, \code{reporterNames(object) <-
	 value}:}{DEPRECATED, see \code{featureNames}}
  }
  Standard generic methods:
  \describe{
    \item{\code{initialize(object)}:}{Object instantiation, can be called by
      derived classes but not usually by the user.}
    \item{\code{validObject(object)}:}{Validity-checking method,
	  ensuring (1) all assayData components have the same number of
	  features and samples; (2) the number and names of
	  \code{phenoData} rows match the number and names of
	  \code{assayData} columns}
     \item{\code{show(object)}}{Informatively display object contents.}
     \item{\code{dim(object)}, \code{ncol}}{Access the common
       dimensions (\code{dim}) or column numbers (\code{ncol}), of all
       memebers (\code{dims}) of \code{assayData}.}
     \item{\code{object[(index)}:}{Conducts subsetting of matrices and
       phenoData components }
     \item{\code{object}, \code{eSet$<-}}{Access and set named columns in \code{phenoData}}
   }
   Additional functions:
   \describe{
     \item{\code{updateOldESet}}{Update versions of \code{eSet}
       constructued using \code{listOrEnv} as \code{assayData} slot
       (before May, 2006)}
   }
}
\author{Biocore team}

\seealso{
  Method use in \code{\link{ExpressionSet-class}}.
  Related  classes
  \code{\link{AssayData-class}}, \code{\link{AnnotatedDataFrame-class}}, \code{\link{MIAME-class}}.
  Derived classes
  \code{\link{ExpressionSet-class}}, \code{\link{SnpSet-class}}.
  To update objects from previous class versions, see \code{\link{updateOldESet}}.
}

\examples{
# create an instance of an existing derived class
new("ExpressionSet")

# update existing exprSet-like class to existing derived class
data(sample.exprSet)
expressionSet <- as(sample.exprSet,"ExpressionSet")
expressionSet

# update previous eSet-like class oldESet to existing derived class
\dontrun{updateOldESet(oldESet, "ExpressionSet")}

# create a new, ad hoc, class, for personal use
# all methods outlined above are available automatically
setClass("MySet", contains="eSet")
new("MySet")

# Create a more robust class, with initialization and validation methods
# to ensure assayData contains specific matricies
setClass("TwoColorSet", contains="eSet")

setMethod("initialize", "TwoColorSet",
          function(.Object,
                   phenoData = new("AnnotatedDataFrame"),
                   experimentData = new("MIAME"),
                   annotation = character(),
                   R = new("matrix"),
                   G = new("matrix"),
                   Rb = new("matrix"),
                   Gb = new("matrix"),
                   ... ) {
            callNextMethod(.Object,
                           phenoData = phenoData,
                           experimentData = experimentData,
                           annotation = annotation,
                           R=R, G=G, Rb=Rb, Gb=Gb,
                           ...)
          })

setValidity("TwoColorSet", function(object) {
  assayDataValidMembers(assayData(object), c("R", "G", "Rb", "Gb"))
})

new("TwoColorSet")

# eSet objects cannot be instantiated directly, only derived objects
try(new("eSet"))
}

\keyword{classes}
