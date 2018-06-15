\name{eSet}
\docType{class}
\alias{class:eSet}
\alias{eSet}

\alias{eSet-class}
\alias{[,eSet-method}
\alias{$,eSet-method}
\alias{$<-,eSet-method}
\alias{[[,eSet-method}
\alias{[[<-,eSet-method}
\alias{abstract,eSet-method}
\alias{annotation,eSet-method}
\alias{annotation<-,eSet,character-method}
\alias{assayData,eSet-method}
\alias{assayData<-,eSet,AssayData-method}
\alias{assayDataElement}
\alias{assayDataElement<-}
\alias{assayDataElementNames}
\alias{assayDataElementReplace}
\alias{combine,eSet,ANY-method}
\alias{combine,eSet,eSet-method}
\alias{description,eSet-method}
\alias{description<-,eSet,MIAME-method}
\alias{dimnames}
\alias{dimnames<-}
\alias{dimnames,eSet-method}
\alias{dimnames<-,eSet-method}
\alias{dim,eSet-method}
\alias{dims}
\alias{dims,eSet-method}
\alias{experimentData,eSet-method}
\alias{experimentData<-,eSet,MIAME-method}
\alias{exprs,eSet-method}
\alias{exprs<-,eSet,AssayData-method}
\alias{featureData,eSet-method}
\alias{featureData<-,eSet,AnnotatedDataFrame-method}
\alias{featureNames,eSet-method}
\alias{featureNames<-,eSet-method}
\alias{fData,eSet-method}
\alias{fData<-,eSet,data.frame-method}
\alias{fvarLabels,eSet-method}
\alias{fvarLabels<-,eSet-method}
\alias{fvarMetadata,eSet-method}
\alias{fvarMetadata<-,eSet,data.frame-method}
\alias{initialize,eSet-method}
\alias{ncol,eSet-method}
\alias{notes,eSet-method}
\alias{notes<-,eSet,ANY-method}
\alias{pData,eSet-method}
\alias{pData<-,eSet,data.frame-method}
\alias{phenoData,eSet-method}
\alias{phenoData<-,eSet,AnnotatedDataFrame-method}
\alias{pubMedIds,eSet-method}
\alias{pubMedIds<-,eSet,character-method}
\alias{preproc,eSet-method}
\alias{preproc<-,eSet-method}
\alias{protocolData,eSet-method}
\alias{protocolData<-,eSet,character-method}
\alias{sampleNames,eSet-method}
\alias{sampleNames<-,eSet,ANY-method}
\alias{show,eSet-method}
\alias{storageMode,eSet-method}
\alias{storageMode<-,eSet,character-method}
\alias{varLabels,eSet-method}
\alias{varLabels<-,eSet-method}
\alias{varMetadata,eSet-method}
\alias{varMetadata<-,eSet,data.frame-method}
\alias{storageMode,eSet-method}
\alias{storageMode<-,eSet,character-method}
\alias{updateObject,eSet-method}
\alias{updateObjectTo,eSet,eSet-method}

\alias{listOrEnv}
\alias{SW}

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
  identical-sized matrices as \code{assayData} elements. Derived
  classes (e.g., \code{\link{ExpressionSet-class}}, \code{\link{SnpSet-class}})
  specify which elements must be present in the \code{assayData} slot.

  \code{eSet} object cannot be instantiated directly; see the examples
  for usage.
}
\section{Creating Objects}{
  \code{eSet} is a virtual class, so instances cannot be created.

  Objects created under previous definitions of \code{eSet-class} can be
  coerced to the current classes derived from \code{eSet} using
  \code{updateOldESet}.
}
\section{Slots}{
   Introduced in \code{eSet}:
   \describe{
      \item{\code{assayData}:}{Contains matrices with equal
	dimensions, and with column number equal to
	\code{nrow(phenoData)}. Class:\code{\link{AssayData-class}}}
      \item{\code{phenoData}:}{Contains experimenter-supplied variables
      describing sample (i.e., columns in \code{assayData}) phenotypes.
	Class: \code{\link{AnnotatedDataFrame-class}}}
      \item{\code{featureData}:}{Contains variables describing features
	(i.e., rows in \code{assayData}) unique to this experiment. Use
	the \code{annotation} slot to efficiently reference feature data
	common to the annotation package used in the experiment. Class:
	\code{\link{AnnotatedDataFrame-class}}} 
      \item{\code{experimentData}:}{Contains details of experimental
	methods. Class: \code{\link{MIAME-class}}}
      \item{\code{annotation}:}{Label associated with the annotation
	package used in the experiment. Class: \code{character}}
      \item{\code{protocolData}:}{Contains microarray equipment-generated
      variables describing sample (i.e., columns in \code{assayData}) phenotypes.
    Class: \code{\link{AnnotatedDataFrame-class}}}
      \item{\code{.__classVersion__}:}{A \code{Versions} object describing
    the R and Biobase version numbers used to created the instance.
    Intended for developer use.}
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
     \item{\code{dimnames(object)}, \code{dimnames(object) <-
	 value}:}{Also \code{rownames} and \code{colnames}; access and set
	 feature and sample names.}
     \item{\code{dims(object)}:}{Access the common dimensions (\code{dim}) or
       column numbers (\code{ncol}), or dimensions of all members
       (\code{dims}) of \code{assayData}.}

     \item{\code{phenoData(object)},
       \code{phenoData(object) <- value}:}{Access and set
	   \code{phenoData}. Adding new columns to \code{phenoData} is often
	   more easily done with \code{eSetObject[["columnName"]] <- value}.}
     \item{\code{pData(object)}, \code{pData(object) <- value}:}{Access and set
       sample data information. Adding new columns to \code{pData} is often
	   more easily done with \code{eSetObject[["columnName"]] <- value}.}
     \item{\code{varMetadata(object)},
       \code{varMetadata(eSet,value)}}{Access and set metadata describing
       variables reported in \code{pData}}
     \item{\code{varLabels(object)}, \code{varLabels(eSet,
	     value)<-}:}{Access and set variable labels in
       \code{phenoData}.}

     \item{\code{featureData(object)},
       \code{featureData(object) <- value}:}{Access and set \code{featureData}.}
     \item{\code{fData(object)}, \code{fData(object) <- value}:}{Access and set
       feature data information.}
     \item{\code{fvarMetadata(object)},
       \code{fvarMetadata(eSet,value)}}{Access and set metadata describing
       features reported in \code{fData}}
     \item{\code{fvarLabels(object)},
       \code{fvarLabels(eSet, value)<-}:}{Access and set variable labels in
       \code{featureData}.} 

     \item{\code{assayData(object), assayData(object) <- value}:}{
       \code{signature(object = "eSet", value = "AssayData")}: Access
       and replace the \code{AssayData} slot of an
       \code{eSet} instance. \code{assayData} returns a list or
       environment; elements in \code{assayData} not accessible in other
       ways (e.g., via \code{exprs} applied directly to the \code{eSet})
       can most reliably be accessed with, e.g., \code{assayData(obj)[["se.exprs"]]}.}
     \item{\code{experimentData(object)},\code{experimentData(object) <- value}:}{Access
       and set details of experimental methods}
     \item{\code{description(object)},\code{description(object) <- value}:}{Synonymous with experimentData.}
     \item{\code{notes(object)},\code{notes(object) <- value}:}{
       \code{signature(object="eSet", value="list")} Retrieve and set unstructured notes associated with \code{eSet}.
       \code{signature(object="eSet", value="character")} As with
       value="list", but \emph{append} value to current list of notes.}
     
     \item{\code{pubMedIds(object)}, \code{pubMedIds(eSet,value)}}{Access
       and set PMIDs in \code{experimentData}.}
     \item{\code{abstract(object)}:}{Access abstract in
       \code{experimentData}.}
     \item{\code{annotation(object)},
       \code{annotation(object) <- value}}{Access and set annotation label
       indicating package used in the experiment.}
     \item{\code{protocolData(object)},
       \code{protocolData(object) <- value}}{Access and set the protocol data.}
     \item{\code{preproc(object)},
       \code{preproc(object) <- value}:}{\code{signature(object="eSet",
	 value="list")} Access and set \code{preprocessing} information
       in the \code{\link{MIAME-class}} object associated with this
       \code{eSet}.}
     \item{\code{combine(eSet,eSet)}:}{Combine two \code{eSet}
       objects. To be combined, eSets must have identical numbers of
       \code{featureNames}, distinct \code{sampleNames}, and identical \code{annotation}.}
     \item{\code{storageMode(object)}, \code{storageMode(eSet,character)<-}:}{Change storage mode
       of \code{\link{assayData}}. Can be used to 'unlock' environments,
       or to change between \code{list} and \code{environment} modes of
       storing \code{assayData}.}
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
     \item{\code{as(eSet, "ExpressionSet")}}{Convert instance of class \code{"eSet"} to instance of \code{\link{ExpressionSet-class}}, if possible.}
     \item{\code{as(eSet, "MultiSet")}}{Convert instance of class \code{"eSet"} to instance of \code{\link{MultiSet-class}}, if possible.}
     \item{\code{updateObject(object, ..., verbose=FALSE)}}{Update instance to current version, if necessary. Usually called through class inheritance rather than directly by the user. See \code{\link{updateObject}}}
     \item{\code{updateObjectTo(object, template, ..., verbose=FALSE)}}{Update instance to current version by updating slots in \code{template}, if necessary. Usually call by class inheritance, rather than directly by the user. See \code{\link{updateObjectTo}}}
     \item{\code{isCurrent(object)}}{Determine whether version of object is current. See \code{\link{isCurrent}}}
     \item{\code{isVersioned(object)}}{Determine whether object contains a 'version' string describing its structure . See \code{\link{isVersioned}}}
     \item{\code{show(object)}}{Informatively display object contents.}
     \item{\code{dim(object)}, \code{ncol}}{Access the common
       dimensions (\code{dim}) or column numbers (\code{ncol}), of all
       memebers (\code{dims}) of \code{assayData}.}
     \item{\code{object[(index)}:}{Conducts subsetting of matrices and
       phenoData components }
     \item{\code{object$name}, \code{object$name<-value}}{Access and set \code{name} column in \code{phenoData}}
     \item{\code{object[[i, ...]]}, \code{object[[i, ...]]<-value}}{Access and set column \code{i} (character or
       numeric index) in \code{phenoData}. The ... argument can include
       named variables (especially \code{labelDescription}) to be added
       to varMetadata.}
   }
   Additional functions:
   \describe{
    \item{assayDataElement(object, element)}{Return matrix
      \code{element} from \code{assayData} slot of \code{object}.}
    \item{assayDataElement(object, element, validate=TRUE) <-
       value)}{Set element \code{element} in \code{assayData} slot of
       \code{object} to matrix \code{value}. If \code{validate=TRUE},
       check that value row and column names of conform to object.}
    \item{assayDataElementReplace(object, element, value,
      validate=TRUE)}{Set element \code{element} in \code{assayData}
      slot of \code{object} to matrix \code{value}. If
      \code{validate=TRUE}, check that row and column names of value
      conform to object.}
    \item{assayDataElementNames(object)}{Return element names in
      \code{assayData} slot of \code{object}}
     \item{\code{updateOldESet}}{Update versions of \code{eSet}
       constructued using \code{listOrEnv} as \code{assayData} slot
       (before May, 2006).}
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

# update previous eSet-like class oldESet to existing derived class
\dontrun{updateOldESet(oldESet, "ExpressionSet")}

# create a new, ad hoc, class, for personal use
# all methods outlined above are available automatically
.MySet <- setClass("MySet", contains="eSet")
.MySet()

# Create a more robust class, with constructor and validation methods
# to ensure assayData contains specific matricies
.TwoColorSet <- setClass("TwoColorSet", contains="eSet")

TwoColorSet <-
    function(phenoData=AnnotatedDataFrame(), experimentData=MIAME(),
             annotation=character(), R=new("matrix"), G=new("matrix"),
             Rb=new("matrix"), Gb=new("matrix"), ...)
{
    .TwoColorSet(phenoData=phenoData, experimentData=experimentData,
                 annotation=annotation, R=R, G=G, Rb=Rb, Gb=Gb, ...)
}

setValidity("TwoColorSet", function(object) {
  assayDataValidMembers(assayData(object), c("R", "G", "Rb", "Gb"))
})

TwoColorSet()

# eSet objects cannot be instantiated directly, only derived objects
try(new("eSet"))

removeClass("MySet")
removeClass("TwoColorSet")
}

\keyword{classes}
