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
\alias{description<-,eSet-method}
\alias{description,eSet-method}
\alias{dim,eSet-method}
\alias{dims,eSet-method}
\alias{eList,eSet-method}
\alias{eList<-,eSet,AssayData-method}
\alias{experimentData,eSet-method}
\alias{experimentData<-,eSet,MIAME-method}
\alias{exprs,eSet-method}
\alias{exprs<-,eSet,AssayData-method}
\alias{featureNames,eSet-method}
\alias{featureNames<-,eSet-method}
\alias{initialize,eSet-method}
\alias{getExpData,eSet,character-method}
\alias{ncol,eSet-method}
\alias{notes<-,eSet-method}
\alias{notes,eSet-method}
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
\alias{eList}
\alias{SW}
\alias{SWPD}
\alias{swrep}
\alias{golubMergeSub}
\alias{assayData}

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
  metadata. \code{eSet} is a VIRTUAL class that can contain one or more identical-sized
  matricies as \code{assayData} elements. Derived classes
  (e.g., \code{\link{ExpressionSet}}, \code{\link{SnpSet}}) specify
  which elements must be present in the \code{assayData} slot.
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
	\code{nrow(phenoData)}. Class:\code{\link{AssayData}}}
      \item{\code{phenoData}:}{Contains variables describing sample
	(i.e., columns in \code{assayData})
	phenotypes.Class: \code{\link{AnnotatedDataFrame-class}}}
      \item{\code{experimentData}:}{Contains details of experimental
	methods. Class: \code{\link{MIAME-class}}}
      \item{\code{annotation}:}{Label associated with the annotation
	package used in the experiment. Class:\code{character}}
  }
}
\section{Methods}{
  Methods defined in derived classes (e.g., \code{\link{ExpressionSet}},
  \code{\link{SnpSet}}) may override the methods described here.

   Class-specific methods:
   \describe{
     \item{\code{sampleNames(eSet)} and
       \code{sampleNames(eSet)<-}:}{Coordinate accessing and setting
       sample names in \code{assayData} and \code{phenoData}}
     \item{\code{featureNames(eSet)},
       \code{featureNames(eSet, value)<-}:}{Coordinate accessing and setting of
       feature names (e.g, genes, probes) in \code{assayData}.}
     \item{\code{dims(eSet)}:}{Access the common dimensions (\code{dim}) or
       column numbers (\code{ncol}), or dimensions of all memebers
       (\code{dims}) of \code{assayData}.}
     \item{\code{phenoData(eSet)},
       \code{phenoData(eSet,value)<-}:}{Access and set \code{phenoData}.}
     \item{\code{varLabels(eSet)}, \code{varLabels(eSet,
	 value)<-}:}{Access and set variable labels in
       \code{phenoData}.}
     \item{\code{varMetadata(eSet)},
       \code{varMetadata(eSet,value)<-}:}{Access and set descriptions of
       variables in \code{phenoData}.}
     \item{\code{pData(eSet)}, \code{pData(eSet,value)<-}:}{Access and set
       sample data information}
     \item{\code{varMetadata(eSet)},
       \code{varMetadata(eSet,value)}}{Access and set metadata describing
       variables reported in \code{pData}}
     \item{\code{experimentData(eSet)},\code{experimentData(eSet,value)<-}:}{Access
       and set details of experimental methods}
     \item{\code{pubMedIds(eSet)}, \code{pubMedIds(eSet,value)}}{Access
       and set PMIDs in \code{experimentData}.}
     \item{\code{abstract(eSet)}:}{Access abstract in
       \code{experimentData}.}
     \item{\code{annotation(eSet)},
       \code{annotation(eSet,value)<-}}{Access and set annotation label
       indicating package used in the experiment.}
     \item{\code{combine(eSet,eSet)}:}{Combine two \code{eSet}
       objects. To be combined, eSets must have identical numbers of
       \code{featureNames}, distinct \code{sampleNames}, and identical \code{annotation}.}
     \item{\code{storageMode(eSet)}, \code{storageMode(eSet,character)<-}:}{Change storage mode
       of \code{\link{assayData}}. Can be used to 'unlock' environments,
       or to change between \code{list} and \code{environment} modes of
       storing \code{assayData}.}
     \item{\code{reporterNames(eSet)}, \code{reporterNames(eSet,value)<-}:}{DEPRECATED}
  }
  Standard generic methods:
  \describe{
    \item{\code{initialize(eSet)}:}{Object instantiate, can be called by
      derived classes but not usually by the user.}
    \item{\code{validObject(eSet)}:}{Validity-checking method,
	  ensuring (1) all assayData components have the same number of
	  features and samples; (2) the number and names of
	  \code{phenoData} rows match the number and names of
	  \code{assayData} columns}
     \item{\code{show(eSet)}}{Informatively display object contents.}
     \item{\code{dim(eSet)}, \code{ncol}}{Access the common
       dimensions (\code{dim}) or column numbers (\code{ncol}), of all
       memebers (\code{dims}) of \code{assayData}.}
     \item{\code{eSet[(index)}:}{Conducts subsetting of matrices and
       phenoData components }
     \item{\code{eSet$}, \code{eSet$<-}}{Access and set named columns in \code{phenoData}}
  }
}
\author{V.J. Carey, after initial design by R. Gentleman }
\note{
  \code{golubMergeSub} is an \code{exprSet} example that can be coerced to \code{eSet}; see the \code{eSet} vignette.}


\seealso{
  \code{\link{AssayData}}, \code{\link{AnnotatedDataFrame}},
  \code{\link{MIAME}}, \code{\link{ExpressionSet}}, \code{\link{SnpSet}}
}
% \examples{}			% no examples -- virutal class
% # load sample.exprSet, to be used below to conveniently make an eSet
% data(sample.exprSet)
% # easy conversion
% pd <- phenoData(sample.exprSet)
% xl <- list(e1=exprs(sample.exprSet))
% gn <- geneNames(sample.exprSet)
% sn <- sampleNames(sample.exprSet)

% # construct an eSet, using sample.exprSet components
% es2 <- new("eSet", phenoData=as(pd,"AnnotatedDataFrame"), assayData=xl,
%   reporterNames=gn, sampleNames=sn)

% # illustrate subset and show for assayData as list
% es2[1:4,1:4]

% # build with environment assayData
% eenv <- new.env()
% assign("exprs", exprs(sample.exprSet), eenv)
% es3 <- new("eSet", phenoData=as(pd,"AnnotatedDataFrame"), assayData=eenv,
%   reporterNames=gn, sampleNames=sn)
% es3
% es3[1:5,1:5]

% # show some legacy behavior: set list element name to
% # exprs and invoke exprs()
% names(assayData(es2)) <- "exprs"
% dim(exprs(es2))

% # show some validity checking
% assayData(es3) <- assayData(es2)
% es3  # now has list
% sampleNames(es2) <- letters # ok
% if (interactive()) {
%  lk <- try(sampleNames(es2) <- letters[-1]) # not ok
% }

% # VJC commented the following out, could not fix varMetadata oddity
% # work with the swirl data
% #data(SW)
% #data(swrep)
% #data(SWPD)
% #sw <- new("eSet",
% #  assayData=list( R=SW$R, G=SW$G, Rb=SW$Rb, Gb=SW$Gb),
% #  phenoData=as(SWPD, "AnnotatedDataFrame"), 
% #  reporterInfo=swrep, sampleNames=as.character(1:4),
% #  reporterNames=as.character(swrep$Name))
% ## remove first reporter from each assay, explicitly or implicitly
% #assayData(sw) <- assayData(sw[-1,])
% #sw <- sw[-1,]
% ## now two fewer than original
% #sw 
% }
\keyword{classes}
