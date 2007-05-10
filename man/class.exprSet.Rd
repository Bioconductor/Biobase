\name{exprSet}
\docType{class}
\alias{class:exprSet}
\alias{exprSet}
\alias{exprSet-class}

\alias{intitialize,exprSet-method}
\alias{abstract,exprSet-method}
\alias{annotation<-,exprSet,ANY-method}
\alias{annotation,exprSet-method}
\alias{as.data.frame.exprSet,exprSet-method}
\alias{assayData,exprSet-method}
\alias{[,exprSet-method}
\alias{description<-,exprSet,ANY-method}
\alias{description,exprSet-method}
\alias{exprs2excel,exprSet-method}
\alias{exprs<-,exprSet,ANY-method}
\alias{exprs,exprSet-method}
\alias{geneNames<-,exprSet,ANY-method}
\alias{geneNames,exprSet-method}
\alias{featureNames<-,exprSet-method}
\alias{featureNames,exprSet-method}
\alias{initialize,exprSet-method}
\alias{iter,exprSet,missing,function-method}
\alias{iter,exprSet,missing,list-method}
\alias{iter,exprSet,character,function-method}
\alias{notes<-,exprSet,ANY-method}
\alias{notes,exprSet-method}
\alias{sampleNames<-,exprSet,ANY-method}
\alias{sampleNames,exprSet-method}
\alias{se.exprs<-,exprSet-method}
\alias{se.exprs,exprSet-method}
\alias{show,exprSet-method}
\alias{split,exprSet,factor,missing-method}
\alias{split,exprSet,vector,missing-method}
\alias{update2MIAME,exprSet-method}
\alias{updateObject,exprSet-method}
\alias{write.exprs,exprSet-method}

\alias{as.data.frame.exprSet}
\alias{split}
\alias{write.exprs}
\alias{update2MIAME}
\alias{exprs2excel}

\concept{annotation}
\concept{description}
\concept{exprs}
\concept{geneNames}
\concept{notes}
\concept{pData}
\concept{phenoData}
\concept{sampleNames}
\concept{se.exprs}
\concept{varLabels}
\concept{read.exprSet}

\title{
   Class for Microarray Data and Methods for Processing Them
}

\description{This is a class representation for microarray data}

\section{Extends}{
   Directly extends class \code{\link{annotatedDataset}}.
}

\section{Creating Objects}{
   \code{ new('exprSet', exprs = [exprMatrix], se.exprs = [exprMatrix], phenoData = [phenoData], annotation = [character], description = [characterORMIAME], notes = [character])}
}
\section{Slots}{
   Derived from \code{\link{annotatedDataset}}:
   \describe{
      \item{\code{reporterInfo}}{class:\code{data.frameOrNULL}}
      \item{\code{phenoData}:}{Object of class 'phenoData' containing the patient (or case) level data. The columns of the pData slot of this entity represent variables and the rows represent patients or cases. }
   }
   Introduced in \code{exprSet}:
   \describe{
      \item{\code{exprs}:}{Object of class 'exprMatrix'. The observed expression levels. This is a matrix with columns representing patients or cases and rows representing genes. }
      \item{\code{se.exprs}:}{Object of class 'exprMatrix'. This is a matrix of the same dimensions as \code{exprs} which contains standard error estimates for the estimated expression levels. }
      \item{\code{annotation}}{A character string identifying the annotation that may be used for the \code{exprSet} instance.}
      \item{\code{description}:}{Object of class 'characterORMIAME'. For compatibility with previous version of this class description can also be a 'character'. The clase \code{characterOrMIAME} has been defined just for this.}
      \item{\code{notes}:}{Object of class 'character' of explanatory text }
  }
}

\section{Methods}{
   Derived from \code{\link{annotatedDataset}}:
   \describe{
      \item{\code{$(exprSet)} and \code{$(exprSet, value)<-}}{An old-style method. It is \code{pData(eset)[[as.character(val)]]} which does not quite have the right semantics but it is close. This operator extracts the named component of the \code{pData} slot in \code{phenoData}.}
      \item{\code{[[(index)} and \code{[[(index, value)<-}:}{see \code{\link{annotatedDataset}}}
      \item{\code{phenoData(exprSet)} and \code{phenoData(exprSet, value)<-}}{see \code{\link{annotatedDataset}}}
      \item{\code{reporterInfo(exprSet)} and \code{reporterInfo(exprSet, value)<-}}{see \code{\link{annotatedDataset}}}
      \item{\code{pData(exprSet)} and \code{pData(exprSet, value)<-}}{see \code{\link{annotatedDataset}}}
      \item{\code{varLabels(exprSet)}}{see \code{\link{annotatedDataset}}}
   }
   Class-specific methods:
   \describe{
      \item{\code{update2MIAME(exprSet)}:}{Converts \code{exprSet}s from previous versions, that have a \code{character} in description to an object that has an instance of the class \code{MIAME} in the description slot. The old description is stored in the \code{title} slot. If the object already has a \code{MIAME} description the same object is returned.}
      \item{\code{assayData(exprSet)}:}{Method to access \code{exprs} slot}
      \item{\code{exprs(exprSet)} and \code{exprs(exprSet)<-}:}{Methods to access/update \code{exprs} slot }
      \item{\code{se.exprs(exprSet)} and \code{se.exprs(exprSet)<-}:}{Methods to access/update \code{se.exprs} slot }
      \item{\code{description(exprSet)} and \code{description(exprSet, value)<-}:}{Methods to access/update \code{description} slot }
      \item{\code{annotation(exprSet)} and \code{annotation(exprSet, value)<-}:}{Methods to access/update \code{annotation} slot}
      \item{\code{notes(exprSet)} and \code{notes(exprSet, value)<-}:}{Methods to access/update \code{notes} slot }
      \item{\code{abstract(exprSet)}:}{Not documented: \code{function(object) abstract(description(object))}}
      \item{\code{sampleNames(exprSet)} and \code{sampleNames(exprSet, value)<-}:}{Methods to access/update \code{dimnames} of the \code{exprs} slot }
      \item{\code{geneNames(exprSet)} and \code{geneNames(exprSet, value)<-}:}{Methods to access/update \code{row.names} of the \code{exprs} slot - gene names }
      \item{\code{featureNames(exprSet)} and \code{featureNames(exprSet, value)<-}:}{Methods to access/update \code{row.names} of the \code{exprs} slot; use \code{featureNames} in preference to \code{geneNames}, to allow easier transition to new Biobase classes like \code{\link{ExpressionSet-class}}.}
      \item{\code{write.exprs(exprSet,...)}:}{Writes the expression levels to file. It takes the same arguments as \code{\link{write.table}}. If called with no arguments it is equivalent to \code{write.table(exprs(exprSet),file="tmp.txt",quote=FALSE,sep="\t")}.}
      \item{\code{exprs2excel(exprSet,...)}:}{Writes the expression levels to \code{csv} file. This file will open nicely in excel. It takes the same arguments as \code{\link{write.table}}. If called with no arguments it is equivalent to \code{write.table(exprs(exprSet),file="tmp.csv", sep = ",", col.names = NA)}.}
      \item{\code{as.data.frame.exprSet(exprSet, row.names = NA, optional = NA)}:}{Converts \code{exprSet} into a \code{data.frame}. In the return value, the first column is called \code{exprs} and contains the values returned by the method \code{exprs()}. The second column is called genenames and contains the values returned by the method \code{geneNames()}. The other columns will depend on the contents of the \code{\link{phenoData}} slot.}
   }
   Iterator-series methods:\cr
   This is a set of methods to iterate over different types of objects. The behaviour of the methods is similar to that of the \code{apply} family.
   \describe{
      \item{\code{iter(exprSet, missing, function)}:}{An iterator over genes. Returns the result of applying \code{function} to the matrix of expressions on margin 1 (see \code{apply})}
      \item{\code{iter(exprSet, missing, list)}:}{A multi-iterator over genes. Concatenates result of applying each function in the list \code{list} in a matrix (assumes result of each function evaluation is a scalar).}
      \item{\code{iter(exprSet, character, function)}:}{An iterator over genes. \code{function} is assumed to have arguments x and y; the pData element named by covlab will be bound to x, the gene expression values will be iteratively bound to y}
   }
   Split-series methods:
   \describe{
      \item{\code{split(exprSet, factor)}:}{See method for \code{vector}}
      \item{\code{split(exprSet, vector)}:}{Splits the exprSet. The returned value is a list, each component of which is an \code{exprSet}. If the length of \code{vector} is a divisor of the number of rows of the phenoData data frame then the split is made on this.}
   }
   Standard generic methods:
   \describe{
     \item{\code{updateObject(object, ..., verbose=FALSE)}}{Update instance to current version, if necessary. See \code{\link{updateObject}}}
     \item{\code{isCurrent(object)}}{Determine whether version of object is current. See \code{\link{isCurrent}}}
     \item{\code{isVersioned(object)}}{Determine whether object contains a 'version' string describing its structure . See \code{\link{isVersioned}}}
      \item{\code{show(exprSet)}:}{Renders information about the exprSet in a concise way on stdout. }
      \item{\code{[(exprSet)}:}{A subset operator. Ensures that both \code{exprs} and \code{phenoData} are subset properly.}
      \item{\code{validObject(exprSet)}:}{Validity-checking method,
	  ensuring the number and names of \code{phenoData} rows match the
	  number and names of \code{exprs} columns}
   }
}
\seealso{
   \code{\link{MIAME}}, \code{\link{annotatedDataset}}, \code{\link{phenoData}}, \code{\link{class:exprMatrix}}, \code{\link{class:characterORMIAME}}, \code{\link{read.exprSet}}, \code{\link{esApply}}
}
\examples{
  data(geneData)
  data(geneCov)
  covdesc<- list("Covariate 1", "Covariate 2", "Covariate 3")
  names(covdesc) <- names(geneCov)
  pdata <- new("phenoData", pData=geneCov, varLabels=covdesc)
  pdata[1,]
  pdata[,2]

  expr <- new("exprSet", exprs=geneData, phenoData=pdata)
  expr
  expr[,1:10]
  expr[,1]
  expr[1,]
  expr[1,1]
  expr[1:100,]
  expr[1:44,c(2,4,6)]
  Means <- iter(expr, f=mean)

  chkdich <- function(x) if(length(unique(x))!=2) stop("x not dichotomous")
  mytt <- function(x,y) {
     chkdich(x)
     d <- split(y,x)
     t.test(d[[1]],d[[2]])$p.val
  }

  Tpvals <- iter(expr, "cov1", mytt )

  sp1 <- split(expr, c(1,2))
  sp2 <- split(expr, c(rep(1,6), rep(2,7)))

  sampleNames(expr)
  sampleNames(expr) <- letters
  featureNames(expr)[1:10]

  # as.data.frame.exprSet - example
  data(sample.exprSet)
  sd.genes <- esApply(sample.exprSet, 1, sd)
  dataf <- as.data.frame(sample.exprSet)
  dataf <- cbind(dataf, sd.genes=rep(unname(sd.genes), length=nrow(dataf)))
  coplot(sd.genes ~ exprs | sex+type, data=dataf)

  # update existing exprSet-like object
  data(sample.exprSet)
  updateObject(sample.exprSet) # to match class definition of same (exprSet) class
  expressionSet <- as(sample.exprSet, "ExpressionSet") # to different class
}
\keyword{methods}
\keyword{classes}
