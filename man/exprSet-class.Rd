\name{exprSet-class}
\docType{class}
\alias{annotation}
\alias{annotation<-}
\alias{description}
\alias{description<-}
\alias{exprs}
\alias{exprs2excel}
\alias{exprs<-}
\alias{geneNames}
\alias{geneNames<-}
\alias{notes}
\alias{notes<-}
\alias{pData}
\alias{pData<-}
\alias{phenoData}
\alias{phenoData<-}
\alias{sampleNames}
\alias{sampleNames<-}
\alias{se.exprs}
\alias{update2MIAME}
\alias{varLabels}
\alias{update2MIAME,exprSet-method}
\alias{write.exprs}
\alias{exprSet-class}
\alias{exprSet}
\alias{$.exprSet}
\alias{description,exprSet-method}
\alias{description<-,exprSet-method}
\alias{exprs,exprSet-method}
\alias{exprs<-,exprSet-method}
\alias{se.exprs,exprSet-method}
\alias{[,exprSet-method}
\alias{[[<-,exprSet-method}
\alias{[[,exprSet-method}
\alias{se.exprs<-}
\alias{se.exprs<-,exprSet-method}
\alias{notes,exprSet-method}
\alias{notes<-,exprSet-method}
\alias{geneNames,exprSet-method}
\alias{geneNames<-,exprSet-method}
\alias{pData,exprSet-method}
\alias{pData<-,exprSet-method}
\alias{varLabels,exprSet-method}
\alias{sampleNames,exprSet-method}
\alias{sampleNames<-,exprSet-method}
\alias{phenoData,exprSet-method}
\alias{phenoData<-,exprSet,phenoData-method}
\alias{annotation,exprSet-method}
\alias{annotation<-,exprSet-method}
\alias{show,exprSet-method}
\alias{split,exprSet,vector-method}
\alias{write.exprs,exprSet-method}
\alias{exprs2excel,exprSet-method}
\title{Class exprSet, a class for microarray data, and methods for
  processing them } 
\description{ This is class representation for Microarray Data}
\section{Creating Objects}{
\code{  new('exprSet',}\cr
\code{    exprs       = ...., # Object of class matrix}\cr
\code{    se.exprs    = ...., # Object of class matrix}\cr
\code{    phenoData   = ...., # Object of class phenoData}\cr
\code{    annotation  = ...., # Object of class character}\cr
\code{    description = ...., # Object of class MIAME}\cr
\code{    notes       = ...., # Object of class character}\cr
\code{  )}}
\section{Slots}{
  \describe{
    \item{\code{exprs}:}{Object of class "matrix" The observed
      expression levels. This is 
     a matrix with columns representing patients or cases and rows
     representing genes. }
    \item{\code{se.exprs}:}{Object of class "matrix" This is
     a matrix of the same dimensions as \code{exprs} which contains
     standard error estimates for the estimated expression levels. }
    \item{\code{phenoData}:}{Object of class "phenoData" This is an
      instance of class \code{phenoData} 
    containing the patient (or case) level data. The columns of the
    pData slot of this entity represent
    variables and the rows represent patients or cases. }
    \item{\code{annotation}}{A character string identifying the
      annotation that may be used for the \code{exprSet} instance.}
    \item{\code{description}:}{Object of class "MIAME". For
      compatibility with previous version of this class description can
      also be a "character". The clase \code{characterOrMIAME} has been
      defined just for this.} 
    \item{\code{notes}:}{Object of class "character" Vector of
      explanatory text }
  }
}

\section{Methods}{
  \describe{
    \item{[}{(exprSet): A subset operator. Ensures that both \code{exprs}
      and \code{phenoData} are subset properly. }
    \item{[[}{(exprSet): Extract the named variable from the
      \code{phenoData} component. }    
    \item{exprs}{(exprSet): An accessor function for \code{exprs}. }
    
    \item{se.exprs}{(exprSet): An accessor function for \code{se.exprs}. }
    
    
    \item{description}{(exprSet): Obtain the description (MIAME class) of  \code{exprSet}. }
    \item{description<-}{(exprSet): Set the description (MIAME class) of
      \code{exprSet}. }

    \item{notes}{(exprSet): Obtain the notes of  \code{exprSet}. }
    \item{notes<-}{(exprSet): Set the notes of
      \code{exprSet}. }

    \item{geneNames}{(exprSet): Obtain the gene names (row names) of
      \code{exprs}. }
    \item{geneNames<-}{(exprSet): Set the gene names (row names) of
      \code{exprs}. }
    
    \item{iter}{(exprSet, missing, function): an iterator over genes.
Returns the result of applying \code{function} to the matrix
of expressions on margin 1 (see \code{apply}) }

    \item{iter}{(exprSet, missing, list): a multi-iterator over genes.
Concatenates result of applying each function in the list \code{list}
in a matrix (assumes result of each function evaluation is a scalar). }

    \item{iter}{(exprSet, covlab=character, function): a iterator over
genes: \code{function} is assumed to have arguments x and y;
the pData element named by covlab will be bound to x, the
gene expression values will be iteratively bound to y}

    \item{phenoData}{(exprSet): An accessor function returns the
      \code{phenoData} instance from the \code{exprSet}.}

   \item{pData}{(exprSet): An accessor function for the \code{pData}
     slot of the \code{phenoData} contained in the instance.}

   \item{pData<-}{(exprSet): A replacement method for the \code{pData}
     slot of the \code{phenoData} contained in the instance.}

     \item{sampleNames}{(exprSet): An accessor function for sample
       labels. These are the column labels of \code{exprs} and the row
       labels from \code{pData}. The function takes preferably the
       column labels of \code{exprs} (if any).
     }
     \item{sampleNames<-}{(exprSet): A replacement method that sets the
       sample names to new values. The values must be \code{character}
       and the length of the vector must be equal to the number of samples.
     }
      
    \item{show}{(exprSet): renders information about the exprSet
      in a concise way on stdout. }

    \item{split}{(exprSet, vector): splits the exprSet. The returned
      value is a list, each component of which is an \code{exprSet}.
      If the length of \code{vector} is a divisor of the number of rows
      in the expression array then the expression array is
      split. Subsequently, if the length of \code{vector} is a divisor
      of the number of rows of the phenoData data frame then the split
      is made on this.}
    \item{annotation}{(exprSet): An accessor function for the annotation
      information.}
    \item{write.exprs}{(exprSet,...): Writes the expression levels to
      file. It takes the same arguments as \code{\link{write.table}}. If
      called with no arguments it is equivalent to
      write.table(exprs(exprSet),file="tmp.txt",quote=FALSE,sep="\t").} 
    \item{exprs2excel}{(exprSet,...): 
      Writes the expression levels to \code{csv}
      file. This file will open nicely in excel. It takes the same
      arguments as \code{\link{write.table}}. If 
      called with no arguments it is equivalent to
      \code{write.table(exprs(exprSet),file="tmp.csv", sep = ",", col.names = NA)}.}
    \item{\$}{An old-style method.
      It is \code{pData(eset)[[as.character(val)]]} which
      does not quite have the right semantics but it is close. This
      operator extracts the named component of the \code{pData} slot in
      \code{phenoData}.}
    \item{update2MIAME}{(exprSet,...): Converts \code{exprSet}s from
      previous versions, that have a \code{character} in description to
      an object that has an instance of the class \code{MIAME} in
      the description slot. The old description is stored in the
      \code{title} slot. If the object already has a \code{MIAME}
      description the same object is returned.}
    } 
  }
}
\examples{
  data(geneData)
  data(geneCov)
  covdesc<- list("Covariate 1", "Covariate 2", "Covariate 3")
  names(covdesc) <- names(geneCov)
  pdata <- new("phenoData", pData=geneCov, varLabels=covdesc)
  pdata[1,]
  pdata[,2]

  eset <- new("exprSet", exprs=geneData, phenoData=pdata)
  eset
  eset[,1:10]
  eset[,1]
  eset[1,]
  eset[1,1]
  eset[1:100,]
  eset[1:44,c(2,4,6)]
  Means <- iter(eset, f=mean)

  chkdich <- function(x) if(length(unique(x))!=2) stop("x not dichotomous")
  mytt <- function(x,y) {
     chkdich(x)
     d <- split(y,x)
     t.test(d[[1]],d[[2]])$p.val
  }
  
  Tpvals <- iter(eset, "cov1", mytt )

  sp1 <- split(eset, c(1,2))
  sp2 <- split(eset, c(rep(1,6), rep(2,7)))

  sampleNames(eset)
  sampleNames(eset) <- letters
}
\seealso{MIAME-class}
\keyword{methods}
