\name{exprSet-class}
\docType{class}
\alias{exprSet-class}
\alias{$.exprSet}
\alias{exprs}
\alias{geneNames}
\alias{geneNames<-}
\alias{pData}
\alias{pData<-}
\alias{varLabels}
\alias{sampleNames}
\alias{phenoData}
\alias{annotation}
\title{Class exprSet, a class for microarray data, and methods for
  processing them } 
\description{ This is class representation for Microarray Data}
\section{Creating Objects}{
\code{  new('exprSet',}\cr
\code{    exprs       = ...., # Object of class matrix}\cr
\code{    se.exprs    = ...., # Object of class matrix}\cr
\code{    phenoData   = ...., # Object of class phenoData}\cr
\code{    annotation  = ...., # Object of class character}\cr
\code{    description = ...., # Object of class character}\cr
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
    \item{\code{description}:}{Object of class "character" Vector of
      covariate labels} 
    \item{\code{notes}:}{Object of class "character" Vector of
      explanatory text }
  }
}

\section{Methods}{
  \describe{
    \item{[}{(exprSet): A subset operator. Ensures that both \code{exprs}
      and \code{phenoData} are subset properly. }
    
    \item{exprs}{(exprSet): An accessor function for \code{exprs}. }
    
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
       labels from \code{pData}.
     }
     
    \item{show}{(exprSet): renders information about the exprSet
      in a concise way on stdout. }

    \item{annotation}{(exprSet): An accessor function for the annotation
      information.}
    
    \item{\$}{An old-style method.
      It is \code{pData(eset)[[as.character(val)]]} which
      does not quite have the right semantics but it is close. This
      operator extracts the named component of the \code{pData} slot in
      \code{phenoData}.}
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
}
\keyword{methods}
