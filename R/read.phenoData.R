read.phenoData <- function(filename=NULL,sampleNames=NULL,widget=getOption("BioC")$Base$use.widgets,...){
  
  if(!is.null(filename)){
    pData <- read.table(filename,...)
    if(!is.null(sampleNames)) row.names(pData) <- sampleNames
    varLabels <- as.list(rep("read from file",ncol(pData)))
    names(varLabels) <- names(pData)
    return(new("phenoData",pData=pData,varLabels=varLabels))
  }
  else{
    if(widget){
      require(tkWidgets)
      if(is.null(sampleNames)) stop("To use widget you must supply sampleNames.\n")
      tmp <- tkphenoData(sampleNames)
      pdata <- data.frame(tmp$pData)
      varlabels <- as.list(tmp$varLabels[,1])
      names(varlabels) <- rownames(tmp$varLabels)
      return(new("phenoData",pData=pdata,varLabels=varlabels))
    }
    else{
      if(is.null(sampleNames)){
        return(new("phenoData")) ##return a blank!
      }
      else{
        pdata <- data.frame(sample=1:length(sampleNames),row.names=sampleNames)
        return(new("phenoData",pData=pdata,varLabels=list(sample="arbitrary numbering")))
      }
    }
  }
}
