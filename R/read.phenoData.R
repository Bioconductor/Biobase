read.phenoData <- function(filename = NULL, sampleNames = NULL,
                           widget = getOption("BioC")$Base$use.widgets,...){

    if(widget){
        if(!is.null(filename)){
            phenoD <- importPhenoData(fileName = filename,
                               sampleNames = sampleNames, from = "file")
        }else{
            phenoD <- importPhenoData(sampleNames = sampleNames)
        }
        return(phenoD)
    }else{
        if(!is.null(filename)){
            pData <- read.table(filename,...)
            if(!is.null(sampleNames)) row.names(pData) <- sampleNames
            varLabels <- as.list(rep("read from file",ncol(pData)))
            names(varLabels) <- names(pData)
            return(new("phenoData",pData=pData,varLabels=varLabels))
        }else{
            if(is.null(sampleNames)){
                return(new("phenoData")) ##return a blank!
            }else{
                pdata <- data.frame(sample=1:length(sampleNames),
                                    row.names=sampleNames)
                return(new("phenoData",pData=pdata,
                           varLabels=list(sample="arbitrary numbering")))
            }
        }
    }
}





