# A function to read exprSet from text a text file. The function
# assumes that exprs and se.exprs are tab separated text files.
# The first 6 arguments are for text files to be read in for the slots
# of an exprSet object. seps is for the separators used for exprs and
# se.exprs files. length(seps) = 1 if all two files share
# the same separator or length(seps) = 2 if not. seps are ordered in
# the order as the two arguments appear in the argument list. A
# header is assumed to exist for exprs file. Only exprs is required
# and the others are optional. phenodata has to be the name of a rda
# file that is a list with a pData and a varLabels element.
#


read.exprSet <- function(exprs, se.exprs, phenoData, annotation,
                        description, notes, seps = "\t" ){
    if(missing(exprs)){
        stop("exprs can not be missing!")
    }
    # Read exprs
    geneData <- as.matrix(read.table(exprs, header = TRUE,
                                  sep = seps[1], as.is =  TRUE))
    # Read se.exprs using the right separator
    if(!missing(se.exprs)){
        se.exprs <- as.matrix(read.table(se.exprs, header = FALSE,
                                     sep = ifelse(length(seps == 1),
                                     seps, seps[2]), as.is = TRUE))
    }
    # If phenoData is missing provide a default
    if(missing(phenoData)){
        phenoData <- read.phenoData(NULL, colnames(geneData), FALSE)
    }else{
        phenoData <- read.phenoData(phenoData, colnames(geneData), FALSE)
    }
    # Each gene should have a coVariate?
    if(nrow(pData(phenoData)) != ncol(geneData)){
        warning("Gene and covariate numbers do not match")
    }
    eSet <- new("exprSet", exprs = geneData, phenoData = phenoData)
    if(!missing(se.exprs)){
        se.exprs(eSet) <- se.exprs
    }
    if(!missing(annotation)){
        annotation(eSet) <-  readLines(annotation)
    }
    if(!missing(description)){
        description(eSet) <- read.MIAME(description, FALSE)
    }else{
        description(eSet) <- new("MIAME")
    }
    if(!missing(notes)){
        notes(eSet) <- readLines(notes)
    }

    return(eSet)
}

