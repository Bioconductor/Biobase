read.MIAME <- function(filename=NULL,widget=getOption("BioC")$Base$use.widgets,...){

  if(!is.null(filename)){
    miame <- scan(filename,what="c",quiet=TRUE,sep="\n",...)
    return(new("MIAME",name=miame[1],lab=miame[2],contact=miame[3],title=miame[4],abstract=miame[5],url=miame[6]))
  }
  else
    if(widget){
      require(tkWidgets) || stop("Requires tkWidgets")
      tmp <- tkMIAME()
      return(new("MIAME",
                 name=tmp$ExperimentName,
                 lab=tmp$LabName,
                 contact=tmp$ContactInfo,
                 title=tmp$ExperimentTitle,
                 abstract=tmp$Description,
                 url=tmp$URL))
    }

    else{
      return(new("MIAME"))
    }
}

