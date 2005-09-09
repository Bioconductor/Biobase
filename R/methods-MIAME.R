# ==========================================================================
setMethod("show", "MIAME",
   function(object) {
      tmp <- c("samples","hybridizations","normalization controls","preprocessing")
      Index <-c(length(object@samples) > 0,
                length(object@hybridizations) > 0,
                length(object@normControls) > 0,
                length(object@preprocessing) > 0)
      cat("Experimenter name:",object@name,"\n")
      cat("Laboratory:",object@lab,"\n")
      cat("Contact information:",object@contact,"\n")
      cat("Title:",object@title,"\n")
      cat("URL:",object@url,"\n")
      if(object@abstract!="")
         cat("\nA",length(strsplit(object@abstract," ")[[1]]),
             "word abstract is available. Use 'abstract' method.\n")
      else
         cat("No abstract available.\n")
      if(any(Index))
         cat("\nInformation is available on:", paste(tmp[Index],collapse=", "),"\n")
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("abstract","MIAME",function(object) object@abstract)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("samples","MIAME",function(object) object@samples)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("hybridizations","MIAME",function(object) object@hybridizations)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("normControls","MIAME",function(object) object@normControls)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("preproc","MIAME",function(object) object@preprocessing)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("otherInfo","MIAME",function(object) object@other)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("expinfo","MIAME",
   function(object) {
      tmp <- c(object@name, object@lab, object@contact, object@title, object@url)
    names(tmp) <- c("name","lab","contact","title","url")
    return(tmp)
   }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
read.MIAME <- function(filename=NULL,widget=getOption("BioC")$Base$use.widgets,...) {
   if(!is.null(filename)) {
      miame <- scan(filename,what="c",quiet=TRUE,sep="\n",...)
      return(new("MIAME",name=miame[1],lab=miame[2],contact=miame[3],title=miame[4],abstract=miame[5],url=miame[6]))
   }
   else
      if(widget) {
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
      else
         return(new("MIAME"))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


