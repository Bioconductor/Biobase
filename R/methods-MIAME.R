setMethod("isCurrent", signature(object="MIAME", value="missing"),
          function(object, value) {
              cver <- callNextMethod()
              MIAMEres <- all(c(cver, "pubMedIds" %in% names(getObjectSlots(object))))
              if("MIAxE" %in% names(cver)) {
                  res <- c(cver["MIAxE"], MIAMEres) 
                  names(res) <- c("MIAxE", "MIAME")
              } else {res <- MIAMEres; names(res) <- "MIAME"}
              res
      })

setMethod("updateObject", signature(object="MIAME"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'MIAME')")
              object <- asS4(object)
              if (isVersioned(object) && isCurrent(object)["MIAME"])
                callNextMethod()
              else
                object <- updateObjectFromSlots(object, ..., verbose=verbose)
                classVersion(object) <- classVersion(class(object))
                object
          })

# ==========================================================================
setMethod("show", "MIAME",
   function(object) {
      tmp <- c("samples","hybridizations","normalization controls","preprocessing")
      Index <-c(length(object@samples) > 0,
                length(object@hybridizations) > 0,
                length(object@normControls) > 0,
                length(object@preprocessing) > 0)
      cat("Experiment data\n")
      cat("  Experimenter name:",object@name,"\n")
      cat("  Laboratory:",object@lab,"\n")
      cat("  Contact information:",object@contact,"\n")
      cat("  Title:",object@title,"\n")
      cat("  URL:",object@url,"\n")
# deal with legacy MIAME objects!
      pmids = try( pubMedIds(object), silent=TRUE )
      if (!inherits(pmids, "try-error")) cat("  PMIDs:",pmids,"\n")
# end of dealing!
      if(length(object@abstract) > 0 && all(object@abstract!=""))
         cat("\n  Abstract: A",length(strsplit(object@abstract," ")[[1]]),
             "word abstract is available. Use 'abstract' method.\n")
      else
         cat("  No abstract available.\n")
      if(any(Index))
         cat("  Information is available on:", paste(tmp[Index],collapse=", "),"\n")
      nO = notes(object)
      if (length(nO) > 0) {
        cat("  notes:\n" )
        if( is.list(nO) ) {
           nms = names(nO)
           ##a print width for the values, so we stay inside the margins
           pw = options("width")[[1]] - 6
           for(i in 1:length(nO) ) {
              cat("   ", nms[i], ":", sep="")
              cat("     ", strbreak(nO[[i]], width=pw, exdent=0), sep="\n      ")
           }
        }
      }
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
setReplaceMethod("preproc", "MIAME", function(object, value) {
    object@preprocessing <- value
    object
})
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("pubMedIds","MIAME",function(object) object@pubMedIds)

setReplaceMethod("pubMedIds","MIAME",function(object,value){
   object@pubMedIds = value
   object
})
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
setMethod("notes", signature(object="MIAME"),
          function(object) object@other)

setReplaceMethod("notes", signature(object="MIAME", value="list"),
                 function(object, value) {
                     object@other <- value
                     object
                 })

setReplaceMethod("notes", signature(object="MIAME", value="character"),
                 function(object, value) {
                     object@other <- append(object@other, value)
                     object
                 })
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
MIAME <- 
    function(name = "", lab = "", contact = "",
             title = "", abstract = "", url = "",
             pubMedIds = "", samples = list(),
             hybridizations = list(), normControls = list(),
             preprocessing = list(), other = list()) {
        .MIAME(name=name, lab=lab, contact=contact, title=title,
               abstract=abstract, url=url, pubMedIds=pubMedIds,
               samples=samples, hybridizations=hybridizations,
               normControls=normControls, preprocessing=preprocessing,
               other=other)
    }


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
read.MIAME <- function(filename=NULL,widget=getOption("BioC")$Base$use.widgets,...) {
   if(!is.null(filename)) {
      miame <- scan(filename,what="c",quiet=TRUE,sep="\n",...)
      MIAME(name=miame[1], lab=miame[2], contact=miame[3],
            title=miame[4], abstract=miame[5], url=miame[6])
   }
   else
      if(widget) {
         requireNamespace("tkWidgets", quietly=TRUE) ||
             stop("Requires tkWidgets")
         tmp <- tkWidgets::tkMIAME()
         MIAME(name=tmp$ExperimentName, lab=tmp$LabName,
               contact=tmp$ContactInfo, title=tmp$ExperimentTitle,
               abstract=tmp$Description, url=tmp$URL)
      }
      else
         MIAME()
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("combine", c("MIAME", "MIAME"), function(x, y, ...) {
  if (identical(x,y)) return (x)
  for (sl in names(getSlots(class(x)))) {
    if (identical(slot(x,sl),slot(y,sl)))
      next
    slot(x,sl) <- 
      switch(sl,
             ## multiple elements possible
             name=,
             lab=,
             contact=,
             title=,
             url=,
             pubMedIds=,
             samples=,
             hybridizations=,
             normControls=,
             preprocessing=,
             other=
             {
               c(slot(x,sl),slot(y,sl))
             },
             ## just a single entry
             abstract= {
               paste(slot(x,sl), slot(y,sl), collapse="\n")
             },
             .__classVersion__= {
                 stop("'MIAME' objects have different class version strings")
             },
             ## unknown
             {
                 warning("\n  unknown or conflicting information in MIAME field '", sl,"'; using information from object 'x'")
                 slot(x,sl)
             })

  }
  x
})
