# ==========================================================================
# aggregator: a simple aggregator (R. Gentleman, 2001)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data are aggregated in the environment env if they are not there then the
# get assigned with initfun, if they are there they get aggregated with agfun
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClass("aggregator",
   representation(
      aggenv  = "environment",
      initfun = "function",
      aggfun  = "function"
   ),
   prototype = list(
      initfun = function(name, val) 1,
      aggfun  = function(name, current, val) current + 1
   )
)
# ==========================================================================
# container: lists containing objects of specified class (R. Gentleman, 2001)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClass("container",
   representation(
      x       = "list",
      content = "character",
      locked  = "logical"
   ),
   prototype = list(
      x       = vector("list", 0),
      content = "object",
      locked  = FALSE
   )
)
# ==========================================================================
# phenoData (DEFUNCT)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClass("phenoData",
   representation(
      pData       = "data.frame",
      varLabels   = "list",
      varMetadata = "data.frame"
   ),
   contains="Versioned",
   validity = function(object) "class phenoData is defunct, convert using as(<<object>>, \"AnnotatedDataFrame\")"
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setOldClass("data.frame")
setClassUnion("data.frameOrNULL", c("data.frame", "NULL"))
# ==========================================================================
# MIAME: a class for microarray data - MIAME information (Rafael A. Irizarry)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# More info: http://www.mged.org/Workgroups/MIAME/miame_1.1.html
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClass("MIAME",
   representation(
      name           = "character",
      lab            = "character",
      contact        = "character",
      title          = "character",
      abstract       = "character",
      url            = "character",
      pubMedIds      = "character",
      samples        = "list",
      hybridizations = "list",
      normControls   = "list",
      preprocessing  = "list",
      other          = "list"
   ),
   contains=c("Versioned"),
   prototype = prototype(
      new("Versioned", versions=c(MIAME="1.0.0")),
      name           = "",
      lab            = "",
      contact        = "",
      title          = "",
      abstract       = "",
      url            = "",
      pubMedIds      = "",
      samples        = list(),
      hybridizations = list(),
      normControls   = list(),
      preprocessing  = list(),
      other          = list()
   )
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# trick so that Plobs works
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClassUnion("characterORMIAME", c("MIAME", "character"))
# ==========================================================================
# annotatedDataset (DEFUNCT)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClass("annotatedDataset",
   representation(
      reporterInfo = "data.frameOrNULL",
      phenoData    = "phenoData",
      "VIRTUAL"
   ),
   contains=c("VersionedBiobase"))
# ==========================================================================
# AnnotatedDataFrame: A data.frame, with annotations about columns named
# in the data slot contained in the metadata slot. The data slot has
# columns identifying different entities (e.g., genes, samples) and
# the columns contain attributes of those entities (e.g., control or
# spike-in information for genes, age or sex for samples). The number
# of columns in the data slot equals the number of rows in the
# metadata slot.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClass("AnnotatedDataFrame",
         representation(varMetadata = "data.frame",
                        data = "data.frame",
                        dimLabels = "character"),
         contains=c("Versioned"),
         prototype = prototype(
           new("Versioned", versions=list(AnnotatedDataFrame="1.1.0")),
           varMetadata = new( "data.frame" ),
           data = new( "data.frame" ),
           dimLabels=c("rowNames", "columnNames")))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClassUnion("AssayData", c("list", "environment"))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# eSet: A VIRTUAL class containing assay data (typically, one or many
# different sets of results obtained from one or many samples in a
# single experiment), phenotypic data (describing the samples involved
# in the experiment), experimental data (describing the methods and
# protocols used), and an annotation (linking to separately maintained
# chip annotation information).
#
# When assayData contains several sets of results, each set must have
# the same dimension (e.g., columns representing genes, rows
# representing samples, all assayData members providing information
# for the same number of genes and samples).
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClass("eSet",
         representation(assayData = "AssayData",
                        phenoData = "AnnotatedDataFrame",
                        featureData = "AnnotatedDataFrame",
                        experimentData = "MIAME",
                        annotation = "character",
                        protocolData="AnnotatedDataFrame",
                        "VIRTUAL"),
         contains="VersionedBiobase",
         prototype = prototype(
           new("VersionedBiobase", versions=c(eSet="1.3.0")),
               assayData = list(), # use initialize to set as env, so different instances have different envs
               phenoData = new("AnnotatedDataFrame",
                 dimLabels=c("sampleNames", "sampleColumns")),
               featureData = new("AnnotatedDataFrame",
                 dimLabels=c("featureNames", "featureColumns")),
               experimentData = new("MIAME"),
               annotation = character(),
               protocolData = new("AnnotatedDataFrame",
                 dimLabels=c("sampleNames", "sampleColumns"))))
setClass("ExpressionSet",
         contains = "eSet",
         prototype = prototype(
           new("VersionedBiobase",
               versions=c(classVersion("eSet"), ExpressionSet="1.0.0"))))
setClass("NChannelSet",
         contains = "eSet",
         prototype = prototype(
           new("VersionedBiobase",
               versions=c(classVersion("eSet"), NChannelSet="1.0.0")),
           phenoData = new("AnnotatedDataFrame",
             data=data.frame(),
             varMetadata=data.frame(
               labelDescription=character(0),
               channelDescription=factor()))))
setClass("MultiSet",                    # any element in assayData slot
         contains = "eSet",
         prototype = prototype(
           new("VersionedBiobase",
               versions=c(classVersion("eSet"), MultiSet="1.0.0"))))
setClass("SnpSet",                      # call, callProbability
         contains = "eSet",
         prototype = prototype(
           new("VersionedBiobase",
               versions=c(classVersion("eSet"), SnpSet="1.0.0"))))
# ==========================================================================
# exprSet (DEFUNCT)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClass("exprSet",
   representation(
      exprs       = "matrix",
      se.exprs    = "matrix",
      description = "characterORMIAME",
      annotation  = "character",
      notes       = "character"
   ),
   contains = c("annotatedDataset"), # contains VersionedBiobase implicitly
   validity = function(object) "class exprSet is defunct, convert using as(<<object>>, \"ExpressionSet\")"
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

setClass("ScalarObject", contains="VIRTUAL",
         validity=function(object) {
             if (length(object) != 1L)
               paste(class(object), "must have length one")
             else
               TRUE
         })

setClass("ScalarLogical", contains=c("ScalarObject", "logical"),
         prototype=NA)

setClass("ScalarCharacter", contains=c("ScalarObject", "character"),
         prototype="")

setClass("ScalarInteger", contains=c("ScalarObject", "integer"),
         prototype=NA_integer_)

setClass("ScalarNumeric", contains=c("ScalarObject", "numeric"),
         prototype=NA_real_)
