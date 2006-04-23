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
      aggenv  = new.env(hash=TRUE),
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
# phenoData: patient or experiment level data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Consists of a 'data.frame' and some accompanying methods suited to handle
# patient level data for microarrays
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClass("phenoData",
   representation(
      pData       = "data.frame",
      varLabels   = "list",
      varMetadata = "data.frame"
   ),
   prototype = list(
      pData       = data.frame(matrix(nr=0,nc=0)),
      varLabels   = list(),
      varMetadata = data.frame(matrix(nr=0,nc=0))
   ),
   validity = function(object) validator.phenoData(object)
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
   prototype = list(
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
# trick so that the old exprSet and Plobs works
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClassUnion("characterORMIAME", c("MIAME", "character"))
# ==========================================================================
# annotatedDataset: virtual superset for 'exprSet', 'eSet' etc (Alan Katz)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Currently defines 'phenoData' specific methods
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Uses: class phenoData
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClass("annotatedDataset",
   representation(
      reporterInfo = "data.frameOrNULL",
      phenoData    = "phenoData",
      "VIRTUAL"
   ),
   prototype = list()
)
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
                        data = "data.frame" ),
         prototype = list(
           varMetadata = new( "data.frame" ),
           data = new( "data.frame" )),
         validity = function(object) validAnnotatedDataFrame(object))
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
                        experimentData = "MIAME",
                        annotation = "character"),
         prototype = list(
           assayData = list(), # use initialize to set as env, so different instances have different envs
           phenoData = new( "AnnotatedDataFrame" ),
           experimentData = new( "MIAME" ),
           annotation = character()),
           "VIRTUAL"
)
setClass("ExpressionSet", contains = "eSet") # exprSet-like
setClass("MultiSet", contains = "eSet") # any elements in the assayData slot
setClass("SnpSet", contains = "eSet")
# ==========================================================================
# exprSet <== annotatedDataset: expression arrays and methods for processing them
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Based on: class 'annotatedDataset'
# Uses: classes 'MIAME' and 'exprMatrix'
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Class union for the exprs and se.exprs slots of exprSet
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
if (!isClass("exprMatrix"))
   setClassUnion("exprMatrix", c("matrix"))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClass("exprSet",
   representation(
      exprs       = "exprMatrix",
      se.exprs    = "exprMatrix",
      description = "characterORMIAME",
      annotation  = "character",
      notes       = "character"
   ),
   prototype = list(
      exprs       = matrix(nr=0,nc=0),
      se.exprs    = matrix(nr=0,nc=0),
      description = new("MIAME"),
      annotation  = "",
      notes       = ""
   ),
   contains = c("annotatedDataset"),
   validity = function(object) validExprSet(object)
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
