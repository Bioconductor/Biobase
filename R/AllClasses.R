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
# sSet <== annotatedDataset: general container for high-throughput assays and
# experimental metadata (V.J. Carey after initial design by R. Gentleman)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Based on: class 'annotatedDataset'
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClassUnion("listOrEnv", c("list", "environment"))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClass("eSet",
   representation(
      assayData     = "listOrEnv",
      sampleNames   = "character",
      reporterNames = "character",
      description   = "characterORMIAME",
      notes         = "character",
      annotation    = "character",
      history       = "character"
   ),
   contains = "annotatedDataset",
   validity = function(object) validEset(object),
   prototype = list(
      assayData     = list(),
      sampleNames   = character(0),
      reporterNames = character(0),
      description   = character(0),
      phenoData     = new("phenoData"),
      reporterInfo  = data.frame()
   )
)
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
   ) ,
   prototype = list(
      exprs       = matrix(nr=0,nc=0),
      se.exprs    = matrix(nr=0,nc=0),
      description = new("MIAME"),
      annotation  = "",
      notes       = ""
   ),
   contains = c("annotatedDataset")
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
