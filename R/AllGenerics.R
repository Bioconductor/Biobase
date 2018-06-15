setGeneric("abstract",        function(object) standardGeneric("abstract"))
setGeneric("aggenv",          function(object) standardGeneric("aggenv"))
setGeneric("aggfun",          function(object) standardGeneric("aggfun"))
setGeneric("annotatedDataFrameFrom", function(object, byrow, ...) standardGeneric("annotatedDataFrameFrom"))
setGeneric("assayData",       function(object)standardGeneric("assayData"))
setGeneric("assayData<-",     function(object, value) standardGeneric("assayData<-"))
setGeneric("channelNames",    function(object, ...) standardGeneric("channelNames"))
setGeneric("channel",         function(object, name, ...) standardGeneric("channel"))
setGeneric("content",         function(object) standardGeneric("content"))
setGeneric("description",     function(object, ...)
           standardGeneric("description"))
setGeneric("description<-",   function(object, value) standardGeneric("description<-"))
setGeneric("dimnames")
setGeneric("dimnames<-")
setGeneric("dimLabels",       function(object) standardGeneric("dimLabels"))
setGeneric("dimLabels<-",     function(object, value) standardGeneric("dimLabels<-"))
setGeneric("esApply",          function(X, MARGIN, FUN, ...) standardGeneric("esApply"),
           signature=c("X"))
setGeneric("experimentData",  function(object) standardGeneric("experimentData"))
setGeneric("experimentData<-",function(object, value) standardGeneric("experimentData<-"))
setGeneric("expinfo",         function(object) standardGeneric("expinfo"))
setGeneric("exprs",           function(object) standardGeneric("exprs"))
setGeneric("exprs<-",         function(object, value) standardGeneric("exprs<-"))
setGeneric("featureNames",    function(object) standardGeneric("featureNames"))
setGeneric("featureNames<-",  function(object, value) standardGeneric("featureNames<-"))
setGeneric("featureData",     function(object) standardGeneric("featureData"))
setGeneric("featureData<-",   function(object, value) standardGeneric("featureData<-"))
setGeneric("fData",           function(object) standardGeneric("fData"))
setGeneric("fData<-",         function(object, value) standardGeneric("fData<-"))
setGeneric("fvarLabels",      function(object) standardGeneric("fvarLabels"))
setGeneric("fvarLabels<-",    function(object, value) standardGeneric("fvarLabels<-"))
setGeneric("fvarMetadata",    function(object) standardGeneric("fvarMetadata"))
setGeneric("fvarMetadata<-",  function(object, value) standardGeneric("fvarMetadata<-"))
setGeneric("hybridizations",  function(object) standardGeneric("hybridizations"))
setGeneric("initfun",         function(object) standardGeneric("initfun"))
setGeneric("locked",          function(object) standardGeneric("locked"))
setGeneric("makeDataPackage", function(object, author, email,
                                       packageName=deparse(substitute(object)),
                                       packageVersion=package_version("1.0.0"),
                                       license="Artistic-2.0",
                                       biocViews="ExperimentData",
                                       filePath=tempdir(), ...) standardGeneric("makeDataPackage"),
           signature="object")
setGeneric("normControls",    function(object) standardGeneric("normControls"))
setGeneric("notes",           function(object) standardGeneric("notes"))
setGeneric("notes<-",         function(object, value) standardGeneric("notes<-"))
setGeneric("otherInfo",       function(object) standardGeneric("otherInfo"))
setGeneric("pData",           function(object) standardGeneric("pData"))
setGeneric("pData<-",         function(object, value) standardGeneric("pData<-"))
setGeneric("phenoData",       function(object) standardGeneric("phenoData"))
setGeneric("phenoData<-",     function(object, value) standardGeneric("phenoData<-"))
setGeneric("preproc",         function(object) standardGeneric("preproc"))
setGeneric("preproc<-",       function(object, value) standardGeneric("preproc<-"))
setGeneric("protocolData",    function(object) standardGeneric("protocolData"))
setGeneric("protocolData<-",  function(object, value) standardGeneric("protocolData<-"))
setGeneric("pubMedIds",       function(object) standardGeneric("pubMedIds"))
setGeneric("pubMedIds<-",     function(object, value) standardGeneric("pubMedIds<-"))
setGeneric("sampleNames",     function(object) standardGeneric("sampleNames"))
setGeneric("sampleNames<-",   function(object, value) standardGeneric("sampleNames<-"))
setGeneric("samples",         function(object) standardGeneric("samples"))
setGeneric("se.exprs",        function(object) standardGeneric("se.exprs"))
setGeneric("se.exprs<-",      function(object, value) standardGeneric("se.exprs<-"))
setGeneric("selectChannels",  function(object, names, ...) standardGeneric("selectChannels"))
setGeneric("selectSomeIndex", function(object, ...) standardGeneric("selectSomeIndex"))
setGeneric("snpCall",        function(object, ...) standardGeneric("snpCall"))
setGeneric("snpCall<-",      function(object, ..., value) standardGeneric("snpCall<-"))
setGeneric("snpCallProbability",   function(object, ...) standardGeneric("snpCallProbability"))
setGeneric("snpCallProbability<-", function(object, ..., value) standardGeneric("snpCallProbability<-"))
setGeneric("storageMode",     function(object) standardGeneric("storageMode"))
setGeneric("storageMode<-",   function(object, value) standardGeneric("storageMode<-"))
setGeneric("varLabels",       function(object) standardGeneric("varLabels"))
setGeneric("varLabels<-",     function(object, value) standardGeneric("varLabels<-"))
setGeneric("varMetadata",     function(object) standardGeneric("varMetadata"))
setGeneric("varMetadata<-",   function(object, value) standardGeneric("varMetadata<-"))
setGeneric("write.exprs",     function(x,...) standardGeneric("write.exprs"))
## Version-related generics
setGeneric("classVersion",    function(object) standardGeneric("classVersion"))
setGeneric("classVersion<-",  function(object, value) standardGeneric("classVersion<-"))
setGeneric("isCurrent",       function(object, value) standardGeneric("isCurrent"))
setGeneric("isVersioned",     function(object) standardGeneric("isVersioned"))
## updateObjectTo
setGeneric("updateObjectTo",
           function(object, template, ..., verbose=FALSE) {
               result <- standardGeneric("updateObjectTo")
               if (!is(result, class(template))) # or strict equality?
                 stop("updateObjectTo returned class '", class(result), "' ",
                      "expected class '", class(template), "'")
               validObject(result)
               result
           })



## Generics for Constructors: AnnotatedDataFrame and ExpresssionSet

setGeneric("AnnotatedDataFrame", function(data, varMetadata, ...)
           standardGeneric("AnnotatedDataFrame"))


setGeneric("ExpressionSet",
           function(assayData,
                    phenoData=annotatedDataFrameFrom(assayData, byrow=FALSE),
                    featureData=annotatedDataFrameFrom(assayData, byrow=TRUE),
                    experimentData=MIAME(),
                    annotation=character(),
                    protocolData=annotatedDataFrameFrom(assayData, byrow=FALSE),
                    ...)
           standardGeneric("ExpressionSet"),
           signature="assayData")
