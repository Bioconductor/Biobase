# 
# 
# if (!isGeneric("varMetadata"))
#  setGeneric("varMetadata", function(object) standardGeneric("varMetadata"))
# 
# setMethod("varMetadata", "phenoData", function(object)
#    object@varMetadata)
# 
# if (!isGeneric("addVarMetadataEntry"))
#  setGeneric("addVarMetadataEntry", function(object,vname,attname, attval)
#    standardGeneric("addVarMetadataEntry"))
# 
# setMethod("addVarMetadataEntry", c("phenoData", "character", "character", "ANY"), 
# 	function(object, vname, attname, attval) {
#  if (attname == "varName") 
# 	stop("varName should not be used as a metadata attribute name")
#  vm <- varMetadata(object)
#  tmp <- list(attval)
#  names(tmp) <- attname
#  tmp <- data.frame(tmp)
#  tmp <- cbind(data.frame(varName=vname), tmp)
#  if (nrow(vm)==0) {
# 	object@varMetadata <- tmp
# 	return(object)
#         }
#  nm <- merge(vm, tmp, all=TRUE)
#  object@varMetadata <- nm
#  object
#  })
# 
# if (!isGeneric("getVarMetadata")) 
#  setGeneric("getVarMetadata", function(object, vname, attname)
#   standardGeneric("getVarMetadata"))
# 
# setMethod("getVarMetadata", c("phenoData", "character", "character"), 
# 	function(object, vname, attname) {
# 		vm <- varMetadata(object)
# 		row <- which(vm$varName == vname)
# 		if (length(row)==0) stop(paste(vname, "not present in varMetadata of object"))
# 		col <- which( names(vm) == attname )
# 		if (length(col)==0) stop(paste(attname, "not an attribute of varMetadata of object"))
# 		vm[row,col]
# 	})
# 
# setMethod("getVarMetadata", c("phenoData", "character", "missing"), 
# 	function(object, vname, attname) {
# 		vm <- varMetadata(object)
# 		row <- which(vm$varName == vname)
# 		if (length(row)==0) stop(paste(vname, "not present in varMetadata of object"))
# 		vm[row,]
# 	})
# 
# if (!isGeneric("getUnits")) 
#  setGeneric("getUnits", function(object, vname)
#   standardGeneric("getUnits"))
# 
# setMethod("getUnits", c("phenoData", "character"), function(object,vname) {
#   getVarMetadata(object, vname, "units") })
# 
# if (!isGeneric("convertVarLabels"))
# setGeneric("convertVarLabels", function(object)
# 	standardGeneric("convertVarLabels"))
# 
# setMethod("convertVarLabels", "phenoData", function(object) {
# 	if (length(object@varLabels) == 0) {
# 		warning("no varLabels to convert")
# 		return(object)
# 	}
# 	vn <- names(object@varLabels)
# 	vv <- as.character(unlist(object@varLabels))
# 	tmp <- data.frame(varName=vn, varLabels=vv)
# 	vm <- varMetadata(object)
# 	if (nrow(vm) == 0)
# 		object@varMetadata <- tmp
# 	else object@varMetadata <- merge(vm, tmp, all=TRUE)
# 	object
# 	})
# 
# x <- new("phenoData", pData=data.frame(age=3, height=88), varLabels=list(age="patient age", height="patient height"))
# print(x)
# x <- addVarMetadataEntry(x, "age", "units", "years")
# print(x)
# x <- addVarMetadataEntry(x, "height", "units", "cm")
# print(x)
# x <- addVarMetadataEntry(x, "height", "maxvalcm", 200)
# print(x)
# x <- addVarMetadataEntry(x, "drug", "name", "vioxx")
# print(x)
# 	
