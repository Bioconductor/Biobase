
setClassUnion("listOrEnv", c("list", "environment"))

# following gets eSet2 to have a phenoData slot
setClass("eSet2", representation(exprs="listOrEnv"), contains="annotatedDataset")

#
# so now Z <- new("eSet2", X, phenoData=Y) will work, as long as X
# is either a list or an environment, but Z$ and Z[[]] 
# deal exclusively with phenoData.  this seems to be as desired 

setGeneric("exprs", function(object)standardGeneric("exprs"))
setMethod("exprs", "eSet2", function(object) object@exprs)
setMethod("exprs", "exprSet", function(object) object@exprs)

setMethod("dim", "eSet2", function(x) {
    isEnv <- is(exprs(x), "environment")
    if (!isEnv) sapply(exprs(x), dim)
    else {
	enms <- ls(exprs(x))
	sapply(enms,function(z) dim(get(z,exprs(x))))
	}
})

setMethod("[", "eSet2", function(x, i, j, ..., drop=FALSE) {
    if( length(list(...)) > 0 )
        stop("extra subscripts cannot be handled")
    if( missing(j) )
        pdata <- phenoData(x)
    else
        pdata <- phenoData(x)[j,, ..., drop=FALSE]
    isEnv <- is(exprs(x), "environment")
    if (!isEnv) {
	if (missing(i))
		if (missing(j))
			return(x)
		else xd <- lapply( exprs(x), function(x)x[,j,drop=drop] )
	else if (missing(j))
		xd <- lapply( exprs(x), function(x)x[i,,drop=drop] )
	     else
		xd <- lapply( exprs(x), function(x)x[i,j,drop=drop] )
	}
    else { # getting an environment, which is assumed to hold matrices (22 aug 2005)
	enms <- ls(exprs(x))
	xd <- new.env()
        for (nm in enms) {
	   if (missing(i))
		{
		if (missing(j))
			return(x)
		else assign(nm, get(nm, env=exprs(x), inherits=FALSE)[,j,drop=drop], env=xd)
		}
	   else if (missing(j))
		assign(nm, get(nm, env=exprs(x), inherits=FALSE)[i,,drop=drop], env=xd)
	   else
		assign(nm, get(nm, env=exprs(x), inherits=FALSE)[i,j,drop=drop], env=xd)
	   }
        }
    new("eSet2", phenoData=pdata, exprs=xd)
})

#data(eset)
#pd <- phenoData(eset)
#xl <- list(e1=exprs(eset))
#es2 <- new("eSet2", phenoData=pd, exprs=xl)

setMethod("show", "eSet2", function(object) {
 cat("instance of eSet2\n")
 cat("exprs component is of class", class(exprs(object)),"\n")
 cat("dimensions of the exprs components:\n")
 print(dim(object))
 show(phenoData(object))
})
