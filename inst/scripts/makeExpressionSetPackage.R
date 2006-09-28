

 makeExpressionSetPackage = function(expS, author, filePath=tempdir(),
    version = "1.0.0", license, email, biocViews="ExperimentData"  )
 {
   if( !inherits(expS, "ExpressionSet") )
     stop("only works for ExpressionSets")
 
   if( missing(email) || !(is.character(email) && (length(email) == 1)
           && grep("@", email) == 1 ) )
     stop("invalid email address")

   if( !is.package_version(version) )
     version = package_version(version)

   if(missing(license) ) 
      license= "The Artistic License, Version 2.0"

   pkgname = deparse(substitute(expS))

   sym = list(AUTHOR = author, VERSION=as.character(version), LICENSE=license,
        TITLE = paste("Experimental Data Package:",pkgname), 
        MAINTAINER = paste(author, ", <", email, ">", sep = ""),
        BVIEWS = biocViews)

   
   createPackage(pkgname, destinationDir=filePath,
         originDir = file.path(.path.package("Biobase"), "ExpressionSet"),
         symbolValues = sym, unlink=TRUE)
 }


library(ALL)
data(ALL)
makeExpressionSetPackage(ALL, author="Robert Gentleman", email="rgentlem@foo")

