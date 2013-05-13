## ==========================================================================
## Versioned: mix-in class to add version information
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

## initialize -- see VersionedClasses

setMethod("initialize", signature(.Object="Versioned"),
          function(.Object, versions = list(), ...) {
              .Object <- callNextMethod(.Object, ...)
              classVersion(.Object)[names(versions)] <- versions
              .Object
          })

## validity

setValidity("Versioned", function(object) {
    msg <- NULL
    if (!isVersioned(object))
      msg <- validMsg(msg, "missing version string")
    if (is.null(msg)) TRUE else msg
})

isValidVersion <- function(object, nm) {# utility
    msg <- NULL
    if (!isS4(object))
      msg <- validMsg(msg, "not an S4 object")
    if (isVersioned(object) && !all(isCurrent(object)[nm])) {
        vers <- isCurrent(object)[nm]
        vers[is.na(vers)] <- FALSE
        names(vers) <- nm
        bad <- names(vers[vers==FALSE])
        msg <- validMsg(msg, paste0("out-of-date class version '", bad, "'"))
    }
    msg
}

##  isVersioned

setMethod("isVersioned", signature(object="ANY"),
          function(object) FALSE)

setMethod("isVersioned", signature(object="character"),
          function(object) {
              ## need to check getNamespace("Biobase") during Biobase installation
              length(object) > 0 && nchar(object)>0 &&
              (isClass(object) ||
               isClass(object, where=getNamespace("Biobase"))) &&
              extends(getClass(object), "Versioned")
      })

setMethod("isVersioned", signature(object="Versioned"),
          function(object) ".__classVersion__" %in% names(attributes(object)))

## classVersion

setMethod("classVersion", signature(object="ANY"),
          function(object) .VersionsNull())

setMethod("classVersion", signature(object="character"),
          function(object) {
              if (isVersioned(object))
                attr(getClass(object)@prototype, ".__classVersion__")
              else callNextMethod()
          })

setMethod("classVersion", signature(object="Versioned"),
          function(object) {
              if (isVersioned(object)) {
                  if (isS4(object@.__classVersion__))
                      object@.__classVersion__
                  else
                      ## force update to S4 instance
                      ## user needs to recognize need for update
                      updateObject(object@.__classVersion__)
              }
              else callNextMethod()
          })

setReplaceMethod("classVersion", signature(object="Versioned", value="Versions"),
                 function(object, value) {
                     object@.__classVersion__ <- value
                     object
                 })
## isCurrent

setMethod("isCurrent", signature(object="ANY"), function(object, value) NA)

setMethod("isCurrent", signature(object="Versioned", value="missing"),
          function(object, value) isCurrent(object, class(object)))

setMethod("isCurrent", signature(object="Versioned", value="character"),
          function(object, value) {
             # vers <- classVersion(value)
             # if(!all(names(vers) %in% names(classVersion(object)))) {
             #     res <- FALSE
             #     names(res) <- value 
             #     return(res)
             # }
              vers <- classVersion(value)
              if (extends(class(object),value) && 
                          isVersioned(object) &&
                          all(names(vers) %in% names(classVersion(object)))) {
                  res <- classVersion(object)[names(vers)] == vers
                  names(res) <- names(vers)
              } else {
                  res <- FALSE
                  names(res) <- value
              }
              res <- c(S4=isS4(object), res)
              res
          })

## show

setMethod("show", signature(object="Versioned"),
          function(object) callNextMethod())
