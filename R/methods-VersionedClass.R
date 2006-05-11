## ==========================================================================
## Versioned: mix-in class to add version information
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

## initialize -- see VersionedClasses

setMethod("initialize", signature(.Object="Versioned"),
          function(.Object, ..., versions = list()) {
              if (length(versions) > 0)
                classVersion(.Object)[names(versions)] <- versions
              .Object
          })

##  isVersioned

setMethod("isVersioned", signature(object="ANY"),
          function(object) FALSE)

setMethod("isVersioned", signature(object="character"),
          function(object) extends(getClass(object), "Versioned"))

setMethod("isVersioned", signature(object="Versioned"),
          function(object) ".__classVersion__" %in% names(attributes(object)))

## classVersion

setMethod("classVersion", signature(object="ANY"),
          function(object) new("VersionsNull"))

setMethod("classVersion", signature(object="character"),
          function(object) {
              if (isVersioned(object))
                attr(getClass(object)@prototype, ".__classVersion__")
              else callNextMethod()
          })

setMethod("classVersion", signature(object="Versioned"),
          function(object) {
              if (isVersioned(object)) object@.__classVersion__
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
              if (extends(class(object),value) && isVersioned(object)) {
                  vers <- classVersion(value)
                  res <- classVersion(object)[names(vers)] == vers
                  if (length(res)==0) TRUE
                  else res
              } else FALSE
          })

## show

setMethod("show", signature(object="Versioned"),
          function(object) invisible())
