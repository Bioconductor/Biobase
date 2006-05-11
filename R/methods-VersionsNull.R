## Unversioned
##

setMethod("initialize", signature(.Object="VersionsNull"),
          function(.Object, ...) {
              if (length(list(...)))
                warning("ignoring arguments to new('VersionsNull')")
              .Object
          })

setMethod("show", signature(object="VersionsNull"),
          function(object) print("No version"))
