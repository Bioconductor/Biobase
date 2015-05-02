## Unversioned
##

setMethod("initialize", signature(.Object="VersionsNull"),
          function(.Object, ...) {
              if (!missing(...))
                warning("ignoring arguments to '.VersionsNull()')")
              .Object
          })

setMethod("show", signature(object="VersionsNull"),
          function(object) print("No version"))
