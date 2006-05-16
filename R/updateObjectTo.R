setMethod("updateObjectTo", signature(object="ANY", template="ANY"),
          function(object, template, ..., verbose=FALSE) {
              if (verbose)
                  message("updateObject default object = '", class(object),
                          "' template = ", class(template), "'")
              if (class(object)==class(template))
                  object
              else {
                  tryCatch(as(object, class(template)),
                           error=function(err) {
                               stop("\ndefault updateObjectTo:\n    ",
                                    "cannot convert object of class '", class(object), "' ",
                                    "to template class '", class(template), "'")})
              }
          })
