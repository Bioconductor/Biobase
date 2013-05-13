mkScalar <- function(obj) {
    switch(typeof(obj),
           character=.ScalarCharacter(obj),
           logical=.ScalarLogical(obj),
           integer=.ScalarInteger(obj),
           double=.ScalarNumeric(obj),
           stop("no scalar class implemented for type: ", typeof(obj)))
}

setMethod("show", "ScalarObject",
          function(object) {
              cat(object, "\n")
          })

setMethod("show", "ScalarCharacter",
          function(object) {
              if (is.na(object))
                  cat(NA, "\n")
              else
                  cat(sprintf('"%s"\n', object))
          })
