mkScalar <- function(obj) {
    switch(typeof(obj),
           character=new("ScalarCharacter", obj),
           logical=new("ScalarLogical", obj),
           integer=new("ScalarInteger", obj),
           double=new("ScalarNumeric", obj),
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
