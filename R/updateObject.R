setMethod("updateObject", signature(object="ANY"),
          function(object, ..., verbose=FALSE) {
              if (verbose)
                  message("updateObject default object = '", class(object), "'")
              object
          })

setMethod("updateObject", signature(object="list"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject object = 'list'")
              lapply(object, updateObject, ..., verbose=verbose)
          })

setMethod("updateObject", signature(object="environment"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject object = 'environment'")
              envLocked <- environmentIsLocked(object)
              if (envLocked) warning("updateObject duplicating locked environment")
              else warning("updateObject modifying environment")
              env <- 
                if (envLocked) new.env()
                else object
              lapply(ls(object, all=TRUE),
                     function(elt) {    # side-effect!
                         bindingLocked <- bindingIsLocked(elt, object)
                         if (!envLocked && bindingLocked)
                           stop("updateObject object = 'environment' ",
                                   "cannot modify locked binding '", elt, "'")
                         else {
                             env[[elt]] <<- updateObject(object[[elt]], ..., verbose=verbose)
                             if (bindingLocked) lockBinding(elt, env)
                         }
                         NULL
                     })
              if (envLocked)
                lockEnvironment(env)
              env
          })

updateObjectFromSlots <- function(object, class, ..., verbose=FALSE) {
    if (verbose)
        message("updateObjectFromSlots object = '", class(object),
                "' class = '", class, "'")
    ## get slots from object, and use as arguments to construct new instance
    objectSlots <- getObjectSlots(object)
    classSlots <- names(getSlots(class))
    joint <- intersect(names(objectSlots), classSlots)
    toDrop <- which(!names(objectSlots) %in% joint)
    if (length(toDrop))
        warning("dropping slot(s) ",
                paste(names(objectSlots)[toDrop],collapse=", "),
                " from object = '", class(object), "'")
    objectSlots <- lapply(objectSlots[joint], updateObject, ..., verbose=verbose)
    do.call("new", c(class, objectSlots))
}
