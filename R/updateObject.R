setMethod("updateObject", signature(object="ANY"),
          function(object, ..., verbose=FALSE) {
              if (verbose)
                  message("updateObject(object = 'ANY') default for object of class '", class(object), "'")
              object
          })

setMethod("updateObject", signature(object="list"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'list')")
              lapply(object, updateObject, ..., verbose=verbose)
          })

setMethod("updateObject", signature(object="environment"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'environment')")
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
        message("updateObjectFromSlots(object = '", class(object),
                "' class = '", class, "')")
    ## get slots from object instance
    objectSlots <- getObjectSlots(object)
    ## de-mangle and remove NULL
    nulls <- sapply(names(objectSlots), function(slt) is.null(slot(object, slt)))
    objectSlots[nulls] <- NULL
    ## two sources of slots -- those from class(object), and those from the class
    ## we're responsible for updating i.e., class...
    classSlots <- names(getSlots(class))
    joint <- intersect(names(objectSlots), classSlots)
    objectSlots[joint] <- lapply(objectSlots[joint], updateObject, ..., verbose=verbose)
    ## We cannot create a new instance with slots that are not defined in object's class
    objectDefSlots <- names(getSlots(class(object)))
    toDrop <- which(!names(objectSlots) %in% objectDefSlots)
    if (length(toDrop)) {
        warning("dropping slot(s) ",
                paste(names(objectSlots)[toDrop],collapse=", "),
                " from object = '", class(object), "'")
        objectSlots <- objectSlots[-toDrop]
    }
    do.call("new", c(class(object), objectSlots))
}
