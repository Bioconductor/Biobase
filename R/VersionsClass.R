## ==========================================================================
## Versions: version string information
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## methods and class defined together so avaiable for prototypes elsewhere

setClass("Versions", contains="list")

setMethod("initialize", signature(.Object="Versions"),
          function(.Object, ...) callNextMethod(.Object, .asValidVersions(list(...))))

.isValidVersion <- function(versions) {
    tryCatch(all(as.integer(versions)==versions) &&
             all(versions >= 0) &&
             length(versions) > 1,
             error=function(err) FALSE,
             warning=function(warn) {})
}

.asValidVersions <- function(versions) {
    res <- list()
    for (i in seq(along=versions))
      res[i] <-
        if (!is.character(versions[[i]]) && .isValidVersion(versions[[i]])) versions[i]
        else unclass(package_version(versions[[i]]))
    names(res) <- names(versions)
    res
}

## update

setMethod("updateObject", signature(object="Versions"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'Versions')")
              if (!isS4(object)) do.call("new", c("Versions", object))
              else object
      })

## access

setMethod("[", signature(x="Versions"),
          function(x, i, j, ..., drop = FALSE) as(callNextMethod(),"Versions"))

## assign

setReplaceMethod("[", signature(x="Versions"),
                 function(x, i, j, value) {
                     lst <- x@.Data
                     names(lst) <- names(x)
                     lst[i] <- .asValidVersions(value)
                     x@.Data <- lst
                     names(x) <- names(lst)
                     x
                 })

setReplaceMethod("[[", signature(x="Versions"),
                 function(x, i, j, value) {
                     lst <- x@.Data
                     names(lst) <- names(x)
                     lst[[i]] <- unlist(.asValidVersions(value))
                     x@.Data <- lst
                     names(x) <- names(lst)
                     x
                 })

setReplaceMethod("$", signature(x="Versions"),
                 function(x, name, value) { x[[name]] <- value; x })

## Compare

setMethod("Compare", signature(e1="Versions", e2="Versions"),
          function(e1, e2) {
              class(e1) <- "package_version"
              class(e2) <- "package_version"
              callNextMethod(e1, e2)
          })

setMethod("Compare", signature(e1="Versions", e2="character"),
          function(e1, e2) {
              class(e1) <- "package_version"
              e2 <- .asValidVersions(e2)
              class(e2) <- "package_version"
              callNextMethod(e1, e2)
          })

setMethod("Compare", signature(e1="character", e2="Versions"),
          function(e1, e2) callGeneric(e2,e1))

## show

setAs("Versions", "character",
      function(from) {
          if (length(from)) sapply(from, paste, collapse=".")
          else "Versioned; no version string"
      })

setMethod("show", signature(object="Versions"),
          function(object) print(as(object, "character")))
