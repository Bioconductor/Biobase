## ==========================================================================
## Versions: version string information
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## methods and class defined together so available for prototypes elsewhere

.Versions <- setClass("Versions", contains="list")

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
    for (i in seq(along.with=versions))
      res[i] <-
        if (!is.character(versions[[i]]) &&
            .isValidVersion(versions[[i]]))
            versions[i]
        else
            unclass(numeric_version(versions[[i]]))
    names(res) <- names(versions)
    res
}

## update

setMethod("updateObject", signature(object="Versions"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'Versions')")
              if (!isS4(object)) do.call(new, c("Versions", object))
              else object
      })

## access

setMethod("[",
          signature=signature(x="Versions"),
          function(x, i, j, ..., drop = FALSE) {
              ## 'dispatch' on i to avoid S4 complaint about j 'missing'
              if (is(i, "character") &&
                  !all(i %in% names(x))) {
                  bad <- unique(i[!i %in% names(x)])
                  cl <- deparse(match.call()[[2]])
                  stop(sprintf("'[' indices '%s' not found in '%s'",
                               paste(bad, collapse="', '"),
                               cl))
              }
              as(callNextMethod(), "Versions")
          })

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

.as.numeric_version <- function(x)
    numeric_version(as(x, "character"))
    
.canVersionCompare <- function(e1, e2) {
    if (length(e1) != length(e2))
        stop(sprintf("cannot compare versions with length '%d', '%d'",
                     length(e1), length(e2)))
    if (length(e1)>1 &&
        (!all(names(e1) %in% names(e2)) ||
         !all(names(e2) %in% names(e1))))
        stop("cannot compare versions with different names")
    TRUE
}

setMethod("Compare",
          signature=signature(
            e1="Versions",
            e2="Versions"),
          function(e1, e2) {
              .canVersionCompare(e1, e2)
              e2 <- e2[names(e1)]
              e1 <- .as.numeric_version(e1)
              e2 <- .as.numeric_version(e2)
              callNextMethod(e1, e2)
          })

setMethod("Compare",
          signature=signature(
            e1="Versions",
            e2="character"),
          function(e1, e2) {
              .canVersionCompare(e1, e2)
              if (length(e2) > 1)
                  e2 <- e2[names(e1)]
              e1 <- .as.numeric_version(e1)
              e2 <- numeric_version(e2)
              callNextMethod(e1, e2)
          })

setMethod("Compare",
          signature=signature(
            e1="character",
            e2="Versions"),
          function(e1, e2) callGeneric(e2,e1))

## show

setAs("Versions", "character",
      function(from) {
          if (length(from)) sapply(from, paste, collapse=".")
          else "Versioned; no version string"
      })

setMethod("show", signature(object="Versions"),
          function(object) print(as(object, "character")))
