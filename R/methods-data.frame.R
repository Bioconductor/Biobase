setMethod("combine",
          signature(x="data.frame", y="data.frame"),
          function(x, y, ...) {
              if (all(dim(x)==0) && all(dim(y)==0))
                return(x)
              else if (all(dim(x)==0)) return(y)
              else if (all(dim(y)==0)) return(x)

              uniqueRows <- unique(c(row.names(x), row.names(y)))
              uniqueCols <- unique(c(names(x), names(y)))
              sharedCols <- intersect(names(x), names(y))

              ## check possible to combine
              alleq <- function(x, y) {
                  res <- all.equal(x, y, check.attributes=FALSE)
                  if (!is.logical(res))  {
                      warning(res)
                      FALSE
                  } else TRUE
              }
              sharedRows <- intersect(row.names(x), row.names(y))
              ok <- sapply(sharedCols, function(nm) {
                  if (!all(class(x[[nm]]) == class(y[[nm]]))) return(FALSE)
                  switch(class(x[[nm]])[[1]],
                         factor= {
                             if (!alleq(levels(x[[nm]]), levels(y[[nm]]))) {
                                 warning("data frame column '", nm,
                                         "' levels not all.equal",
                                         call.=FALSE)
                                 TRUE
                             } else if (!alleq(x[sharedRows, nm, drop=FALSE],
                                               y[sharedRows, nm, drop=FALSE])) {
                                 warning("data frame column '", nm,
                                         "' shared rows not all equal",
                                         call.=FALSE)
                                 FALSE
                             } else TRUE
                         },
                         ## ordered and non-factor columns need to
                         ## satisfy the following identity; it seems
                         ## possible that ordered could be treated
                         ## differently, but these have not been
                         ## encountered.
                         ordered=,
                         if (!alleq(x[sharedRows, nm, drop=FALSE],
                                    y[sharedRows, nm, drop=FALSE])) {
                             warning("data frame column '", nm,
                                     "' shared rows not all equal")
                             FALSE
                         } else TRUE)
              })
              if (!all(ok))
                stop("data.frames contain conflicting data:",
                     "\n\tnon-conforming colname(s): ", paste(sharedCols[!ok], collapse=", "))

              ## x or y with zero rows -- make palatable to merge, but drop before return
              if (length(uniqueRows)==0) {
                  x <- x["tmp",,drop=FALSE]
                  y <- y["tmp",,drop=FALSE]
              } else if (nrow(x)==0) {
                  x <- x[row.names(y),,drop=FALSE]
                  row.names(x) <- row.names(y)
              } else if (nrow(y)==0) {
                  y <- y[row.names(x),,drop=FALSE]
                  row.names(y) <- row.names(x)
              }

              ## make colnames of merged data robust
              if (length(uniqueCols)>0)
                extLength <- max(nchar(sub(".*\\.", "", uniqueCols)))+1
              else extLength <- 1
              extX <- paste(c(".", rep("x", extLength)), collapse="")
              extY <- paste(c(".", rep("y", extLength)), collapse="")
              z <- merge(x, y, by="row.names", all=TRUE, suffixes=c(extX,extY))

              ## shared cols
              for (nm in sharedCols) {
                  nmx <- paste(nm, extX, sep="")
                  nmy <- paste(nm, extY, sep="")
                  z[[nm]] <-
                    switch(class(z[[nmx]])[[1]],
                           AsIs= I(ifelse(is.na(z[[nmx]]), z[[nmy]], z[[nmx]])),
                           factor= {
                               col <- ifelse(is.na(z[[nmx]]),
                                             as.character(z[[nmy]]), as.character(z[[nmx]]))
                               if (!identical(levels(z[[nmx]]), levels(z[[nmy]])))
                                 factor(col)
                               else factor(col, levels=levels(z[[nmx]]))
                           },
                           {
                               col <- ifelse(is.na(z[[nmx]]), z[[nmy]], z[[nmx]])
                               class(col) <- class(z[[nmx]])
                               col
                           })
              }

              ## tidy
              row.names(z) <-
                  if (is.integer(attr(x, "row.names")) &&
                      is.integer(attr(y, "row.names")))
                      as.integer(z$Row.names)
              else z$Row.names
              z$Row.names <- NULL
              z[uniqueRows, uniqueCols, drop=FALSE]
          })
