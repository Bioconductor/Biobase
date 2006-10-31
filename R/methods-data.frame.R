setMethod("combine",
          signature(x="data.frame", y="data.frame"),
          function(x, y) {
              if (all(dim(x)==0) && all(dim(y)==0))
                return(x)
              else if (all(dim(x)==0)) return(y)
              else if (all(dim(y)==0)) return(x)

              uniqueRows <- unique(c(row.names(x), row.names(y)))
              uniqueCols <- unique(c(names(x), names(y)))
              sharedCols <- intersect(names(x), names(y))

              ## check possible to combine
              sharedRows <- intersect(row.names(x), row.names(y))
              ok <- sapply(sharedCols, function(nm) {
                  if (class(x[[nm]]) != class(y[[nm]])) return(FALSE)
                  switch(class(x[[nm]])[[1]],
                         factor= {
                             if (identical(levels(x[[nm]]), levels(y[[nm]])) &&
                                 identical(x[sharedRows, nm, drop=FALSE],
                                           y[sharedRows, nm, drop=FALSE])) TRUE
                             else FALSE
                         },
                         ordered=,
                         identical(x[sharedRows, nm, drop=FALSE],
                                   y[sharedRows, nm, drop=FALSE]))
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
                    switch(class(z[[nmx]]),
                           AsIs= I(ifelse(is.na(z[[nmx]]), z[[nmy]], z[[nmx]])),
                           factor= {
                               col <- ifelse(is.na(z[[nmx]]),
                                             as.character(z[[nmy]]), as.character(z[[nmx]]))
                               if (!identical(levels(z[[nmx]]), levels(z[[nmy]])))
                                 factor(col)
                               else factor(col, levels=levels(z[[nmx]]))
                           },
                           ifelse(is.na(z[[nmx]]), z[[nmy]], z[[nmx]]))
              }
              
              ## tidy
              row.names(z) <- z$Row.names
              z$Row.names <- NULL
              z[uniqueRows, uniqueCols, drop=FALSE]
          })
