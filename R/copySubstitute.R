copySubstitute = function(src, dest, symbolValues,
                          symbolDelimiter = "@",
                          allowUnresolvedSymbols = FALSE,
                          removeExtension = "\\.in$") {
  ## check integrity of arguments (...lots of bureaucracy)
  mess = NULL
  if (!is.list(symbolValues) && !is.vector(symbolValues))
    mess = "'symbolValues' must be a list or vector."
  if (!all(sapply(symbolValues, is.character)))
    mess = "'symbolValues' must only contain characters."
  if (is.null(names(symbolValues)) || any(names(symbolValues)==""))
    mess = "'symbolValues' must have non-empty names."
  if (!(is.character(symbolDelimiter) && nchar(symbolDelimiter)==1))
    mess = "'symbolDelimiter' must be a single character."
  if (!is.logical(allowUnresolvedSymbols))
    mess = "'allowUnresolvedSymbols' must be of type logical."
  if(!is.null(mess))
    stop(mess)

  ##----------------------------------------------------------------------
  ## Local function: here the actual subsitution and copying work is done
  ## cin and cout are single files or connections
  ##----------------------------------------------------------------------
  cpSubsCon = function(cin, cout) {
    txt = readLines(cin)
    for (i in seq(along=symbolValues))
      txt = gsub(nm[i], symbolValues[[i]], txt)

    ## check for unresolved symbols
    if(!allowUnresolvedSymbols){
      re = regexpr(paste(symbolDelimiter, ".+", symbolDelimiter, sep=""), txt)
      wh = which(re>0)
      if(length(wh)>0) {
        ml   = attr(re, "match.length")
        mess = "UNRESOLVED SYMBOLS:\n"
        mess = paste(mess, paste(sapply(wh, function(i)
          paste("Line", i, ":", substr(txt[i], re[i], re[i] + ml[i]))), collapse="\n"),
          sep="")
        stop(mess)
      }
    }
    ## finito
    writeLines(text=txt, con=cout)
  }

  ##----------------------------------------------------------------------
  ## Local function: iterate over character vectors of filenames and
  ## recursively descend into directories
  ##----------------------------------------------------------------------
  cpSubs = function(src, dest) {
    if (is.character(src)) {
      if (!is.character(dest) || length(dest) != 1)
        stop("'dest' must be a character vector of length 1.'")
      if (file.access(dest) != 0)
        stop(paste("'dest' does not exist:", dest))
      if (file.info(dest)$isdir != TRUE)
        stop(paste("'dest' must be a directory:", dest))

      ## directories: recursively descend
      isdir = file.info(src)$isdir
      for (k in seq(along=src)) {
        ## symbol substitution also in directory names and filenames
        tmp      = unlist(strsplit(src[k], .Platform$file.sep))
        destname = gsub(removeExtension, "", tmp[length(tmp)])
        
        for (i in seq(along=symbolValues))
          destname = gsub(nm[i], symbolValues[[i]], destname)
        if(isdir[k]) {
          newdir = file.path(dest, destname)
          dir.create(newdir)
          cpSubs(dir(src[k], full.names=TRUE), newdir)
        } else {
          cpSubsCon(src[k], file.path(dest, destname))
        }
      } ## for k
    } else {
      if (!("connection" %in% class(src))  || !isOpen(src, rw="r"))
        stop("'src' must be a connection open for reading.")
      if (!("connection" %in% class(dest)) || !isOpen(dest, rw="w"))
        stop("'dest' must be a connection open for writing.")
      cpSubsCon(src, dest)
    }
  }

  ##------------------------------------------------------------
  ## Do it!
  ##------------------------------------------------------------
  nm  = paste(symbolDelimiter, names(symbolValues), symbolDelimiter, sep="")
  cpSubs(src, dest)
}

