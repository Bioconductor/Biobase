copySubstitute = function(src, dest, symbolValues,
                          symbolDelimiter = "@",
                          allowUnresolvedSymbols = FALSE,
                          recursive = FALSE,
                          removeExtension = "\\.in$") {
  ## check integrity of arguments (...lots of bureaucracy)
  mess = NULL
  if (!is.list(symbolValues) && !is.vector(symbolValues))
    mess = "'symbolValues' must be a list or vector."
  if (!all(sapply(symbolValues, is.character)))
    mess = "'symbolValues' must only contain characters."
  if (is.null(names(symbolValues)) || any(names(symbolValues)==""))
    mess = "'symbolValues' must have non-empty names."
  if (!(is.character(symbolDelimiter) && length(symbolDelimiter)==1 && all(nchar(symbolDelimiter)==1)))
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

  ##------------------------------------------------------------
  ## substitution on filenames
  ##------------------------------------------------------------
  subsFileName = function(x) {
    res = gsub(removeExtension, "", x)
    for (i in seq(along=symbolValues))
      res = gsub(nm[i], symbolValues[[i]], res)
    return(res)
  }
  
  ##----------------------------------------------------------------------
  ## Local function: iterate over character vectors of filenames and
  ## recursively descend into directories
  ##----------------------------------------------------------------------
  cpSubs = function(src, dest) {
    usage = paste("\n* Usage:",
      "\n* with recursive=FALSE:",
      "\n* 'src' a connection open for reading and 'dest' a connection open for writing OR",
      "\n* 'src' a file name and 'dest' a file name OR",
      "\n* with recursive=TRUE:",
      "\n* 'src' a vector of file and directory names and 'dest' a directory name\n\n")
    if (!recursive) {
      ## {file,connection} to {file,connection}
      if ((("connection" %in% class(src) && isOpen(src, rw="r")) ||
           (is.character(src) && length(src)==1)) &&
          (("connection" %in% class(dest) && isOpen(dest, rw="w")) ||
           (is.character(dest) && length(dest)==1))) {
 
        if(is.character(dest))
          dest = subsFileName(dest)
        cpSubsCon(src, dest)
        return(invisible(NULL))
      } 
    } else {
      ## recursive: file(s) and/or directory(ies) to directory
      if (is.character(src) && is.character(dest) && length(dest==1)) {
        ## if 'dest' does not exist, create
        if (file.access(dest) != 0) {
          if (!dir.create(dest))
            stop(paste("'dest' does not exist, and it cannot be created:", dest))
        }
        ## process src
        isdir = file.info(src)$isdir
        for (k in seq(along=src)) {
          ## name of source file or directory (without path)
          tmp  = unlist(strsplit(src[k], .Platform$file.sep))
          tmp  = subsFileName(tmp[length(tmp)])
          ## name of destination file or directory (with path)
          destname = file.path(dest, tmp)
          if (isdir[k]) {
            if(!dir.create(destname))
              stop(paste("directory cannot be created:", destname))
            cpSubs(dir(src[k], full.names=TRUE), destname)
          } else {
            cpSubsCon(src[k], destname)
          }
        } ## for k
        return(invisible(NULL))
      }
    } ## if(recursive)else 
    stop(usage)
  } ## cpSubs

  ##------------------------------------------------------------
  ## Do it!
  ##------------------------------------------------------------
  nm  = paste(symbolDelimiter, names(symbolValues), symbolDelimiter, sep="")
  cpSubs(src, dest)
  
}

