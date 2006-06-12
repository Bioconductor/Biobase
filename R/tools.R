# ==========================================================================
# Uncategorized tools for Biobase
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# testBioCConnection
# openPDF
# listLen; reverseSplit
# rowQ; rowMedians; rowMin; rowMax
# copySubstitute
# isUnique
# cache
# ==========================================================================
testBioCConnection <- function() {
   ## Stifle the "connected to www.... garbage output
   curNetOpt <- getOption("internet.info")
   on.exit(options(internet.info=curNetOpt), add=TRUE)
   options(internet.info=3)
   ## First check to make sure they have HTTP capability.  If they do
   ## not, there is no point to this exercise.
   http <- as.logical(capabilities(what="http/ftp"))
   if (http == FALSE)
      return(FALSE)
   ## find out where we think that bioC is
   bioCoption <- getOption("BIOC")
   if (is.null(bioCoption))
      bioCoption <- "http://www.bioconductor.org"
   ## Now check to see if we can connect to the BioC website
   biocURL <- url(paste(bioCoption,"/main.html",sep=""))
   options(show.error.messages=FALSE)
   test <- try(readLines(biocURL)[1])
   options(show.error.messages=TRUE)
   if (inherits(test,"try-error"))
      return(FALSE)
   else
      close(biocURL)
   return(TRUE)
}
# ==========================================================================
openPDF <- function(file, bg=TRUE) {
   OST <- .Platform$OS.type
   if (OST=="windows")
      shell.exec(file)
   else
      if (OST == "unix") {
         bioCOpt <- getOption("BioC")
         pdf <- getOption("pdfviewer")
         if (is.null(pdf)) {
            warning(paste("pdfViewer is set to:",pdf,
                          "which does not seem to exist.  Please",
                          "run the command setOptionPdfViewer()"))
            return(FALSE)
         }
         cmd <- paste(pdf,file)
         if( bg )
            cmd <- paste(cmd, "&")
         system(cmd)
      }
    return(TRUE)
}
# ==========================================================================
reverseSplit = function(inList) {
   lens = sapply(inList, length)
   nms = rep(names(inList), lens)
   vals = unlist(inList)
   split(nms, vals)
}
# ==========================================================================
copySubstitute = function(src, dest, symbolValues,
                          symbolDelimiter = "@",
                          allowUnresolvedSymbols = FALSE,
                          recursive = FALSE,
                          removeExtension = "\\.in$") {
   ## Check integrity of arguments (...lots of bureaucracy)
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
   ## Here the actual subsitution and copying work is done
   ## cin and cout are single files or connections
   cpSubsCon = function(cin, cout) {
      txt = readLines(cin)
      for (i in seq(along=symbolValues))
      txt = gsub(nm[i], symbolValues[[i]], txt)
      ## check for unresolved symbols
      if(!allowUnresolvedSymbols) {
         re = regexpr(paste(symbolDelimiter, ".+", symbolDelimiter, sep=""), txt)
         wh = which(re>0)
         if(length(wh)>0) {
            ml   = attr(re, "match.length")
            mess = "UNRESOLVED SYMBOLS:\n"
            mess = paste(mess, paste(sapply(wh, function(i)
            paste("Line", i, ":", substr(txt[i], re[i], re[i] + ml[i]))), collapse="\n"), sep="")
            stop(mess)
         }
      }
      ## finito
      writeLines(text=txt, con=cout)
   }
   ## Substitution on filenames
   subsFileName = function(x) {
      res = gsub(removeExtension, "", x)
      for (i in seq(along=symbolValues))
         res = gsub(nm[i], symbolValues[[i]], res)
      return(res)
   }
   ## Iterate over character vectors of filenames and
   ## recursively descend into directories
   cpSubs = function(src, dest) {
      usage = paste("\n* Usage:",
         "\n* with recursive=FALSE:",
         "\n* 'src' a connection open for reading or a file name AND",
         "\n* 'dest' a connection open for writing or a file name",
         "\n* with recursive=TRUE:",
         "\n* 'src' a vector of file and directory names and 'dest' a directory name\n\n")
      if (!recursive) {
         ## {file,connection} to {file,connection}
         if ((("connection" %in% class(src) && isOpen(src, rw="r")) ||
             (is.character(src) && length(src)==1)) &&
             (("connection" %in% class(dest) && isOpen(dest, rw="w")) ||
             (is.character(dest) && length(dest)==1)))
         {
            if(is.character(dest))
               dest = subsFileName(dest)
            cpSubsCon(src, dest)
            return(invisible(NULL))
         }
      }
      else {
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
               }
               else
                  cpSubsCon(src[k], destname)
            } ## for k
            return(invisible(NULL))
         } ## if(is.character...)
      } ## if(recursive)else
      stop(usage)
   } ## cpSubs
   ## DO IT!
   nm  = paste(symbolDelimiter, names(symbolValues), symbolDelimiter, sep="")
   cpSubs(src, dest)
}
# ==========================================================================
note <- function(...) {
    ## A "less drastic" version of warning()
    if (nargs() > 0) {
        message <- paste("Note:",...,"\n")
        cat(message)
    }
}

## ==================================================
isUnique = function(x){

  rv = rep(TRUE, length(x))

  if(length(x)>=2) {
    ord = order(x)
    ox = x[ord]
    ## compare consecutive values
    neq = (ox[-length(ox)]!=ox[-1])
    ## a value is unique if neither its predecessor nor successor
    ## in the ordered vector are the same
    rv[ord] = c(neq, TRUE) & c(TRUE, neq)
  }

  return(rv)
}
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## O.Sklyar, EBI, 2006
matchpt <- function(x, y = NULL) {
    if (is.vector(x))
        x <- matrix(as.double(x), ncol = 1, nrow = length(x))
    if (!is.matrix(x))
        stop("x must be a matrix in call to nn (nearest neighbour)")
    dims <- dim(x)
    if (length(dims) != 2)
        stop("wrong argument dimensions")
    if (!is.null(y)) {
        if (is.vector(y))
            y <- matrix(as.double(y), ncol = 1, nrow = length(y))
        if (!is.matrix(y))
            stop("y must be a matrix in call to nn (nearest neighbour)")
        if (length(dims) != length(dim(y)))
            stop("x and y must have the same dimensionality")
        if (dims[[2]] != dim(y)[[2]])
            stop("x and y must have the same dimensionality")
    }
    res <- .Call("matchpt", x, y, PACKAGE = "Biobase")
    colnames(res) <- c("index", "distance")
    return(res)
}
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## W.Huber, EBI, 2006
cache_old <- function(name, expr) {
    .Deprecated("cache, but with different args, see man page")
  if(!(is.character(name) && length(name)==1))
    stop("'name' must be a single character string.")
  cachefile <- paste("tmp-", name, ".RData", sep="")
  if(file.exists(cachefile)) {
    load(cachefile)
  } else {
    assign(name, expr)
    save(list=name, file=cachefile, compress=TRUE)
  }
  get(name)
}

cache <- function(expr, dir=".", prefix="tmp_R_cache_", name) {
    pexpr <- parse(text=deparse(substitute(expr)))
    pexpr <- as.list(pexpr[[1]])
    useOld <- (pexpr[[1]] != "<-" || !missing(name))
    if  (useOld) { ## compatibility layer
        if (missing(name))
          name <- expr
        if (pexpr[[1]] != "<-" && !missing(dir))
          expr <- dir
        cache__usage <- cache_old
        return(cache__usage(name, expr))
    }
    name <- as.character(pexpr[[2]])
    RHS <- pexpr[[3]]
    cachefile <- file.path(dir, paste(prefix, name, ".RData", sep=""))
    if(file.exists(cachefile)) {
        load(cachefile)
        assign(name, get(name), envir=.GlobalEnv)
    } else {
        dir.create(dir, recursive=TRUE, showWarnings=FALSE)
        assign(name, eval(RHS), envir=.GlobalEnv)
        save(list=name, file=cachefile)
    }
    invisible(get(name))
}
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
getObjectSlots <- function(object) { # object, rather than class defn, slots
    if(!is.object(object) || isVirtualClass(class(object)))
      return(NULL)
    value <- attributes(object)
    value$class <- NULL
    if(is(object, "vector"))
      value$.Data <- as.vector(object)
    value
}

validMsg <- function(msg, result) {
    if (is.character(result)) {
        append(msg, result)
    } else msg
}
