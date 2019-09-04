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
# userQuery
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
   biocURL <- url(paste0(bioCoption,"/main.html"))
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
         msg <- NULL
         if (is.null(pdf))
             msg <- "getOption('pdfviewer') is NULL"
         else if (length(pdf)==1 && nchar(pdf[[1]])==0)
             msg <- "getOption('pdfviewer') is ''"
         if (!is.null(msg))
             stop(msg, "; please use 'options(pdfviewer=...)'")
         cmd <- paste(pdf,file)
         if( bg )
            cmd <- paste(cmd, "&")
         system(cmd)
      }
    return(TRUE)
}
# ==========================================================================
reverseSplit = function(inList) {
   if (length(inList)==0) {
       return(inList)
   }
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
      for (i in seq(along.with=symbolValues)) {
          txt = gsub(nm[i], symbolValues[[i]], txt, fixed=TRUE)
          if (any(is.na(txt)))
              stop("trying to replace ", nm[i], " by an NA")
      }
      ## check for unresolved symbols
      if(!allowUnresolvedSymbols) {
         re = regexpr(paste0(symbolDelimiter, ".+", symbolDelimiter), txt)
         wh = which(re>0)
         if(length(wh)>0) {
            ml   = attr(re, "match.length")
            mess = "UNRESOLVED SYMBOLS:\n"
            mess <- paste0(mess, paste(sapply(wh, function(i) {
                paste("Line", i, ":", substr(txt[i], re[i], re[i] + ml[i]))
            }), collapse="\n"))
            stop(mess)
         }
      }
      ## finito
      writeLines(text=txt, con=cout)
   }
   ## Substitution on filenames
   subsFileName = function(x) {
      res = gsub(removeExtension, "", x)
      for (i in seq(along.with=symbolValues)) {
         res = gsub(nm[i], symbolValues[[i]], res)
         if (any(is.na(res)))
             stop("trying to replace ", nm[i], " by an NA")
      }
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
                  stop("'dest' does not exist, and cannot be created: ", dest)
            }
            ## process src
            isdir = file.info(src)$isdir
            for (k in seq(along.with=src)) {
               ## name of source file or directory (without path)
               tmp  = unlist(strsplit(src[k], .Platform$file.sep))
               tmp  = subsFileName(tmp[length(tmp)])
               ## name of destination file or directory (with path)
               destname = file.path(dest, tmp)
               if (isdir[k]) {
                  if(!dir.create(destname))
                     stop("directory cannot be created: ", destname)
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
   nm  = paste0(symbolDelimiter, names(symbolValues), symbolDelimiter)
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
matchpt <- function(x, y) {
  storage.mode(x)="double"
  if (is.vector(x))
    x <- matrix(x, ncol = 1L, nrow = length(x))
  if (!(is.matrix(x) && is.numeric(x)))
    stop("'x' must be a numeric matrix.")

  if (!missing(y)) {
    storage.mode(y)="double"
    if (is.vector(y))
      y <- matrix(y, ncol = 1L, nrow = length(y))
    if (!(is.matrix(y) && is.numeric(y)))
      stop("y must be a numeric matrix.")
    if (ncol(x) != ncol(y))
      stop("x and y must have the same number of columns.")
  } else {
    y <- NULL
  }
  
  res <- .Call("matchpt", x, y, PACKAGE = "Biobase")
  res <- as.data.frame(res)
  rownames(res) <- rownames(x)
  return(res)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cache <- function(expr, dir=".", prefix="tmp_R_cache_") {
    pexpr <- parse(text=deparse(substitute(expr)))
    pexpr <- as.list(pexpr[[1]])
    name <- as.character(pexpr[[2]])
    RHS <- pexpr[[3]]
    cachefile <- file.path(dir, paste0(prefix, name, ".RData"))
    if(file.exists(cachefile)) {
        load(cachefile)
        assign(name, get(name), envir=parent.frame())
    } else {
        dir.create(dir, recursive=TRUE, showWarnings=FALSE)
        assign(name, eval(RHS, envir=parent.frame()), envir=parent.frame())
        save(list=name, file=cachefile, envir=parent.frame())
    }
    invisible(get(name, envir=parent.frame()))
}
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
validMsg <- function(msg, result) {
    if (is.character(result)) {
        append(msg, result)
    } else msg
}

checkClass <- function(object, expected, prefix="", call.=FALSE, ...) {
    if (!is(object, expected))
      stop(prefix, " '", deparse(substitute(object)), "' is class '",
           paste(class(object), collapse="', '"),
           "' but should be or extend '",
           paste(expected, collapse="', '"), "'", call.=call., ...)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

userQuery <- function(msg, allowed=c("y","n"), default = "n",
                      case.sensitive = FALSE) {
    ## Prompts the user with a string and for an answer
    ## repeats until it gets allowable input
  if(interactive()){
    repeat {
      allowMsg <- paste0("[", paste(allowed,collapse="/"), "] ")
      outMsg <- paste(msg, allowMsg)
      cat(outMsg)
      if(case.sensitive)
        ans <- readLines(n=1)
      else
        ans <- tolower(readLines(n=1))
      if (ans %in% allowed)
        break
      else
        cat(ans, "is not a valid response, try again.\n")
    }
    return(ans)
  }else{
    return(default)
  }
}

unsafeSetSlot <- function(obj, slot, value) {
    
    ## This function _assumes_ that there is exactly on references to
    ## 'obj'; the number of references is not usually detectable from
    ## casual perusal of the code, because R only maintains the
    ## _illusion_ of pass by value. Use this with the greatest care,
    ## if at all.

    invisible(.Call("unsafe_set_slot", obj, slot, value))
}

subListExtract <- function(L, name, simplify=FALSE, keep.names=TRUE) {
    ## Return a list or vector obtained by extracting the element named
    ## 'name' from each inner list of L.
    ##
    ## L - list
    ## name - character vector length 1, name of inner list element
    ## simplify - return an atomic vector or error if TRUE
    ## keep.names - if TRUE, keep names of L on result
    ##
    ## this is just a mockup, to play with the desired interface
    ## and behavior.  will be replaced with a C version.
    .Call(sublist_extract, L, name, simplify, keep.names)
    
##     f <- function(x) {
##         wh <- match(name, names(x), 0)
##         if (wh > 0)
##           x[[wh]]
##         else                            # would be nice to have index in msg
##           stop("bad inner list, no element named ", name)
##     }

##     if (!simplify)
##       ans <- lapply(L, f)
##     else {
##         ans <- sapply(L, f)
##         if (is.list(ans))
##           stop("unable to simplify")
##     }
##     if (!keep.names)
##       names(ans) <- NULL
##     ans
}
