copySubstitute = function(x, cin, cout, symbol.delimiter="@", allow.unresolved.symbols=FALSE) {
  ## check integrity of arguments (...lots of bureaucracy)
  mess = NULL
  if (!is.list(x) && !is.vector(x))
    mess = "Argument \"x\" of this function must be a list or vector."
  if (!all(sapply(x, is.character)))
    mess = "Argument \"x\" of this function must only contain characters."
  if (is.null(names(x)) || any(names(x)==""))
    mess = "Argument \"x\" of this function must have non-empty names."
  if (!(("connection" %in% class(cin)  && isOpen(cin, rw="r")) || is.character(cin)))
    mess = "Argument \"cin\" of this function must be a read connection or a file name."
  if (!(("connection" %in% class(cout) && isOpen(cout, rw="w"))|| is.character(cout)))
    mess = "Argument \"cout\" of this function must be a write connection or a file name."
  if (!is.logical(allow.unresolved.symbols))
    mess = "Argument \"allow.unresolved.symbols\" must be of type logical."
  if (!(is.character(symbol.delimiter) && nchar(symbol.delimiter)==1))
    mess = "Argument \"symbol.delimiter\" must be a single character."

  if(!is.null(mess))
    stop(mess)
  
  ## here the actual work is done
  txt = readLines(cin)
  nm  = paste(symbol.delimiter, names(x), symbol.delimiter, sep="")
  for (i in 1:length(x))
    txt = gsub(nm[i], x[[i]], txt)

  ## check for unresolved symbols
  if(!allow.unresolved.symbols){
    re = regexpr(paste(symbol.delimiter, ".+", symbol.delimiter, sep=""), txt)
    wh = which(re>0)
    if(length(wh)>0) {
      mess = "UNRESOLVED SYMBOLS:\n"
      mess = paste(mess, paste(sapply(wh, function(i)
        paste("Line", i, ":", substr(txt[i], re[i], re[i] + re@match.length[i]))), collapse="\n"),
             sep="")
      stop(mess)
    }
  }

  # finito
  writeLines(text=txt, con=cout)
}

