# ==========================================================================
# Functions to deal with strings:
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# strbreak
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
strbreak <- function(x, width=getOption("width"), exdent=2, collapse="\n") {
   width <- as.integer(width)
   if(is.na(width) || width<=1)
      stop("invalid argument 'width'")
   exdent <- as.integer(exdent)
   if(is.na(exdent) || exdent>width)
      stop("invalid argument 'exdent'")
   ww <- width-exdent
   lb <- paste0(collapse, paste(rep(" ", exdent), collapse=""))
   rv <- character(length(x))
   for(i in seq(along.with=x)) {
      first <- 1
      last  <- width
      if(nchar(x[i])>width) {
         f  <- seq(width+1, nchar(x[i]), ww)
         first <- c(first, f)
         last  <- c(last, f+ww-1)
      }
      rv[i] <-  paste(substring(x[i], first=first, last=last), collapse=lb)
   }
   return(rv)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

lcSuffix <- function(x, ignore.case = FALSE) {
    x <- as.character(x)
    if( ignore.case )   
       x <- toupper(x)

    nc <- nchar(x, type="char")
    for(i in 1:min(nc)) {
        ## The +1 and +2 are because substr is funny
       ss = substr(x, nc - i + 1, nc)
       if( any(ss != ss[1] )) {
           if (i == 1L)                 # trailing char mismatch
             return("")
           return(substr(x[1], nc - i + 2, nc))
       }
    }
    return(substr(x[1], nc - i + 1, nc))
 }


lcPrefix <- function(x, ignore.case=FALSE) {
    x <- as.character(x)
    if (ignore.case)
      x <- toupper(x)
    nc <- nchar(x, type="char")
    for (i in 1:min(nc)) {
        ss <- substr(x, 1, i)
        if (any(ss != ss[1])) {
            return(substr(x[1], 1, i-1))
        }
    }
    substr(x[1], 1, i)
}


lcPrefixC <- function(x, ignore.case=FALSE) {
    .Call("lc_prefix", x, ignore.case)
}
