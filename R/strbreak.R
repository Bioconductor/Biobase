strbreak <- function(x, width=getOption("width"), exdent=2, collapse="\n") {
  width <- as.integer(width)
  if(is.na(width) || width<=1)
    stop("invalid argument 'width'")
  exdent <- as.integer(exdent)
  if(is.na(exdent) || exdent>width)
    stop("invalid argument 'exdent'")
  ww <- width-exdent
  lb <- paste(collapse, paste(rep(" ", exdent), collapse=""), sep="")
  rv <- character(length(x))
  
  for(i in seq(along=x)) {
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
