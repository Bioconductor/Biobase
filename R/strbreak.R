strbreak <- function(x, width=getOption("width"), exdent=5, breakchar="\n") {
  width <- as.integer(width)
  if(is.na(width) || width<=1)
    stop("invalid argument 'width'")
  exdent <- as.integer(exdent)
  if(is.na(exdent) || exdent>width)
    stop("invalid argument 'exdent'")
  ww <- width-exdent
  lb <- paste(breakchar, paste(rep(" ", exdent), collapse=""), sep="")
  rv <- character(length(x))
  
  for(i in seq(along=x)) {
    fl  <- c(1, width)
    if(nchar(x[i])>width) {
      f  <- seq(width+1, nchar(x[i]), ww)
      fl <- rbind(fl, cbind(f, f+ww-1))
    }
    rv[i] <-  paste(substring(x[i], first=fl[,1], last=fl[,2]), collapse=lb)
  }
  return(rv)
}
