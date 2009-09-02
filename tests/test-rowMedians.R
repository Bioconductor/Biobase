library(Biobase)
set.seed(1)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Consistency checks
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# rowMedians() by rowQ()
rowMedians2 <- function(imat) {
  nr <- ncol(imat)
  half <- (nr + 1)/2
  if (nr%%2 == 1) {
    return(rowQ(imat, half))
  } else {
    return((rowQ(imat, half) + rowQ(imat, half+1))/2)
  }
}

cat("Consistency checks:\n")
set.seed(1)
for (kk in 1:20) {
  cat("Random test #", kk, "\n", sep="")

  # Simulate data in a matrix of any shape
  nrow <- sample(2000, size=1)
  ncol <- sample(2000, size=1)
  x <- rnorm(nrow*ncol)
  dim(x) <- c(nrow, ncol)

  # Add NAs?
  nas <- sample(c(TRUE,FALSE), size=1)
  if (nas) {
    nna <- sample(nrow*ncol, size=1)
    x[sample(length(x), size=nna)] <- NA
  }

  na.rm <- nas
  t1 <- system.time({
    y1 <- rowMedians(x, na.rm=na.rm)
  })
  t2 <- system.time({
    y2 <- apply(x, MARGIN=1, FUN=median, na.rm=na.rm)
  })
  # When all values of 'y2' are NA, 'y2' is logical
  if (is.logical(y2)) y2 <- as.double(y2)
  stopifnot(all.equal(y1,y2))
  cat(sprintf("rowMedians()/apply(): %.3g\n", (t1/t2)[3]))

  if (!nas) {
    t3 <- system.time({
      y3 <- rowMedians2(x)
    })
    stopifnot(all.equal(y1,y3))
    cat(sprintf("rowMedians()/rowMedians2(): %.3g\n", (t1/t3)[3]))
  }
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Benchmarking
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cat("Benchmarking:\n")

# Simulate data in a matrix of any shape
nrow <- 1000
ncol <- 1000
x <- rnorm(nrow*ncol)
dim(x) <- c(nrow, ncol)

gc()
t0 <- system.time({
  for (rr in 1:20)
    y0 <- apply(x, MARGIN=1, FUN=median, na.rm=FALSE)
})
gc()
t1 <- system.time({
  for (rr in 1:20)
    y1 <- rowMedians(x, na.rm=FALSE)
})
gc()
stopifnot(all.equal(y0,y1))
cat(sprintf("rowMedians()/apply(): %.3g\n", (t1/t0)[3]))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Consistency checks
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cat("Consistency checks without NAs:\n")
for (kk in 1:20) {
  cat("Random test #", kk, "\n", sep="")

  # Simulate data in a matrix of any shape
  nrow <- sample(1000, size=1)
  ncol <- sample(1000, size=1)
  x <- rnorm(nrow*ncol)
  dim(x) <- c(nrow, ncol)

  t0 <- system.time({
    y0 <- apply(x, MARGIN=1, FUN=median, na.rm=FALSE)
  })
  t1 <- system.time({
    y1 <- rowMedians(x, na.rm=FALSE)
  })
  stopifnot(all.equal(y0,y1))
} # for (kk in ...)



cat("Consistency checks with NAs:\n")
for (kk in 1:20) {
  cat("Random test #", kk, "\n", sep="")

  # Simulate data in a matrix of any shape
  nrow <- sample(1000, size=1)
  ncol <- sample(1000, size=1)
  x <- rnorm(nrow*ncol)
  dim(x) <- c(nrow, ncol)

  # Add NAs
  nna <- sample(nrow*ncol-1, size=1)
  x[sample(length(x), size=nna)] <- NA

  t0 <- system.time({
    y0 <- apply(x, MARGIN=1, FUN=median, na.rm=TRUE)
    y0[is.na(y0)] <- NA
  })
  t1 <- system.time({
    y1 <- rowMedians(x, na.rm=TRUE)
  })
  stopifnot(all.equal(y0,y1))
} # for (kk in ...)
