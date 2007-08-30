library("Biobase")

test_basic_l2e <- function() {
    L <- as.list(1:3)
    names(L) <- letters[1:3]
    e <- l2e(L)
    checkEquals(3, length(e))
    checkEquals(names(L), ls(e))
    checkEquals(L, as.list(e))
}

test_no_emptystring_keys <- function() {
    L <- as.list(1:3)
    names(L) <- letters[1:3]
    names(L)[2] <- ""
    checkException(l2e(L))
}

test_no_NA_keys <- function() {
    L <- as.list(1:3)
    names(L) <- letters[1:3]
    names(L)[2] <- NA
    checkException(l2e(L))
}

test_non_unique_names <- function() {
    L <- as.list(1:3)
    names(L) <- c("a", "b", "b")
    checkException(tryCatch(l2e(L),
                   warning=function(w) stop("found my error")),
                   silent=TRUE)
}
