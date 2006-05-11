testNewVersioned <- function() {
  new("Versioned")
  new("VersionedBiobase")
  ## Biobase:::.unversionedObj

  v <- new("Versioned", versions=list(x="1.0.0"))
  checkTrue(classVersion(v)["x"]=="1.0.0")

  ## use class definition defined in RUnit.R
  a <- new("A")
  checkTrue(all(classVersion(a) == classVersion(new("VersionedBiobase", versions=list(A="1.0.1")))))
  checkTrue(all(a@x==10:1))
  a <- new("A", x=1:10)
  checkTrue(all(classVersion(a) == classVersion(new("VersionedBiobase", versions=list(A="1.0.1")))))
  checkTrue(all(a@x==1:10))
  a <- new("A", x=1:10, versions=list(x="1.0.1"))
  checkTrue(all(a@x==1:10))
  checkTrue(all(classVersion(a) == classVersion(new("VersionedBiobase", versions=list(A="1.0.1",x="1.0.1")))))
}

testIsVersioned <- function() {
  checkTrue(isVersioned(new("VersionedBiobase")))
  checkTrue(isVersioned(new("A")))
  checkTrue(isVersioned("VersionedBiobase"))
  checkTrue(isVersioned("A"))
  checkTrue(!isVersioned(1:10))
}

testClassVersion <- function() {
  classVersion(new("Versioned"))
  classVersion(new("VersionedBiobase"))
  checkTrue(is(classVersion(new("VersionedBiobase")),"Versions"))

  checkTrue(all(classVersion("VersionedBiobase") == classVersion(new("VersionedBiobase"))))
}

testClassVersionReplace <- function() {
  ref <- obj <- new("VersionedBiobase")
  classVersion(obj)["x"] <- "1.0.0"
  checkTrue(all(classVersion(obj)[names(classVersion(ref))] == classVersion(ref)))
  checkTrue(classVersion(obj)["x"] == "1.0.0")
                              
  y <- new("Versions", y="1.0.1")
  classVersion(obj)[names(y)] <- y
  checkTrue(all(classVersion(obj)[names(classVersion(ref))] == classVersion(ref)))
  checkTrue(classVersion(obj)["x"] == "1.0.0")
  checkTrue(classVersion(obj)["y"] == "1.0.1")
  checkTrue(classVersion(obj)["y"] != "1.0.0")

  obj <- ref
  classVersion(obj) <- y
  checkTrue(all(classVersion(obj)[names(classVersion(ref))] != classVersion(ref)))
  checkTrue(classVersion(obj)["y"] == "1.0.1")
}

testIsCurrent <- function() {
  checkTrue(is.na(isCurrent(1:10)))
  checkTrue(all(isCurrent(new("VersionedBiobase"))))
  checkTrue(all(isCurrent(new("A"))))
}
