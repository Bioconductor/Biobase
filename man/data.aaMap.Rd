\name{data:aaMap}
\alias{data:aaMap}
\alias{aaMap}
\docType{data}
\title{Dataset: Names and Characteristics of Amino Acids }
\usage{data(aaMap)}
\description{
The \code{aaMap} data frame has 20 rows and 6 columns. Includes elementary information about amino acids.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{name}{amino acid name}
    \item{let.1}{one-letter code}
    \item{let.3}{three-letter code}
    \item{scProp}{side chain property at pH 7 (polar/nonpolar)}
    \item{hyPhilic}{logical: side chain is hydrophilic at pH 7}
    \item{acidic}{logical: side chain is acidic at pH 7}
  }
}
\source{
Nei M and Kumar S: Molecular evolution and phylogenetics (Oxford 2000), Table 1.2
}
\examples{
data(aaMap)
}
\keyword{datasets}
