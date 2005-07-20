\name{df2pD}
\alias{df2pD}
\alias{coerce,phenoData-method}

\title{ A function to coerce from a data.frame to a phenoData object. }
\description{
 A simple function to coerce from a \code{\link{data.frame}} to a
\code{phenoData} object. The user can supply \code{varLabels} and
\code{varMetadata} if they would like. If these are not supplied then
default values are used and they should be edited if required.
}
\usage{
df2pD(x, varLabels, varMetadata)
}
\arguments{
  \item{x}{ The \code{data.frame} to be coerced. }
  \item{varLabels}{ A character vector describing each of the columns
  in \code{x}.}
  \item{varMetadata}{ More extensive descriptions of the variables in
  \code{x}, including units of measurement etc. }
}
\details{
 As above.
}
\value{
 An instance of the \code{phenoData} class.
}

\seealso{ \code{\link{phenoData}}}
\examples{
 data(cars)
 d1 = df2pD(cars)
}
\keyword{ manip }
