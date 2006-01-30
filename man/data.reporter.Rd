\name{reporter}
\alias{reporter}
\docType{data}
\title{Example Data Frame Object for the ReporterInfo Slot}
\description{
  The \code{reporter} object is a 500 by 1 data frame.  The rows represent
  the 500 probe ids in the \code{geneData} data.  The values in
  \code{reporter} are the predefined probe types for the probes.
  \code{reporter} is used to created the exprSet object \code{sample.exprSet}.  
}
\usage{data(reporter)}
\format{
  A 500 by 1 data frame
}
\details{
  There are 10 predefined probe types:

  \item{\code{AFFX-}}{Quality Control (QC)}
  \item{\code{_f_}}{SequenceFamily}
  \item{\code{_g_}}{CommonGroups}
  \item{\code{_s_}}{SimilarityConstraint}
  \item{\code{_r_}}{RulesDropped}
  \item{\code{_i_}}{Incomplete}
  \item{\code{_b_}}{AmbiguousProbeSet}
  \item{\code{_l_}}{LongProbeSet}
  \item{\code{_at}}{AntiSenseTarget}
  \item{\code{_st}}{SenseTarget}
  
}
\source{
  Affymetrix GeneChip Expression Analysis Data Analysis Fundamentals (\url{http://www.affymetrix.com/Auth/support/downloads/manuals/data_analysis_fundamentals_manual.pdf})
}
\references{
  ~~ possibly secondary sources and usages ~~
}
\examples{
data(reporter)
## maybe str(reporter) ; plot(reporter) ...
}
\keyword{datasets}
