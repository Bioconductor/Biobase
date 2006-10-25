\name{aggregator}
\docType{class}
\alias{class:aggregator}
\alias{aggregator}
\alias{aggregator-class}
\alias{initialize,aggregator-method}

\alias{aggenv,aggregator-method}
\alias{aggfun,aggregator-method}
\alias{initfun,aggregator-method}

\concept{aggenv}
\concept{aggfun}
\concept{initfun}

\title{A Simple Class for Aggregators }
\description{ A class of objects designed to help aggregate
   calculations over an iterative computation. The aggregator consists
   of three objects. An environment to hold the values.
   A function that sets up an initial value the first time an object is
   seen. An aggregate function that increments the value of an object
   seen previously. }
\section{Creating Objects}{
   \code{new('aggregator', aggenv  = [environment], initfun = [function], aggfun  = [function])}
}
\section{Slots}{
   \describe{
      \item{\code{aggenv}:}{Object of class 'environment', holds the values between iterations}
      \item{\code{initfun}:}{Object of class 'function' specifies how to initialize the value for a name the first time it is encountered}
      \item{\code{aggfun}:}{Object of class 'function' used to increment (or perform any other function) on a name}
   }
}
\section{Methods}{
   \describe{
      \item{\code{aggenv(aggregator)}:}{Used to access the environment of the aggregator}
      \item{\code{aggfun(aggregator)}:}{Used to access the function that aggregates}
      \item{\code{initfun(aggregator)}:}{Used to access the initializer function}
  }
}
\seealso{
   \code{\link{Aggregate}}
}
\details{
  This class is used to help aggregate different values over function
  calls. A very simple example is to use leave one out cross-validation
  for prediction. At each stage we first perform feature selection and
  then cross-validate. To keep track of how often each feature is
  selected we can use an aggregator. At the end of the cross-validation
  we can extract the names of the features chosen from \code{aggenv}.
  }
\keyword{methods}
\keyword{classes}
