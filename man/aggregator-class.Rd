\name{aggregator-class}
\docType{class}
\alias{aggregator-class}
\alias{aggenv}
\alias{aggfun}
\alias{initfun}
\title{Class aggregator, a class of aggregators }
\description{ A class of objects designed to help aggregate
   calculations over an iterative computation. The aggregator consists
   of three objects. An environment to hold the values.
   A function that sets up an initial value the first time an object is
   seen. An aggregate function that increments the value of an object
   seen previously. }
\section{Creating Objects}{
\code{  new('aggregator',}\cr
\code{    aggenv  = ..../, # Object of class environment}\cr
\code{    initfun = ...., # Object of class function}\cr
\code{    aggfun  = ...., # Object of class function}\cr
\code{  )}}
\section{Slots}{
  \describe{
    \item{\code{aggenv}:}{Object of class "environment", holds the
      values between iterations. }
    \item{\code{initfun}:}{Object of class "function" specifies how to
      initialize the value for a name the first time it is encountered. }
    \item{\code{aggfun}:}{Object of class "function" used to increment
      (or perform any other function) on a name. }
  }
}
\section{Prototype}{
  describe{
    \item{\code{aggenv}:}{\code{new.env(hash=TRUE)} }
    \item{\code{initfun}:}{\code{function(name, val) 1} }
    \item{\code{aggfun}:}{ \code{function(name, current, val) current+1} }
  }
}
\details{
  This class is used to help aggregate different values over function
  calls. A very simple example is to use leave one out cross-validation
  for prediction. At each stage we first perform feature selection and
  then cross-validate. To keep track of how often each feature is
  selected we can use an aggregator. At the end of the cross-validation
  we can extract the names of the features chosen from \code{aggenv}.
  }
\section{Methods}{
  \describe{
    \item{aggenv}{(aggregator): Used to access the environment of the
      aggregator. }
    \item{aggfun}{(aggregator): Used to access the function that aggregates. }
    \item{initfun}{(aggregator): Used to access the initializer function. }
  }
}
\keyword{methods}
