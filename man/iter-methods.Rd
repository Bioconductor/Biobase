\name{iter-methods}
\docType{methods}
\title{ Methods for the generic iter.}
\alias{iter-methods}
\alias{iter}
\description{
  This is a set of methods to iterate over different types of
  objects. The behaviour of the methods is similar to that of the
  \code{apply} family.}
\section{Methods}{\describe{
\item{object = exprSet, covlab = missing, f = function}{ Apply the
  function \code{f} to the rows of the expression data, \code{exprs}, of
  \code{object}. } 
\item{object = exprSet, covlab = missing, f = list}{ In this case it is
  presumed that \code{f} is a list of functions, each of which returns a
single value. The functions are applied sequentially to the rows of the
expression data, \code{exprs}, of \code{object}. The answers are stored in
the columns of the return matrix. }
\item{object = exprSet, covlab = character, f = function}{ The function
  \code{f} is assumed to be a function of two arguments. The covariate in
the \code{object} with name specified by \code{covlab} is extracted and
bound to the second argument of \code{f}. Then the resultant function is
passed back through \code{iter} and we effectively obtain the result of
this function applied to the rows of the expression data, \code{exprs}, of
  \code{object}. } 
}}
\keyword{methods}

