\name{ScalarCharacter-class}
\docType{class}
\alias{ScalarCharacter-class}

\title{A length one (scalar) character class}

\description{\code{ScalarCharacter} extends \code{character} and
provides a validity method that requires the vector to be of length
one.  This class is intended to ease the validation burden on classes
that require strings as slots.
}
\section{Objects from the Class}{
Objects can be created by calls of the form
\code{new("ScalarCharacter", ...)}.

}
\section{Slots}{
	 \describe{
    \item{\code{.Data}:}{Object of class \code{"character"} which must
    have length one for the object to be valid.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{character}"}, from data part.
Class \code{"\linkS4class{vector}"}, by class "character", distance 2.
}
\section{Methods}{
No methods defined with class "ScalarCharacter" in the signature.
}

\author{Seth Falcon}
\examples{
str <- new("ScalarCharacter")
str
str <- new("ScalarCharacter", "foo")
str

}
\keyword{classes}
