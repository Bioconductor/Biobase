\name{openPDF}
\alias{openPDF}
\title{Open PDF Files in a Standard Viewer}
\description{
Displays the specified PDF file.
}
\usage{
openPDF(file, bg=TRUE)
}
\arguments{
  \item{file}{A character string, indicating the file to view}
  \item{bg}{Should the pdf viewer be opened in the background.}
}
\details{
  Currently this function works on Windows and Unix platforms.  Under
  Windows, whatever program is associated with the file extension will
  be used.  Under Unix, the function will use the program named in the
%  option "pdfViewer" (default is 'acroread'). -- the code checks pdfviewer!
% and that's the option that options() documents.
  option "pdfviewer" (see \code{help(options)} for information on
how this is set.)

  The \code{bg} argument is only interpreted on Unix.
}
\value{
This function is executed for its side effects.  The specified PDF file
is opened in the PDF viewer and \code{TRUE} is returned.
}
\author{ Jeff Gentry}

\examples{
\dontrun{openPDF("annotate.pdf")}
}
\keyword{utilities}

