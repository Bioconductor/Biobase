\name{openPDF}
\alias{openPDF}
\title{A function to view PDF files}
\description{
Displays the specified PDF file.
}
\usage{
openPDF(file)
}
\arguments{
  \item{file}{A character string, indicating the file to view}
}
\details{
  Currently this function works on Windows and Unix platforms.  Under
  Windows, whatever program is associated with the file extension will
  be used.  Under Unix, the function will use the program named in the
  option "pdfViewer" (default is 'acroread').
}
\value{
This function is executed for its side effects.  The specified PDF file
is opened in the PDF viewer and 'NULL' is returned.
}
\author{ Jeff Gentry}

\examples{
\dontrun{openPDF("annotate.pdf")}
}
\keyword{utilities}

