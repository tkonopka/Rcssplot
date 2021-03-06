% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcssWrappers.R
\name{pdf}
\alias{pdf}
\title{Create a styled pdf figure}
\usage{
pdf(file, Rcss = "default", Rcssclass = NULL, ...)
}
\arguments{
\item{file}{character string with file name}

\item{Rcss}{style sheet object. Leave "default" to use a style
defined via RcssSetDefaultStyle()}

\item{Rcssclass}{character, style class}

\item{...}{Further parameters, see documentation of grDevices::pdf}
}
\description{
This is a wrapper for R's pdf function.
See R's documentation for grDevices::pdf for further details
}
\examples{

# send content of graphics to a pdf file
# to run this, un-comment the pdf() and dev.off() lines
# png(file="example-file.pdf")
barplot(1:5)
# dev.off()

}
