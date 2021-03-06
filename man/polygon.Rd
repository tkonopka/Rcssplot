% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcssWrappers.R
\name{polygon}
\alias{polygon}
\title{Draw a styled polygon on a plot}
\usage{
polygon(x, y = NULL, Rcss = "default", Rcssclass = NULL, ...)
}
\arguments{
\item{x, y}{coordinates for polygon vertices}

\item{Rcss}{style sheet object. Leave "default" to use a style
defined via RcssSetDefaultStyle()}

\item{Rcssclass}{character, style class}

\item{...}{Further parameters, see documentation of graphics::polygon}
}
\description{
This is a wrapper for R's polygon function.
See R's documentation for graphics::polygon for further details.
}
\examples{

# draw a multi-sided shape on an existing plot
plot(c(0, 10), c(0, 10), type="n", xlab="", ylab="")
polygon(c(1, 4, 7, 7, 1), c(1, 1, 4, 8, 8), col="blue")

}
