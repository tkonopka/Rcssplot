% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcssWrappers.R
\name{plot}
\alias{plot}
\title{Create a styled plot}
\usage{
plot(x, y, Rcss = "default", Rcssclass = NULL, ...)
}
\arguments{
\item{x, y}{coordinates for points on the plot}

\item{Rcss}{style sheet object. Leave "default" to use a style
defined via RcssSetDefaultStyle()}

\item{Rcssclass}{character, style class}

\item{...}{Further parameters, see documentation of graphics::plot}
}
\description{
This is a wrapper for R's plot function.
See R's documentation for graphics::plot for further details.
}
\examples{

# draw a new empty plot area - unit square
plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="")
# draw a plot area, automatically add some points
plot(runif(20), rpois(20, 100))

}
