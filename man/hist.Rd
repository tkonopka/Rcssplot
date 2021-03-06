% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcssWrappers.R
\name{hist}
\alias{hist}
\title{Draw a styled histogram}
\usage{
hist(x, Rcss = "default", Rcssclass = NULL, ...)
}
\arguments{
\item{x}{numeric vector}

\item{Rcss}{style sheet object. Leave "default" to use a style
defined via RcssSetDefaultStyle()}

\item{Rcssclass}{character, style class}

\item{...}{Further parameters, see documentation of graphics::hist}
}
\description{
This is a wrapper for R's hist function.
See R's documentation for graphics::hist for further details.
}
\examples{

# draw a complete histogram
dataset <- rpois(400, 6)
hist(dataset, breaks=seq(0, max(dataset)))
# only obtain the bin counts, without plotting
histdata <- hist(dataset, breaks=seq(0, 2+max(dataset), by=2), plot=FALSE)
histdata

}
