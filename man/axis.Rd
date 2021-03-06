% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcssWrappers.R
\name{axis}
\alias{axis}
\title{Add a styled axis to a plot}
\usage{
axis(side, Rcss = "default", Rcssclass = NULL, ...)
}
\arguments{
\item{side}{integer specifying what side of the plot to draw the axis.
The codes are 1: bottom, 2: left, 3: top, 4: top.
vertices}

\item{Rcss}{style sheet object. Leave "default" to use a style
defined via RcssSetDefaultStyle()}

\item{Rcssclass}{sub class of style sheet}

\item{...}{Further parameters, see documentation of graphics::axis}
}
\description{
This is a wrapper for R's axis function.
See R's documentation for graphics::axis for further details.
}
\examples{

# draw separate axes on an empty plot
plot(c(0, 1), c(0, 1), type="n", axes=FALSE, xlab="x-axis", ylab="")
axis(1)
axis(3)

}
