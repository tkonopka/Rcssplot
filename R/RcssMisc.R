# Miscellaneous graphics functions, aking to RcssWrappers
#
#


#' combination of par and plot
#'
#' The sequence of par() and plot() occurs so frequently that
#' it a shortcut is helpful. 
#'
#' @export
#' @param x,y coordinates for points on the plot
#' @param Rcss style sheet object, leave "default" to use a style
#' defined via RcssDefaultStyle()
#' @param Rcssclass character, style class
#' @param ... Further parameters, passed to plot()
#'
#' @examples
#'
#' parplot(x=1:4, y=c(1,3,2,4))
#'
parplot <- function(x, y,
                    Rcss="default", Rcssclass=NULL, ...) {
  par(Rcss=Rcss, Rcssclass=Rcssclass)
  plot(x, y, Rcss=Rcss, Rcssclass=Rcssclass, ...)
}



#' Write styled text into a plot corner
#'
#' This can be suitable for placing a label in a multi-panel figure.
#' Note the automatic placement does not work when a plot is generated
#' with logarithmic scales.
#'
#' @export
#' @param label character, text for corner label
#' @param x,y numeric, positions for manual placement
#' @param adj numeric of length 2, argument adj for text
#' @param cex numeric, argument cex for text
#' @param Rcss style sheet object
#' @param Rcssclass character, style class
#' @param ... additional argument, passed to text()
#'
#' @examples
#'
#' plot(1:10, 1:10)
#' ctext("A")
#'
ctext <- function(label, x=NULL, y=NULL, adj=NULL, cex=NULL,
                  Rcss="default", Rcssclass=NULL, ...) {
  # determine position of the top-left corner
  p <- graphics::par()
  pw <- p$plt[2] - p$plt[1]
  ph <- p$plt[4] - p$plt[3]
  uw <- p$usr[2] - p$usr[1]
  uh <- p$usr[4] - p$usr[3]
  if (is.null(x)) {
    x <- p$usr[1] - (uw/pw)*p$plt[1]
  }
  if (is.null(y)) {
    y <- p$usr[3] - (uh/ph)*p$plt[3] + (uh/ph)
  }
  # find styling for the text component
  if (is.null(adj)) {
    adj <- RcssValue("ctext", "adj", default=c(0, 1), Rcss=Rcss, Rcssclass=Rcssclass)
  }
  if (is.null(cex)) {
    cex <- RcssValue("ctext", "cex", default=1.5, Rcss=Rcss, Rcssclass=Rcssclass)
  }
  text(x, y, label, xpd=1, adj=adj, cex=cex, Rcss=Rcss, Rcssclass=Rcssclass)
  invisible(c(x, y))
}

