# Wrappers for R's base graphics
#
# Each of the functions below is a wrapper for one of R's base
# graphics functions. The comment above each wrapper shows the
# definition of the function it is wrapping.
#
# The code of each wrapper contains two parts. First, the wrappers
# collect information about graphical tweaks from the Rcss object
# and from explicit values set by the user (through ...)
#
# Second, the wrappers build a command that uses the base R
# graphics function, and executes it.
#
#


# abline(a = NULL, b = NULL, h = NULL, v = NULL, reg = NULL,
#            coef = NULL, untf = FALSE, ...)
#     
#' Add a styled straight line to a plot
#'
#' This is a wrapper for R's abline function.
#' See R's documentation for graphics::abline for further details. 
#'
#' @export
#' @param a,b coefficient (intercet and slope) for line
#' @param h,v horizontal, vertical positions for line
#' @param reg an object with a coef method
#' @param coef vector with interect and slope for line
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStyle().
#' @param Rcssclass sub class of style sheet
#' @param ... Further parameters, see documentation of abline()
#'
#' @examples
#'
#' # draw a set of horizontal lines and a vertical line
#' plot(c(0, 1), c(0, 1), type="n")
#' abline(h=seq(0, 1, by=0.2))
#' abline(v=0.8)
#'
abline <-  function(a = NULL, b = NULL, h = NULL, v = NULL,
                    reg = NULL, coef = NULL,
                    Rcss = "default", Rcssclass = NULL, ...) {  
  
  # get a list of properties
  nowcss <- RcssGetProperties(Rcss, "abline", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss,
                                 list(..., a=a, b=b, h=h, v=v,
                                      reg=reg, coef=coef))
  # execute R's graphics function with custom properties
  do.call(graphics::abline, nowcss)
}


# arrows(x0, y0, x1 = x0, y1 = y0, length = 0.25, angle = 30,
#            code = 2, col = par("fg"), lty = par("lty"),
#            lwd = par("lwd"), ...)
#
#' Add styled arrows to a plot
#'
#' This is a wrapper for R's arrows function.
#' See R's documentation for graphics::arrows for further details.
#'
#' @export
#' @param x0,y0 coordinates of *from* endpoint
#' @param x1,y1 coordinates of *to* endpoint
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStle()
#' @param Rcssclass sub class of style sheet
#' @param ... Futher parameters, see documentation of graphics::arrows
#'
#' @examples
#'
#' # draw an arrow
#' plot(c(0, 1), c(0, 1), type="n")
#' arrows(0.2, 0.2, x1=0.8, y1=0.5)
#'
arrows <- function(x0, y0, x1=x0, y1=y0,
                   Rcss = "default", Rcssclass = NULL, ...) {  
  
  nowcss <- RcssGetProperties(Rcss, "arrows", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(..., x0=x0, y0=y0, x1=x1, y1=y1))
  do.call(graphics::arrows, nowcss)
}


# axis(side, at = NULL, labels = TRUE, tick = TRUE, line = NA,
#          pos = NA, outer = FALSE, font = NA, lty = "solid",
#          lwd = 1, lwd.ticks = lwd, col = NULL, col.ticks = NULL,
#          hadj = NA, padj = NA, ...)
#
#' Add a styled axis to a plot
#' 
#' This is a wrapper for R's axis function.
#' See R's documentation for graphics::axis for further details.
#' 
#' @export
#' @param side integer specifying what side of the plot to draw the axis.
#' The codes are 1: bottom, 2: left, 3: top, 4: top.
#' vertices
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStyle()
#' @param Rcssclass sub class of style sheet
#' @param ... Further parameters, see documentation of graphics::axis
#'
#' @examples
#'
#' # draw separate axes on an empty plot
#' plot(c(0, 1), c(0, 1), type="n", axes=FALSE, xlab="x-axis", ylab="")
#' axis(1)
#' axis(3)
#'
axis <-  function(side,
                  Rcss = "default", Rcssclass = NULL, ...) {
  
  nowcss <- RcssGetProperties(Rcss, "axis", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(..., side=side))
  do.call(graphics::axis, nowcss)
}


# barplot(height, ...)
#
#' Draw a styled barplot
#' 
#' This is a wrapper for R's barplot function.
#' See R's documentation for graphics::barplot for further details.
#' 
#' @export 
#' @param height numeric vector giving bar lengths
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStyle()
#' @param Rcssclass sub class of style sheet
#' @param ... Further parameters, see documentation of graphics::barplot
#'
#' @examples
#'
#' # draw a complete barplot
#' barplot(1:5)
#'
barplot <- function(height,
                    Rcss = "default", Rcssclass = NULL, ...) {  
  
  nowcss <- RcssGetProperties(Rcss, "barplot", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(..., height=height))
  do.call(graphics::barplot, nowcss)
}


# box(which = "plot", lty = "solid", ...)
#
#' Add a styled box around a plot 
#' 
#' This is a wrapper for R's box function.
#' See R's documentation for graphics::box for further details.
#' 
#' @export 
#' @param which character specifying where to draw a box;
#' see documentation of box()
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStyle()
#' @param Rcssclass sub class of style sheet
#' @param ... Further parameters, see documentation of graphics::box
#'
#' @examples
#'
#' # draw a box around an existing plot
#' plot(c(0, 1), c(0, 1), type="n", frame=FALSE)
#' box(lwd=3)
#'
box <-  function(which = "plot",
                 Rcss = "default", Rcssclass = NULL, ...) {
  
  nowcss <- RcssGetProperties(Rcss, "box", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(..., which=which))
  do.call(graphics::box, nowcss)
}


# boxplot(x, ...)
#
#' Draw a styled boxplot
#' 
#' This is a wrapper for R's boxplot function.
#' See R's documentation for graphics::boxplot for further details.
#' 
#' @export 
#' @param x data for boxplot; either single numeric vector or a list
#' of numeric vectors; see documentation of boxplot() 
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStyle()
#' @param Rcssclass sub class of style sheet
#' @param ... Further parameters, see documentation of graphics::boxplot
#'
#' @examples
#'
#' # draw a complete boxplot
#' dataset <- list(A=rpois(30, 10), B=rpois(30, 20))
#' boxplot(dataset, col=c("#dd0000", "#dd8888"))
#'
boxplot <-  function(x,
                     Rcss = "default", Rcssclass = NULL, ...) {
  
  nowcss <- RcssGetProperties(Rcss, "boxplot", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(..., x=x))
  do.call(graphics::boxplot, nowcss)
}


# contour(x = seq(0, 1, length.out = nrow(z)),
#             y = seq(0, 1, length.out = ncol(z)),
#             z,
#             nlevels = 10, levels = pretty(zlim, nlevels),
#             labels = NULL,
#             xlim = range(x, finite = TRUE),
#             ylim = range(y, finite = TRUE),
#             zlim = range(z, finite = TRUE),
#             labcex = 0.6, drawlabels = TRUE, method = "flattest",
#             vfont, axes = TRUE, frame.plot = axes,
#             col = par("fg"), lty = par("lty"), lwd = par("lwd"),
#             add = FALSE, ...)
#
#' Draw a styled contour 
#'
#' This is a wrapper for R's contour function.
#' See R's documentation for graphics::contour for further details.
#'
#' @export
#' @param x numeric vector; locations of grid lines
#' @param y numeric vector; locations of grid lines
#' @param z matrix of values
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStyle()
#' @param Rcssclass sub class of style sheet
#' @param ... Further parameters, see documentation of graphics::contour
#'
#' @examples
#'
#' # draw a complete contour plot
#' dataset <- outer(1:10, 1:10)
#' contour(z=dataset)
#'
contour <- function(x = seq(0, 1, length.out = nrow(z)),
                    y = seq(0, 1, length.out = ncol(z)), z,
                    Rcss = "default", Rcssclass = NULL, ...) {
  
  nowcss <- RcssGetProperties(Rcss, "contour", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(..., x=x, y=y, z=z))
  do.call(graphics::contour, nowcss)  
}


# grid(...)
#
#' #' Draw a styled grid
#' 
#' This is a wrapper for R's grid function.
#' See R's documentation for graphics::grid for further details.
#' 
#' @export 
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStyle()
#' @param Rcssclass sub class of style sheet
#' @param ... Further parameters, see documentation of graphics::grid
#'
#' @examples
#'
#' # add a grid to an existing plot
#' plot(c(0, 10), c(0, 10), type="n", xaxs="i", yaxs="i", las=1)
#' grid(nx=10, ny=5, col="#777777")
#' 
grid <-  function(Rcss = "default", Rcssclass = NULL, ...) {
  
  nowcss <- RcssGetProperties(Rcss, "grid", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(...))
  do.call(graphics::grid, nowcss)
}


# hist(x, ...)
#
#' Draw a styled histogram
#' 
#' This is a wrapper for R's hist function.
#' See R's documentation for graphics::hist for further details.
#' 
#' @export 
#' @param x numeric vector
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStyle()
#' @param Rcssclass sub class of style sheet
#' @param ... Further parameters, see documentation of graphics::hist
#'
#' @examples
#'
#' # draw a complete histogram
#' dataset <- rpois(400, 6)
#' hist(dataset, breaks=seq(0, max(dataset)))
#' # only obtain the bin counts, without plotting
#' histdata <- hist(dataset, breaks=seq(0, 2+max(dataset), by=2), plot=FALSE)
#' histdata
#'
hist <- function(x,
                 Rcss = "default", Rcssclass = NULL, ...) {
  
  nowcss <- RcssGetProperties(Rcss, "hist", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(...))
  # this implementation is slightly different
  # not exactly sure why the simpler implementation does not work here
  cmd <- paste0("graphics::hist (x",
                RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
  eval(parse(text = cmd))
}


# jpeg(filename = "Rplot%03d.jpeg",
#          width = 480, height = 480, units = "px", pointsize = 12,
#          quality = 75,
#          bg = "white", res = NA, ...,
#          type = c("cairo", "Xlib", "quartz"), antialias)
#     
#' Create a styled jpg figure
#'
#' This is a wrapper for R's jpeg function.
#' See R's documentation for grDevices::jpeg for further details
#'
#' @export
#' @param file character string with file name
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStyle()
#' @param Rcssclass sub class of style sheet
#' @param ... Further parameters, see documentation of grDevices::jpeg
#'
#' @examples
#'
#' # send content of graphics to a jpg file
#' # to run this, un-comment the jpeg() and dev.off() lines
#' # jpeg(file="example-file.jpg")
#' barplot(1:5)
#' # dev.off()
#'
jpeg <- function(file,
                 Rcss = "default", Rcssclass = NULL, ...) {
  
  nowcss <- RcssGetProperties(Rcss, "jpeg", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(..., file=file))
  do.call(grDevices::jpeg, nowcss)  
}


# legend(x, y = NULL, legend, fill = NULL, col = par("col"),
#            border = "black", lty, lwd, pch,
#            angle = 45, density = NULL, bty = "o", bg = par("bg"),
#            box.lwd = par("lwd"), box.lty = par("lty"), box.col = par("fg"),
#            pt.bg = NA, cex = 1, pt.cex = cex, pt.lwd = lwd,
#            xjust = 0, yjust = 1, x.intersp = 1, y.intersp = 1,
#            adj = c(0, 0.5), text.width = NULL, text.col = par("col"),
#            text.font = NULL, merge = do.lines && has.pch, trace = FALSE,
#            plot = TRUE, ncol = 1, horiz = FALSE, title = NULL,
#            inset = 0, xpd, title.col = text.col, title.adj = 0.5,
#            seg.len = 2)
#
#' Add a styled legend to aplot
#' 
#' This is a wrapper for R's legend function.
#' See R's documentation for graphics::legend for further details.
#'
#' @export
#' @param x,y position of the legend
#' @param legend character vector with labels (text appears in the legend)
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStyle()
#' @param Rcssclass sub class of style sheet
#' @param ... Further parameters, see documentation of graphics::legend
#'
#' @examples
#'
#' # add a legend to an existing plot
#' plot(1:8, 1:8, col=rep(c(1,2), each=4), pch=19)
#' legend(7, 3, c("A", "B"), pch=19, col=1:2)
#'
legend <- function(x, y = NULL, legend,
                   Rcss = "default", Rcssclass = NULL, ...) {
  
  # get a list of properties
  nowcss <- RcssGetProperties(Rcss, "legend", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(..., x=x))
  if(!missing(legend)) {
    nowcss <- RcssUpdateProperties(nowcss, list(y=y, legend=legend))
  } else {
    nowcss <- RcssUpdateProperties(nowcss, list(legend=y))
  }
  # execute R's graphics function with custom properties 
  do.call(graphics::legend, nowcss)
}


# lines(x, y = NULL, type = "l", ...)
#
#' Add styled line segments to a plot
#' 
#' This is a wrapper for R's lines function.
#' See R's documentation for graphics::lines for further details.
#'
#' @export
#' @param x,y coordinates for start and end points for lines
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStyle()
#' @param Rcssclass sub class of style sheet
#' @param ... Further parameters, see documentation of graphics::lines
#'
#' @examples
#'
#' # add lines to an existing plot area
#' plot(c(0, 10), c(0, 10), type="n")
#' lines(c(1,8), c(2, 2), lwd=3, col="black")
#' lines(c(1, 7, NA, 4, 9), c(1, 6, NA, 1, 3), lwd=1, col="blue")
#' lines(c(8, 3), c(7, 9), lwd=3, lty=2, col="red")
#' 
lines <- function(x, y = NULL,
                  Rcss = "default", Rcssclass = NULL, ...) {   
  
  nowcss <- RcssGetProperties(Rcss, "lines", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(..., x=x, y=y))
  do.call(graphics::lines, nowcss)
}


#' Add styled line segments to a plot
#' 
#' This is a wrapper for R's matplot function.
#' See R's documentation for graphics::matplot for further details.
#'
#' @export
#' @param x,y vectors or matrices of data for plotting. The number of rows 
#' should match. If one of them are missing, the other is taken as y and an x 
#' vector of 1:n is used. Missing values (NAs) are allowed.
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStyle()
#' @param Rcssclass sub class of style sheet
#' @param ... Further parameters, see documentation of graphics::lines
#'
#' @examples
#'
#' # draw scatter based on column in a matrix
#' dataset = cbind(A=rnorm(20), B=rnorm(20))
#' matplot(dataset)
#'
matplot <- function(x, y,
                    Rcss = "default", Rcssclass = NULL, ...) {

  # get a list of properties
  nowcss <- RcssGetProperties(Rcss, "matplot", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(...))
  if (!missing(y)) {
    cmd <- paste0("graphics::matplot (x, y",
                  RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
  } else {
    cmd <- paste0("graphics::matplot (x,",
                  RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
  }
  eval(parse(text = cmd))
}


# mtext(text, side = 3, line = 0, outer = FALSE, at = NA,
#           adj = NA, padj = NA, cex = NA, col = NA, font = NA, ...)
#
#' Write styled text into a plot margin
#' 
#' This is a wrapper for R's mtext function.
#' See R's documentation for graphics::mtext for further details.
#'
#' @export
#' @param text characters to print on the plot
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStyle()
#' @param Rcssclass sub class of style sheet
#' @param ... Further parameters, see documentation of graphics::mtext
#'
#' @examples
#'
#' # draw text into a margin
#' plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="")
#' mtext(side=1, "bottom x-axis label", line=2.5)
#' mtext(side=2, "left y-axis label", line=2.5)
#' mtext(side=3, "top x-axis label")
#' mtext(side=4, "right y-axis label")
#'
mtext <- function(text,
                  Rcss = "default", Rcssclass = NULL, ...) {
  
  nowcss <- RcssGetProperties(Rcss, "mtext", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(..., text=text))
  do.call(graphics::mtext, nowcss)
}


# par(...)
#
#' Set styled parameters for base graphics
#'
#' This is a wrapper for R's par function.
#' See R's documentation for graphics::par for further details.
#'
#' @export
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStyle()
#' @param Rcssclass sub class of style sheet
#' @param ... Further parameters, see documentation of graphics::par
#'
#' @examples
#'
#' # set properties for plot
#' par(ps=8, mar=c(3, 8, 3, 1))
#' plot(c(0, 1), c(0, 1), type="n", frame=FALSE)
#' text(rep(0.5, 2), c(0.2, 0.5), c("abc", "def"))
#' par(ps=12)
#' text(0.5, 0.8, "xyz")
#'
par <- function(Rcss = "default", Rcssclass = NULL, ...) {
  
  nowcss <- RcssGetProperties(Rcss, "par", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(...))
  do.call(graphics::par, nowcss)
}


# pdf(file = ifelse(onefile, "Rplots.pdf", "Rplot%03d.pdf"),
#         width, height, onefile, family, title, fonts, version,
#         paper, encoding, bg, fg, pointsize, pagecentre, colormodel,
#         useDingbats, useKerning, fillOddEven, compress)
#     
#' Create a styled pdf figure
#'
#' This is a wrapper for R's pdf function.
#' See R's documentation for grDevices::pdf for further details
#'
#' @export
#' @param file character string with file name
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStyle()
#' @param Rcssclass sub class of style sheet
#' @param ... Further parameters, see documentation of grDevices::pdf
#'
#' @examples
#'
#' # send content of graphics to a pdf file
#' # to run this, un-comment the pdf() and dev.off() lines
#' # png(file="example-file.pdf")
#' barplot(1:5)
#' # dev.off()
#'
pdf <- function(file,
                Rcss = "default", Rcssclass = NULL, ...) {
  
  nowcss <- RcssGetProperties(Rcss, "pdf", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(..., file=file))
  do.call(grDevices::pdf, nowcss)
}


# plot(x, y, ...)
#
#' Create a styled plot
#' 
#' This is a wrapper for R's plot function.
#' See R's documentation for graphics::plot for further details.
#'
#' @export 
#' @param x,y coordinates for points on the plot
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStyle()
#' @param Rcssclass sub class of style sheet
#' @param ... Further parameters, see documentation of graphics::plot
#'
#' @examples
#'
#' # draw a new empty plot area - unit square
#' plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="")
#' # draw a plot area, automatically add some points
#' plot(runif(20), rpois(20, 100))
#'
plot <- function(x, y,
                 Rcss = "default", Rcssclass = NULL, ...) {
  
  # get a list of properties
  nowcss <- RcssGetProperties(Rcss, "plot", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(...))
  # execute R's graphics function with custom properties
  if (!missing(y)) {
    cmd <- paste0("graphics::plot (x, y",
                  RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
  } else {
    cmd <- paste0("graphics::plot (x,",
                  RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
  }
  eval(parse(text = cmd))
}


# png(filename = "Rplot%03d.png",
#          width = 480, height = 480, units = "px", pointsize = 12,
#          bg = "white",  res = NA, ...,
#         type = c("cairo", "cairo-png", "Xlib", "quartz"), antialias)
#     
#' Create a styled png figure
#'
#' This is a wrapper for R's png function.
#' See R's documentation for grDevices::png for further details.
#'
#' @export 
#' @param file character string with file name
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStyle()
#' @param Rcssclass sub class of style sheet
#' @param ... Further parameters, see documentation of grDevices::png
#'
#' @examples
#'
#' # send content of graphics to a png file
#' # to run this, un-comment the png() and dev.off() lines
#' # png(file="example-file.png")
#' barplot(1:5)
#' # dev.off()
#'
png <- function(file,
                Rcss = "default", Rcssclass = NULL, ...) {
  
  nowcss <- RcssGetProperties(Rcss, "png", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(file=file, ...))
  do.call(grDevices::png, nowcss)
}


# points(x, y = NULL, type = "p", ...)
#
#' Add styled points to a plot
#' 
#' This is a wrapper for R's points function.
#' See R's documentation for graphics::points for further details.
#'
#' @export
#' @param x,y coordinates for points on the plot
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStyle()
#' @param Rcssclass sub class of style sheet
#' @param ... Further parameters, see documentation of graphics::points
#'
#' @examples
#'
#' # draw a set of points onto an existing plot
#' plot(c(0, 1), c(0, 1), type="n")
#' points(runif(10), runif(10))
#' points(runif(10), runif(10), col="blue", pch=19)
#'
points <- function(x, y = NULL,
                   Rcss = "default", Rcssclass = NULL, ...) {
  
  nowcss <- RcssGetProperties(Rcss, "points", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(x=x, y=y, ...))
  do.call(graphics::points, nowcss)
}


# polygon(x, y = NULL, density = NULL, angle = 45,
#             border = NULL, col = NA, lty = par("lty"),
#             ..., fillOddEven = FALSE)
#
#' Draw a styled polygon on a plot
#' 
#' This is a wrapper for R's polygon function.
#' See R's documentation for graphics::polygon for further details.
#'
#' @export 
#' @param x,y coordinates for polygon vertices
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStyle()
#' @param Rcssclass sub class of style sheet
#' @param ... Further parameters, see documentation of graphics::polygon
#'
#' @examples
#'
#' # draw a multi-sided shape on an existing plot
#' plot(c(0, 10), c(0, 10), type="n", xlab="", ylab="")
#' polygon(c(1, 4, 7, 7, 1), c(1, 1, 4, 8, 8), col="blue")
#'
polygon <- function(x, y = NULL,
                        Rcss = "default", Rcssclass = NULL, ...) {

  nowcss <- RcssGetProperties(Rcss, "polygon", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(x=x, y=y, ...))
  do.call(graphics::polygon, nowcss)
}


#rect(xleft, ybottom, xright, ytop, density = NULL, angle = 45,
#          col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"),
#          ...)
#
#' Draw styled rectangles on a plot
#' 
#' This is a wrapper for R's rect function.
#' See R's documentation for graphics::rect for further details.
#'
#' @export
#' @param xleft,ybottom,xright,ytop vector of coordinates for rectangles'
#' vertices
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStyle()
#' @param Rcssclass sub class of style sheet
#' @param ... Further parameters, see documentation of graphics::rect
#'
#' @examples
#'
#' # draw rectangles on an existing plot
#' plot(c(0, 10), c(0, 10), type="n", xlab="", ylab="")
#' rect(4.5, 1, 5.5, 3)
#' rect(c(1, 7.5), c(6, 6), c(2.5, 9), c(8, 8))
#'
rect <- function(xleft, ybottom, xright, ytop, 
                 Rcss = "default", Rcssclass = NULL, ...) {
  
  nowcss <- RcssGetProperties(Rcss, "rect", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss,
                                 list(xleft=xleft, ybottom=ybottom,
                                      xright=xright, ytop=ytop, ...))
  do.call(graphics::rect, nowcss)
}


# stripchart(x, ...)
#
#' Draw styled strip chart
#' 
#' This is a wrapper for R's stripchart function.
#' See R's documentation for graphics::stripchart for further details.
#'
#' @export 
#' @param x list of numeric vectors
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStyle()
#' @param Rcssclass sub class of style sheet
#' @param ... Further parameters, see documentation of graphics::stripchart
#'
#' @examples
#'
#' # draw a complete strip-chart plot
#' dataset <- list(A=c(1,9,3,8), B=c(3,4,2,9,2), C=rpois(8, 10))
#' stripchart(dataset)
#' stripchart(dataset, method="jitter", vertical=TRUE, pch=19)
#'
stripchart <- function(x,
                       Rcss = "default", Rcssclass = NULL, ...) {

  nowcss <- RcssGetProperties(Rcss, "stripchart", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(x=x, ...))
  do.call(graphics::stripchart, nowcss)
}


# text(x, y = NULL, labels = seq_along(x), adj = NULL,
#          pos = NULL, offset = 0.5, vfont = NULL,
#          cex = 1, col = NULL, font = NULL, ...)
#
#' Add styled text to a plot
#' 
#' This is a wrapper for R's text function.
#' See R's documentation for graphics::text for further details.
#'
#' @export 
#' @param x,y coordinates where to write labels
#' @param labels characters to print on the plot
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStyle()
#' @param Rcssclass sub class of style sheet
#' @param ... Further parameters, see documentation of graphics::text
#'
#' @examples
#'
#' # add text to an existing plot
#' plot(c(0, 1), c(0, 1), type="n")
#' text(0.1, 0.1, "A")
#' text(c(0.2, 0.7), c(0.8, 0.6), c("B", "C"))
#'
text <- function(x, y=NULL, labels=seq_along(x),
                 Rcss = "default", Rcssclass = NULL, ...) {
  
  nowcss <- RcssGetProperties(Rcss, "text", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(x=x, y=y, labels=labels, ...))
  do.call(graphics::text, nowcss)
}


# title(main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
#           line = NA, outer = FALSE, ...)
#
#' Add styled annotation to a plot
#' 
#' This is a wrapper for R's title function.
#' See R's documentation for graphics::title for further details.
#' 
#' @export 
#' @param main plot title
#' @param sub plot sub title
#' @param xlab,ylab labels on axes
#' @param Rcss style sheet object. Leave "default" to use a style
#' defined via RcssSetDefaultStyle()
#' @param Rcssclass sub class of style sheet
#' @param ... Further parameters, see documentation of graphics::title
#'
#' @examples
#'
#' # add a title
#' plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="")
#' title("This is the title")
#' title(sub="This is a bottom title")
#'
title <- function(main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                  Rcss = "default", Rcssclass = NULL, ...) {
  
  nowcss <- RcssGetProperties(Rcss, "title", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss,
                                 list(main=main, sub=sub,
                                      xlab=xlab, ylab=ylab, ...))
  do.call(graphics::title, nowcss)
}

