##
## Wrappers for R's base graphics
##
## Each of the functions below is a wrapper for one of R's base
## graphics functions. The comment above each wrapper shows the
## definition of the function it is wrapping.
##
## The code of each wrapper contains two parts. First, the wrappers
## collect information about graphical tweaks from the Rcss object
## and from explicit values set by the user (through ...)
##
## Second, the wrappers build a command that uses the base R
## graphics function, and executes it.
##
##
## Note 1:
##
## There is a bit of repetition here, but some of the wrappers
## differ somewhat because of required arguments.
## e.g. "side" for axis() vs. "x" and "y" for plot
## Perhaps some of these function could be generated automatically.
##



## abline(a = NULL, b = NULL, h = NULL, v = NULL, reg = NULL,
##            coef = NULL, untf = FALSE, ...)
##     
##' Add a style straight line to a plot
##'
##' @param a,b coefficient (intercet and slope) for line
##' @param h,v horizontal, vertical positions for line
##' @param reg an object with a coef method
##' @param coef vector with interect and slope for line
##' @param Rcss style sheet object. Leave "default" to use a style
##' defined via RcssSetDefaultStyle().
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of abline()
##' @export
Rcssabline <-  function(a = NULL, b = NULL, h = NULL, v = NULL,
                        reg = NULL, coef = NULL,
                        Rcss = "default", Rcssclass = NULL, ...) {
  ## convert between a description of a default Rcss to an actual object
  if (identical(Rcss, "default")) {
    Rcss <- getOption("RcssDefaultStyle", default = NULL);
  }
  ## get a list of properties
  nowcss <- RcssGetProperties(Rcss, "abline", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss,
                                 list(..., a=a, b=b, h=h, v=v,
                                      reg=reg, coef=coef))
  ## execute R's graphics function with custom properties
  do.call(abline, nowcss)
}



## axis(side, at = NULL, labels = TRUE, tick = TRUE, line = NA,
##          pos = NA, outer = FALSE, font = NA, lty = "solid",
##          lwd = 1, lwd.ticks = lwd, col = NULL, col.ticks = NULL,
##          hadj = NA, padj = NA, ...)
##
##' Add a styled axis to a plot
##' 
##' Rcssaxis is a wrapper for R's axis() function.
##' See R's documentation for axis() for further details.
##' 
##' @param side integer specifying what side of the plot to draw the axis.
##' The codes are 1: bottom, 2: left, 3: top, 4: top.
##' vertices
##' @param Rcss style sheet object. Leave "default" to use a style
##' defined via RcssSetDefaultStyle()
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of axis()
##' @export 
Rcssaxis <-  function(side,
                      Rcss = "default", Rcssclass = NULL, ...) {

  ## convert between a description of a default Rcss to an actual object
  if (identical(Rcss, "default")) {
    Rcss <- getOption("RcssDefaultStyle", default = NULL);
  }
  ## get a list of properties
  nowcss <- RcssGetProperties(Rcss, "axis", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(..., side=side))
  ## execute R's graphics function with custom properties
  do.call(axis, nowcss)
}



## barplot(height, ...)
##
##' Draw a styled barplot
##' 
##' Rcssbarplot is a wrapper for R's barplot() function.
##' See R's documentation for barplot() for further details.
##' 
##' @param height numeric vector giving bar lengths
##' @param Rcss style sheet object. Leave "default" to use a style
##' defined via RcssSetDefaultStyle()
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of barplot()
##' @export 
Rcssbarplot <- function(height,
                        Rcss = "default", Rcssclass = NULL, ...) {
  ## convert between a description of a default Rcss to an actual object
  if (identical(Rcss, "default")) {
    Rcss <- getOption("RcssDefaultStyle", default = NULL);
  }
  ## get a list of properties
  nowcss <- RcssGetProperties(Rcss, "barplot", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(..., height=height))
  ## execute R's graphics function with custom properties
  do.call(barplot, nowcss)
}



## box(which = "plot", lty = "solid", ...)
##
##' Add a styled box around a plot 
##' 
##' Rcssbox is a wrapper for R's box() function.
##' See R's documentation for box() for further details.
##' 
##' @param which character specifying where to draw a box;
##' see documentation of box()
##' @param Rcss style sheet object. Leave "default" to use a style
##' defined via RcssSetDefaultStyle()
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of box()
##' @export 
Rcssbox <-  function(which = "plot",
                     Rcss = "default", Rcssclass = NULL, ...) {
  ## convert between a description of a default Rcss to an actual object
  if (identical(Rcss, "default")) {
    Rcss <- getOption("RcssDefaultStyle", default = NULL);
  }
  ## get a list of properties
  nowcss <- RcssGetProperties(Rcss, "box", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(..., which=which))
  ## execute R's graphics function with custom properties
  do.call(box, nowcss)
}



## boxplot(x, ...)
##
##' ##' Draw a styled boxplot
##' 
##' Rcssboxplot is a wrapper for R's boxplot() function.
##' See R's documentation for boxplot() for further details.
##' 
##' @param x data for boxplot; either single numeric vector or a list
##' of numeric vectors; see documentation of boxplot() 
##' @param Rcss style sheet object. Leave "default" to use a style
##' defined via RcssSetDefaultStyle()
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of boxplot()
##' @export 
Rcssboxplot <-  function(x,
                         Rcss = "default", Rcssclass = NULL, ...) {
  ## convert between a description of a default Rcss to an actual object
  if (identical(Rcss, "default")) {
    Rcss <- getOption("RcssDefaultStyle", default = NULL);
  }
  ## get a list of properties
  nowcss <- RcssGetProperties(Rcss, "boxplot", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(..., x=x))
  ## execute R's graphics function with custom properties
  do.call(boxplot, nowcss)
}



## hist(x, ...)
##
##' ##' Draw a styled histogram
##' 
##' Rcsshist is a wrapper for R's hist() function.
##' See R's documentation for hist() for further details.
##' 
##' @param x numeric vector
##' @param Rcss style sheet object. Leave "default" to use a style
##' defined via RcssSetDefaultStyle()
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of hist()
##' @export 
Rcsshist <- function(x,
                     Rcss = "default", Rcssclass = NULL, ...) {
  ## convert between a description of a default Rcss to an actual object
  if (identical(Rcss, "default")) {
    Rcss <- getOption("RcssDefaultStyle", default = NULL);
  }
  ## get a list of properties
  nowcss <- RcssGetProperties(Rcss, "hist", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(...))
  ## execute R's graphics function with custom properties
  cmd <- paste0("hist (x",
             RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
  eval(parse(text = cmd))
}



## jpeg(filename = "Rplot%03d.jpeg",
##          width = 480, height = 480, units = "px", pointsize = 12,
##          quality = 75,
##          bg = "white", res = NA, ...,
##          type = c("cairo", "Xlib", "quartz"), antialias)
##     
##' Create a styled jpg figure
##'
##' Rcssjpeg is a wrapper for R's jpeg() function.
##' See R's documentation for jpeg() for further details
##'
##' @param file character string with file name
##' @param Rcss style sheet object. Leave "default" to use a style
##' defined via RcssSetDefaultStyle()
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of jpeg()
##' @export 
Rcssjpeg <- function(file,
                    Rcss = "default", Rcssclass = NULL, ...) {
  ## convert between a description of a default Rcss to an actual object
  if (identical(Rcss, "default")) {
    Rcss <- getOption("RcssDefaultStyle", default = NULL);
  }
  ## get a list of properties
  nowcss <- RcssGetProperties(Rcss, "jpeg", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(..., file=file))
  ## execute R's graphics function with custom properties
  do.call(jpeg, nowcss)  
}



## legend(x, y = NULL, legend, fill = NULL, col = par("col"),
##            border = "black", lty, lwd, pch,
##           angle = 45, density = NULL, bty = "o", bg = par("bg"),
##            box.lwd = par("lwd"), box.lty = par("lty"), box.col = par("fg"),
##            pt.bg = NA, cex = 1, pt.cex = cex, pt.lwd = lwd,
##            xjust = 0, yjust = 1, x.intersp = 1, y.intersp = 1,
##            adj = c(0, 0.5), text.width = NULL, text.col = par("col"),
##            text.font = NULL, merge = do.lines && has.pch, trace = FALSE,
##            plot = TRUE, ncol = 1, horiz = FALSE, title = NULL,
##            inset = 0, xpd, title.col = text.col, title.adj = 0.5,
##            seg.len = 2)
##
##' Add a styled legend to aplot
##' 
##' Rcsslegend is a wrapper for R's legend() function.
##' See R's documentation for legend() for further details.
##'
##' @param x,y position of the legend
##' @param legend character vector with labels (text appears in the legend)
##' @param Rcss style sheet object. Leave "default" to use a style
##' defined via RcssSetDefaultStyle()
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of legend()
##' @export 
Rcsslegend <- function(x, y = NULL, legend,
                       Rcss = "default", Rcssclass = NULL, ...) {
  ## convert between a description of a default Rcss to an actual object
  if (identical(Rcss, "default")) {
    Rcss <- getOption("RcssDefaultStyle", default = NULL);
  }
  ## get a list of properties
  nowcss <- RcssGetProperties(Rcss, "legend", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(..., x=x))
  if(!missing(legend)) {
    nowcss <- RcssUpdateProperties(nowcss, list(y=y, legend=legend))
  } else {
    nowcss <- RcssUpdateProperties(nowcss, list(legend=y))
  }
  ## execute R's graphics function with custom properties
  # There is a confusion between the arg "legend" and the function
  # "legend". This is why we use "graphics::"
  do.call(graphics::legend, nowcss)
}



## lines(x, y = NULL, type = "l", ...)
##
##' Add styled line segments to a plot
##' 
##' Rcsslines is a wrapper for R's lines() function.
##' See R's documentation for lines() for further details.
##'
##' @param x,y coordinates for start and end points for lines
##' @param Rcss style sheet object. Leave "default" to use a style
##' defined via RcssSetDefaultStyle()
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of lines()
##' @export 
Rcsslines <- function(x, y = NULL,
                      Rcss = "default", Rcssclass = NULL, ...) {   
  ## convert between a description of a default Rcss to an actual object
  if (identical(Rcss, "default")) {
    Rcss <- getOption("RcssDefaultStyle", default = NULL);
  }
  ## get a list of properties
  nowcss <- RcssGetProperties(Rcss, "lines", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(..., x=x, y=y))
  ## execute R's graphics function with custom properties
  do.call(lines, nowcss)
}

##' Add styled line segments to a plot
##' 
##' Rcssmatplot is a wrapper for R's \code{\link{matplot}()} function.
##' See R's documentation for \code{\link{matplot}()} for further details.
##'
##' @param x,y vectors or matrices of data for plotting. The number of rows 
##' should match. If one of them are missing, the other is taken as y and an x 
##' vector of 1:n is used. Missing values (NAs) are allowed.
##' @param Rcss style sheet object. Leave "default" to use a style
##' defined via RcssSetDefaultStyle()
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of lines()
##' @export 
Rcssmatplot <- function(x, y,
                        Rcss = "default", Rcssclass = NULL, ...) {
  ## convert between a description of a default Rcss to an actual object
  if (identical(Rcss, "default")) {
    Rcss <- getOption("RcssDefaultStyle", default = NULL);
  }
  ## get a list of properties
  nowcss <- RcssGetProperties(Rcss, "matplot", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(...))
  if (!missing(y)) {
    cmd <- paste0("matplot (x, y",
                  RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
  } else {
    cmd <- paste0("matplot (x,",
                  RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
  }
  eval(parse(text = cmd))
}

## mtext(text, side = 3, line = 0, outer = FALSE, at = NA,
##           adj = NA, padj = NA, cex = NA, col = NA, font = NA, ...)
##
##' Write styled text into a plot margin
##' 
##' Rcssmtext is a wrapper for R's mtext() function.
##' See R's documentation for mtext() for further details.
##'
##' @param text characters to print on the plot
##' @param Rcss style sheet object. Leave "default" to use a style
##' defined via RcssSetDefaultStyle()
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of mtext()
##' @export 
Rcssmtext <- function(text,
                      Rcss = "default", Rcssclass = NULL, ...) {
  ## convert between a description of a default Rcss to an actual object
  if (identical(Rcss, "default")) {
    Rcss <- getOption("RcssDefaultStyle", default = NULL);
  }
  ## get a list of properties
  nowcss <- RcssGetProperties(Rcss, "mtext", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(..., text=text))
  ## execute R's graphics function with custom properties
  do.call(mtext, nowcss)
}



## par(...)
##
##' Set styled parameters for base graphics
##'
##' Rcsspar is a wrapper for R's par() function.
##' See R's documentation for par() for further details.
##'
##' @param Rcss style sheet object. Leave "default" to use a style
##' defined via RcssSetDefaultStyle()
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of lines()
##' @export 
##' 
Rcsspar <- function(Rcss = "default", Rcssclass = NULL, ...) {
  ## convert between a description of a default Rcss to an actual object
  if (identical(Rcss, "default")) {
    Rcss <- getOption("RcssDefaultStyle", default = NULL);
  }
  ## get a list of properties
  nowcss <- RcssGetProperties(Rcss, "par", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(...))
  ## execute R's graphics function with custom properties
  do.call(par, nowcss)
}



## pdf(file = ifelse(onefile, "Rplots.pdf", "Rplot%03d.pdf"),
##         width, height, onefile, family, title, fonts, version,
##         paper, encoding, bg, fg, pointsize, pagecentre, colormodel,
##         useDingbats, useKerning, fillOddEven, compress)
##     
##' Create a styled pdf figure
##'
##' Rcsspdf is a wrapper for R's pdf() function.
##' See R's documentation for pdf() for further details
##'
##' @param file character string with file name
##' @param Rcss style sheet object. Leave "default" to use a style
##' defined via RcssSetDefaultStyle()
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of pdf()
##' @export 
Rcsspdf <- function(file,
                    Rcss = "default", Rcssclass = NULL, ...) {
  ## convert between a description of a default Rcss to an actual object
  if (identical(Rcss, "default")) {
    Rcss <- getOption("RcssDefaultStyle", default = NULL);
  }
  ## get a list of properties
  nowcss <- RcssGetProperties(Rcss, "pdf", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(..., file=file))
  ## execute R's graphics function with custom properties
  do.call(pdf, nowcss)
}



## plot(x, y, ...)
##
##' Create a styled plot
##' 
##' Rcssplot is a wrapper for R's plot() function.
##' See R's documentation for plot() for further details.
##'
##' @param x,y coordinates for points on the plot
##' @param Rcss style sheet object. Leave "default" to use a style
##' defined via RcssSetDefaultStyle()
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of plot()
##' @export 
Rcssplot <- function(x, y,
                     Rcss = "default", Rcssclass = NULL, ...) {
  ## convert between a description of a default Rcss to an actual object
  if (identical(Rcss, "default")) {
    Rcss <- getOption("RcssDefaultStyle", default = NULL);
  }
  ## get a list of properties
  nowcss <- RcssGetProperties(Rcss, "plot", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(...))
  ## execute R's graphics function with custom properties
  if (!missing(y)) {
    cmd <- paste0("plot (x, y",
                  RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
  } else {
    cmd <- paste0("plot (x,",
                  RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
  }
  
  eval(parse(text = cmd))
}



## png(filename = "Rplot%03d.png",
##          width = 480, height = 480, units = "px", pointsize = 12,
##          bg = "white",  res = NA, ...,
##         type = c("cairo", "cairo-png", "Xlib", "quartz"), antialias)
##     
##' Create a styled png figure
##'
##' Rcsspng is a wrapper for R's png() function.
##' See R's documentation for png() for further details
##'
##' @param file character string with file name
##' @param Rcss style sheet object. Leave "default" to use a style
##' defined via RcssSetDefaultStyle()
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of png()
##' @export 
Rcsspng <- function(file,
                    Rcss = "default", Rcssclass = NULL, ...) {
  ## convert between a description of a default Rcss to an actual object
  if (identical(Rcss, "default")) {
    Rcss <- getOption("RcssDefaultStyle", default = NULL);
  }
  ## get a list of properties
  nowcss <- RcssGetProperties(Rcss, "png", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(file=file, ...))
  ## execute R's graphics function with custom properties
  do.call(png, nowcss)
}



## points(x, y = NULL, type = "p", ...)
##
##' Add styled points to a plot
##' 
##' Rcsspoints is a wrapper for R's points() function.
##' See R's documentation for points() for further details.
##'
##' @param x,y coordinates for points on the plot
##' @param Rcss style sheet object. Leave "default" to use a style
##' defined via RcssSetDefaultStyle()
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of points()
##' @export 
Rcsspoints <- function(x, y = NULL,
                       Rcss = "default", Rcssclass = NULL, ...) {
  ## convert between a description of a default Rcss to an actual object
  if (identical(Rcss, "default")) {
    Rcss <- getOption("RcssDefaultStyle", default = NULL);
  }
  ## get a list of properties
  nowcss <- RcssGetProperties(Rcss, "points", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(x=x, y=y, ...))
  ## execute R's graphics function with custom properties
  do.call(points, nowcss)
}



## polygon(x, y = NULL, density = NULL, angle = 45,
##             border = NULL, col = NA, lty = par("lty"),
##             ..., fillOddEven = FALSE)
##
##' Draw a styled polygon on a plot
##' 
##' Rcsspolygon is a wrapper for R's polygon() function.
##' See R's documentation for polygon() for further details.
##'
##' @param x,y coordinates for polygon vertices
##' @param Rcss style sheet object. Leave "default" to use a style
##' defined via RcssSetDefaultStyle()
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of polygon()
##' @export 
Rcsspolygon <- function(x, y = NULL,
                        Rcss = "default", Rcssclass = NULL, ...) {
  ## convert between a description of a default Rcss to an actual object
  if (identical(Rcss, "default")) {
    Rcss <- getOption("RcssDefaultStyle", default = NULL);
  }
  ## get a list of properties
  nowcss <- RcssGetProperties(Rcss, "polygon", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(x=x, y=y, ...))
  ## execute R's graphics function with custom properties
  do.call(polygon, nowcss)
}



##rect(xleft, ybottom, xright, ytop, density = NULL, angle = 45,
##          col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"),
##          ...)
##
##' Draw styled rectangles on a plot
##' 
##' Rcssrect is a wrapper for R's rect() function.
##' See R's documentation for rect() for further details.
##'
##' @param xleft,ybottom,xright,ytop vector of coordinates for rectangles'
##' vertices
##' @param Rcss style sheet object. Leave "default" to use a style
##' defined via RcssSetDefaultStyle()
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of rect()
##' @export 
Rcssrect <- function(xleft, ybottom, xright, ytop, 
                     Rcss = "default", Rcssclass = NULL, ...) {
  ## convert between a description of a default Rcss to an actual object
  if (identical(Rcss, "default")) {
    Rcss <- getOption("RcssDefaultStyle", default = NULL);
  }
  ## get a list of properties
  nowcss <- RcssGetProperties(Rcss, "rect", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss,
                                 list(xleft=xleft, ybottom=ybottom,
                                      xright=xright, ytop=ytop, ...))
  ## execute R's graphics function with custom properties
  do.call(rect, nowcss)
}


## stripchart(x, ...)
##
##' Draw styled strip chart
##' 
##' Rcssstripchart is a wrapper for R's stripchart() function.
##' See R's documentation for stripchart() for further details.
##'
##' @param x list of numeric vectors
##' @param Rcss style sheet object. Leave "default" to use a style
##' defined via RcssSetDefaultStyle()
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of stripchart()
##' @export 
Rcssstripchart <- function(x,
                           Rcss = "default", Rcssclass = NULL, ...) {
  ## convert between a description of a default Rcss to an actual object
  if (identical(Rcss, "default")) {
    Rcss <- getOption("RcssDefaultStyle", default = NULL);
  }
  ## get a list of properties
  nowcss <- RcssGetProperties(Rcss, "stripchart", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(x=x, ...))
  ## execute R's graphics function with custom properties
  do.call(stripchart, nowcss)
}



## text(x, y = NULL, labels = seq_along(x), adj = NULL,
##          pos = NULL, offset = 0.5, vfont = NULL,
##          cex = 1, col = NULL, font = NULL, ...)
##
##' Add styled text to a plot
##' 
##' Rcsstext is a wrapper for R's text() function.
##' See R's documentation for text() for further details.
##'
##' @param x,y coordinates where to write labels
##' @param labels characters to print on the plot
##' @param Rcss style sheet object. Leave "default" to use a style
##' defined via RcssSetDefaultStyle()
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of text()
##' @export 
Rcsstext <- function(x, y=NULL, labels=seq_along(x),
                     Rcss = "default", Rcssclass = NULL, ...) {
  ## convert between a description of a default Rcss to an actual object
  if (identical(Rcss, "default")) {
    Rcss <- getOption("RcssDefaultStyle", default = NULL);
  }
  ## get a list of properties
  nowcss <- RcssGetProperties(Rcss, "text", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss, list(x=x, y=y, labels=labels, ...))
  ## execute R's graphics function with custom properties
  do.call(text, nowcss)
}



## title(main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
##           line = NA, outer = FALSE, ...)
##
##' Add styled annotation to a plot
##' 
##' Rcsstitle is a wrapper for R's title() function.
##' See R's documentation for title() for further details.
##' 
##' @param main plot title
##' @param sub plot sub title
##' @param xlab,ylab labels on axes
##' @param Rcss style sheet object. Leave "default" to use a style
##' defined via RcssSetDefaultStyle()
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of title()
##' @export 
Rcsstitle <- function(main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                      Rcss = "default", Rcssclass = NULL, ...) {
  ## convert between a description of a default Rcss to an actual object
  if (identical(Rcss, "default")) {
    Rcss <- getOption("RcssDefaultStyle", default = NULL);
  }
  ## get a list of properties
  nowcss <- RcssGetProperties(Rcss, "title", Rcssclass = Rcssclass)
  nowcss <- RcssUpdateProperties(nowcss,
                                 list(main=main, sub=sub,
                                      xlab=xlab, ylab=ylab, ...))
  ## execute R's graphics function with custom properties
  do.call(title, nowcss)
}


