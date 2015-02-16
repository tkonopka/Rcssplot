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
## Note 2:
##
## The is.null(Rcss) statement surrounding each body is not elegant
## either. It is there to improve performance when an Rcss object
## is absent or does not specify settings for a function.
##
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
##' @param Rcss style sheet object
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of abline()
##' @export
Rcssabline <-  function(a = NULL, b = NULL, h = NULL, v = NULL,
                        reg = NULL, coef = NULL,
                        Rcss = NULL, Rcssclass = NULL, ...) {
  ## possibly make a shortcut if Rcss object is missing
  if (is.null(Rcss) | !("abline" %in% names(Rcss))) {
    abline(a = a, b = b, h = h, v = v, reg = reg, coef = coef, ...)
  } else {
    ## get a list of properties
    nowcss <- RcssGetProperties(Rcss, "abline", Rcssclass = Rcssclass)
    nowcss <- RcssUpdateProperties(nowcss, list(...))
    ## execute R's graphics function with custom properties
    cmd <- PSZ("abline (a = a, b = b, h = h, v = v, ",
               "reg = reg, coef = coef",             
               RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
    eval(parse(text = cmd))
  }  
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
##' @param Rcss style sheet object
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of axis()
##' @export 
Rcssaxis <-  function(side,
                      Rcss = NULL, Rcssclass = NULL, ...) {
  ## possibly make a shortcut if Rcss object is missing
  if (is.null(Rcss) | !("axis" %in% names(Rcss))) {
    axis(side, ...)
  } else {
    ## get a list of properties
    nowcss <- RcssGetProperties(Rcss, "axis", Rcssclass = Rcssclass)
    nowcss <- RcssUpdateProperties(nowcss, list(...))
    ## execute R's graphics function with custom properties
    cmd <- PSZ("axis (side",
               RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
    eval(parse(text = cmd))
  }
}



## barplot(height, ...)
##
##' Draw a styled barplot
##' 
##' Rcssbarplot is a wrapper for R's barplot() function.
##' See R's documentation for barplot() for further details.
##' 
##' @param height numeric vector giving bar lengths
##' @param Rcss style sheet object
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of barplot()
##' @export 
Rcssbarplot <- function(height,
                        Rcss = NULL, Rcssclass = NULL, ...) {
  ## possibly make a shortcut if Rcss object is missing
  if (is.null(Rcss) | !("barplot" %in% names(Rcss))) {
    barplot(height, ...)
  } else {
    ## get a list of properties
    nowcss <- RcssGetProperties(Rcss, "barplot", Rcssclass = Rcssclass)
    nowcss <- RcssUpdateProperties(nowcss, list(...))
    ## execute R's graphics function with custom properties
    cmd <- PSZ("barplot (height",
               RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
    eval(parse(text = cmd))
  }
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
##' @param Rcss style sheet object
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of box()
##' @export 
Rcssbox <-  function(which = "plot",
                     Rcss = NULL, Rcssclass = NULL, ...) {
  ## possibly make a shortcut if Rcss object is missing
  if (is.null(Rcss) | !("box" %in% names(Rcss))) {
    box(which = which, ...)
  } else {
    ## get a list of properties
    nowcss <- RcssGetProperties(Rcss, "box", Rcssclass = Rcssclass)
    nowcss <- RcssUpdateProperties(nowcss, list(...))
    ## execute R's graphics function with custom properties
    cmd <- PSZ("box (which = which",
               RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
    eval(parse(text = cmd))
  }
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
##' @param Rcss style sheet object
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of boxplot()
##' @export 
Rcssboxplot <-  function(x,
                         Rcss = NULL, Rcssclass = NULL, ...) {
  ## possibly make a shortcut if Rcss object is missing
  if (is.null(Rcss) | !("boxplot" %in% names(Rcss))) {
    boxplot(x, ...)
  } else {
    ## get a list of properties
    nowcss <- RcssGetProperties(Rcss, "boxplot", Rcssclass = Rcssclass)
    nowcss <- RcssUpdateProperties(nowcss, list(...))
    ## execute R's graphics function with custom properties
    cmd <- PSZ("boxplot (x",
               RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
    eval(parse(text = cmd))
  }
}



## hist(x, ...)
##
##' ##' Draw a styled histogram
##' 
##' Rcsshist is a wrapper for R's hist() function.
##' See R's documentation for hist() for further details.
##' 
##' @param x numeric vector
##' @param Rcss style sheet object
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of hist()
##' @export 
Rcsshist <- function(x,
                     Rcss = NULL, Rcssclass = NULL, ...) {
  ## possibly make a shortcut if Rcss object is missing
  if (is.null(Rcss) | !("hist" %in% names(Rcss))) {
    hist(x, ...)
  } else {
    ## get a list of properties
    nowcss <- RcssGetProperties(Rcss, "hist", Rcssclass = Rcssclass)
    nowcss <- RcssUpdateProperties(nowcss, list(...))
    ## execute R's graphics function with custom properties
    cmd <- PSZ("hist (x",
               RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
    eval(parse(text = cmd))
  }
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
##' @param Rcss style sheet object
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of jpeg()
##' @export 
Rcssjpeg <- function(file,
                    Rcss = NULL, Rcssclass = NULL, ...) {
  ## possibly make a shortcut if Rcss object is missing
  if (is.null(Rcss) | !("jpeg" %in% names(Rcss))) {
    jpeg(file, ...)
  } else {    
    ## get a list of properties
    nowcss <- RcssGetProperties(Rcss, "jpeg", Rcssclass = Rcssclass)
    nowcss <- RcssUpdateProperties(nowcss, list(...))
    ## execute R's graphics function with custom properties
    cmd <- PSZ("jpeg (file",
               RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
    eval(parse(text = cmd))
  }
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
##' @param Rcss style sheet object
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of legend()
##' @export 
Rcsslegend <- function(x, y = NULL, legend,
                       Rcss = NULL, Rcssclass = NULL, ...) {  
  ## possibly make a shortcut if Rcss object is missing
  if (is.null(Rcss) | !("legend" %in% names(Rcss))) {
    legend(x, y = y, legend = legend, ...)
  } else {    
    ## get a list of properties
    nowcss <- RcssGetProperties(Rcss, "legend", Rcssclass = Rcssclass)
    nowcss <- RcssUpdateProperties(nowcss, list(...))
    ## execute R's graphics function with custom properties
    cmd <- PSZ("legend (x, y = y, legend = legend",
               RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
    cat(cmd,"\n")
    eval(parse(text = cmd))
  }
}



## lines(x, y = NULL, type = "l", ...)
##
##' Add styled line segments to a plot
##' 
##' Rcsslines is a wrapper for R's lines() function.
##' See R's documentation for lines() for further details.
##'
##' @param x,y coordinates for start and end points for lines
##' @param Rcss style sheet object
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of lines()
##' @export 
Rcsslines <- function(x, y = NULL,
                      Rcss = NULL, Rcssclass = NULL, ...) {
  ## possibly make a shortcut if Rcss object is missing
  if (is.null(Rcss) | !("legend" %in% names(Rcss))) {
    lines(x, y = y, ...)
  } else {    
    ## get a list of properties
    nowcss <- RcssGetProperties(Rcss, "lines", Rcssclass = Rcssclass)
    nowcss <- RcssUpdateProperties(nowcss, list(...))
    ## execute R's graphics function with custom properties
    cmd <- PSZ("lines (x, y = y",
               RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
    eval(parse(text = cmd))
  }
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
##' @param Rcss style sheet object
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of mtext()
##' @export 
Rcssmtext <- function(text,
                      Rcss = NULL, Rcssclass = NULL, ...) {
  ## possibly make a shortcut if Rcss object is missing
  if (is.null(Rcss) | !("mtext" %in% names(Rcss))) {
    mtext(text, ...)
  } else {
    ## get a list of properties
    nowcss <- RcssGetProperties(Rcss, "mtext", Rcssclass = Rcssclass)
    nowcss <- RcssUpdateProperties(nowcss, list(...))
    ## execute R's graphics function with custom properties
    cmd <- PSZ("mtext (text",
               RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
    eval(parse(text = cmd))
  }
}



## par(...)
##
##' Set styled parameters for base graphics
##'
##' Rcsspar is a wrapper for R's par() function.
##' See R's documentation for par() for further details.
##'
##' @param Rcss style sheet object
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of lines()
##' @export 
##' 
Rcsspar <- function(Rcss = NULL, Rcssclass = NULL, ...) {
  ## possibly make a shortcut if Rcss object is missing
  if (is.null(Rcss) | !("par" %in% names(Rcss))) {
    par(...)
  } else {    
    ## get a list of properties
    nowcss <- RcssGetProperties(Rcss, "par", Rcssclass = Rcssclass)
    nowcss <- RcssUpdateProperties(nowcss, list(...))
    ## execute R's graphics function with custom properties
    ## here cut out the "comma" inserted automatically by RcssMakeCallCodeString
    temp = RcssMakeCallCodeString(names(nowcss), "nowcss")
    cmd <- PSZ("par (", substring(temp, 2), ")")
    eval(parse(text = cmd))
  }
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
##' @param Rcss style sheet object
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of pdf()
##' @export 
Rcsspdf <- function(file,
                    Rcss = NULL, Rcssclass = NULL, ...) {
  ## possibly make a shortcut if Rcss object is missing
  if (is.null(Rcss) | !("pdf" %in% names(Rcss))) {
    pdf(file, ...)
  } else {        
    ## get a list of properties
    nowcss <- RcssGetProperties(Rcss, "pdf", Rcssclass = Rcssclass)
    nowcss <- RcssUpdateProperties(nowcss, list(...))
    ## execute R's graphics function with custom properties
    cmd <- PSZ("pdf (file",
               RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
    eval(parse(text = cmd))
  }
}



## plot(x, y, ...)
##
##' Create a styled plot
##' 
##' Rcssplot is a wrapper for R's plot() function.
##' See R's documentation for plot() for further details.
##'
##' @param x,y coordinates for points on the plot
##' @param Rcss style sheet object
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of plot()
##' @export 
Rcssplot <- function(x, y,
                     Rcss = NULL, Rcssclass = NULL, ...) {
  ## possibly make a shortcut if Rcss object is missing
  if (is.null(Rcss) | !("plot" %in% names(Rcss))) {
    plot(x, y, ...)
  } else {        
    ## get a list of properties
    nowcss <- RcssGetProperties(Rcss, "plot", Rcssclass = Rcssclass)
    nowcss <- RcssUpdateProperties(nowcss, list(...))
    ## execute R's graphics function with custom properties
    cmd <- PSZ("plot (x, y",
               RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
    eval(parse(text = cmd))
  }
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
##' @param Rcss style sheet object
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of png()
##' @export 
Rcsspng <- function(file,
                    Rcss = NULL, Rcssclass = NULL, ...) {
  ## possibly make a shortcut if Rcss object is missing
  if (is.null(Rcss) | !("png" %in% names(Rcss))) {
    png(file, ...)
  } else {        
    ## get a list of properties
    nowcss <- RcssGetProperties(Rcss, "png", Rcssclass = Rcssclass)
    nowcss <- RcssUpdateProperties(nowcss, list(...))
    ## execute R's graphics function with custom properties
    cmd <- PSZ("png (file",
               RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
    eval(parse(text = cmd))  
  }
}



## points(x, y = NULL, type = "p", ...)
##
##' Add styled points to a plot
##' 
##' Rcsspoints is a wrapper for R's points() function.
##' See R's documentation for points() for further details.
##'
##' @param x,y coordinates for points on the plot
##' @param Rcss style sheet object
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of points()
##' @export 
Rcsspoints <- function(x, y = NULL,
                       Rcss = NULL, Rcssclass = NULL, ...) {
  ## possibly make a shortcut if Rcss object is missing
  if (is.null(points) | !("points" %in% names(Rcss))) {
    points(x, y = y, ...)
  } else {        
    ## get a list of properties
    nowcss <- RcssGetProperties(Rcss, "points", Rcssclass = Rcssclass)
    nowcss <- RcssUpdateProperties(nowcss, list(...))
    ## execute R's graphics function with custom properties
    cmd <- PSZ("points (x, y = y",
               RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
    eval(parse(text = cmd))
  }
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
##' @param Rcss style sheet object
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of polygon()
##' @export 
Rcsspolygon <- function(x, y = NULL,
                        Rcss = NULL, Rcssclass = NULL, ...) {
  ## possibly make a shortcut if Rcss object is missing
  if (is.null(Rcss) | !("polygon" %in% names(Rcss))) {
    polygon(x, y = y, ...)
  } else {
    ## get a list of properties
    nowcss <- RcssGetProperties(Rcss, "polygon", Rcssclass = Rcssclass)
    nowcss <- RcssUpdateProperties(nowcss, list(...))
    ## execute R's graphics function with custom properties
    cmd <- PSZ("polygon (x, y = y",
               RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
    eval(parse(text = cmd))
  }
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
##' @param Rcss style sheet object
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of rect()
##' @export 
Rcssrect <- function(xleft, ybottom, xright, ytop, 
                     Rcss = NULL, Rcssclass = NULL, ...) {
  ## possibly make a shortcut if Rcss object is missing
  if (is.null(Rcss) | !("rect" %in% names(Rcss))) {
    rect(xleft, ybottom, xright, ytop, ...)
  } else {    
    ## get a list of properties
    nowcss <- RcssGetProperties(Rcss, "rect", Rcssclass = Rcssclass)
    nowcss <- RcssUpdateProperties(nowcss, list(...))
    ## execute R's graphics function with custom properties
    cmd <- PSZ("rect (xleft, ybottom, xright, ytop",
               RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
    eval(parse(text = cmd))
  }
}


## stripchart(x, ...)
##
##' Draw styled strip chart
##' 
##' Rcssstripchart is a wrapper for R's stripchart() function.
##' See R's documentation for stripchart() for further details.
##'
##' @param x list of numeric vectors
##' @param Rcss style sheet object
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of stripchart()
##' @export 
Rcssstripchart <- function(x,
                           Rcss = NULL, Rcssclass = NULL, ...) {
  ## possibly make a shortcut if Rcss object is missing
  if (is.null(Rcss) | !("stripchart" %in% names(Rcss))) {
    stripchart(x, ...)
  } else {    
    ## get a list of properties
    nowcss <- RcssGetProperties(Rcss, "stripchart", Rcssclass = Rcssclass)
    nowcss <- RcssUpdateProperties(nowcss, list(...))
    ## execute R's graphics function with custom properties
    cmd <- PSZ("stripchart (x",
               RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
    eval(parse(text = cmd))
  }
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
##' @param Rcss style sheet object
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of text()
##' @export 
Rcsstext <- function(x, y=NULL, labels=seq_along(x),
                     Rcss = NULL, Rcssclass = NULL, ...) {
  ## possibly make a shortcut if Rcss object is missing
  if (is.null(Rcss) | !("text" %in% names(Rcss))) {
    text(x, y = y, labels = labels, ...)
  } else {
    ## get a list of properties
    nowcss <- RcssGetProperties(Rcss, "text", Rcssclass = Rcssclass)
    nowcss <- RcssUpdateProperties(nowcss, list(...))
    ## execute R's graphics function with custom properties
    cmd <- PSZ("text (x, y = y, labels = labels",
               RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
    eval(parse(text = cmd))
  }
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
##' @param Rcss style sheet object
##' @param Rcssclass sub class of style sheet
##' @param ... Further parameters, see documentation of title()
##' @export 
Rcsstitle <- function(main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                      Rcss = NULL, Rcssclass = NULL, ...) {
  ## possibly make a shortcut if Rcss object is missing
  if (is.null(Rcss) | !("title" %in% names(Rcss))) {
    title (main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
  } else {        
    ## get a list of properties
    nowcss <- RcssGetProperties(Rcss, "title", Rcssclass = Rcssclass)
    nowcss <- RcssUpdateProperties(nowcss, list(...))
    ## execute R's graphics function with custom properties
    cmd <- PSZ("title (main = main, sub = sub, xlab = xlab, ylab = ylab",
               RcssMakeCallCodeString(names(nowcss), "nowcss"), ")")
    eval(parse(text = cmd))
  }
}


