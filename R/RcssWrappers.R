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


RcssGeneric <- function(..., Rcss, Rcssclass, f) {

  RcssCall <- substitute(f(...))
  if(!missing(Rcss)){
    css <- RcssGetProperties(Rcss, as.character(RcssCall[[1]]), Rcssclass = Rcssclass)
    css[names(RcssCall)] <- NULL
    if(length(css) > 0) {
      i <- length(RcssCall) + seq_along(css)
      RcssCall[i] <- css
      names(RcssCall)[i] <- names(css)
    }
  }
  eval(RcssCall, 1);
}

#' RCss Wrapper functions
#' 
#' @export
#' @name  RcssWrappers
#' @aliases Rcssbox Rcssboxplot Rcsshist Rcssjpeg Rcsslegend Rcsslines Rcssmtext Rcsspar Rcsspdf Rcssplot Rcsspng Rcsspoints Rcsspolygon Rcssrect Rcssstripchart Rcsstext Rcsstitle
NULL

towrap <- list(
  quote(box),
  quote(boxplot),                   
  quote(hist),
  quote(jpeg),
  quote(legend),
  quote(lines),
  quote(mtext),
  quote(par),
  quote(pdf),
  quote(plot),
  quote(png),
  quote(points),
  quote(polygon),
  quote(rect),
  quote(stripchart),
  quote(text),
  quote(title)
)

f <- RcssGeneric
for(symbol in towrap){
  formals(f)$f <- symbol
  assign(paste0("Rcss", symbol), f)
}
rm(symbol, f, towrap)


