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
  if(!missing(Rcss) || !(formals()$Rcss == "")){
    css <- RcssGetProperties(Rcss, as.character(RcssCall[[1]][[3]]), Rcssclass = Rcssclass)
    css[names(RcssCall)] <- NULL
    if(length(css) > 0) {
      i <- length(RcssCall) + seq_along(css)
      RcssCall[i] <- css
      names(RcssCall)[i] <- names(css)
    }
  }
  eval(RcssCall, -1);
}

#' RCss Wrapper functions
#' 
#' @export
#' @name  RcssWrappers
#' @aliases Rcssbox Rcssboxplot Rcsshist Rcssjpeg Rcsslegend Rcsslines Rcssmtext Rcsspar Rcsspdf Rcssplot Rcsspng Rcsspoints Rcsspolygon Rcssrect Rcssstripchart Rcsstext Rcsstitle
NULL

towrap <- list(
  quote(graphics::box),
  quote(graphics::boxplot),                   
  quote(graphics::hist),
  quote(graphics::jpeg),
  quote(graphics::legend),
  quote(graphics::lines),
  quote(graphics::mtext),
  quote(graphics::par),
  quote(graphics::pdf),
  quote(graphics::plot),
  quote(graphics::png),
  quote(graphics::points),
  quote(graphics::polygon),
  quote(graphics::rect),
  quote(graphics::stripchart),
  quote(graphics::text),
  quote(graphics::title)
)

f <- RcssGeneric
for(symbol in towrap){
  formals(f)$f <- symbol
  assign(paste0("Rcss", symbol[[3]]), f)
}
rm(symbol, f)


#' RCSS - Don't repeat yourself 
#' 
#' @param 
#' @return a 'convert to Rcss' object
#' 
dryer <- function(Rcss) {
  e <- new.env(parent = .GlobalEnv)
  CSS <- substitute(Rcss)
  
  f <- RcssGeneric
  for(symbol in towrap) {
    formals(f)$f <- symbol
    formals(f)$Rcss <- CSS
    assign(as.character(symbol[[3]]), f, e)
  }
  
  
  e
}




