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

RcssGeneric <- function(Rcss="default", Rcssclass, .f) {

  # construct call
  RcssCall <- match.call(.f, sys.call())
  RcssCall[[1]] = substitute(.f)
  
  
  if(!missing(Rcss) || !(formals()$Rcss == "")){
    ## convert between a description of a default Rcss to an actual object
    if (identical(Rcss, "default")) {
      Rcss <- getOption("RcssDefaultStyle", default = NULL);
    }
    
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
#' @name RcssWrappers
#' @aliases Rcssabline Rcssaxis Rcssbarplot Rcssbox Rcssboxplot Rcsshist Rcssjpeg Rcsslegend Rcsslines Rcssmtext Rcsspar Rcsspdf Rcssplot Rcsspng Rcsspoints Rcsspolygon Rcssrect Rcssstripchart Rcsstext Rcsstitle
#' @export  Rcssabline Rcssaxis Rcssbarplot Rcssbox Rcssboxplot Rcsshist Rcssjpeg Rcsslegend Rcsslines Rcssmtext Rcsspar Rcsspdf Rcssplot Rcsspng Rcsspoints Rcsspolygon Rcssrect Rcssstripchart Rcsstext Rcsstitle
NULL

towrap <- list(
  quote(graphics::abline),
  quote(graphics::axis),
  quote(graphics::barplot),
  quote(graphics::box),
  quote(graphics::boxplot),                   
  quote(graphics::hist),
  quote(graphics::legend),
  quote(graphics::lines),
  quote(graphics::mtext),
  quote(graphics::par),
  quote(graphics::plot),
  quote(graphics::points),
  quote(graphics::polygon),
  quote(graphics::rect),
  quote(graphics::stripchart),
  quote(graphics::text),
  quote(graphics::title),
  quote(grDevices::pdf),
  quote(grDevices::jpeg),
  quote(grDevices::png)
)

wrap <- function(symbol) {
  f <- RcssGeneric
  formals(f)$.f <- symbol
  formals(f) <- c(formals(eval(symbol)), formals(f))

  f  
}

for(symbol in towrap){
  assign(paste0("Rcss", symbol[[3]]), wrap(symbol))
}
rm(symbol)


#' RCSS - Don't repeat yourself 
#' 
#' @param Rcss the style sheet to reuse
#' @return a 'convert to Rcss' object
#' @export
#' 
dryer <- function(Rcss) {
  e <- new.env(parent = .GlobalEnv)
  CSS <- substitute(Rcss)
  
  for(symbol in towrap) {
    f <- wrap(symbol)
    formals(f)$Rcss <- CSS
    assign(as.character(symbol[[3]]), f, e)
  }
  
  
  e
}
