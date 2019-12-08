# Functions part of Rcssplot package
#
# Watch capabilities for faster development of Rcssplot styles and custom functions
#  
# Author: Tomasz Konopka


#' development tool for adjusting Rcss and R graphics code
#'
#' This is a macro script that loads R code and a default Rcss style,
#' and then executes a function. This process is repeated indefinitely.
#'
#' @export
#' @param f function or character of function name, executed at each iteration
#' @param Rcss.files character, paths to Rcss files
#' @param R.files character, paths to R files
#' @param ... other arguments, passed to function f
#'
#' @examples
#'
#' # this repeatedly draws and redraws a bar plot
#' RcssWatch(plot, x=1:4, y=1:4)
#' # for more interesting behavior, specify a files with styles and R source
#'
RcssWatch <- function(f, Rcss.files=NULL, R.files=NULL, ...) {

  print.space = function(x) {
    cat(paste("\n", x, "\n", sep=""))
  }
  
  first <- TRUE
  continue <- TRUE
  while (continue) {
    if (!first) {
      invisible(readline(prompt="Press [enter] to re-run, or [Ctrl-C] to stop"))
    }
    # reload Rcss and code
    tryCatch({
      for (x in R.files) {
        source(x)
      }
      RcssDefaultStyle <- Rcss(Rcss.files)
      # execute the watched function
      if (is(f, "character")) {
        eval(parse(text=paste(f, "(...)")))
      } else if (is(f, "function")) {
        f(...)
      }
    }, warning=print.space, error=print.space)
    
    # abort if there is no need to watch the Rcss or R files
    first <- FALSE
    if (identical(Rcss.files, NULL) & identical(R.files, NULL)) {
      message("stopping - no files to watch\n")
      continue <- FALSE
    }
  }
}

