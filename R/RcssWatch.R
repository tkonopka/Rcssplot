# Functions part of Rcssplot package
#
# Watch capabilities for faster development of Rcssplot styles and custom functions
#  
# Author: Tomasz Konopka


# This is used to determine how the watch function listens to more/stop cues
options(Rcssplot.connection = stdin())


#' development tool for adjusting Rcss and R graphics code
#'
#' This is a macro script that loads R code and a default Rcss style,
#' and then executes a function. This process is repeated indefinitely.
#'
#' @export
#' @param f function or character of function name, executed at each iteration
#' @param files character, paths to R and Rcss files
#' @param ... other arguments, passed to function f
#'
#' @examples
#'
#' # Note: the examples below draw a charat once and exit.
#' # To enable quick re-drawing, RcssWatch must be provided with file paths
#'
#' # draw and redraw a bar plot
#' RcssWatch(plot, x=1:4, y=1:4)
#'
#' # alternative syntax, using a function name as a string
#' custom.barplot <- function(x=1:4, main="") { barplot(x, main=main) }
#' RcssWatch("custom.barplot", main="Custom")
#' 
#' # for more interesting behavior, specify a files with styles and R source
#'
RcssWatch <- function(f, files=NULL, ...) {
  
  print.space = function(x) {
    message(paste("\n", x, "\n", sep=""))
  }
  
  # split up the files by extension
  Rcss.files = grep("css$", files, value=TRUE)
  R.files = grep("[R|r]$", files, value=TRUE)
  
  continue <- TRUE
  while (continue) {
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
    if (identical(files, NULL) | length(files)==0) {
      message("stopping - no files to watch")
      continue <- FALSE
    } else {
      message("Press [enter] to re-run, or [q] and [enter] to stop")
      response <-readLines(con=getOption("Rcssplot.connection"), n=1)
      continue <- !startsWith(response, "q")
    }
    
  }
}

