# File part of Rcssplot package
# These functions define and display objects of class Rcss
#
# Author: Tomasz Konopka


# Package imports
#' @import stats
#' @import utils
#' @import graphics
NULL


#########################################################
# Object of class Rcss

#' Create an Rcss style object
#'
#' Creates a style sheet object using definition specified in an
#' Rcss file. When a file is not specified, creates a base object
#' object without any styling.
#'
#' See also related functions RcssGetDefaultStyle() and RcssOverload().
#' 
#' @export
#' @param file filename containing Rcss definitions. If set to NULL,
#' function returns a basic Rcss object. If multiple files, function
#' reads each one and produces a joint style.
#'
#' @return Rcss object
Rcss <- function(file = NULL) {
  
  # create the css object
  ans <- RcssConstructor()
  
  # if user does not specify a css file, return bare-bones object
  if (is.null(file)) {
    return(ans)    
  }
  
  # get a parsetree for the input file
  parsetree <- RcssParser(file)
  if (length(parsetree) < 1) {
    return(ans)
  }

  # get names of all selectors and make sure that they are added to ans
  allselectors <- lapply(parsetree, function(x) {
    nowselset <- x$SelectorSet
    nowselectors <- sapply(nowselset, function(y) {y[1]})
    return (nowselectors)
  })
  allselectors = unique(unlist(allselectors))
  allselectors = allselectors[allselectors != ""]
  for (nowselector in allselectors) {
    ans[[nowselector]] = RcssPropertiesConstructor()
  }
  
  # walk through the parsed css and add all the properties/values
  # into the Rcss object
  for (i in 1:length(parsetree)) {
    ## for each selector set, declaration set
    nowselset <- parsetree[[i]]$SelectorSet
    nowdecset <- parsetree[[i]]$DeclarationSet
    # because selector set can contain mulitple selectors
    # loop over each selector
    for (j in 1:length(nowselset)) {
      nowselset2 = nowselset[[j]];
      # here nowselset2 should contain
      # c(SELECTOR, CLASS, SUBCLASS, SUBSUBCLASS, ...)
      nowRcssclass <- NULL
      if (length(nowselset2) > 1) {
        nowRcssclass <- nowselset2[-1]
      } 
      # record cascading classes into the Rcss object      
      ans <- RcssChange(selector = nowselset2[1],
                        Rcssclass = nowRcssclass,
                        propertylist = nowdecset,
                        Rcss=ans)
    }
  }
      
  invisible(ans)  
}


#########################################################
# Displaying information in Rcss object


#' Show basic information about an Rcss object
#'
#' Display selectors encoded in an Rcss object.
#' For more detailed information about the object, see function printRcss()
#' 
#' @export
#' @param x style sheet object
#' @param ... Further parameters are ignored
print.Rcss <- function(x, ...) {
  if (class(x) != "Rcss") {
    stopCF("print.Rcss: input object is not Rcss\n")
  }
  cat("Rcssplot:\n")
  cat("Defined selectors: ", paste(names(x), collapse = ", "), "\n")
  cat("Use function printRcss() to view details for individual selectors\n")
}


#' Display properties encoded in an Rcss object
#'
#' Display properties encoded in an Rcss object, including any subclasses.
#' 
#' @export
#' @param Rcss style sheet object
#' @param selector character string with name of selector to print
#' @param verbose logical. If TRUE, function prints all information
#' about the selector, including subclasses. If FALSE, function omits
#' detailed information about subclasses. 
printRcss <- function(Rcss, selector = NULL, verbose = FALSE) {
  
  # some basic checks
  if (class(Rcss) != "Rcss") {
    stopCF("printRcss: input object is not Rcss\n")
  }
  
  # if selector is not specified, print all the available ones
  if (is.null(selector)) {
    stopCF(paste0("printRcss: must specify selector.\n",
               "Defined selectors: ",paste(names(Rcss), collapse = ", "),
               "\n"))
  }
  if (!(selector %in% names(Rcss))) {
    stopCF("printRcss: ",
           "selector ", selector, " is not specified in Rcss object\n")
  }
  if (!is.logical(verbose)) {
    stopCF("printRcss: verbose must be logical\n")
  }
    
  # now print information stored within the css object
  cat("Rcssplot:", selector, "\n")
  print.RcssProperties(Rcss[[selector]], verbose = verbose)
  cat("\n")
}


#' print a set of property key/value pairs
#' 
#' @keywords internal
#' @param RcssProperties object
#' @param verbose logical
#' @param indent integer, prefix capturing spaces for indentation
print.RcssProperties <- function(RcssProperties,
                                 verbose = FALSE, indent="   ") {

  # prints on screen (property: value) pairs
  printPropertySet = function(propset) {
    if (length(propset) > 0) {
      for (i in names(propset)) {
        cat(paste0(indent, "| ",  i, ": ",paste(propset[[i]], collapse=" "),
                "\n"))
      }
    }    
  }

  baseRcss <- RcssProperties$base
  classesRcss <- RcssProperties$classes  
  printPropertySet(baseRcss)
  
  if (verbose) {
    for (nowclass in names(classesRcss)) {
      cat("\n")
      cat(paste0(indent,"class ",nowclass,"\n"))
      nowclass = classesRcss[[nowclass]]
      print.RcssProperties(nowclass, verbose = verbose,
                           indent = paste0("  ", indent))
    }        
  }  else {
    cat("\n")
    cat(paste0(indent, "Defined classes: ",
            paste(names(classesRcss), collapse = ", "), "\n"))   
  }    
}


#########################################################
# Defaults


#' Default Rcssplot style sheet
#'
#' This style sheet will be applied in all functions of the Rcss family.
#' 
#' @export
RcssDefaultStyle <- NULL


#' Get default Rcssplot style object
#'
#' Fetches the value of the RcssDefaultStyle object defined in
#' parent environments. 
#'
#' @export 
#' @param Rcss Rcss object, replacement default style object. When
#' set to "default", the function returns a copy of the default object
#' defined in parent environment. When set to Rcss object, the function
#' ignores the default and returns the set object back.
RcssGetDefaultStyle <- function(Rcss="default") {
  # perhaps ignore the current default and return the new object
  if (class(Rcss) == "Rcss") {
    return(Rcss)    
  }
  if (is.na(Rcss) | is.null(Rcss) >0) {
    return(Rcss)  
  }
  # if here, the user is not asking to reset the default.
  # So fetch current default object from parent environments
  RcssGetDefault("RcssDefaultStyle")  
}


#' Vector holding set a compulsory Rcssclass
#'
#' These style class (or classes) are applied in all functions of
#' the Rcss family.
#' 
#' @export
RcssCompulsoryClass <- c()


#' Get current state of compulsory Rcssclass
#'
#' Fetches the value of the RcssCompulsoryClass object defined in
#' parent environments. 
#' 
#' @export
#' @param Rcssclass character vector, set of additional compulsory classes.
#' When NULL, function returns the current set of compulsory classes
#' defined in parent environments. When non-NULL, functions returns
#' the concatentation of the current set and new set. 
RcssGetCompulsoryClass <- function(Rcssclass=NULL) {
  nowclass <- RcssGetDefault("RcssCompulsoryClass")      
  unique(c(nowclass, Rcssclass))    
}


#' gets an object from a calling environment (recursively)
#'
#' note: automatic object fetching uses chain of binding environments,
#' while this uses parent.frame() chain
#' 
#' @keywords internal
#' @param what character, use either "RcssDefaultStyle" or "RcssCompulsoryClass"
RcssGetDefault <- function(what) {
  # parent frame counter
  n <- 0
  parent <- parent.frame()
  
  # check all parents
  # what is the implementation of parent.frame(n)? and is this a
  # performance O(n^2) issue? In practice the depth of the environment
  # stack is not large, so should not be significant
  empty <- emptyenv()
  glob <- globalenv()    
  while (n==0 | (!identical(parent, empty) & !identical(parent, glob))) {
    n <- n+1
    parent <- parent.frame(n)
    if (exists(what, parent, inherits=F)) {
      return(get(what, parent, inherits=F))
    }
  }
  
  # if reached here, not found
  NULL
}


#########################################################
# Create the structure of each Rcss element
# (plot, text, points, etc.)


#' make Rcss structure for one Rcss selector
#' (includes space for base properties and for subclasses)
#'
#' @keywords internal
RcssPropertiesConstructor <- function() {
  # RcssProperties object will contain two lists,
  # one list for basic properties
  # one list for subclasses
  result <- list(base=list(), classes=list())
  class(result) <- "RcssProperties"
  result
}


#' creates an empty Rcss object (i.e. just a list with a class name)
#' the items inserted into this list/object will be called "selectors"
#'
#' @keywords internal
RcssConstructor <- function() {  
  result <- list()
  class(result) <- "Rcss"  
  result
}


#' object maintenance
#'
#' @keywords internal
#' @param RcssProperties object
#' @param Rcssclass character, style class
#'
#' @return true if the Rcssclass has been defined for all the selectors
RcssPropertiesContainsClass <- function(RcssProperties, Rcssclass) {
  (Rcssclass %in% names(RcssProperties$classes))  
}


#########################################################
## Functions to overload base graphics


#' Overloads base graphics functions by their Rcssplot wrappers
#' 
#' Rcssplot graphics functions have 'Rcss' prefixes,
#' e.g Rcsstext(). This function can be invoked to overload 
#' base-graphics functions by their Rcss wrappers. i.e. After executing
#' this function, you can execute e.g. text() and
#' automatically use the Rcss capabilities.
#'
#' Warning: this function creates masking objects in your current
#' environment for many base-graphics functions. See documentation
#' for details.
#' 
#' @export 
RcssOverload = function() {

  msg = c("RcssOverload is deprecated and redundant.",
          "All Rcssplot wrappers already mask graphics functions")
  warning(paste(msg, collapse="\n"))

  if (FALSE) {
    # a vector with all the wrappers
    overload <- c("abline", "arrows", "axis", "barplot", "box",
                  "boxplot", "contour", "grid", "hist", "jpeg", "legend",
                  "lines", "matplot", "mtext", "text", "par", "pdf", "plot",
                  "png", "points", "polygon", "rect", "stripchart", "text",
                  "title")
    
    # create and evaluate commands to overload standard functions by Rcss
    for (x in overload) {
      eval(parse(text=paste0(x, " = function(...) { Rcss", x, "(...) }")),
           envir=parent.frame())
    }
  }
}

