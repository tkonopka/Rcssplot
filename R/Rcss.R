##
## File part of Rcssplot package
## These functions define and manipualte objects of class Rcss
##
##
## Author: Tomasz Konopka
##



#########################################################
## Object of class Rcss

##' Create an Rcss style object
##'
##' Creates a style sheet object using definition specified in an
##' Rcss file. When a file is not specified, creates a base object
##' object without any styling.
##'
##' See also related functions RcssGetDefaultStyle(), RcssSetDefaultStyle(),
##' and RcssOverload()
##' 
##' @param file filename containing Rcss definitions. If set to NULL,
##' function returns a basic Rcss object. If multiple files, function
##' reads each one and produces a joint style.
##' @param default (logical) Should the style object be used as a default
##' style? See also RcssGetDefaultStyle() and RcssSetDefaultStyle()
##' @param overload (logical) Should the standard graphics functions be
##' overloaded by their Rcss wrappers? See also RcssOverload()
##' 
##' @export 
Rcss <- function(file = NULL, default = FALSE, overload = FALSE) {
  
  ## create the css object
  ans <- RcssConstructor()
  
  ## if user does not specify a css file, return bare-bones object
  if (is.null(file)) {
    return(ans)
    if (default) {
      RcssSetDefaultStyle(NULL, overload = FALSE)
    }
    if (overload) {
      eval(parse(text = "RcssOverload()"), envir = parent.frame())
    }
  }
  
  ## get a parsetree for the input file
  parsetree <- RcssParser(file)

  ## use the parse tree to change the basic Rcss
  if (length(parsetree) < 1) {
    return(ans)
  }

  ## get names of all selectors and make sure that they are added to ans
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

  ## walk through the parsed css and add all the properties/values
  ## into the Rcss object
  for (i in 1:length(parsetree)) {
    ## for each selector set, declaration set
    nowselset <- parsetree[[i]]$SelectorSet
    nowdecset <- parsetree[[i]]$DeclarationSet
    ## because selector set can contain mulitple selectors
    ## loop over each selector
    for (j in 1:length(nowselset)) {
      nowselset2 = nowselset[[j]];
      ## here nowselset2 should contain
      ## c(SELECTOR, CLASS, SUBCLASS, SUBSUBCLASS)
      if (length(nowselset2) > 1) {
        nowRcssclass <- nowselset2[2:length(nowselset2)]
      } else {
        nowRcssclass <- NULL
      }
      ## record cascading classes into the Rcss object      
      ans <- RcssChangePropertyValue(ans, selector = nowselset2[1],
                                     Rcssclass = nowRcssclass,
                                     propertylist = nowdecset)
    }
  }
  
  ## perhaps record this style sheet as a default
  if (default) {    
    RcssSetDefaultStyle(ans, overload = FALSE)
  }
  ## perhaps also overload base graphics functions
  if (overload) {
    eval(parse(text = "RcssOverload()"), envir = parent.frame())
  }
  
  return(ans)  
}




##' Set default Rcssplot style sheet
##'
##' Set a default Rcssplot style sheet. This style sheet will be
##' applied in all functions of the Rcss family. See also related
##' function RcssGetDefaultStyle()
##' 
##' @param Rcss style sheet object (set NULL to remove default style)
##' @param overload logical. Should base graphics function be overloaded
##' by the Rcssplot wrappers?
##' 
##' @export
RcssSetDefaultStyle <- function(Rcss, overload = FALSE) {
  ## default style is remembered using the options/getOption system
  if (class(Rcss)=="Rcss") {
    options(RcssDefaultStyle=Rcss)
  } else if (class(Rcss)=="NULL") {
    options(RcssDefaultStyle=NULL)
  }
  if (overload) {
    eval(parse(text="RcssOverload()"), envir=parent.frame())
  }
}




##' Get current default Rcssplot style sheet
##'
##' Get a copy of the current default Rcssplot style sheet. See also
##' related RcssSetDefaultStyle()
##' 
##' @export
RcssGetDefaultStyle <- function() {
  ## Dummy; does the same as a straight call to getOption().
  ## However, it provides an interface with an "Rcss" feel.
  ## Also, the user does not need to know the "RcssDeaultsStyle" label
  return(getOption("RcssDefaultStyle", default = NULL))
}




## Function from file zzz.R by cuche27 - loads a default style
## when the package is loaded. Is this really necessary?
## .onLoad <- function(libname, pkgname) {
##  options(RcssDefaultStyle=Rcss())
##}




##' Show basic information about an Rcss object
##'
##' Display selectors encoded in an Rcss object.
##' For more detailed information about the object, see function printRcss()
##' 
##' @param x style sheet object
##' @param ... Further parameters are ignored
##' @export 
print.Rcss <- function(x, ...) {  
  cat("Rcssplot:\n")
  cat("Defined selectors: ", paste(names(x), collapse = ", "), "\n")
  cat("Use function printRcss() to view details for individual selectors\n")
}




##' Display properties encoded in an Rcss object
##'
##' Display properties encoded in an Rcss object, including any subclasses.
##' 
##' @param Rcss style sheet object
##' @param selector character string with name of selector to print
##' @param verbose logical. If TRUE, function prints all information
##' about the selector, including subclasses. If FALSE, function omits
##' detailed information about subclasses. 
##' @export 
printRcss <- function(Rcss, selector = NULL, verbose = FALSE) {
  
  ## some basic checks
  if (class(Rcss) != "Rcss") {
    stopCF("printRcss: input object is not Rcss\n")
  }
  
  ## if selector is not specified, print all the available ones
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
    
  ## now print information stored within the css object
  cat("Rcssplot:", selector, "\n")
  print.RcssProperties(Rcss[[selector]], verbose = verbose)
  cat("\n")
  
}




## Helper print function
## This one displays all property/value pairs in a RcssProperties object
##
print.RcssProperties <- function(RcssProperties,
                                 verbose = FALSE, indent="   ") {

  ## another helper function
  ## prints on screen (property: value) pairs
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
## Functions to create the structure of each Rcss element
## (plot, text, points, etc.)


## makes Rcss structure for one Rcss selector
## (includes space for base properties and for subclasses)
RcssPropertiesConstructor <- function() {
  ans <- list()
  ## RcssProperties object will contain two lists,
  ## one list for basic properties
  ## one list for subclasses
  ans$base <- list()
  ans$classes <- list()
  class(ans) <- "RcssProperties"
  return(ans)  
}


## helper function
## creates an empty Rcss object (i.e. just a list with a class name)
## the items inserted into this list/object will be called "selectors"
RcssConstructor <- function() {  
  ans <- list()
  class(ans) <- "Rcss"  
  return(ans)
}



## Helper function for Rcss object maintenance
## returns true if the Rcssclass has been defined for all the selectors
##
RcssPropertiesContainsClass <- function(RcssProperties, Rcssclass) {
  return (Rcssclass %in% names(RcssProperties$classes))  
}




#########################################################
## Functions to modify the structure of one Rcss object
## or one RcssProperties object
##


##' Modify an Rcss style sheet object
##' 
##' Creates a new Rcss style sheet object from the input, modifying
##' one or more properties.
##' 
##' @param Rcss style sheet object
##' @param selector name of one selector ("text", "plot", "axis", etc.)
##' @param Rcssclass subclass of style sheet. Leave NULL to change
##' base property. Provide one character value to edit one subclass.
##' Provide a vector to edit a subclass of a subclass of a ...
##' @param propertylist list with property/value pairs to update
##' @param property name of a single property. This is only used
##' when propertylist is set to NULL
##' @param value new values associated with property above. This is
##' only used propertylist is set to NULL
##' @export 
RcssChangePropertyValue <- function(Rcss, selector, Rcssclass = NULL, 
                                    propertylist = NULL,
                                    property = NULL, value = NULL) {

  ## handling special case when Rcss is not set or set at default
  if (identical(Rcss, "default")) {
    Rcss <- getOption("RcssDefaultStyle", default = RcssConstructor())
  }
  if (is.null(Rcss)) {    
    Rcss <- RcssConstructor()
  }
  
  ## sanity check, function only works to modify valid Rcss objects
  if (class(Rcss) != "Rcss") {
    stopCF("RcssChangePropertyValue: input is not Rcss \n")
  }
  ## check that the selector is one of the accepted ones
  if (length(selector) != 1) {
    stopCF("RcssChangePropertyValue: specify one selector\n")
  }
  if (!(selector %in% names(Rcss)) & selector != "") {
    Rcss[[selector]] <- RcssPropertiesConstructor()
  }

  ## Handle property/value pairs
  ##
  ## For useability, allow user not to specify either a property list
  ## or one property/value at a time.
  ## 
  ## For implementation, reduce both cases to a property list  
  if (is.null(propertylist)) {
    propertylist <- list()
    if (is.null(property)) {
      stopCF(paste0("RcssChangePropertyValue: ",
                 "specify at least one property to change\n"))
    }
    propertylist[[property]] <- value
  }
  
  ## if selector is blank, apply to all selector
  if (selector == "") {
    selector <- names(Rcss)
  }  
  
  ## after all the checks,
  ## call the helper function that edits RcssProperties objects
  for (nowselector in selector) {    
    Rcss[[nowselector]] <-
      RcssPropertiesChangeValue(Rcss[[nowselector]],
                                Rcssclass = Rcssclass,
                                propertylist = propertylist)
  }
  return (Rcss)
}



## Helper function to RcssChangePropertyValue (very similar name...)
##
##
RcssPropertiesChangeValue <- function(RcssProperties,
                                      Rcssclass = NULL,
                                      propertylist = NULL) {      
  
  ## When multiple classes/subclasses, need to apply recursion
  if (length(Rcssclass) > 1) {
    firstclass <- Rcssclass[1]
    if (!RcssPropertiesContainsClass(RcssProperties, firstclass)) {
      RcssProperties$classes[[firstclass]] <- RcssPropertiesConstructor()
    }
    ## update the properties of the subclass recursively
    RcssProperties$classes[[firstclass]] <-
      RcssPropertiesChangeValue(RcssProperties$classes[[firstclass]],
                                Rcssclass[2:length(Rcssclass)],
                                propertylist = propertylist)
    return(RcssProperties)
  }
  
  ## Add all the properties to this selector
  if (is.null(Rcssclass)) {
    ## add to the base properties
    for (nowprop in names(propertylist)) {
      RcssProperties$base[[nowprop]] <- propertylist[[nowprop]]
    }      
  } else {
    ## create a subclass if it does not exist
    if (is.null(RcssProperties$classes[[Rcssclass]])) {
      RcssProperties$classes[[Rcssclass]] <- RcssPropertiesConstructor()
    }
    for (nowprop in names(propertylist)) {
      RcssProperties$classes[[Rcssclass]]$base[[nowprop]] <- 
        propertylist[[nowprop]]
    }      
  }    
    
  return(RcssProperties)
}









## Extracts a value for one property in one selector
## (Used internally, but also useful for user)
##' Extracts a value for an Rcss property
##' 
##' Extracts a value for a property from an Rcss style sheet object. Returns
##' a list with two items. "Defined" is a boolean that indicates the
##' property is defined in the style sheet. "Value" gives the actual value
##' of the property.
##' 
##' @param Rcss style sheet object
##' @param selector name of selector of interest (e.g. "plot", "axis",
##' "text", etc.)
##' @param property name of property of interest (e.g. "col", "pch", etc.)
##' @param Rcssclass subclass of style sheet
##' @export 
RcssGetPropertyValue <- function(Rcss, selector, property,
                                 Rcssclass = NULL) {

  ## handling special case when Rcss is not set or set at default
  if (identical(Rcss, "default")) {
    Rcss <- getOption("RcssDefaultStyle", default = NULL)
  }
  if (is.null(Rcss)) {
    ans <- list(defined = FALSE, value = NULL)
    return (ans)
  }

  ## case when the selector is not specified (avoids work)
  if (!(selector %in% names(Rcss))) {
    ##warning("RcssGetPropertyValue: absent selector: ",selector,"\n")
    ans <- list(defined = FALSE, value = NULL)
    return(ans)
  }
  
  ## get properties for the selector
  bestvalue <- RcssHelperGetPropertyValue(Rcss[[selector]],
                                          property, Rcssclass = Rcssclass)
  
  ## check if the property was defined and what its value was
  ans <- list(defined = FALSE, value = NULL)
  if (bestvalue$level >= 0) {
    ans$defined <- TRUE
    if (!identical(bestvalue$value, NULL)) {
      ans[["value"]] <- bestvalue$value
    }
  }
  
  return(ans)
}




##' Extracts a value for an Rcss property
##' 
##' If the requested property is defined within an Rcss object, this
##' function will return the associated value. If the property is not
##' defined, the function returns a default value that can be passed
##' into the function and is set NULL otherwise. See also related function
##' RcssGetPropertyValue()
##' 
##' @param Rcss style sheet object
##' @param selector name of selector of interest (e.g. "plot", "axis",
##' "text", etc.)
##' @param property name of property of interest (e.g. "col", "pch", etc.)
##' @param default value to return if the desired property is not defined
##' in Rcss
##' @param Rcssclass subclass of style sheet
##' @export 
RcssGetPropertyValueOrDefault <- function(Rcss, selector, property,
                                          default=NULL,
                                          Rcssclass = NULL) {

  ## deal with case where input is not specified
  if (is.null(Rcss)) {
    return(default)
  }
  
  ans <- RcssGetPropertyValue(Rcss, selector, property, Rcssclass = Rcssclass);
  if (ans$defined) {
    if (identical(ans$value, "NULL")) {
      return (NULL)
    } else if (identical(ans$value, "NA")) {
      return (NA)
    } else {
      return (ans$value)
    }
  } else {
    return (default)
  }    
}




## This is a helper function for RcssGetPropertyValue
## It is a recursive function that traverses the style sheets
## and subclasses breadth first and obtains values for the property
## returns a list(level, values)
## (From this, another function can choose between competing values)
## inclass - vector of class names (used in recursion)
## bestvalue - list with best-guess property value
RcssHelperGetPropertyValue <- function(RcssProperties, property,
                                       Rcssclass = NULL,
                                       inclass = c(), bestvalue = NULL) {

  ## create a first bestvalue with no value (level set to negative)
  if (is.null(bestvalue)) {
    bestvalue <- list(level = -1, value = 0)
  }
  
  ## check for the property in the base properties
  if (property %in% names(RcssProperties$base)) {
    if (length(inclass) >= bestvalue$level) {
      ## replace the current best value with this one here
      bestvalue$level <- length(inclass)
      if (identical(RcssProperties$base[[property]], NULL)) {
        bestvalue["value"] <- list(NULL)      
      } else {
        bestvalue[["value"]] <- RcssProperties$base[[property]]
      }      
    }
  }
  
  ## recursively check all the subclasses
  ## This is a depth-first search, but the results with the   
  nowRcssclasses <- RcssProperties$classes
  ## loop over all classes asked for by the user
  for (nowclass in names(nowRcssclasses)) {
    ## check that the definition of this subclass is consistent
    ## with the desired class
    tryinclass <- c(inclass, nowclass)
    if (sum(tryinclass %in% Rcssclass) == length(tryinclass)) {
      bestvalue <-
        RcssHelperGetPropertyValue(
                                   nowRcssclasses[[nowclass]],
                                   property, Rcssclass = Rcssclass, 
                                   inclass = tryinclass,
                                   bestvalue = bestvalue)
    } 
  }
  
  return(bestvalue)  
}







#####################################################
## Functions used in the wrappers

## Helper functions, returns a vector with all the properties listed
## under "base" or under "base" in subclasses
getAllProperties <- function(RcssProperties) {

  ## get a vector with base properties
  baseproperties <- names(RcssProperties$base)

  ## recursively look at all subclasses
  subclassproperties <- list()
  for (nowclass in names(RcssProperties$classes)) {
    subclassproperties[[nowclass]] <-
      getAllProperties(RcssProperties$classes[[nowclass]])
  }
  
  return(unique(c(baseproperties, unlist(subclassproperties))))  
}


##
## get a list of properties relevant for a given selector and Rcssclasses
## (Used internally)
##
RcssGetProperties <- function(Rcss, selector, Rcssclass = NULL) {

  ## if there is no Rcss, return an empty list
  if (is.null(Rcss)) {
    return(list())
  }

  ## get the RcssProperties object for this selector
  nowProperties <- Rcss[[selector]]

  ## avoid work if the selector is not styled
  if (is.null(nowProperties)) {
    return(list());
  }
  
  ## get a vector with all the property codes associated with the selector
  allproperties <- getAllProperties(nowProperties)

  ## find the best value for each property, one by one
  ans <- list();
  for (nowprop in allproperties) {
    temp <-
      RcssHelperGetPropertyValue(nowProperties, nowprop,
                                 Rcssclass = Rcssclass)
    if (temp$level >= 0) {
      if (identical(temp$value, NULL)) {
        ans[nowprop] <- list(NULL)
      } else if (identical(temp$value, NA)) {
        ans[nowprop] <- list(NA)
      } else {
        if (length(temp$value)==1) {
          if (identical(temp$value, "NULL")) {
            ans[nowprop] <- list(NULL)
          } else if (identical(temp$value, "NA")) {
            ans[nowprop] <- list(NA)
          } else {
            ans[[nowprop]] <- temp$value
          }
        } else {
          ans[[nowprop]] <- temp$value
        }
      }
    }
  }
  
  return(ans)  
}





##
## This updates a current set of properties and values
## nowcss - a starting list of properties
## changelist - a list of properties to update 
##
## returns - a list similar to nowcss, but with values updated according
##           to changelist
##
RcssUpdateProperties <- function(nowcss, changelist) {
  ## start with an updated css list that is equal to the first
  newcss <- nowcss
  
  ## supercede any existing values with values from userlist
  for (nowprop in names(changelist)) {
    ## when a property is set to null
    ## need to leave it as null in the newcss object
    ## (To avoid removing the property from the list, need to handle special)
    if (identical(changelist[[nowprop]], NULL)) {
      newcss[nowprop] <- list(NULL)
    } else {
      newcss[[nowprop]] <- changelist[[nowprop]]
    }
  }
  
  ## return the updated css list
  return(newcss)  
}




#########################################################
## Functions to overload base graphics


##' Overloads base graphics functions by their Rcssplot wrappers
##' 
##' Rcssplot graphics functions are defined using Rcss prefixes,
##' e.g Rcsstext(). This function can be invoked to overload the
##' existing functions by their Rcss wrappers. i.e. After executing
##' this function, the user can write execute e.g. text() and
##' automatically use the Rcss capabilities.
##' 
##' @export 
RcssOverload = function() {

  ## a vector with all the wrappers
  overload <- c("abline", "arrows", "axis", "barplot", "box",
                "boxplot", "contour", "grid", "hist", "jpeg", "legend",
                "lines", "matplot", "mtext", "text", "par", "pdf", "plot",
                "png", "points", "polygon", "rect", "stripchart", "text",
                "title")
  
  ## create and evaluate commands to overload standard functions by Rcss
  for (x in overload) {
    eval(parse(text=paste0(x, " = function(...) { Rcss", x, "(...) }")),
         envir=parent.frame())
  }
  
}

