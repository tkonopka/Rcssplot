#
# File part of Rcssplot package
# These functions focus on extracting property/values from Rcss class objects
#
#
# Author: Tomasz Konopka
#


# Extracts a value for one property in one selector
# (Used internally, but also useful for user)
#
#' Extract a value for an Rcss property
#' 
#' Extract a value for a property from an Rcss style sheet object. Returns
#' a list with two items. "Defined" is a boolean that indicates the
#' property is defined in the style sheet. "Value" gives the actual value
#' of the property.
#' 
#' Equivalent to RcssProperty; use RcssProperty instead. 
#' 
#' @export 
#' @param Rcss style sheet object
#' @param selector name of selector of interest (e.g. "plot", "axis",
#' "text", etc.)
#' @param property name of property of interest (e.g. "col", "pch", etc.)
#' @param Rcssclass subclass of style sheet
RcssGetPropertyValue <- function(Rcss, selector, property,
                                 Rcssclass = NULL) {
  warning("RcssGetPropertyValue is deprecated. Use RcssProperty instead.")
  RcssProperty(selector, property, Rcssclass=Rcssclass, Rcss=Rcss)
}


#' Extract information about property and its value
#'
#' @export 
#' @param selector character, name of selector, e.g. 'points'
#' @param property character, name of property, e.g. 'col'
#' @param Rcssclass character or vector, subclass in Rcss
#' @param Rcss Rcss object
#'
#' @return list with two ites. Component "defined" is a boolean that indicates
#' whether the property is defined in the style. Component "value" gives
#' the actual value associated to the property. 
RcssProperty <- function(selector, property,
                         Rcssclass=NULL, Rcss="default") {
  
  # handling special case when Rcss is not set or set at default
  if (identical(Rcss, "default")) {
    Rcss <- RcssGetDefault("RcssDefaultStyle")
  }
  if (is.null(Rcss)) {
    ans <- list(defined = FALSE, value = NULL)
    return(ans)
  }

  # case when the selector is not specified (avoids work)
  if (!(selector %in% names(Rcss))) {
    ans <- list(defined = FALSE, value = NULL)
    return(ans)
  }
  
  # augment Rcssclass with a compulsory class
  Rcsscompulsory <- RcssGetDefault("RcssCompulsoryClass")
  Rcssclass <- unique(c(Rcsscompulsory, Rcssclass))

  # get properties for the selector
  bestvalue <- RcssHelperGetPropertyValue(Rcss[[selector]],
                                          property, Rcssclass = Rcssclass)
  
  # check if the property was defined and what its value was
  ans <- list(defined = FALSE, value = NULL)
  if (bestvalue$level >= 0) {
    ans$defined <- TRUE
    if (!identical(bestvalue$value, NULL)) {
      ans[["value"]] <- bestvalue$value
    }
  }
  
  ans
}


#' Extract a value for an Rcss property
#' 
#' If the requested property is defined within an Rcss object, this
#' function will return the associated value. If the property is not
#' defined, the function returns a default value that can be passed
#' into the function and is set NULL otherwise. See also
#' RcssGetPropertyValue().
#'
#' Equivalent to RcssValue(); use RcssValue() instead
#' 
#' @export
#' @param Rcss style sheet object
#' @param selector name of selector of interest (e.g. "plot", "axis",
#' "text", etc.)
#' @param property name of property of interest (e.g. "col", "pch", etc.)
#' @param default value to return if the desired property is not defined
#' in Rcss
#' @param Rcssclass subclass of style sheet
RcssGetPropertyValueOrDefault <- function(Rcss, selector, property,
                                          default=NULL,
                                          Rcssclass = NULL) {
  warning("RcssGetPropertyValueOrDefault is depreacted. Use RcssValue instead.")
  RcssValue(selector, property, default=default, Rcssclass=Rcssclass, Rcss=Rcss)
}


#' Extracts a value from an Rcss object
#'
#' If the selector and property are defined in the Rcss object,
#' this function will return the value stored in the Rcss object.
#' Otherwise, the function will return a default value.
#' See also related functions RcssGetPropertyValueOrDefault, which
#' is the same, except that RcssValue is shorter to write and takes
#' the Rcss object as its last argument.
#'
#' @export 
#' @param selector character, name of selector, e.g. 'points'
#' @param property character, name of property to get, e.g. 'col'
#' @param default value to return if selector/property are not defined
#' @param Rcssclass character or vector, subclass in Rcss
#' @param Rcss Rcss object
#'
#' @return a value from the Rcss object
RcssValue <- function(selector, property,
                      default=NULL, Rcssclass=NULL, Rcss="default") {
  
  # deal with case where input Rcss is not specified/default
  if (identical(Rcss, "default")) {
    Rcss <- RcssGetDefault("RcssDefaultStyle")
  }
  if (is.null(Rcss)) {
    return(default)
  }

  # augment Rcssclass with a compulsory class
  Rcsscompulsory <- RcssGetDefault("RcssCompulsoryClass") 
  Rcssclass <- unique(c(Rcsscompulsory, Rcssclass))
  
  ans <- RcssProperty(selector, property, Rcss=Rcss, Rcssclass = Rcssclass);
  if (!ans$defined) {
    ans$value <- default
  }
  ans$value
}


#' traverses the style sheets breadth first and obtains values for the property
#'
#' This is a helper function for RcssGetPropertyValue
#'
#' @keywords internal
#' @param RcssProperties object
#' @param property character
#' @param Rcssclass character, style class
#' @param inclass vector of class names (used in recursion)
#' @param bestvalue list with best-guess property value
#'
#' @return list(level, values)
#' (From this, another function can choose between competing values)
RcssHelperGetPropertyValue <- function(RcssProperties, property,
                                       Rcssclass = NULL,
                                       inclass = c(), bestvalue = NULL) {
  
  # create a first bestvalue with no value (level set to negative)
  if (is.null(bestvalue)) {
    bestvalue <- list(level = -1, value = 0)
  }
  
  # check for the property in the base properties
  if (property %in% names(RcssProperties$base)) {
    if (length(inclass) >= bestvalue$level) {
      # replace the current best value with this one here
      bestvalue$level <- length(inclass)
      if (identical(RcssProperties$base[[property]], NULL)) {
        bestvalue["value"] <- list(NULL)      
      } else {
        bestvalue[["value"]] <- RcssProperties$base[[property]]
      }      
    }
  }
  
  # recursively check all the subclasses
  # This is a depth-first search, but the results with the   
  nowRcssclasses <- RcssProperties$classes
  # loop over all classes asked for by the user
  for (nowclass in names(nowRcssclasses)) {
    # check that the definition of this subclass is consistent
    # with the desired class
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
  
  bestvalue
}


#####################################################
# Helpers used in the wrappers


#' get properties, recursively
#'
#' @keywords internal
#' @param RcssProperties object
#'
#' @return character vector
getAllProperties <- function(RcssProperties) {

  # get a vector with base properties
  baseproperties <- names(RcssProperties$base)

  # recursively look at all subclasses
  subclassproperties <- list()
  for (nowclass in names(RcssProperties$classes)) {
    subclassproperties[[nowclass]] <-
      getAllProperties(RcssProperties$classes[[nowclass]])
  }
  
  unique(c(baseproperties, unlist(subclassproperties)))
}


#' get a list of properties relevant for a given selector and Rcssclasses
#' (Used internally, 2 frames down from user interaction)
#'
#' @keywords internal
#' @param Rcss object
#' @param selector character
#' @param Rcssclass character, style class
RcssGetProperties <- function(Rcss, selector, Rcssclass = NULL) {

  # if there is no Rcss, return an empty list
  if (is.null(Rcss)) {
    return(list())
  }
  
  # if Rcss is default string, fetch the default style
  if (identical(Rcss, "default")) {
    Rcss <- RcssGetDefault("RcssDefaultStyle")
  }
  if (is.null(Rcss)) {
    Rcss <- RcssConstructor()
  }
  
  # get the RcssProperties object for this selector
  nowProperties <- Rcss[[selector]]

  # avoid work if the selector is not styled
  if (is.null(nowProperties)) {
    return(list())
  }

  # adjust Rcssclass with a compulsory class
  Rcsscompulsory = RcssGetDefault("RcssCompulsoryClass")
  Rcssclass = unique(c(Rcsscompulsory, Rcssclass))
  
  # get a vector with all the property codes associated with the selector
  allproperties <- getAllProperties(nowProperties)

  # find the best value for each property, one by one
  ans <- list();
  for (nowprop in allproperties) {
    temp <- RcssHelperGetPropertyValue(nowProperties, nowprop,
                                       Rcssclass = Rcssclass)
    if (temp$level >= 0) {
      if (is.null(temp$value)) {
        ans[nowprop] <- list(NULL)
      } else {
        ans[[nowprop]] = temp$value
      }
    }
  }
  
  ans
}

