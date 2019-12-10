# File part of Rcssplot package
# These functions define and display objects of class Rcss
#
#
# Author: Tomasz Konopka



#########################################################
# Modify the structure of Rcss and RcssProperties objects


#' Modify an Rcss style sheet object
#' 
#' Creates a new Rcss style sheet object from the input, modifying
#' one or more properties.
#'
#' Equivallent to RcssChange: use RcssChange instead
#'
#' @export
#' @param Rcss style sheet object
#' @param selector name of one selector ("text", "plot", "axis", etc.)
#' @param Rcssclass subclass of style sheet. Leave NULL to change
#' base property. Provide one character value to edit one subclass.
#' Provide a vector to edit a subclass of a subclass of a ...
#' @param propertylist list with property/value pairs to update
#' @param property name of a single property. This is only used
#' when propertylist is set to NULL
#' @param value new values associated with property above. This is
#' only used propertylist is set to NULL
#'
#' @examples
#'
#' # use RcssChange instead
#'
RcssChangePropertyValue <- function(Rcss, selector, Rcssclass = NULL, 
                                    propertylist = NULL,
                                    property = NULL, value = NULL) {
  warning("RcssChangePropertyValue is deprecated. Use RcssChange instead.")
  RcssChange(selector, propertylist=propertylist,
             property=property, value=value,
             Rcssclass=Rcssclass,
             Rcss=Rcss)
}


#' Modify an Rcss style sheet object
#' 
#' Creates a new Rcss style sheet object from the input, modifying
#' one or more properties.
#' 
#' @export
#' @param selector name of one selector ("text", "plot", "axis", etc.)
#' @param propertylist list with property/value pairs to update
#' @param property name of a single property. This is only used
#' when propertylist is set to NULL
#' @param value new values associated with property above. This is
#' only used propertylist is set to NULL
#' @param Rcssclass subclass of style sheet. Leave NULL to change
#' base property. Provide one character value to edit one subclass.
#' Provide a vector to edit a subclass of a subclass of a ...
#' @param Rcss style sheet object
#'
#' @return always returns an Rcss object. Note: when changing
#' the default style, this will return a new style without
#' actually affecting the default style. To change how the
#' default works in practice, assign this return value to
#' RcssDefaultStyle
#'
#'
#' @examples
#'
#' style1 <- Rcss(text="points { cex: 1; pch: 19; }")
#' printRcss("points", Rcss=style1, verbose=TRUE)
#' style2 <- RcssChange("points", list(cex=2), Rcss=style1)
#' printRcss("points", Rcss=style2, verbose=TRUE)
#'
RcssChange <- function(selector,  
                       propertylist = NULL,
                       property = NULL, value = NULL,
                       Rcssclass = NULL,
                       Rcss="default") {
  
  # handling special case when Rcss is not set or set at default
  if (identical(Rcss, "default")) {
    Rcss <- RcssGetDefault("RcssDefaultStyle")
  }
  if (is.null(Rcss)) {    
    Rcss <- RcssConstructor()
  }
  
  # sanity check, function only works to modify valid Rcss objects
  if (class(Rcss) != "Rcss") {
    stopCF("RcssChange: input is not Rcss \n")
  }
  # check that the selector is one of the accepted ones
  if (length(selector) != 1) {
    stopCF("RcssChange: specify one selector\n")
  }
  if (!(selector %in% names(Rcss)) & selector != "") {
    Rcss[[selector]] <- RcssPropertiesConstructor()
  }

  # Handle property/value pairs
  #
  # For useability, allow user not to specify either a property list
  # or one property/value at a time.
  # 
  # For implementation, reduce both cases to a property list
  if (!is.null(propertylist)) {
    if (length(propertylist)>0 & is.null(names(propertylist))) {
      stopCF("RcssChange: property list must have names \n")
    }
  }
  if (is.null(propertylist)) {
    if (is.null(property)) {
      stopCF(paste0("RcssChange: ",
                 "specify at least one property to change\n"))
    }
    propertylist <- setNames(list(value), property)
  }
  
  # if selector is blank, apply to all selector
  if (selector == "") {
    selector <- names(Rcss)
  }  
  
  # after all the checks,
  # call the helper function that edits RcssProperties objects
  for (nowselector in selector) {    
    Rcss[[nowselector]] <-
      RcssPropertiesChangeValue(Rcss[[nowselector]],
                                Rcssclass = Rcssclass,
                                propertylist = propertylist)
  }
  Rcss
}


#' Helper to RcssChangePropertyValue (very similar name...)
#'
#' @keywords internal
#' @param RcssProperties object
#' @param Rcssclass character, style class
#' @param propertylist list
RcssPropertiesChangeValue <- function(RcssProperties,
                                      Rcssclass = NULL,
                                      propertylist = NULL) {      
  
  # When multiple classes/subclasses, need to apply recursion
  if (length(Rcssclass) > 1) {
    firstclass <- Rcssclass[1]
    if (!RcssPropertiesContainsClass(RcssProperties, firstclass)) {
      RcssProperties$classes[[firstclass]] <- RcssPropertiesConstructor()
    }
    # update the properties of the subclass recursively
    RcssProperties$classes[[firstclass]] <-
      RcssPropertiesChangeValue(RcssProperties$classes[[firstclass]],
                                Rcssclass[2:length(Rcssclass)],
                                propertylist = propertylist)
    return(RcssProperties)
  }
  
  # Add all the properties to this selector
  if (is.null(Rcssclass)) {
    # add to the base properties
    RcssProperties$base[names(propertylist)] = propertylist
  } else {
    # create a subclass if it does not exist
    if (is.null(RcssProperties$classes[[Rcssclass]])) {
      RcssProperties$classes[[Rcssclass]] <- RcssPropertiesConstructor()
    }
    for (nowprop in names(propertylist)) {
      RcssProperties$classes[[Rcssclass]]$base[[nowprop]] <- 
        propertylist[[nowprop]]
    }      
  }    
    
  RcssProperties
}


#' update a current set of properties and values
#'
#' @keywords internal
#' @param nowcss - a starting list of properties
#' @param changelist - a list of properties to update 
#'
#' @return a list similar to nowcss, but with values updated according
#' to changelist
RcssUpdateProperties <- function(nowcss, changelist) {
  # start with an updated css list that is equal to the first
  newcss <- nowcss
  
  # supercede any existing values with values from userlist
  for (nowprop in names(changelist)) {
    # when a property is set to null
    # need to leave it as null in the newcss object
    # (To avoid removing the property from the list, need to handle special)
    if (identical(changelist[[nowprop]], NULL)) {
      newcss[nowprop] <- list(NULL)
    } else {
      newcss[[nowprop]] <- changelist[[nowprop]]
    }
  }
  
  newcss
}

