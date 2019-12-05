# File part of Rcssplot package
# Functions provide some generic add-ons 
# 
#
# Author: Tomasz Konopka


#' stop with call. = FALSE
stopCF <- function(...) {
  stop(..., call. = FALSE)
}


#' produces code of the type
#' "col=VALUE, lty=VALUE, " (with comma at the end)
#'
#' @param varnames character vector
#' @param listname character
#'
#' @return character
RcssMakeCallCodeString <- function(varnames, listname) {
  if (length(varnames) == 0) {
    return ("")
  }
  ans = sapply(as.list(varnames), function(x) {
    paste0(x, "=", listname, "$", x)
  })  
  paste0(", ",paste(ans, collapse = ", "))
}

