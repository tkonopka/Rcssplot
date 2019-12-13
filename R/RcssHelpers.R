# File part of Rcssplot package
# Functions provide some generic add-ons 
# 
#
# Author: Tomasz Konopka


#' stop with call. = FALSE
#'
#' @keywords internal
#' @noRd
#' @param ... arguments passed to stop()
stopCF <- function(...) {
  stop(..., call. = FALSE)
}


#' trigger a stop, with a message 
#'
#' @keywords internal
#' @noRd
#' @param msg character, message to display
#' @param line.number integer, used as part of the error message
stopAtLine <- function(msg, line.number) {
  stop(paste0("line ", line.number, "\n", msg, "\n") , call.=FALSE)
}


#' produces code of the type
#' "col=VALUE, lty=VALUE, " (with comma at the end)
#'
#' @keywords internal
#' @noRd
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

