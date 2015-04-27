##
## File part of Rcssplot package
## Functions provide some generic add-ons 
## 
##
##
## Author: Tomasz Konopka
##


##
## Helper function stopCF = stop with call. = FALSE
##
stopCF <- function(...) {
  stop(..., call. = FALSE)
}



##
## Helper function produces code of the type
## "col=VALUE, lty=VALUE, " (with comma at the end)
## given set of argumes ("col","lty") and a list containing
## the values
##
RcssMakeCallCodeString <- function(varnames, listname) {
  if (length(varnames) == 0) {
    return ("")
  }
  ans = sapply(as.list(varnames), function(x) {
    return (paste0(x, "=", listname, "$", x))
  })  
  return (paste0(", ",paste(ans, collapse = ", ")));    
}





