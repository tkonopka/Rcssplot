##
## File part of Rcssplot package
## This file is named such that it runs at the end of a package load
##
##
## Author: Tomasz Konopka
##



## create RcssDefaultStyle and RcssCompulsoryClass objects
## in the global environment
.onLoad <- function(libname, pkgname){
  RcssDefaultStyle <<- RcssConstructor()
  RcssCompulsoryClass <<- c()
}

