## Tests for parsing files
##

library("Rcssplot")
source("00.helpers.R")

## Start the test

cattitle("Test set: 01.parsing")

## Loading/parsing

style.files = paste0("data/style.", seq(1, 4), ".Rcss")
styles = list()

## Initialize blank style
styles$zero = Rcss()
checkcat(length(styles$zero), 0, "constructor")

## Load style 1 
styles$one = Rcss(style.files[1])
checkcat(length(styles$one), 1, "style.1")

## Load style 2
styles$two = Rcss(style.files[2])
checkcat(length(styles$two), 1, "style.2")

## Load style 3
styles$three = Rcss(style.files[3])
checkcat(length(styles$three), 2, "style.3")

## Load composite styles (multiple files)
styles$onethree = Rcss(style.files[1:3])
checkcat(length(styles$onethree), 3, "composite styles")

## Extracting single value from Rcss
style13 = styles$onethree
temp.pch = RcssGetPropertyValue(style13, "points", "pch")$value
checkcat(temp.pch, 19, "get one value")

## Extractign multiple values from Rcss
temp.col = RcssGetPropertyValue(style13, "points", "col")$value
checkcat(temp.col, c("#777777", "#ff0000", "#00ff00"),
         "get vector values")

## Extracting something undefined
temp.lwd = RcssGetPropertyValue(style13, "points", "lwd")$defined
checkcat(temp.lwd, FALSE, "get undefined values")

## Fetching with defaults
temp.cex = RcssGetPropertyValueOrDefault(style13, "text", "cex", default=3)
checkcat(temp.cex, 2, "get with default (return value)")

## Fetching to return a default
temp.cx = RcssGetPropertyValueOrDefault(style13, "text", "cx", default=3)
checkcat(temp.cx, 3, "get with default (return default)")


cat("\n")

