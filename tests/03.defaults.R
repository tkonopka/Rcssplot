## tests for setting/extracting default style and compulsory class
##

## tests for individual Rcss wrappers

library("Rcssplot")
source("00.helpers.R")

## Start the test

cattitle("Test set: 03.defaults")

## some working styles
style6 = Rcss("data/style.6.Rcss")

## some data objects
x0 = 1:5
names(x0) = letters[1:5]



## ##################
## Define some helper functions (very similar to vignette example)

## base graphics only
testplot1 <- function(x, main="Testplot1", submain="") {  
    barpos <- barplot(x, col="#000080", axes=FALSE, axisnames=FALSE, border=NA)
    axis(1, at=barpos[,1], labels=names(x), lwd=0, cex.axis=1.2, line=-0.35)
    axis(2, cex.axis=1.2, lwd=1.2, las=1, tck=-0.03, lwd.ticks=1.2)
    mtext(main, adj=0, line=2.2, cex=1.1)
    mtext(submain, adj=0, line=0.7, cex=1, col="#777777")
}
## with Rcssplot
testplot2 <- function(x, main="Testplot2", submain="",
                      Rcss="default", Rcssclass=c()) {
    barpos <- Rcssbarplot(x, axes=FALSE, axisnames=FALSE,
                          Rcss=Rcss, Rcssclass=Rcssclass)
    Rcssaxis(1, at=barpos[,1], labels=names(x),
             Rcss=Rcss, Rcssclass=c(Rcssclass,"x"))
    Rcssaxis(2, Rcss=Rcss, Rcssclass=c(Rcssclass,"y"))  
    Rcssmtext(main, Rcss=Rcss, Rcssclass=c(Rcssclass,"main"))
    Rcssmtext(submain, Rcss=Rcss, Rcssclass=c(Rcssclass,"submain"))
}
## uses overloading internally
testplot3 <- function(x, main="Testplot3", submain="",
                      Rcss="default", Rcssclass=c()) {
    RcssOverload()    
    barpos <- barplot(x, axes=FALSE, axisnames=FALSE,
                      Rcss=Rcss, Rcssclass=Rcssclass)
    axis(1, at=barpos[,1], labels=names(x),
         Rcss=Rcss, Rcssclass=c(Rcssclass, "x"))
    axis(2, Rcss=Rcss, Rcssclass=c(Rcssclass, "y"))  
    mtext(main, Rcss=Rcss, Rcssclass=c(Rcssclass, "main"))
    mtext(submain, Rcss=Rcss, Rcssclass=c(Rcssclass, "submain"))
}
## uses overloading and default styles
testplot4 <- function(x, main="Testplot4", submain="",
                      Rcss="default", Rcssclass=c()) {
    RcssOverload()
    RcssSetDefaultStyle(Rcss)
    RcssSetCompulsoryClass(Rcssclass)
    barpos <- barplot(x, axes=FALSE, axisnames=FALSE)
    axis(1, at=barpos[,1], labels=names(x), Rcssclass="x")
    axis(2, Rcssclass="y")  
    mtext(main, Rcssclass="main")
    mtext(submain, Rcssclass="submain")
}
## assumes global overloading
testplot5 <- function(x, main="Custom Rcss plot", submain="",
                      Rcssclass="typeB") {
    RcssSetCompulsoryClass(Rcssclass)
    barpos <- barplot(x, axes=FALSE, axisnames=FALSE)
    axis(1, at=barpos[,1], labels=names(x), Rcssclass="x")
    axis(2, Rcssclass="y")  
    mtext(main, Rcssclass="main")
    mtext(submain, Rcssclass="submain")
}



## Perform tests with various settings


pdf("figures/Rcss-defaults.pdf", width=9, height=9)
par(mfrow=c(3,3))

testplot1(x0, main="Base graphics", subm="Starting point")
testplot2(x0, main="Style6", subm="... should be same", Rcss=style6)
testplot3(x0, main="Local overL", subm="... should be same", Rcss=style6)
testplot4(x0, main="Local overL, defaults", subm="...should be same",
          Rcss=style6)
testplot4(x0, main="Loc overL, defaults, abc",  subm="... new color",
          Rcss=style6, Rcssclass="abc")

RcssOverload()
RcssSetDefaultStyle(style6)
testplot5(x0, main="Glob overL, Glob style", ".. back to normal")
testplot5(x0, main="Glob overL, Glob style", subm="..should be same")
RcssSetCompulsoryClass("abc")
testplot5(x0, main="Glob overL, Glob style class",
          submain="..should change color")

## because the style and class are set globally, testplot2 should
## give light bars even though this command would not have worked above
testplot2(x0, main="Style6", submain=".. should still be light")

dev.off()
checkcat(1,1, "figures/Rcss-defaults.pdf")
cat("\n")


## Book-keeping (in case test is run multiple times in one session)
RcssSetCompulsoryClass(NA)
RcssSetDefaultStyle(NA)
