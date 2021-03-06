# tests for producing figures using Rcss wrappers, overloading, etc
# tests for setting/extracting default style and compulsory class
#


cat("\ntest_composite.R ")
source("helpers.R")


# some working styles
style6 = Rcss(file.path("data","style.6.Rcss"))
# some data objects
x0 = 1:5
names(x0) = letters[1:5]
# Book-keeping (in case test is run multiple times in one session)
RcssCompulsoryClass = NULL
RcssDefaultStyle = NULL


# uses test plot functions above to create a pdf figure
metaplot <- function(filename) {

  # base graphics only
  testplot1 <- function(x, main="Testplot1", submain="") {  
    barpos <- barplot(x, col="#000080", axes=FALSE, axisnames=FALSE, border=NA)
    axis(1, at=barpos[,1], labels=names(x), lwd=0, cex.axis=1.2, line=-0.35)
    axis(2, cex.axis=1.2, lwd=1.2, las=1, tck=-0.03, lwd.ticks=1.2)
    mtext(main, adj=0, line=2.2, cex=1.1)
    mtext(submain, adj=0, line=0.7, cex=1, col="#777777")
  }
  # with Rcssplot
  testplot2 <- function(x, main="Testplot2", submain="",
                        Rcss="default", Rcssclass=c()) {
    barpos <- barplot(x, axes=FALSE, axisnames=FALSE,
                          Rcss=Rcss, Rcssclass=Rcssclass)
    axis(1, at=barpos[,1], labels=names(x),
             Rcss=Rcss, Rcssclass=c(Rcssclass,"x"))
    axis(2, Rcss=Rcss, Rcssclass=c(Rcssclass,"y"))  
    mtext(main, Rcss=Rcss, Rcssclass=c(Rcssclass,"main"))
    mtext(submain, Rcss=Rcss, Rcssclass=c(Rcssclass,"submain"))
  }
  # uses overloading internally
  testplot3 <- function(x, main="Testplot3", submain="",
                        Rcss="default", Rcssclass=c()) {   
    barpos <- barplot(x, axes=FALSE, axisnames=FALSE,
                      Rcss=Rcss, Rcssclass=Rcssclass)
    axis(1, at=barpos[,1], labels=names(x),
         Rcss=Rcss, Rcssclass=c(Rcssclass, "x"))
    axis(2, Rcss=Rcss, Rcssclass=c(Rcssclass, "y"))  
    mtext(main, Rcss=Rcss, Rcssclass=c(Rcssclass, "main"))
    mtext(submain, Rcss=Rcss, Rcssclass=c(Rcssclass, "submain"))
  }
  # uses overloading and default styles
  testplot4 <- function(x, main="Testplot4", submain="",
                        Rcss="default", Rcssclass=c()) {
    RcssDefaultStyle = RcssGetDefaultStyle(Rcss)
    RcssCompulsoryClass = RcssGetCompulsoryClass(Rcssclass)
    barpos <- barplot(x, axes=FALSE, axisnames=FALSE)
    axis(1, at=barpos[,1], labels=names(x), Rcssclass="x")
    axis(2, Rcssclass="y")  
    mtext(main, Rcssclass="main")
    mtext(submain, Rcss=NULL, Rcssclass="submain")
  }
  # assumes global overloading
  testplot5 <- function(x, main="Custom Rcss plot", submain="",
                        Rcssclass="typeB") {
    RcssCompulsoryClass = RcssGetCompulsoryClass(Rcssclass)
    barpos <- barplot(x, axes=FALSE, axisnames=FALSE)
    axis(1, at=barpos[,1], labels=names(x), Rcssclass="x")
    axis(2, Rcssclass="y")  
    mtext(main, Rcssclass="main")
    mtext(submain, Rcssclass="submain")
  }
  
  
  pdf(filename, width=9, height=9)
  par(mfrow=c(3,3))
  
  testplot1(x0, main="Base graphics", subm="Starting point")
  testplot2(x0, main="Style6", subm="... should be same", Rcss=style6)
  testplot3(x0, main="Local overL", subm="... should be same", Rcss=style6)
  testplot4(x0, main="Local overL, defaults", subm="...should be same",
            Rcss=style6)
  testplot4(x0, main="Loc overL, defaults, abc",  subm="... new color",
            Rcss=style6, Rcssclass="abc")
  
  RcssDefaultStyle = style6
  testplot5(x0, main="Glob overL, Glob style", ".. back to normal")
  testplot5(x0, main="Glob overL, Glob style", subm="..should be same")
  RcssCompulsoryClass = "abc"
  testplot5(x0, main="Glob overL, Glob style class",
            submain="..should change color")
  
  # because the style and class are set globally, testplot2 should
  # give light bars even though this command would not have worked above
  testplot2(x0, main="Style6", submain=".. should still be light")
  
  dev.off()
}


###############################################################################


test_that("plot creation with/without overloading", {
  # getting a filename here removes any existing copy
  filename = testfile("overloading")
  # so testing for existence should give false, even if test is re-run
  expect_false(file.exists(filename))
  expect_silent(metaplot(filename))
  expect_true(file.exists(filename))
})

