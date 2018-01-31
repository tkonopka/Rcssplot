## Tests for setting default styles and compulsory classes
##

cat("\ntest_defaults.R ")


## create a style for testing
styletext = "
text {
  cex: 1.5;
}
"
style = Rcss(tofile(styletext))


###############################################################################
## using a default Rcss style

test_that("can set default style", {
  RcssDefaultStyle = NULL
  cex1 = RcssGetPropertyValue("default", "text", "cex")$value
  expect_equal(cex1, NULL)
  RcssDefaultStyle = RcssGetDefaultStyle(style)
  cex2 = RcssGetPropertyValue("default", "text", "cex")$value
  expect_equal(cex2, 1.5)
})


test_that("can unset a default style", {
  mystyle = RcssGetDefaultStyle(style)
  expect_equal(class(mystyle), "Rcss")
  mystyle = RcssGetDefaultStyle(NA)
  expect_equal(mystyle, NA)
  RcssDefaultStyle = style
  mystyle = RcssGetDefaultStyle("default")
  expect_equal(style, mystyle)
})



###############################################################################
## using a compulsory class

test_that("can set compulsory class", {
  before = RcssGetCompulsoryClass(NULL)
  expect_equal(before, NULL)
  after = RcssGetCompulsoryClass("abc")
  expect_equal(after, "abc")
})

test_that("using a compulsory class in a function", {
  RcssCompulsoryClass = RcssGetCompulsoryClass("abc")
  myfun = function() {
    inner = RcssGetCompulsoryClass("xyz")
    inner
  }
  inner = myfun()
  expect_equal(inner, c("abc", "xyz"))
})




## cleanup
unlink(tofile(""))

