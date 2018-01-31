## Tests for getting values from Rcss objects
##

cat("\ntest_get.R ")
source("helpers.R")

## create a style for testing
styletext = "
points {
  cex: 2 4;
  col: #ff0000 #00ff00 #0000ff88;
  lwd: 1.5;
  pch: 19;
}
plot {
  xaxs: i;
  frame: FALSE;
  xlab: abctext there;
  ylab: \"Two words\";
  main: ;
}
plot.escaped {
  main: \"Title is 'ABC'\";
  xlab: \"escaped \\\\\";
}
text, mtext {
  cex: 1;
}
text.subclass {
  cex: 0.75;
}
rect {
  col: NULL;
  border: NA;
}
.all {
  xpd: 1;
}
"
style = Rcss(tofile(styletext))




###############################################################################
## extracting data types from style sheets


test_that("get from empty style", {
  pch = RcssProperty("points", "pch", Rcss=NULL)
  expect_false(pch$defined)
})


test_that("get from an undefined selector", {
  type = RcssProperty("missing", "hello", Rcss=style)
  expect_false(type$defined)
  expect_equal(type$value, NULL)
})


test_that("get an undefined property", {
  type = RcssProperty("points", "type", Rcss=style)$defined
  expect_false(type)
})


test_that("get single integer", {
  pch = RcssProperty("points", "pch", Rcss=style)
  expect_true(pch$defined)
  expect_equal(pch$value, 19)
})


test_that("get single integer", {
  pch = RcssProperty("points", "pch", Rcss=style)
  expect_true(pch$defined)
  expect_equal(pch$value, 19)
})


test_that("get real numbers", {
  lwd = RcssProperty("points", "lwd", Rcss=style)
  expect_true(lwd$defined)
  expect_equal(lwd$value, 1.5)
})


test_that("get boolean", {
  ff = RcssProperty("plot", "frame", Rcss=style)
  expect_equal(ff$value, FALSE)
})


test_that("get NA", {
  ff = RcssProperty("rect", "border", Rcss=style)
  expect_true(ff$defined)
  expect_identical(ff$value, NA)
})


test_that("get NULL", {
  ff = RcssProperty("rect", "col", Rcss=style)
  expect_true(ff$defined)
  expect_equal(ff$value, NULL)
})


test_that("get string", {
  axs = RcssProperty("plot", "xaxs", Rcss=style)
  expect_equal(axs$value, "i")
})


test_that("get string without quotes", {
  lab = RcssProperty("plot", "xlab", Rcss=style)
  labval = paste(lab$value, collapse=" ")
  expect_equal(labval, "abctext there")
})


test_that("get string in quotes", {
  lab = RcssProperty("plot", "ylab", Rcss=style)
  labval = lab$value
  expect_equal(labval, "Two words")
})


test_that("get string with quote characters", {
  main = RcssProperty("plot", "main", Rcssclass="escaped", Rcss=style)
  mainval = main$value
  expect_equal(mainval, "Title is 'ABC'")
})


test_that("get strange case with escape characters", {
  main = RcssProperty("plot", "xlab", Rcssclass="escaped", Rcss=style)
  mainval = main$value
  expect_equal(mainval, "escaped \\\\")
})


test_that("get empty string", {
  main = RcssProperty("plot", "main", Rcss=style)
  expect_equal(main$value, "")
})


test_that("get multiple values from style", {
  col = RcssProperty("points", "col", Rcss=style)
  expect_equal(col$value, c("#ff0000", "#00ff00", "#0000ff88"))
})




###############################################################################
## using common properties for several selectors


test_that("get multiple values from style", {
  cex1 = RcssProperty(style, "text", "cex")
  cex2 = RcssProperty(style, "mtext", "cex")
  expect_equal(cex1$value, cex2$value)
})




###############################################################################
## using default values


test_that("get value, default not necessary", {
  cex = RcssValue("text", "cex", default=3, Rcss=style)
  expect_equal(cex, 1)
})


test_that("get value, default necessary", {
  abc = RcssValue("text", "abc", default=3, Rcss=style)
  expect_equal(abc, 3)
})


test_that("get value, default necessary when style absent", {
  abc = RcssValue("text", "abc", default=3, Rcss=NULL)
  expect_equal(abc, 3)
})


test_that("get value, using default style, default value available", {
  RcssDefaultStyle = style
  cex = RcssValue("text", "cex", default=3)
  expect_equal(cex, 1)
})


test_that("get value, using default style, default value necessary", {
  RcssDefaultStyle = style
  abc = RcssValue("text", "abc", default=3)
  expect_equal(abc, 3)
})




###############################################################################
## using subclasses


test_that("get using subclass", {
  cex1 = RcssProperty("text", "cex", Rcss=style)
  expect_equal(cex1$value, 1)
  cex2 = RcssProperty("text", "cex", Rcssclass="subclass", Rcss=style)
  expect_equal(cex2$value, 0.75)
})


test_that("get when subclass does not apply", {
  cex = RcssProperty("mtext", "cex", Rcssclass="subclass", Rcss=style)
  expect_equal(cex$value, 1)
})


test_that("get when subclass modifies all selectors", {
  xpd1 = RcssProperty("mtext", "xpd", Rcssclass="all", Rcss=style)
  expect_equal(xpd1$value, 1)
  xpd2 = RcssProperty("points", "xpd", Rcssclass="all", Rcss=style)
  expect_equal(xpd2$value, 1)
  xpd3 = RcssProperty("plot", "xpd", Rcssclass="all", Rcss=style)
  expect_equal(xpd3$value, 1)
})


test_that("global subclass does not modify non-explicit selectors", {
  ## selector "custom" does not exist in the style
  xpd0 = RcssProperty("custom", "xpd", Rcssclass="all", Rcss=style)
  expect_false(xpd0$defined)
})




###############################################################################
## legacy functions with long names


test_that("get using Rcss-first function, single integer", {
  pch1 = RcssGetPropertyValue(style, "points", "pch")
  pch2 = RcssProperty("points", "pch", Rcss=style)
  expect_equal(pch1, pch2)
})


test_that("get using Rcss-first function, using default", {
  cx = RcssGetPropertyValueOrDefault(style, "text", "cx", default=3)
  expect_equal(cx, 3)
})




## clean up any temporary file
unlink(tofile(""))
