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
  frame: F;
  xlab: abctext there;
  ylab: \"Two words\";
  main: ;
}
plot.escaped {
  main: \"Title is 'ABC'\";
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
"
style = Rcss(tofile(styletext))




###############################################################################
## extracting data types from style sheets


test_that("get from empty style", {
  pch = RcssGetPropertyValue(NULL, "points", "pch")
  expect_false(pch$defined)
})


test_that("get single integer", {
  pch = RcssGetPropertyValue(style, "points", "pch")
  expect_true(pch$defined)
  expect_equal(pch$value, 19)
})

test_that("get single integer", {
  pch = RcssGetPropertyValue(style, "points", "pch")
  expect_true(pch$defined)
  expect_equal(pch$value, 19)
})


test_that("get real numbers", {
  lwd = RcssGetPropertyValue(style, "points", "lwd")
  expect_true(lwd$defined)
  expect_equal(lwd$value, 1.5)
})


test_that("get boolean", {
  ff = RcssGetPropertyValue(style, "plot", "frame")
  expect_equal(ff$value, "F")
})


test_that("get NA", {
  ff = RcssGetPropertyValue(style, "rect", "border")
  expect_true(ff$defined)
  ##expect_equal(ff$value, NA)
})


test_that("get NULL", {
  ff = RcssGetPropertyValue(style, "rect", "col")
  expect_true(ff$defined)
  ##expect_equal(ff$value, NULL)
})


test_that("get string", {
  axs = RcssGetPropertyValue(style, "plot", "xaxs")
  expect_equal(axs$value, "i")
})


test_that("get string without quotes", {
  lab = RcssGetPropertyValue(style, "plot", "xlab")
  labval = paste(lab$value, collapse=" ")
  expect_equal(labval, "abctext there")
})

test_that("get string in quotes", {
  lab = RcssGetPropertyValue(style, "plot", "ylab")
  labval = lab$value
  expect_equal(labval, "\"Two words\"")
})

test_that("get string with escaped characters", {
  lab = RcssGetPropertyValue(style, "plot", "main", Rcssclass="escaped")
  labval = lab$value
  expect_equal(labval, "\"Title is 'ABC'\"")
})

test_that("get empty string", {
  main = RcssGetPropertyValue(style, "plot", "main")
  expect_equal(main$value, "")
})

test_that("get multiple values from style", {
  col = RcssGetPropertyValue(style, "points", "col")
  expect_equal(col$value, c("#ff0000", "#00ff00", "#0000ff88"))
})




###############################################################################
## using common properties for several selectors


test_that("get multiple values from style", {
  cex1 = RcssGetPropertyValue(style, "text", "cex")
  cex2 = RcssGetPropertyValue(style, "mtext", "cex")
  expect_equal(cex1$value, cex2$value)
})




###############################################################################
## using default values


test_that("get from an undefined selector", {
  type = RcssGetPropertyValue(style, "missing", "hello")
  expect_false(type$defined)
  expect_equal(type$value, NULL)
})

test_that("get an undefined property", {
  type = RcssGetPropertyValue(style, "points", "type")$defined
  expect_false(type)
})


test_that("fetch value, default not necessary", {
  cex = RcssGetPropertyValueOrDefault(style, "text", "cex", default=3)
  expect_equal(cex, 1)
})


test_that("fetch value, default necessary", {
  cx = RcssGetPropertyValueOrDefault(style, "text", "cx", default=3)
  expect_equal(cx, 3)
})


test_that("fetch value, default necessary", {
  cx = RcssGetPropertyValueOrDefault(style, "text", "cx", default=3)
  expect_equal(cx, 3)
})




###############################################################################
## using subclasses


test_that("get using subclass", {
  cex1 = RcssGetPropertyValue(style, "text", "cex")
  expect_equal(cex1$value, 1)
  cex2 = RcssGetPropertyValue(style, "text", "cex", Rcssclass="subclass")
  expect_equal(cex2$value, 0.75)
})

test_that("get when subclass does not apply", {
  cex = RcssGetPropertyValue(style, "mtext", "cex", Rcssclass="subclass")
  expect_equal(cex$value, 1)
})



## clean up any temporary file
unlink(tofile(""))
