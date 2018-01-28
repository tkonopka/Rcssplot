## Tests printing information to summarize from Rcss objects
##

cat("\ntest_print.R ")

source("helpers.R")


empty = Rcss()

styletext = "
lines {
  lwd: 1;
}
points {
  cex: 1;
}
points.abc {
  cex: 1.5;
}
points.xyz {
  lwd: 1.5;
}
points.abc.qq {
  pch: 21;
}
"
style = Rcss(tofile(styletext))



###############################################################################
## printing of style summary


test_that("print summary gives error on wrong input", {
  expect_error(print.Rcss(1:4))
})


test_that("print summary of empty style", {
  expect_output(print.Rcss(empty), "Rcssplot")
  expect_output(print.Rcss(empty), "Defined")
  expect_output(print.Rcss(empty), "Use function")
})


test_that("print summary shows all selectors", {
  expect_output(print.Rcss(style), "lines")
  expect_output(print.Rcss(style), "points")
})


###############################################################################
## printing style details


test_that("print details gives error on non-Rcss", {
  expect_error(printRcss(1:4))
})


test_that("print details gives error on non-existent selector", {
  ## error should give hint of those selectors that are specified
  expect_error(printRcss(style), "points")
})


test_that("print details gives error on non-existent selector", {
  expect_error(printRcss(style, "grid"))
})


test_that("print details gives when verbose not logical", {
  expect_error(printRcss(style, "points", verbose="0"))
})


test_that("print details shows all subclasses", {
  expect_output(printRcss(style, "lines"), "lines")
  expect_output(printRcss(style, "lines"), "Defined classes")
})


test_that("print verbose details shows all subclasses", {
  expect_output(printRcss(style, "points"), "abc")
  expect_output(printRcss(style, "points"), "xyz")
  ## should display level-2 subclasses in verbose mode, but not in regular
  expect_failure(expect_output(printRcss(style, "points", verbose=F), "qq"))
  expect_success(expect_output(printRcss(style, "points", verbose=T), "qq"))
  # nested print should display properties defined in subclass
  expect_failure(expect_output(printRcss(style, "points", verbose=F), "lwd"))
  expect_failure(expect_output(printRcss(style, "points", verbose=F), "pch"))
  expect_success(expect_output(printRcss(style, "points", verbose=T), "lwd"))
  expect_success(expect_output(printRcss(style, "points", verbose=T), "pch"))
})



## cleanup temporary files
unlink(tofile(""))
