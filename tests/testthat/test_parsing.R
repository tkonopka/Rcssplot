## Tests for parsing files
##

cat("\ntest_parsing.R ")
source("helpers.R")


## files with misc style sheets
style.files = file.path("data", paste0("style.", seq(1, 3), ".Rcss"))
style13 = Rcss(style.files[1:3])




###############################################################################
## loading styles from files


test_that("can initializae a blank style", {
  zero = Rcss()
  expect_equal(length(zero), 0)
})


test_that("load style from one file", {
  ## file 1
  style1 = Rcss(style.files[1])
  expect_equal(length(style1), 1)
  ## file 2
  style2 = Rcss(style.files[2])
  expect_equal(length(style2), 1)
  ## file 3
  style3 = Rcss(style.files[3])
  expect_equal(length(style3), 2)
})


test_that("load style with just comments", {
  data = "/** only has a comment **/"
  ## file with one comment
  comments1 = Rcss(tofile(data))
  expect_equal(length(comments1), 0)
  unlink(tofile(""))
})


test_that("load style with nested comments", {
  data = "
/** simple comment comments **/

/** 
 Multi-line
 comment
 **/

/** Outer comment
  /** inner comment **/
  outer comment 
**/
"
  comments = Rcss(tofile(data))
  expect_equal(length(comments), 0)
  unlink(tofile(""))
})


test_that("load composite style from multiple files", {
  style = Rcss(style.files[1:3])
  expect_equal(length(style), 3)
})



###############################################################################
## reporing errors on misformed data


test_that("report error; missing selector", {
  data = "
{
  cex: 2;
}"
  expect_error(Rcss(tofile(data)))
})


test_that("report error; missing semicolon", {
  data = "
points {
  cex 2
}"
  expect_error(Rcss(tofile(data)))
})


test_that("report error; missing property", {
  data = "
points {
  2;
}"
  expect_error(Rcss(tofile(data)))
})


test_that("report error; missing colon", {
  data = "
points {
  pch: 18
  cex: 2;
}"
  expect_error(Rcss(tofile(data)))
})


test_that("report error; missing opening brace", {
  data = "
points 
 cex: 2;
}"
  expect_error(Rcss(tofile(data)))
})


test_that("report error; missing closing brace", {
  data = "
points {
  cex 2;

lines {
  lwd: 2;
}"
  expect_error(Rcss(tofile(data)))
})


test_that("report error; missing class name after dot", {
  data = "
points. {
  cex: 2;
}"
  expect_error(Rcss(tofile(data)))
})


test_that("report error; missing second selector name after comma", {
  data = "
points, {
  cex: 2;
}"
  expect_error(Rcss(tofile(data)))
})


test_that("report error; missing comma between selectors", {
  data = "
points lines {
  lwd: 2;
}"
  expect_error(Rcss(tofile(data)))
})


test_that("report error; selector is not a string", {
  data = "
points, 123 {
  lwd: 2;
}"
  expect_error(Rcss(tofile(data)))
})


test_that("report error; class is not a string", {
  data = "
points.123 {
  cex: 2;
}"
  expect_error(Rcss(tofile(data)))
})


test_that("report error; too many dots between classes", {
  data = "
points..abc {
  cex: 2;
}"
  expect_error(Rcss(tofile(data)))
})


test_that("report error; too many commas between selectors", {
  data = "
text,,mtext {
  cex: 2;
}"
  expect_error(Rcss(tofile(data)))
})


## clean up any temporary file
unlink(tofile(""))

