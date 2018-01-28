## Tests for getting values from Rcss objects
##

cat("\ntest_updates.R ")


## create a style for testing
styletext = "
text {
  cex: 1.5;
}
"
style = Rcss(tofile(styletext))


###############################################################################
## updating values manually in a style


test_that("update fails on non-Rcss input", {
  expect_error(RcssChangePropertyValue(1:4, "text"))
})


test_that("update on null style creates a new object", {
  mystyle = RcssChangePropertyValue(NULL, "points", property="cex", value=0.5)
  expect_equal(class(mystyle), "Rcss")
  expect_equal(RcssGetPropertyValue(mystyle, "points", "cex")$value, 0.5)
})


test_that("update of style can add new selector", {
  style2 = RcssChangePropertyValue(style, "points", property="cex", value=2)
  expect_equal(class(style2), "Rcss")
  ## new style should have old data
  expect_equal(RcssGetPropertyValue(style2, "text", "cex")$value, 1.5)
  ## new style should have new data
  expect_equal(RcssGetPropertyValue(style2, "points", "cex")$value, 2)
})


test_that("update of style returns style object with new property:value", {
  style2 = RcssChangePropertyValue(style, "text", property="pos", value=2)
  expect_equal(class(style2), "Rcss")
  ## new style should have old properties
  expect_equal(RcssGetPropertyValue(style2, "text", "cex")$value, 1.5)
  ## new style should have new property
  expect_equal(RcssGetPropertyValue(style2, "text", "pos")$value, 2)
})


test_that("update of style throws error when property missing", {
  expect_error(RcssChangePropertyValue(style, "text", value=2))
})


test_that("update of style can change existing property:value", {
  style2 = RcssChangePropertyValue(style, "text", property="cex", value=1.2)
  expect_equal(RcssGetPropertyValue(style2, "text", "cex")$value, 1.2)
})


test_that("update of style can create subclass", {
  style2 = RcssChangePropertyValue(style, "text", property="cex", value=1.2, Rcssclass="abc")
  expect_equal(RcssGetPropertyValue(style2, "text", "cex")$value, 1.5)
  expect_equal(RcssGetPropertyValue(style2, "text", "cex", Rcssclass="abc")$value, 1.2)
})


test_that("update of style can create nested subclass", {
  style2 = RcssChangePropertyValue(style, "text", property="cex", value=1.2,
                                   Rcssclass=c("abc", "xyz"))
  output = c(RcssGetPropertyValue(style2, "text", "cex")$value,
             RcssGetPropertyValue(style2, "text", "cex", Rcssclass="abc")$value,
             RcssGetPropertyValue(style2, "text", "cex", Rcssclass=c("abc", "xyz"))$value)
  expected = c(1.5, 1.5, 1.2)
  expect_equal(output, expected)
})


test_that("update of style can only change one selector", {
  expect_error(RcssChangePropertyValue(style, c("text", "points"), property="cex", value=1))
})


test_that("update style takes list", {
  props = list(pos=3, col="#ff0000")
  style2 = RcssChangePropertyValue(style, "text", propertylist=props, Rcssclass="abc")
  expect_equal(RcssGetPropertyValue(style2, "text", "cex")$value, 1.5)
  expect_equal(RcssGetPropertyValue(style2, "text", "pos", Rcssclass="abc")$value, 3)
  expect_equal(RcssGetPropertyValue(style2, "text", "col", Rcssclass="abc")$value, "#ff0000")
})


test_that("update style only takes a named list", {
  props = list(3,4)
  expect_error(RcssChangePropertyValue(style, "text", propertylist=props))
})


test_that("update on default style", {
  RcssDefaultStyle = style
  style2 = RcssChangePropertyValue("default", "text", property="col", value="#f0f")
  ## new style should have old as well as new data
  expect_equal(RcssGetPropertyValue(style2, "text", "cex")$value, 1.5)
  expect_equal(RcssGetPropertyValue(style2, "text", "col")$value, "#f0f")
})



## cleanup
unlink(tofile(""))

