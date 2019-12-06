# Tests for getting values from Rcss objects
#

cat("\ntest_updates.R ")


# create a style for testing
styletext = "
text {
  cex: 1.5;
}
"
style = Rcss(tofile(styletext))


###############################################################################
# updating values manually in a style


test_that("update fails on non-Rcss input", {
  expect_error(RcssChange("text", Rcss=1:4))
})


test_that("update on null style creates a new object", {
  mystyle = RcssChange("points", property="cex", value=0.5, Rcss=NULL)
  expect_equal(class(mystyle), "Rcss")
  expect_equal(RcssValue("points", "cex", Rcss=mystyle), 0.5)
})


test_that("update of style can add new selector", {
  style2 = RcssChange("points", property="cex", value=2, Rcss=style)
  expect_equal(class(style2), "Rcss")
  # new style should have old data
  expect_equal(RcssValue("text", "cex", Rcss=style2), 1.5)
  # new style should have new data
  expect_equal(RcssValue("points", "cex", Rcss=style2), 2)
})


test_that("update of style returns style object with new property:value", {
  style2 = RcssChange("text", property="pos", value=2, Rcss=style)
  expect_equal(class(style2), "Rcss")
  # new style should have old properties
  expect_equal(RcssValue("text", "cex", Rcss=style2), 1.5)
  # new style should have new property
  expect_equal(RcssValue("text", "pos", Rcss=style2), 2)
})


test_that("update of style throws error when property missing", {
  expect_error(RcssChange("text", value=2, Rcss=style))
})


test_that("update of style can change existing property:value", {
  style2 = RcssChange("text", property="cex", value=1.2, Rcss=style)
  expect_equal(RcssValue("text", "cex", Rcss=style2), 1.2)
})


test_that("update of style can create subclass", {
  style2 = RcssChange("text", property="cex", value=1.2, Rcss=style, Rcssclass="abc")
  expect_equal(RcssValue("text", "cex", Rcss=style2), 1.5)
  expect_equal(RcssValue("text", "cex", Rcss=style2, Rcssclass="abc"), 1.2)
})


test_that("update of style can create nested subclass", {
  style2 = RcssChange("text", property="cex", value=1.2,
                      Rcss=style, Rcssclass=c("abc", "xyz"))
  output = c(RcssValue("text", "cex", Rcss=style2),
             RcssValue("text", "cex", Rcss=style2, Rcssclass="abc"),
             RcssValue("text", "cex", Rcss=style2, Rcssclass=c("abc", "xyz")))
  expected = c(1.5, 1.5, 1.2)
  expect_equal(output, expected)
})


test_that("update of style can only change one selector", {
  expect_error(RcssChange(c("text", "points"), property="cex", value=1), Rcss=style)
})



test_that("update style takes list", {
  props = list(pos=3, col="#ff0000")
  style2 = RcssChange("text", propertylist=props, Rcss=style, Rcssclass="abc")
  expect_equal(RcssValue("text", "cex", Rcss=style2), 1.5)
  expect_equal(RcssValue("text", "pos", Rcss=style2, Rcssclass="abc"), 3)
  expect_equal(RcssValue("text", "col", Rcss=style2, Rcssclass="abc"), "#ff0000")
})


test_that("update style only takes a named list", {
  props = list(3,4)
  expect_error(RcssChange("text", propertylist=props, Rcss=style))
})


test_that("update style can take an empty list", {
  style2 = RcssChange("text", propertylist=list(), Rcss=style)
  expect_equal(style, style2)
})


test_that("update style with empty list creates selector", {
  style2 = RcssChange("points", propertylist=list(), Rcss=style)
  expect_equal(length(style2), length(style)+1)
})


test_that("update on default style", {
  RcssDefaultStyle = style
  style2 = RcssChange("text", property="col", value="#f0f")
  # new style should have old as well as new data
  expect_equal(RcssValue("text", "cex", Rcss=style2), 1.5)
  expect_equal(RcssValue("text", "col", Rcss=style2), "#f0f")
})


###############################################################################
# updating with special values (i.e. NA, NULL)

test_that("update can insert NA", {
  style2 = RcssChange("text", property="cex", value=NA, Rcss=style)
  cex0 = RcssValue("text", "cex", default=5, Rcss=style)
  cex1 = RcssValue("text", "cex", default=5, Rcss=style2)
  expect_identical(cex0, 1.5)
  expect_identical(cex1, NA)
})


test_that("update can insert NULL", {
  style2 = RcssChange("text", property="cex", value=NULL, Rcss=style)
  cex0 = RcssValue("text", "cex", default=5, Rcss=style)
  cex1 = RcssValue("text", "cex", default=5, Rcss=style2)
  expect_identical(cex0, 1.5)
  expect_identical(cex1, NULL)
})


###############################################################################
# deprecated functions give warning

test_that("update using deprecated interface", {  
  expect_warning(RcssChangePropertyValue(style, "text", property="cex"))
})


# cleanup
unlink(tofile(""))

