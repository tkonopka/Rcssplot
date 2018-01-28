## tests for helper functions R/RcssHelpers.R


cat("\ntest_helpers.R ")




###############################################################################
## unit tests


test_that("raise error", {
  expect_error(stopCF("hello"))
})


test_that("get argument=value strings (empty)", {
  expect_equal(RcssMakeCallCodeString(c(), list()), "")
})


test_that("get argument=value strings", {
  result = RcssMakeCallCodeString(letters[1:3], "obj")
  expect_equal(result, ", a=obj$a, b=obj$b, c=obj$c")
})

