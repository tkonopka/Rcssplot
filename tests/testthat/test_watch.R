# Tests for parsing files
#

cat("\ntest_watch.R ")


test_that("watch stops early when used without Rcss.files and without R.files", {
  expect_message(RcssWatch(barplot, height=1:4), "stopping")
  try(dev.off())
})

