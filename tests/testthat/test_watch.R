# Tests for re-loading and re-running css/R files during development
#

cat("\ntest_watch.R ")


test_that("watch stops early when used without Rcss.files and without R.files", {
  expect_message(RcssWatch(barplot, height=1:4), "stopping")
  try(dev.off())
})


test_that("watch accepts a function string", {
  expect_message(RcssWatch("barplot", height=1:4), "stopping")
  try(dev.off())
})


test_that("watch reads sources from files", {
  # This test pattern for testing user interaction borrowed from:
  # https://stackoverflow.com/questions/41372146/test-interaction-with-users-in-r-package
  R.sources = file.path("data", "custom.function.R")
  Rcss.sources = file.path("data", "style.1.Rcss")
  cues <- file()
  options(Rcssplot.connection = cues)
  write(c("", "q"), cues)
  expect_message(RcssWatch("custom.function", files=c(R.sources, Rcss.sources), x=1:4),
                 "re-run")
  try(dev.off())
  close(cues) 
  options(Rcssplot.connection = stdin())
})


test_that("watch generates output", {
  R.sources = file.path("data", "custom.function.R")
  Rcss.sources = file.path("data", "style.1.Rcss")
  cues <- file()
  options(Rcssplot.connection = cues)
  write(c("", "q"), cues)
  # this generates an error, not because of parsing but because
  # custom.function expects an argument x, but that is not provided
  expect_message(RcssWatch("custom.function", files=c(R.sources, Rcss.sources)),
                 "re-run")
  pass <- function(x) { }
  tryCatch({dev.off()}, warning=pass, error=pass)
  close(cues) 
  options(Rcssplot.connection = stdin())
})


