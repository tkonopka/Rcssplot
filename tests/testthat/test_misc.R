
cat("\ntest_misc.R ")
source("helpers.R")


# some working styles
style0 = Rcss()
style5 = Rcss(file.path("data","style.5.Rcss"))

# some data objects
set.seed(12345)
x0 = 1:5
x1a = sort(sample(40:80, 10, replace=F))
x1b = x1a+rpois(10, 30)
x2 = rnorm(200)
x3 = list(A=rnorm(20), B=c(rnorm(20,1)), C=c(rnorm(20,3),0.4, 0,-0.4))



###############################################################################
# unit tests for new Rcss functions


test_that("parplot", {
  filename = testfile("parplot")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    pdf(filename, width=12, height=3)
    layout(matrix(1:4, nrow=1, ncol=4))
    parplot(x0, x0, type="n", main="plain")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("corner text", {
  filename = testfile("ctext")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    pdf(filename, width=12, height=3)
    par(mfrow=c(1,4))
    plot(x0, x0, type="n", main="panel A")
    ctext("A")
    plot(x0, x0, type="n", main="panel B")
    ctext("B")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


