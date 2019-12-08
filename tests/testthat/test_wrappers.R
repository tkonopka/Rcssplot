# test wrappers

cat("\ntest_wrappers.R ")
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
# unit tests


test_that("abline wrapper", {
  filename = testfile("abline")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    pdf(filename, width=12, height=3)
    par(mfrow=c(1,4))
    plot(x0, x0, type="n", main="plain")
    abline(h=x0)
    plot(x0, x0, type="n", main="empty style")
    abline(h=x0, Rcss=style0)
    plot(x0, x0, type="n", main="style5")
    abline(h=x0, Rcss=style5)
    plot(x0, x0, type="n", main="style5, abc")
    abline(h=x0, Rcss=style5, Rcssclass="abc")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("arrows wrapper", {
  filename = testfile("arrows")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    pdf(filename, width=12, height=3)
    par(mfrow=c(1,4))
    plot(x0, x0, type="n", main="plain")
    arrows(2,2,4,4)
    plot(x0, x0, type="n", main="empty style")
    arrows(2,2,4,4, Rcss=style0)
    plot(x0, x0, type="n", main="style5")
    arrows(2,2,4,4, Rcss=style5)
    plot(x0, x0, type="n", main="style5, abc")
    arrows(2,2,4,4, Rcss=style5, Rcssclass="abc")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("axis wrapper", {
  filename = testfile("axis")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    pdf(filename, width=12, height=3)
    par(mfrow=c(1,4))
    barplot(x0, axes=FALSE, main="plain")
    axis(2)
    barplot(x0, axes=FALSE, main="empty style")
    axis(2, Rcss=style0)
    barplot(x0, axes=FALSE, main="style5")
    axis(2, Rcss=style5)
    barplot(x0, axes=FALSE, main="style5, abc")
    axis(2, Rcss=style5, Rcssclass="abc")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("barplot wrapper", {
  filename = testfile("barplot")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    pdf(filename, width=12, height=3)
    par(mfrow=c(1,4))
    barplot(x0, main="plain")
    barplot(x0, main="empty style", Rcss=style0)
    barplot(x0, main="style5", Rcss=style5)
    barplot(x0, main="style5, abc", Rcss=style5, Rcssclass="abc")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("box wrapper", {
  filename = testfile("box")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    pdf(filename, width=12, height=3)
    par(mfrow=c(1,4))
    plot(x0, x0, type="n", frame=F, axes=F, main="plain")
    box()
    plot(x0, x0, type="n", frame=F, axes=F, main="empty style")
    box(Rcss=style0)
    plot(x0, x0, type="n", frame=F, axes=F, main="style5")
    box(Rcss=style5)
    plot(x0, x0, type="n", frame=F, axes=F, main="style5, abc")
    box(Rcss=style5, Rcssclass="abc")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("boxplot wrapper", {
  filename = testfile("boxplot")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    pdf(filename, width=12, height=3)
    par(mfrow=c(1,4))
    boxplot(x3, main="plain")
    boxplot(x3, main="empty style", Rcss=style0)
    boxplot(x3, main="style5", Rcss=style5)
    boxplot(x3, main="style5, abc", Rcss=style5, Rcssclass="abc")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("contour wrapper", {
  filename = testfile("contour")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    pdf(filename, width=12, height=3)
    par(mfrow=c(1,4))
    contour(z=volcano, main="plain")
    contour(z=volcano, main="empty style", Rcss=style0)
    contour(z=volcano, main="style5", Rcss=style5)
    contour(z=volcano, main="style5, abc", Rcss=style5, Rcssclass="abc")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("grid wrapper", {
  filename = testfile("grid")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    pdf(filename, width=12, height=3)
    par(mfrow=c(1,4))
    plot(x1a, x1b, type="n", main="plain")
    grid(nx=5, ny=5)
    plot(x1a, x1b, type="n", main="empty style")
    grid(nx=5, ny=5, Rcss=style0)
    plot(x1a, x1b, type="n", main="style5")
    grid(nx=5, ny=5, Rcss=style5)
    plot(x1a, x1b, type="n", main="style5, abc")
    grid(nx=5, ny=5, Rcss=style5, Rcssclass="abc")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("hist wrapper", {
  filename = testfile("hist")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    pdf(filename, width=12, height=3)
    par(mfrow=c(1,4))
    hist(x2, main="plain")
    hist(x2, main="empty style", Rcss=style0)
    hist(x2, main="style5", Rcss=style5)
    hist(x2, main="style5, abc", Rcss=style5, Rcssclass="abc")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("jpeg wrapper", {
  filename = testfile("jpeg-a")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    jpeg(filename)
    plot(x0, main="plain")
    dev.off()
    jpeg("figures/jpeg-b.jpeg", Rcss=style0)
    plot(x0, main="empty style")
    dev.off()
    jpeg("figures/jpeg-c.jpeg", Rcss=style5)
    plot(x0, x0, main="style5")
    dev.off()
    jpeg("figures/jpeg-d.jpeg", Rcss=style5, Rcssclass="abc")
    plot(x0, x0, main="style5, abc")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("legend wrapper", {
  filename = testfile("legend")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    pdf(filename, width=12, height=3)
    par(mfrow=c(1,4))
    plot(x0, x0, main="plain")
    legend("topleft", y=letters[1:4])
    plot(x0, x0, main="empty style")
    legend("topleft", legend=letters[1:4], Rcss=style0)
    plot(x0, x0, main="style5")
    legend("topleft", legend=letters[1:4], Rcss=style5)
    plot(x0, x0, main="style5, abc")
    legend("topleft", legend=letters[1:4], Rcss=style5, Rcssclass="abc" )
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("lines wrapper", {
  filename = testfile("lines")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    pdf(filename, width=12, height=3)
    par(mfrow=c(1,4))
    plot(x1a, x1b, type="n", main="plain")
    lines(x1a, x1b)
    plot(x1a, x1b, type="n", main="empty style")
    lines(x1a, x1b, Rcss=style0)
    plot(x1a, x1b, type="n", main="style5")
    lines(x1a, x1b, Rcss=style5)
    plot(x1a, x1b, type="n", main="style5, abc")
    lines(x1a, x1b, Rcss=style5, Rcssclass="abc")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("matplot wrapper", {
  filename = testfile("matplot")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    xx = as.matrix(data.frame(A=1:10, B=11:20, C=21:30))
    pdf(filename, width=12, height=6)
    par(mfrow=c(1,2))
    matplot(xx)
    matplot(xx, xx+1)
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("mtext wrapper", {
  filename = testfile("mtext")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    pdf(filename, width=12, height=3)
    par(mfrow=c(1,4))
    plot(x0, x0, main="")
    mtext(side=3, "plain")
    plot(x0, x0, main="")
    mtext(side=3, "empty style", Rcss=style0)
    plot(x0, x0, main="")
    mtext(side=3, "style5", Rcss=style5)
    plot(x0, x0, main="")
    mtext(side=3, "style5, abc", Rcss=style5, Rcssclass="abc")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("par wrapper", {
  filename = testfile("par")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    pdf(filename, width=12, height=3)
    par(mfrow=c(1,4))
    par()
    barplot(x0, main="plain")
    par(Rcss=style0)
    barplot(x0, main="empty style")
    par(Rcss=style5)
    barplot(x0, main="style5")
    par(Rcss=style5, Rcssclass="abc")
    barplot(x0, main="style5, abc")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("pdf wrapper", {
  filename = testfile("pdf-a")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    pdf(filename)
    plot(x0, main="plain")
    dev.off()
    pdf("figures/pdf-b.pdf", Rcss=style0)
    plot(x0, main="empty style")
    dev.off()
    pdf("figures/pdf-c.pdf", Rcss=style5)
    plot(x0, x0, main="style5")
    dev.off()
    pdf("figures/pdf-d.pdf", Rcss=style5, Rcssclass="abc")
    plot(x0, x0, main="style5, abc")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("plot wrapper", {
  filename = testfile("abline")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    pdf(filename, width=15, height=3)
    par(mfrow=c(1,5))
    plot(x0, x0, main="plain")
    plot(x0, x0, main="empty style", Rcss=style0)
    plot(x0, x0, main="style5", Rcss=style5)
    plot(x0, x0, main="style5, abc", Rcss=style5, Rcssclass="abc")
    plot(x0, main="no y", Rcss=style5)
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("png wrapper", {
  filename = testfile("png-a")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    png(filename)
    plot(x0, main="plain")
    dev.off()
    png("figures/png-b.png", Rcss=style0)
    plot(x0, main="empty style")
    dev.off()
    png("figures/png-c.png", Rcss=style5)
    plot(x0, x0, main="style5")
    dev.off()
    png("figures/png-d.png", Rcss=style5, Rcssclass="abc")
    plot(x0, x0, main="style5, abc")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("points wrapper", {
  filename = testfile("points")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    pdf(filename, width=12, height=3)
    par(mfrow=c(1,4))
    plot(x0, x0, type="n", main="plain")
    points(x0, x0)
    plot(x0, x0, type="n", main="empty style")
    points(x0, x0, Rcss=style0)
    plot(x0, x0, type="n", main="style5")
    points(x0, x0, Rcss=style5)
    plot(x0, x0, type="n", main="style5, abc")
    points(x0, x0, Rcss=style5, Rcssclass="abc")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("polygon wrapper", {
  filename = testfile("polygon")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    pdf(filename, width=12, height=3)
    par(mfrow=c(1,4))
    plot(x1a, x1b, type="n")
    polygon(x1a, x1b, main="plain")
    plot(x1a, x1b, type="n", main="empty style")
    polygon(x1a, x1b, Rcss=style0)
    plot(x1a, x1b, type="n", main="style5")
    polygon(x1a, x1b, Rcss=style5)
    plot(x1a, x1b, type="n", main="style5, abc")
    polygon(x1a, x1b, Rcss=style5, Rcssclass="abc")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("rect wrapper", {
  filename = testfile("rect")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    pdf(filename, width=12, height=3)
    par(mfrow=c(1,4))
    plot(x0, x0, type="n", main="plain")
    rect(2,2,4,5)
    plot(x0, x0, type="n", main="empty style")
    rect(2,2,4,5, Rcss=style0)
    plot(x0, x0, type="n", main="style5")
    rect(2,2,4,5, Rcss=style5)
    plot(x0, x0, type="n", main="style5, abc")
    rect(2,2,4,5, Rcss=style5, Rcssclass="abc")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("stripchart wrapper", {
  filename = testfile("stripchart")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    pdf(filename, width=12, height=3)
    par(mfrow=c(1,4))
    stripchart(x3, main="plain")
    stripchart(x3, main="empty style", Rcss=style0)
    stripchart(x3, main="style5", Rcss=style5)
    stripchart(x3, main="style5, abc", Rcss=style5, Rcssclass="abc")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("text wrapper", {
  filename = testfile("text")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    pdf(filename, width=12, height=3)
    par(mfrow=c(1,4))
    plot(x0, x0, type="n", main="plain")
    text(x0, x0, letters[1:5])
    plot(x0, x0, type="n", main="empty style")
    text(x0, x0, letters[1:5], Rcss=style0)
    plot(x0, x0, type="n", main="style5")
    text(x0, x0, letters[1:5], Rcss=style5)
    plot(x0, x0, type="n", main="style5, abc")
    text(x0, x0, letters[1:5], Rcss=style5, Rcssclass="abc")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("title wrapper", {
  filename = testfile("title")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    pdf(filename, width=12, height=3)
    par(mfrow=c(1,4))
    plot(x0, x0, type="n")
    title(main="title")
    plot(x0, x0, type="n")
    title(sub="subtitle")
    plot(x0, x0, type="n")
    title(xlab="x label")
    plot(x0, x0, type="n")
    title(ylab="y label")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})

