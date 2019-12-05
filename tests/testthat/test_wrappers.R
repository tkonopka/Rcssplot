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
    Rcssabline(h=x0)
    plot(x0, x0, type="n", main="empty style")
    Rcssabline(h=x0, Rcss=style0)
    plot(x0, x0, type="n", main="style5")
    Rcssabline(h=x0, Rcss=style5)
    plot(x0, x0, type="n", main="style5, abc")
    Rcssabline(h=x0, Rcss=style5, Rcssclass="abc")
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
    Rcssarrows(2,2,4,4)
    plot(x0, x0, type="n", main="empty style")
    Rcssarrows(2,2,4,4, Rcss=style0)
    plot(x0, x0, type="n", main="style5")
    Rcssarrows(2,2,4,4, Rcss=style5)
    plot(x0, x0, type="n", main="style5, abc")
    Rcssarrows(2,2,4,4, Rcss=style5, Rcssclass="abc")
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
    Rcssaxis(2)
    barplot(x0, axes=FALSE, main="empty style")
    Rcssaxis(2, Rcss=style0)
    barplot(x0, axes=FALSE, main="style5")
    Rcssaxis(2, Rcss=style5)
    barplot(x0, axes=FALSE, main="style5, abc")
    Rcssaxis(2, Rcss=style5, Rcssclass="abc")
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
    Rcssbarplot(x0, main="plain")
    Rcssbarplot(x0, main="empty style", Rcss=style0)
    Rcssbarplot(x0, main="style5", Rcss=style5)
    Rcssbarplot(x0, main="style5, abc", Rcss=style5, Rcssclass="abc")
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
    Rcssbox()
    plot(x0, x0, type="n", frame=F, axes=F, main="empty style")
    Rcssbox(Rcss=style0)
    plot(x0, x0, type="n", frame=F, axes=F, main="style5")
    Rcssbox(Rcss=style5)
    plot(x0, x0, type="n", frame=F, axes=F, main="style5, abc")
    Rcssbox(Rcss=style5, Rcssclass="abc")
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
    Rcssboxplot(x3, main="plain")
    Rcssboxplot(x3, main="empty style", Rcss=style0)
    Rcssboxplot(x3, main="style5", Rcss=style5)
    Rcssboxplot(x3, main="style5, abc", Rcss=style5, Rcssclass="abc")
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
    Rcsscontour(z=volcano, main="plain")
    Rcsscontour(z=volcano, main="empty style", Rcss=style0)
    Rcsscontour(z=volcano, main="style5", Rcss=style5)
    Rcsscontour(z=volcano, main="style5, abc", Rcss=style5, Rcssclass="abc")
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
    Rcssgrid(nx=5, ny=5)
    plot(x1a, x1b, type="n", main="empty style")
    Rcssgrid(nx=5, ny=5, Rcss=style0)
    plot(x1a, x1b, type="n", main="style5")
    Rcssgrid(nx=5, ny=5, Rcss=style5)
    plot(x1a, x1b, type="n", main="style5, abc")
    Rcssgrid(nx=5, ny=5, Rcss=style5, Rcssclass="abc")
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
    Rcsshist(x2, main="plain")
    Rcsshist(x2, main="empty style", Rcss=style0)
    Rcsshist(x2, main="style5", Rcss=style5)
    Rcsshist(x2, main="style5, abc", Rcss=style5, Rcssclass="abc")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("jpeg wrapper", {
  filename = testfile("jpeg-a")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    Rcssjpeg(filename)
    plot(x0, main="plain")
    dev.off()
    Rcssjpeg("figures/Rcssjpeg-b.jpeg", Rcss=style0)
    plot(x0, main="empty style")
    dev.off()
    Rcssjpeg("figures/Rcssjpeg-c.jpeg", Rcss=style5)
    plot(x0, x0, main="style5")
    dev.off()
    Rcssjpeg("figures/Rcssjpeg-d.jpeg", Rcss=style5, Rcssclass="abc")
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
    Rcsslegend("topleft", y=letters[1:4])
    plot(x0, x0, main="empty style")
    Rcsslegend("topleft", legend=letters[1:4], Rcss=style0)
    plot(x0, x0, main="style5")
    Rcsslegend("topleft", legend=letters[1:4], Rcss=style5)
    plot(x0, x0, main="style5, abc")
    Rcsslegend("topleft", legend=letters[1:4], Rcss=style5, Rcssclass="abc" )
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
    Rcsslines(x1a, x1b)
    plot(x1a, x1b, type="n", main="empty style")
    Rcsslines(x1a, x1b, Rcss=style0)
    plot(x1a, x1b, type="n", main="style5")
    Rcsslines(x1a, x1b, Rcss=style5)
    plot(x1a, x1b, type="n", main="style5, abc")
    Rcsslines(x1a, x1b, Rcss=style5, Rcssclass="abc")
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
    Rcssmatplot(xx)
    Rcssmatplot(xx, xx+1)
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
    Rcssmtext(side=3, "plain")
    plot(x0, x0, main="")
    Rcssmtext(side=3, "empty style", Rcss=style0)
    plot(x0, x0, main="")
    Rcssmtext(side=3, "style5", Rcss=style5)
    plot(x0, x0, main="")
    Rcssmtext(side=3, "style5, abc", Rcss=style5, Rcssclass="abc")
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
    Rcsspar()
    barplot(x0, main="plain")
    Rcsspar(Rcss=style0)
    barplot(x0, main="empty style")
    Rcsspar(Rcss=style5)
    barplot(x0, main="style5")
    Rcsspar(Rcss=style5, Rcssclass="abc")
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
    Rcsspdf(filename)
    plot(x0, main="plain")
    dev.off()
    Rcsspdf("figures/Rcsspdf-b.pdf", Rcss=style0)
    plot(x0, main="empty style")
    dev.off()
    Rcsspdf("figures/Rcsspdf-c.pdf", Rcss=style5)
    plot(x0, x0, main="style5")
    dev.off()
    Rcsspdf("figures/Rcsspdf-d.pdf", Rcss=style5, Rcssclass="abc")
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
    Rcssplot(x0, x0, main="empty style", Rcss=style0)
    Rcssplot(x0, x0, main="style5", Rcss=style5)
    Rcssplot(x0, x0, main="style5, abc", Rcss=style5, Rcssclass="abc")
    Rcssplot(x0, main="no y", Rcss=style5)
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})


test_that("png wrapper", {
  filename = testfile("png-a")
  expect_false(file.exists(filename))
  myplot = function(filename) {
    Rcsspng(filename)
    plot(x0, main="plain")
    dev.off()
    Rcsspng("figures/Rcsspng-b.png", Rcss=style0)
    plot(x0, main="empty style")
    dev.off()
    Rcsspng("figures/Rcsspng-c.png", Rcss=style5)
    plot(x0, x0, main="style5")
    dev.off()
    Rcsspng("figures/Rcsspng-d.png", Rcss=style5, Rcssclass="abc")
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
    Rcsspoints(x0, x0)
    plot(x0, x0, type="n", main="empty style")
    Rcsspoints(x0, x0, Rcss=style0)
    plot(x0, x0, type="n", main="style5")
    Rcsspoints(x0, x0, Rcss=style5)
    plot(x0, x0, type="n", main="style5, abc")
    Rcsspoints(x0, x0, Rcss=style5, Rcssclass="abc")
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
    Rcsspolygon(x1a, x1b, main="plain")
    plot(x1a, x1b, type="n", main="empty style")
    Rcsspolygon(x1a, x1b, Rcss=style0)
    plot(x1a, x1b, type="n", main="style5")
    Rcsspolygon(x1a, x1b, Rcss=style5)
    plot(x1a, x1b, type="n", main="style5, abc")
    Rcsspolygon(x1a, x1b, Rcss=style5, Rcssclass="abc")
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
    Rcssrect(2,2,4,5)
    plot(x0, x0, type="n", main="empty style")
    Rcssrect(2,2,4,5, Rcss=style0)
    plot(x0, x0, type="n", main="style5")
    Rcssrect(2,2,4,5, Rcss=style5)
    plot(x0, x0, type="n", main="style5, abc")
    Rcssrect(2,2,4,5, Rcss=style5, Rcssclass="abc")
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
    Rcssstripchart(x3, main="empty style", Rcss=style0)
    Rcssstripchart(x3, main="style5", Rcss=style5)
    Rcssstripchart(x3, main="style5, abc", Rcss=style5, Rcssclass="abc")
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
    Rcsstext(x0, x0, letters[1:5])
    plot(x0, x0, type="n", main="empty style")
    Rcsstext(x0, x0, letters[1:5], Rcss=style0)
    plot(x0, x0, type="n", main="style5")
    Rcsstext(x0, x0, letters[1:5], Rcss=style5)
    plot(x0, x0, type="n", main="style5, abc")
    Rcsstext(x0, x0, letters[1:5], Rcss=style5, Rcssclass="abc")
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
    Rcsstitle(main="title")
    plot(x0, x0, type="n")
    Rcsstitle(sub="subtitle")
    plot(x0, x0, type="n")
    Rcsstitle(xlab="x label")
    plot(x0, x0, type="n")
    Rcsstitle(ylab="y label")
    dev.off()
  }
  expect_silent(myplot(filename))
  expect_true(file.exists(filename))
})

