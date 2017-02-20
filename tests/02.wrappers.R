## tests for individual Rcss wrappers
##

library("Rcssplot")
source("00.helpers.R")

## Start the test

cattitle("Test set: 02.wrappers")

## some working styles
style0 = Rcss()
style5 = Rcss("data/style.5.Rcss")

## some data objects
set.seed(12345)
x0 = 1:5
x1a = sort(sample(40:80, 10, replace=F))
x1b = x1a+rpois(10, 30)
x2 = rnorm(200)
x3 = list(A=rnorm(20), B=c(rnorm(20,1)), C=c(rnorm(20,3),0.4, 0,-0.4))



## abline
pdf("figures/Rcssabline.pdf", width=12, height=3)
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
checkcat(1,1, "figures/Rcssabline.pdf")


## arrows
pdf("figures/Rcssarrows.pdf", width=12, height=3)
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
checkcat(1,1, "figures/Rcssarrows.pdf")


## axis
pdf("figures/Rcssaxis.pdf", width=12, height=3)
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
checkcat(1,1, "figures/Rcssaxis.pdf")


## barplot
pdf("figures/Rcssbarplot.pdf", width=12, height=3)
par(mfrow=c(1,4))
Rcssbarplot(x0, main="plain")
Rcssbarplot(x0, main="empty style", Rcss=style0)
Rcssbarplot(x0, main="style5", Rcss=style5)
Rcssbarplot(x0, main="style5, abc", Rcss=style5, Rcssclass="abc")
dev.off()
checkcat(1,1, "figures/Rcssbarplot.pdf")


## box
pdf("figures/Rcssbox.pdf", width=12, height=3)
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
checkcat(1,1, "figures/Rcssbox.pdf")


## boxplot
pdf("figures/Rcssboxplot.pdf", width=12, height=3)
par(mfrow=c(1,4))
Rcssboxplot(x3, main="plain")
Rcssboxplot(x3, main="empty style", Rcss=style0)
Rcssboxplot(x3, main="style5", Rcss=style5)
Rcssboxplot(x3, main="style5, abc", Rcss=style5, Rcssclass="abc")
dev.off()
checkcat(1,1, "figures/Rcssboxplot.pdf")


## contour
pdf("figures/Rcsscontour.pdf", width=12, height=3)
par(mfrow=c(1,4))
Rcsscontour(z=volcano, main="plain")
Rcsscontour(z=volcano, main="empty style", Rcss=style0)
Rcsscontour(z=volcano, main="style5", Rcss=style5)
Rcsscontour(z=volcano, main="style5, abc", Rcss=style5, Rcssclass="abc")
dev.off()
checkcat(1,1, "figures/Rcsscontour.pdf")


## grid
pdf("figures/Rcssgrid.pdf", width=12, height=3)
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
checkcat(1,1, "figures/Rcssgrid.pdf")


## hist
pdf("figures/Rcsshist.pdf", width=12, height=3)
par(mfrow=c(1,4))
Rcsshist(x2, main="plain")
Rcsshist(x2, main="empty style", Rcss=style0)
Rcsshist(x2, main="style5", Rcss=style5)
Rcsshist(x2, main="style5, abc", Rcss=style5, Rcssclass="abc")
dev.off()
checkcat(1,1, "figures/Rcsshist.pdf")


## jpeg (modulates sizes of files)
Rcssjpeg("figures/Rcssjpeg-a.jpeg")
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
checkcat(1,1, "figures/Rcssjpeg-a,b,c,d.pdf")


## legend
pdf("figures/Rcsslegend.pdf", width=12, height=3)
par(mfrow=c(1,4))
plot(x0, x0, main="plain")
Rcsslegend("topleft", legend=letters[1:4])
plot(x0, x0, main="empty style")
Rcsslegend("topleft", legend=letters[1:4], Rcss=style0)
plot(x0, x0, main="style5")
Rcsslegend("topleft", legend=letters[1:4], Rcss=style5)
plot(x0, x0, main="style5, abc")
Rcsslegend("topleft", legend=letters[1:4], Rcss=style5, Rcssclass="abc" )
dev.off()
checkcat(1,1, "figures/Rcsslegend.pdf")


## lines
pdf("figures/Rcsslines.pdf", width=12, height=3)
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
checkcat(1,1, "figures/Rcsslines.pdf")


## matplot


## mtext
pdf("figures/Rcssmtext.pdf", width=12, height=3)
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
checkcat(1,1, "figures/Rcssmtext.pdf")


## par
pdf("figures/Rcsspar.pdf", width=12, height=3)
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
checkcat(1,1, "figures/Rcsspar.pdf")


## pdf (modulates sizes of files)
Rcsspdf("figures/Rcsspdf-a.pdf")
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
checkcat(1,1, "figures/Rcsspdf-a,b,c,d.pdf")


## plot
pdf("figures/Rcssplot.pdf", width=12, height=3)
par(mfrow=c(1,4))
plot(x0, x0, main="plain")
Rcssplot(x0, x0, main="empty style", Rcss=style0)
Rcssplot(x0, x0, main="style5", Rcss=style5)
Rcssplot(x0, x0, main="style5, abc", Rcss=style5, Rcssclass="abc")
dev.off()
checkcat(1,1, "figures/Rcssplot.pdf")


## png (modulates sizes of files)
Rcsspng("figures/Rcsspng-a.png")
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
checkcat(1,1, "figures/Rcsspng-a,b,c,d.pdf")


## points
pdf("figures/Rcsspoints.pdf", width=12, height=3)
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
checkcat(1,1, "figures/Rcsspoints.pdf")


## polygon
pdf("figures/Rcsspolygon.pdf", width=12, height=3)
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
checkcat(1,1, "figures/Rcsspolygon.pdf")


## rect
pdf("figures/Rcssrect.pdf", width=12, height=3)
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
checkcat(1,1, "figures/Rcssrect.pdf")


## stripchart
pdf("figures/Rcssstripchart.pdf", width=12, height=3)
par(mfrow=c(1,4))
stripchart(x3, main="plain")
Rcssstripchart(x3, main="empty style", Rcss=style0)
Rcssstripchart(x3, main="style5", Rcss=style5)
Rcssstripchart(x3, main="style5, abc", Rcss=style5, Rcssclass="abc")
dev.off()
checkcat(1,1, "figures/Rcssstripchart.pdf")


## text
pdf("figures/Rcsstext.pdf", width=12, height=3)
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
checkcat(1,1, "figures/Rcsstext.pdf")


cat("\n")

