# Rcssplot

![Status](https://travis-ci.org/tkonopka/Rcssplot.svg?branch=master)
[![codecov](https://codecov.io/gh/tkonopka/Rcssplot/branch/master/graph/badge.svg)](https://codecov.io/gh/tkonopka/Rcssplot)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/Rcssplot)](http://cran.r-project.org/package=Rcssplot)


The Rcssplot package brings cascading style sheets to the R graphical 
environment. It provides a means to separate the aesthetics from 
data crunching in plots and charts.


## Installation

The package can be installed from CRAN.

```
install.packages("Rcssplot")
```

Alternatively, it can be installed from this github repository via devtools.

```
library("devtools")
install_github("tkonopka/Rcssplot")
```


## Using Rcssplot

After installation, the package can be loaded using the library command.

```
library("Rcssplot")
```

The [vignette on CRAN](https://cran.r-project.org/web/packages/Rcssplot/vignettes/Rcssplot.pdf) provides an in-depth tutorial and the [github wiki](http://github.com/tkonopka/Rcssplot/wiki) provides examples. As a minimal use-case, consider a barplot.

```
barplot(1:4, col="red", density=10, border="red")
```

Styling of the plot is incorporated within the function call. In the Rcssplot framework, these settings can be delegated to a cascading style sheet.

```
RcssDefaultStyle <- Rcss(text="barplot { col: red; border: red; density: 10 }")
barplot(1:4)
```

The style definitions are here provided as a string, but in practice, it is easier to edit them in a file in a text editor. The output of the final barplot command reproduces the styling above.


## Compatibility with base graphics

From version 1.0, loading the package using `library(Rcssplot)` triggers messages from the R environment that the package masks some well-known functions from base graphics. The masked functions include `plot`, `text`, `mtext`, and many others. Thus, after the package is loaded, executing commands such as `plot` will be trigger `Rcssplot` rather than base `graphics`. 

The masking functions are designed to mimic the originals. Thus, in many cases, existing code using base graphics should work as before. Some exceptions may appear, however, if existing code makes extensive use of positional arguments. As an example, consider the following.

```
x100 <- rnorm(100)
hist(x100, 10)
```

While this is valid in base graphics, it triggers an error under Rcssplot because the argument `10` carries meaning through its position in the function call, and not through a named argument or keyword. To avoid the error, the command can be adjusted with an argument name.

```
hist(x100, breaks=10)
```


## Acknowledgments

Many thanks to the CRAN team and contributors: FrancoisGuillem, nfultz

