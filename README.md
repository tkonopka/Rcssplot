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

The package can be loaded using the library command.

```
library("Rcssplot")
```

The package [vignette on CRAN](https://cran.r-project.org/web/packages/Rcssplot/vignettes/Rcssplot.pdf) and the [github wiki](http://github.com/tkonopka/Rcssplot/wiki) provide examples. 

**Note:** As for version 1.0, loading the package using the `library` command triggers messages from the R environment that the package masks some well-known functions from base graphics. The masked functions include `plot`, `text`, `mtext`, and many others. Thus, after the package is loaded, executing commands such as `plot` will be trigger `Rcssplot` rather than base `graphics`. 

The masking functions are designed to mimic the original functions. Thus, in general, existing code using base graphics should in principle work as before. Some exceptions may appear, however, if existing code makes extensive use of positional arguments. As an example, a command `hist(rnorm(100), 10)` would work under base graphics, would trigger an error in Rcssplot. To overcome this, the existing code can be adjusted to forcibly use the original version of the plot function, i.e. `graphics::hist(rnorm(100), 10)`, or edited to use a named argument, i.e. `hist(rnorm(100), breaks=10)`. 


## Acknowledgments

Many thanks to the CRAN team and contributors: FrancoisGuillem, nfultz

