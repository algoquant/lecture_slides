library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
# function "double_it" returns the double of its argument
double_it <- function(in_var=NULL) {
# check if argument is valid and return double
  if (is.null(in_var)) {
    return("double_it: in_var is missing")
  } else if (is.numeric(in_var)) {
    2*in_var
  } else {
    cat("double_it: in_var is not numeric")
  }
}  # end double_it
double_it(3)
double_it("a")
double_it()

double_it <- function(in_var) {
# check if argument is valid and return double
  if (missing(in_var)) {
    return("double_it: in_var is missing")
  } else if (is.numeric(in_var)) {
    2*in_var
  } else {
    cat("double_it: in_var is not numeric")
  }
}  # end double_it
double_it(3)
double_it("a")
double_it()
# function "double_it" returns the double of its argument
double_it <- function(in_var=NULL) {
# check if argument is valid and return double
  if (missing(in_var)) {
    stop("double_it: in_var is missing")
  } else if (!is.numeric(in_var)) {
    stop("double_it: in_var is not numeric")
  } else {
    2*in_var
  }
}  # end double_it
double_it(3)
double_it("a")
double_it()
# function "double_it" returns the double of its argument
double_it <- function(in_var=NULL) {
# check if argument is valid and return double
  stopifnot(!is.null(in_var) && is.numeric(in_var))
  2*in_var
}  # end double_it
double_it(3)
double_it("a")
double_it()
# function "sum_two" returns the sum of its two arguments
sum_two <- function(in_var1, in_var2) {  # even more robust
# check if at least one argument is not missing
  stopifnot(!missing(in_var1) || !missing(in_var2))
# check if arguments are valid and return sum
  if (is.numeric(in_var1) && is.numeric(in_var2)) {
    in_var1 + in_var2  # both valid
  } else if (is.numeric(in_var1)) {
    cat("in_var2 is not numeric")
    in_var1  # in_var1 is valid
  } else if (is.numeric(in_var2)) {
    cat("in_var1 is not numeric")
    in_var2  # in_var2 is valid
  } else {
    stop("none of the arguments are numeric")
  }
}  # end sum_two
sum_two(1, 2)
sum_two(5, 'a')
sum_two('a', 5)
sum_two('a', 'b')
sum_two()
# ?options
getOption("warn")
getOption("error")
catch_missing <- function(in_var) {
# returns its argument
  if (missing(in_var)) {
    warning("catch_missing: in_var was missing")
  } else {
    in_var
  }
}  # end catch_missing
catch_missing(5)
options(warn=-1)
catch_missing()
options(warn=0)
catch_missing()
options(warn=1)
catch_missing()
options(warn=3)
catch_missing()
str(tryCatch)  # get arguments of tryCatch()
tryCatch(  # without error handler
  {  # evaluate expressions
    num_var <- 101  # assign
    stop('my error')  # throw error
  }, 
  finally=print(paste("num_var=", num_var))
)  # end tryCatch

tryCatch(  # with error handler
  {  # evaluate expressions
    num_var <- 101  # assign
    stop('my error')  # throw error
  }, 
  error=function(error_cond)  # handler captures error condition
    print(paste("error handler: ", error_cond)),
  finally=print(paste("num_var=", num_var))
)  # end tryCatch
rm(list=ls())
# apply loop without tryCatch
apply(as.matrix(1:5), 1, function(num_var) {  # anonymous function
    stopifnot(num_var != 3)  # check for error
    cat("(cat) num_var =", num_var, "\n")  # broadcast
    paste("(return) num_var =", num_var)  # return value
  }  # end anonymous function
)  # end apply
# apply loop with tryCatch
apply(as.matrix(1:5), 1, function(num_var) {  # anonymous function
    tryCatch(  # with error handler
{  # body
  stopifnot(num_var != 3)  # check for error
  cat("(cat) num_var =", num_var, "\t")  # broadcast
  paste("(return) num_var =", num_var)  # return value
},
error=function(error_cond)  # handler captures error condition
  paste("handler: ", error_cond),
finally=print(paste("(finally) num_var =", num_var))
    )  # end tryCatch
  }  # end anonymous function
)  # end apply
getOption("repos")  # get default package source
.libPaths()  # get package save directory
## install.packages("AER")  # install "AER" from CRAN
## # install "PerformanceAnalytics" from R-Forge
## install.packages(pkgs="PerformanceAnalytics",  # name
##            lib="C:/Users/Jerzy/Downloads",  # directory
##            repos="http://R-Forge.R-project.org")  # source
## # install devtools from CRAN
## install.packages("devtools")
## # load devtools
## library(devtools)
## # install package "babynames" from GitHub
## install_github(repo="hadley/babynames")
## library("MASS")
## # or
## require("MASS")
getOption("defaultPackages")
pack_info <- installed.packages()  # matrix of packages
# get a few package names and their versions
pack_info[sample(x=1:100, 5), c("Package", "Version")]
t(pack_info["xts", ])  # get info for package "xts"
## library()  # list all packages installed on the system
## search()  # list all loaded packages on search path
## 
## # get documentation for package "Ecdat"
## packageDescription("Ecdat")  # get short description
## help(package="Ecdat")  # load help page
## library(Ecdat)  # load package "Ecdat"
## data(package="Ecdat")  # list all datasets in "Ecdat"
## ls("package:Ecdat")  # list all objects in "Ecdat"
## detach("package:Ecdat")  # remove Ecdat from search path
library("Ecdat")  # load econometric data sets
class(Garch)  # Garch is a data frame from "Ecdat"
dim(Garch)  # daily currency prices
head(Garch[, -2])  # col 'dm' is Deutsch Mark
detach("package:Ecdat")  # remove Ecdat from search path
rm(list=ls())
search()  # get search path for R objects
library("MASS")  # load package "MASS"
head(ls("package:MASS"))  # list some objects in "MASS"
detach("package:MASS")  # remove "MASS" from search path
loadedNamespaces()  # get names of loaded namespaces

search()  # get search path for R objects
# get session info,
# including packages not attached to the search path
sessionInfo()
plot.xts  # package xts isn't loaded and attached
head(xts::plot.xts, 3)
methods("cbind")  # get all methods for function "cbind"
stats::cbind.ts  # cbind isn't exported from package stats
stats:::cbind.ts  # view the non-visible function
getAnywhere("cbind.ts")
library("MASS")  # load package 'MASS'
select  # code of primitive function from package 'MASS'
getAnywhere("cbind.ts")
