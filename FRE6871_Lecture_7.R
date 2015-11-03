library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
rm(list=ls())
baseenv()  # get base environment
globalenv()  # get global environment
environment()  # get current environment
class(environment())  # get environment class
glob_var <- 1  # define variable in current environment
ls(environment())  # get objects in current environment

new_env <- new.env()  # create new environment
parent.env(new_env)  # get calling environment of new environment
assign("new_var1", 3, envir=new_env)  # assign Value to Name
new_env$new_var2 <- 11  # create object in new environment
ls(new_env)  # get objects in new environment
ls(environment())  # get objects in current environment
new_env$new_var1  # environments are subset like lists
new_env[["new_var1"]]  # environments are subset like lists
search()  # get search path for R objects
my_list <- list(flowers=c("rose", "daisy", "tulip"),  # create a list
                trees=c("pine", "oak", "maple"))
my_list$trees
attach(my_list)
trees
search()  # get search path for R objects
detach(my_list)
head(trees)  # "trees" is part of the datasets base package
# "trees" is in datasets base package
head(trees, 3)
colnames(trees)
mean(Girth)
mean(trees$Girth)
with(trees, c(mean(Girth), mean(Height), mean(Volume)))
library("MASS")
# or
require("MASS")
getOption("defaultPackages")
pack_info <- installed.packages()  # matrix of packages
# get a few package names and their versions
pack_info[sample(x=1:100, 5), c("Package", "Version")]
t(pack_info["xts", ])  # get info for package "xts"
library()  # list all packages installed on the system
search()  # list all loaded packages on search path

# get documentation for package "Ecdat"
packageDescription("Ecdat")  # get short description
help(package="Ecdat")  # load help page
library(Ecdat)  # load package "Ecdat"
data(package="Ecdat")  # list all datasets in "Ecdat"
ls("package:Ecdat")  # list all objects in "Ecdat"
detach("package:Ecdat")  # remove Ecdat from search path
library("Ecdat")  # load econometric data sets
class(Garch)  # Garch is a data frame from "Ecdat"
dim(Garch)  # daily currency prices
head(Garch[, -2])  # col 'dm' is Deutsch Mark
detach("package:Ecdat")  # remove Ecdat from search path
getOption("repos")  # get default package source
.libPaths()  # get package save directory
install.packages("AER")  # install "AER" from CRAN
# install "PerformanceAnalytics" from R-Forge
install.packages(
  pkgs="PerformanceAnalytics",  # name
  lib="C:/Users/Jerzy/Downloads",  # directory
  repos="http://R-Forge.R-project.org")  # source
# install devtools from CRAN
install.packages("devtools")
# load devtools
library(devtools)
# install package "babynames" from GitHub
install_github(repo="hadley/babynames")
# install package "PortfolioAnalytics" from source
install.packages("PortfolioAnalytics",
  type="source",
  repos="http://r-forge.r-project.org")
# download files for package "PortfolioAnalytics"
download.packages(pkgs = "PortfolioAnalytics",
  destdir = ".",  # download to cwd
  type = "source",
  repos="http://r-forge.r-project.org")
# install "PortfolioAnalytics" from local tar source
install.packages(
  "C:/Users/Jerzy/Downloads/PortfolioAnalytics_0.9.3598.tar.gz",
  repos=NULL, type="source")
# list directories in "PortfolioAnalytics" sub-directory
gsub(
  "C:/Users/Jerzy/Documents/R/win-library/3.1", 
  "~",
  list.dirs(
    file.path(
      .libPaths()[1], 
      "PortfolioAnalytics")))
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
library(zoo)  # load package zoo
# show the generic function "merge"
merge
# show the "merge" method dispatched to "zoo" objects
merge.zoo
library(zoo)  # load package zoo
# get all methods for generic function merge()
methods(generic.function="merge")
# get generic function methods applied to "zoo" objects
methods(class="zoo")
# define a generic function
gen_sum <- function (a, b, ...) {
  UseMethod("gen_sum")
}  # end gen_sum

# define method for "numeric" class
gen_sum.numeric <- function (a, b, ...) {
  sum(a, b)
}  # end gen_sum.character

# define method for "character" class
gen_sum.character <- function (a, b, ...) {
  paste(a, "plus", b)
}  # end gen_sum.character

# apply gen_sum to "numeric" objects
gen_sum(1, 2)
# apply gen_sum to "character" objects
gen_sum("a", "b")
# 'cbind' is an internal generic function
cbind
# define "+" method for "character" class
"+.character" <- function (a, b, ...) {
  paste(a, "plus", b)
}  # end +.character
methods("+")  # view methods for "+" operator
# define variables with "character" class
char1 <- "a"
char2 <- "b"
class(char1)
char1 + char2  # add two "character" objects - doesn't work
attributes(char1)  # doesn't have explicit "character" class - only implicit
char1 <- structure("a", class="character")
char2 <- structure("b", class="character")
attributes(char1)  # now has explicit "character" class
# add two "character" objects
char1 + char2
# define object of class "string"
obj_string <- "how are you today?"
class(obj_string) <- "string"
obj_string
# overload "print" method for string objects
print.string <- function (str_ing) {
  print(
    paste(strsplit(str_ing, split=" ")[[1]], 
  collapse=" + "))
}  # end print.string
# methods("print")  # view new methods for "print" function
print(obj_string)
obj_string
# overwrite "+" operator
"+" = function(a, b) {
  if(is.character(a) && is.character(b)) {
    paste(a, "plus", b)
  } else {
    .Primitive("+") (a, b)
  }
}
methods("+")  # view methods for "+" operator
# add two "numeric" objects
1 + 2
# add two "character" objects
"a" + "b"
# overwrite "+" operator with a generic function
"+" <- function (a, b, ...) {
  UseMethod("+")
}  # end gen_sum
# define method for "numeric" class
"+.numeric" <- function (a, b, ...) {
  sum(a, b)
}  # end gen_sum.character
# define method for "character" class
"+.character" <- function (a, b, ...) {
  paste(a, "plus", b)
}  # end gen_sum.character
methods("+")  # view methods for "+" operator
# add two "numeric" objects
1 + 2
# add two "character" objects
"a" + "b"
cbind.ts  # can't view non-visible method
stats::cbind.ts  # can't view non-visible method
stats:::cbind.ts  # display non-visible method
getAnywhere(cbind.ts)  # display non-visible method
rm(list=ls())
new_zoo <- zoo(rnorm(10), order.by=(Sys.Date() + 0:9))
# coerce "zoo" object to new class "zoo_xtra"
class(new_zoo) <- "zoo_xtra"
class(new_zoo)
methods(generic.function="length")
length  # primitive function
# define "length" method for class "zoo_xtra"
length.zoo_xtra <- function(in_ts) {
  cat("length of zoo_xtra object:\n")
# unclass object, then calculate length
  length(unclass(in_ts))
}  # end length.zoo_xtra
length(new_zoo)  # apply "length" method to "zoo_xtra" object
methods(generic.function="length")
# define "last" method for class "zoo_xtra"
last.zoo_xtra <- function(in_ts) {
  in_ts[length(in_ts)]
}  # end last.zoo_xtra
last(new_zoo)  # doesn't work
last.zoo_xtra(new_zoo)  # works
# define a generic function
last <- function (a, b, ...) {
  UseMethod("last")
}  # end last
last(new_zoo)  # now works
# define generic "string" class converter
as.string <- function (str_ing, ...) 
  UseMethod("as.string")
# default "string" class converter
as.string.default <- function (str_ing, ...)
  structure(str_ing, class="string", ...)
# numeric "string" class converter
as.string.numeric <- function (str_ing, ...)
  structure(as.character(str_ing), class="string", ...)
# "string" class checker
is.string <- function (str_ing)
  inherits(x=str_ing, what="string")
# define "string" object
obj_string <- as.string("how are you today?")
obj_string
is.string(obj_string)
is.string("hello")
as.string(123)
is.string(as.string(123))
rm(list=ls())
library(xts)
new_xts <- xts(rnorm(10), order.by=(Sys.Date() + 0:9))
class(new_xts)  # class attribute is a vector
# "last" is a generic function from package "xts"
last
methods(generic.function="last")
last(new_xts)  # apply "last" method from "xts" class
# derive object "xts_xtra" from "xts" object
class(new_xts) <- c("xts_xtra", class(new_xts))
class(new_xts)  # class attribute is a vector
# "xts_xtra" object inherits "last" method from "xts" class
last(new_xts)
# define new "last" method for class "xts_xtra"
last.xts_xtra <- function(in_ts) {
  cat("last element of xts_xtra object:\n")
  drop(in_ts[length(in_ts), ])
}  # end last.xts_xtra
last(new_xts)  # apply "last" from "xts_xtra" class
# define "last" method for class "xts_xtra"
last.xts_xtra <- function(in_ts) {
  cat("last element of xts_xtra object:\n")
  drop(NextMethod())
}  # end last.xts_xtra
last(new_xts)  # apply "last" from "xts_xtra" class
