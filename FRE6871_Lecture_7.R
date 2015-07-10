library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
2 + 3  # standard infix operator call syntax
'+' (2, 3)  # infix operator applied using prefix syntax
# define infix operator that returns string
'%+%' <- function(a, b) paste(a, b, sep=" + ")
2 %+% 3
2 %+% 3 %+% 4
"hello" %+% 2 %+% 3 %+% "bye"
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
getOption("repos")  # get default package source
.libPaths()  # get package save directory
install.packages("AER")  # install "AER" from CRAN
# install "PerformanceAnalytics" from R-Forge
install.packages(pkgs="PerformanceAnalytics",  # name
           lib="C:/Users/Jerzy/Downloads",  # directory
           repos="http://R-Forge.R-project.org")  # source
# install devtools from CRAN
install.packages("devtools")
# load devtools
library(devtools)
# install package "babynames" from GitHub
install_github(repo="hadley/babynames")
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
