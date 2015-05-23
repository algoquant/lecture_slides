library(knitr)
opts_chunk$set(prompt=TRUE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)
rm(list=ls())
glob_var <- 1  # define a global variable
ls(environment())  # get all variables in environment
func_env <- function() {  # explore function environments
  loc_var <- 1  # define a local variable
  cat('objects in evaluation environment:\t', 
      ls(environment()), '\n')
  cat('objects in enclosing environment:\t', 
      ls(parent.env(environment())), '\n')
  cat('this is the enclosing environment:')
  parent.env(environment())  # return enclosing environment
}  # end func_env
func_env()

environment(func_env)
environment(print)  # package namespace is the enclosure
rm(list=ls())
glob_var <- 1  # define a global variable
probe_scope <- function() {  # explore function scope
  loc_var <- 2*glob_var  # define a local variable
  new_globvar <<- 11  # define a global variable
  cat('objects in evaluation environment:\t', 
      ls(environment()), '\n')
  cat('this is a local loc_var:\t', loc_var, '\n')
  cat('objects in enclosing environment:\n', 
      ls(parent.env(environment())), '\n')
  cat('this is glob_var:\t', glob_var, '\n')
  glob_var <- 10  # define local glob_var
  cat('this is the local glob_var:\t', glob_var, '\n')
}  # end probe_scope
probe_scope()
glob_var  # global variable is unaffected
new_globvar  # new_globvar is preserved
loc_var  # local variable is gone!
a <- 1  # define a variable
# new variable "b" points to value of "a"
b <- a  # define a new variable
# when "b" is modified, R makes a copy of it
b <- b+1
# function doubles its argument and returns it
double_it <- function(in_var) {
  in_var <- 2*in_var
  cat("input argument was doubled to:", in_var, "\n")
  in_var
}
double_it(a)
a  # variable "a" is unchanged
rm(list=ls())
glob_var <- 1  # define a global variable
probe_scope <- function() {  # explore function scope
  cat('this is the global glob_var:\t', glob_var, '\n')
  glob_var <- 10  # define local 'glob_var' variable
  glob_var <<- 2  # re-define the global variable
  cat('this is a local glob_var:\t', glob_var, '\n')
}  # end probe_scope
probe_scope()
glob_var  # the global variable
rm(list=ls())
num_var1 <- 1

if (num_var1) {  # numeric zero is FALSE, all other numbers are TRUE
  num_var2 <- 4
} else if (num_var1 == 0) {  # 'else if' together on same line
  num_var2 <- 0
} else {  # 'else' together with curly braces
  num_var2 <- -4
}  # end if

num_var2
rm(list=ls())
# create two numeric vectors
vec_var1 <- sin(0.25*pi*1:10)
vec_var2 <- cos(0.25*pi*1:10)
# create third vector using 'ifelse'
vec_var3 <- ifelse(vec_var1 > vec_var2, 
          vec_var1, vec_var2)
# cbind all three together
vec_var4 <- cbind(vec_var1, vec_var2, vec_var3)

# set plotting parameters
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), 
    cex.lab=0.8, cex.axis=0.8, cex.main=0.8, 
    cex.sub=0.5)
# plot matrix
matplot(vec_var4, type="l", lty="solid", 
col=c("green", "blue", "red"), 
lwd=c(2, 2, 2), xlab="", ylab="")
# add legend
legend(x="bottomright", legend=colnames(vec_var4), 
       title="", inset=0.05, cex=0.8, lwd=2, 
       lty=c(1, 1, 1), col=c("green", "blue", "red"))
rm(list=ls())
color_list <- list("red", "white", "blue")
for (some_color in color_list) {  # loop over list
  print(some_color)
}
for (in_dex in 1:3) {  # loop over vector
  print(color_list[[in_dex]])
}

in_dex <- 1  # 'while' loops need initialization
while (in_dex < 4) {  # while loop
  print(color_list[[in_dex]])
  in_dex <- in_dex + 1
}
rm(list=ls())
# fib_seq <- numeric()  # zero length numeric vector
# pre-allocate vector instead of "growing" it
fib_seq <- numeric(10)
fib_seq[1] <- 0  # initialize
fib_seq[2] <- 1  # initialize

for (i in 3:10) {  # perform recurrence loop
  fib_seq[i] <- fib_seq[i-1] + fib_seq[i-2]
}  # end for

fib_seq
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # for reproducibility
simu_max <- 1000  # max simulation trials
simu_prices <- 0*1:simu_max  # initialize prices
barrier_level <- 20  # barrier level
simu_prices[1] <- 0  # first simulated price
in_dex <- 2  # initialize simulation index
while ((in_dex <= simu_max) && 
 (simu_prices[in_dex - 1] < barrier_level)) {
  simu_prices[in_dex] <- # simulate next price
    simu_prices[in_dex - 1] + rnorm(1)
  in_dex <- in_dex + 1  # advance in_dex
}  # end while
if (in_dex <= simu_max) {  # fill zero prices
  simu_prices[in_dex:simu_max] <- simu_prices[in_dex - 1]
}
# create daily time series starting 2011
ts_var <- ts(data=simu_prices, frequency=365, start=c(2011, 1))
plot(ts_var, type="l", col="black",  # create plot
     lty="solid", xlab="", ylab="")
abline(h=barrier_level, lwd=2, col="red")  # add horizontal line
title(main="Random Prices", line=0)  # add title
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # for reproducibility
simu_max <- 1000  # max simulation trials
barrier_level <- 20  # barrier level
# simulated prices
simu_prices <- cumsum(rnorm(simu_max))
# in_dex is "1" after prices cross barrier_level
in_dex <- cummax(simu_prices > barrier_level)
# find index when prices cross barrier_level
which_index <- which(diff(in_dex)==1)
# fill prices after crossing barrier_level
if (length(which_index)>0) {
  simu_prices[as.logical(in_dex)] <- 
    simu_prices[which_index + 1]
}  # end if
# create daily time series starting 2011
ts_var <- ts(data=simu_prices, frequency=365, start=c(2011, 1))
plot(ts_var, type="l", col="black",  # create plot
     lty="solid", xlab="", ylab="")
abline(h=barrier_level, lwd=2, col="red")  # add horizontal line
title(main="Random Prices", line=0)  # add title
big_matrix <- matrix(rnorm(1000000), ncol=10)
# allocate memory
row_sums <- numeric(nrow(big_matrix))

system.time(
  for(i in 1:nrow(big_matrix)) {
    row_sums[i] <- sum(big_matrix[i,])
  }  # end for
)  # end system.time

system.time(row_sums <- apply(big_matrix, 1, sum))
system.time(row_sums <- apply(big_matrix, 1, sum))

str(rowSums)  # get list of arguments

# calculate row sums
system.time(row_sums <- rowSums(big_matrix))
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
