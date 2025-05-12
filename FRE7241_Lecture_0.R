# Display documentation on function "getwd"
help(getwd)
# Equivalent to "help(getwd)"
?getwd
# Open the hypertext documentation
help.start()
# Calculate cumulative sum of a vector
vecv <- runif(1e5)
# Use compiled function
cumsumv <- cumsum(vecv)
# Use for loop
cumsumv2 <- vecv
for (i in 2:NROW(vecv))
  cumsumv2[i] <- (vecv[i] + cumsumv2[i-1])
# Compare the outputs of the two methods
all.equal(cumsumv, cumsumv2)
# Microbenchmark the two methods
library(microbenchmark)
summary(microbenchmark(
  cumsum=cumsum(vecv), # Vectorized
  loop_alloc={cumsumv2 <- vecv # Allocate memory to cumsumv3
    for (i in 2:NROW(vecv))
cumsumv2[i] <- (vecv[i] + cumsumv2[i-1])
  },
  loop_nalloc={cumsumv3 <- vecv[1] # Doesn't allocate memory to cumsumv3
    for (i in 2:NROW(vecv))
cumsumv3[i] <- (vecv[i] + cumsumv3[i-1])
  }, times=10))[, c(1, 4, 5)]
# "<-" and "=" are valid assignment operators
myvar <- 3
# Typing a symbol or expression evaluates it
myvar
# Text in quotes is interpreted as a string
myvar <- "Hello World!"
# Typing a symbol or expression evaluates it
myvar
myvar  # Text after hash is treated as comment
getwd()  # Get cwd
setwd("/Users/jerzy/Develop/R")  # Set cwd
getwd()  # Get cwd
Sys.time()  # Get date and time
Sys.Date()  # Get date only
rm(list=ls())
setwd("/Users/jerzy/Develop/lecture_slides/data")
var1 <- 3  # Define new object
ls()  # List all objects in workspace
# List objects starting with "v"
ls(pattern=glob2rx("v*"))
# Delete all objects in workspace starting with "v"
rm(list=ls(pattern=glob2rx("v*")))
save.image()  # Save workspace to file .RData in cwd
rm(var1)  # Remove object
ls()  # List objects
load(".RData")
ls()  # List objects
var2 <- 5  # Define another object
save(var1, var2,  # Save selected objects
     file="/Users/jerzy/Develop/lecture_slides/data/my_data.RData")
rm(list=ls())  # Delete all objects in workspace
ls()  # List objects
loadobj <- load(file="/Users/jerzy/Develop/lecture_slides/data/my_data.RData")
loadobj
ls()  # List objects
  q()  # quit R session
history(5)  # Display last 5 commands
savehistory(file="myfile")  # Default is ".Rhistory"
loadhistory(file="myfile")  # Default is ".Rhistory"
sessionInfo()  # Get R version and other session info
# ?options  # Long list of global options
# Interpret strings as characters, not factors
getOption("stringsAsFactors")  # Display option
options("stringsAsFactors")  # Display option
options(stringsAsFactors=FALSE)  # Set option
# Number of digits printed for numeric values
# Control exponential scientific notation of print method
# Positive "scipen" values bias towards fixed notation
# Negative "scipen" values bias towards scientific notation
options(scipen=100)
# Maximum number of items printed to console
options(max.print=30)
# Warning levels options
# Negative - warnings are ignored
options(warn=-1)
# zero - warnings are stored and printed after top-confl function has completed
options(warn=0)
# One - warnings are printed as they occur
options(warn=1)
# 2 or larger - warnings are turned into errors
options(warn=2)
# Save all options in variable
optionv <- options()
# Restore all options from variable
options(optionv)
rm(list=ls())
# Get base environment
baseenv()
# Get global environment
globalenv()
# Get current environment
environment()
# Get environment class
class(environment())
# Define variable in current environment
globv <- 1
# Get objects in current environment
ls(environment())
# Create new environment
envv <- new.env()
# Get calling environment of new environment
parent.env(envv)
# Assign Value to Name
assign("new_var1", 3, envir=envv)
# Create object in new environment
envv$new_var2 <- 11
# Get objects in new environment
ls(envv)
# Get objects in current environment
ls(environment())
# Environments are subset like listv
envv$new_var1
# Environments are subset like listv
envv[["new_var1"]]
search()  # Get search path for R objects
my_list <- list(flowers=c("rose", "daisy", "tulip"),
        trees=c("pine", "oak", "maple"))
my_list$trees
attach(my_list)
trees
search()  # Get search path for R objects
detach(my_list)
head(trees)  # "trees" is in datasets base package
library(rutils)  # Load package rutils
# Define ETF symbols
symbolv <- c("VTI", "VEU", "IEF", "VNQ")
# Extract symbolv from rutils::etfenv
pricev <- mget(symbolv, envir=rutils::etfenv)
# prices is a list of xts series
class(pricev)
class(pricev[[1]])
# Extract Close prices
pricev <- lapply(pricev, quantmod::Cl)
# Collapse list into time series the hard way
xts1 <- cbind(pricev[[1]], pricev[[2]], pricev[[3]], pricev[[4]])
class(xts1)
dim(xts1)
# Collapse list into time series using do.call()
pricev <- do.call(cbind, pricev)
all.equal(xts1, pricev)
class(pricev)
dim(pricev)
# Extract and cbind in single step
pricev <- do.call(cbind, lapply(
  mget(symbolv, envir=rutils::etfenv), quantmod::Cl))
# Or
# Extract and bind all data, subset by symbolv
pricev <- lapply(symbolv, function(symbol) {
    quantmod::Cl(get(symbol, envir=rutils::etfenv))
})  # end lapply
# Same, but loop over etfenv without anonymous function
pricev <- do.call(cbind,
  lapply(as.list(rutils::etfenv)[symbolv], quantmod::Cl))
# Same, but works only for OHLC series - produces error
pricev <- do.call(cbind,
  eapply(rutils::etfenv, quantmod::Cl)[symbolv])
# Drop ".Close" from column names
colnames(pricev)
do.call(rbind, strsplit(colnames(pricev), split="[.]"))[, 1]
colnames(pricev) <- do.call(rbind, strsplit(colnames(pricev), split="[.]"))[, 1]
# Or
colnames(pricev) <- unname(sapply(colnames(pricev),
    function(colname) strsplit(colname, split="[.]")[[1]][1]))
tail(pricev, 3)
# Which objects in global environment are class xts?
unlist(eapply(globalenv(), is.xts))
# Save xts to csv file
write.zoo(pricev,
  file="/Users/jerzy/Develop/lecture_slides/data/etf_series.csv", sep=",")
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
# install package "babynamev" from GitHub
install_github(repo="hadley/babynamev")
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
getOption("defaultPackages")
# matrix of installed package information
packinfo <- installed.packages()
dim(packinfo)
# get all installed package names
sort(unname(packinfo[, "Package"]))
# get a few package names and their versions
packinfo[sample(x=1:100, 5), c("Package", "Version")]
# get info for package "xts"
t(packinfo["xts", ])
# list directories in "PortfolioAnalytics" sub-directory
gsub(
  "C:/Users/Jerzy/Documents/R/win-library/3.1",
  "~",
  list.dirs(
    file.path(
      .libPaths()[1],
      "PortfolioAnalytics")))
# load package, produce error if can't be loaded
library(MASS)
# load package, return TRUE if loaded successfully
require(MASS)
# load quietly
library(MASS, quietly=TRUE)
# load without any messages
suppressMessages(library(MASS))
# remove package from search path
detach(MASS)
# install package if it can't be loaded successfully
if (!require("xts")) install.packages("xts")
# calculate VTI volume-weighted average price
vwapv <- TTR::VWAP(
  price=quantmod::Cl(rutils::etfenv$VTI),
  volume=quantmod::Vo(rutils::etfenv$VTI), n=10)
library()  # list all packages installed on the system
search()  # list all loaded packages on search path
# get documentation for package "Ecdat"
packageDescription("Ecdat")  # get short description
help(package="Ecdat")  # load help page
library(Ecdat)  # load package "Ecdat"
data(package="Ecdat")  # list all datasets in "Ecdat"
ls("package:Ecdat")  # list all objects in "Ecdat"
browseVignettes("Ecdat")  # view package vignette
detach("package:Ecdat")  # remove Ecdat from search path
library(Ecdat)  # load econometric data sets
class(Garch)  # Garch is a data frame from "Ecdat"
dim(Garch)  # daily currency prices
head(Garch[, -2])  # col 'dm' is Deutsch Mark
detach("package:Ecdat")  # remove Ecdat from search path
rm(list=ls())
search()  # get search path for R objects
library(MASS)  # load package "MASS"
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
library(MASS)  # load package 'MASS'
select  # code of primitive function from package 'MASS'
getAnywhere("cbind.ts")
library(microbenchmark)
vecv <- runif(1e6)
# sqrt() and "^0.5" are the same
all.equal(sqrt(vecv), vecv^0.5)
# sqrt() is much faster than "^0.5"
system.time(vecv^0.5)
microbenchmark(
  power = vecv^0.5,
  sqrt = sqrt(vecv),
  times=10)
# Calculate matrix of random data with 5,000 rows
matv <- matrix(rnorm(10000), ncol=2)
# Allocate memory for row sums
rowsumv <- numeric(NROW(matv))
summary(microbenchmark(
  rowsumv = rowSums(matv),  # end rowsumv
  applyloop = apply(matv, 1, sum),  # end apply
  lapply = lapply(1:NROW(matv), function(indeks)
    sum(matv[indeks, ])),  # end lapply
  vapply = vapply(1:NROW(matv), function(indeks)
    sum(matv[indeks, ]),
    FUN.VALUE = c(sum=0)),  # end vapply
  sapply = sapply(1:NROW(matv), function(indeks)
    sum(matv[indeks, ])),  # end sapply
  forloop = for (i in 1:NROW(matv)) {
    rowsumv[i] <- sum(matv[i,])
  },  # end for
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
vecv <- rnorm(5000)
summary(microbenchmark(
# Allocate full memory for cumulative sum
  forloop = {cumsumv <- numeric(NROW(vecv))
    cumsumv[1] <- vecv[1]
    for (i in 2:NROW(vecv)) {
      cumsumv[i] <- cumsumv[i-1] + vecv[i]
    }},  # end for
# Allocate zero memory for cumulative sum
  nalloc = {cumsumv <- numeric(0)
    cumsumv[1] <- vecv[1]
    for (i in 2:NROW(vecv)) {
# Add new element to "cumsumv" ("grow" it)
      cumsumv[i] <- cumsumv[i-1] + vecv[i]
    }},  # end for
# Allocate zero memory for cumulative sum
  combine = {cumsumv <- numeric(0)
    cumsumv[1] <- vecv[1]
    for (i in 2:NROW(vecv)) {
# Add new element to "cumsumv" ("grow" it)
      cumsumv <- c(cumsumv, vecv[i])
    }},  # end for
  times=10))[, c(1, 4, 5)]
vec1 <- rnorm(1000000)
vec2 <- rnorm(1000000)
vecbig <- numeric(1000000)
# Sum two vectors in two different ways
summary(microbenchmark(
  # Sum vectors using "for" loop
  rloop = (for (i in 1:NROW(vec1)) {
    vecbig[i] <- vec1[i] + vec2[i]
  }),
  # Sum vectors using vectorized "+"
  vectorized = (vec1 + vec2),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Allocate memory for cumulative sum
cumsumv <- numeric(NROW(vecbig))
cumsumv[1] <- vecbig[1]
# Calculate cumulative sum in two different ways
summary(microbenchmark(
# Cumulative sum using "for" loop
  rloop = (for (i in 2:NROW(vecbig)) {
    cumsumv[i] <- cumsumv[i-1] + vecbig[i]
  }),
# Cumulative sum using "cumsum"
  vectorized = cumsum(vecbig),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Calculate matrix of random data with 5,000 rows
matv <- matrix(rnorm(10000), ncol=2)
# Calculate row sums two different ways
all.equal(rowSums(matv), apply(matv, 1, sum))
summary(microbenchmark(
  rowsumv = rowSums(matv),
  applyloop = apply(matv, 1, sum),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
library(microbenchmark)
str(pmax)
# Calculate row maximums two different ways
summary(microbenchmark(
  pmax=do.call(pmax.int,
lapply(seq_along(matv[1, ]),
  function(indeks) matv[, indeks])),
  applyloop=unlist(lapply(seq_along(matv[, 1]),
  function(indeks) max(matv[indeks, ]))),
  times=10))[, c(1, 4, 5)]
install.packages("matrixStats")  # Install package matrixStats
library(matrixStats)  # Load package matrixStats
# Calculate row min values three different ways
summary(microbenchmark(
  rowmins = rowMins(matv),
  pmin =
    do.call(pmin.int,
      lapply(seq_along(matv[1, ]),
             function(indeks)
               matv[, indeks])),
  as_dframe =
    do.call(pmin.int,
      as.data.frame.matrix(matv)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
install.packages("Rfast")  # Install package Rfast
library(Rfast)  # Load package Rfast
# Benchmark speed of calculating ranks
vecv <- 1e3
all.equal(rank(vecv), Rfast::Rank(vecv))
library(microbenchmark)
summary(microbenchmark(
  rcode = rank(vecv),
  Rfast = Rfast::Rank(vecv),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Benchmark speed of calculating column medians
matv <- matrix(1e4, nc=10)
all.equal(matrixStats::colMedians(matv), Rfast::colMedians(matv))
summary(microbenchmark(
  matrixStats = matrixStats::colMedians(matv),
  Rfast = Rfast::colMedians(matv),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
summary(microbenchmark(  # Assign values to vector three different ways
# Fast vectorized assignment loop performed in C using brackets "[]"
  brackets = {vecv <- numeric(10)
    vecv[] <- 2},
# Slow because loop is performed in R
  forloop = {vecv <- numeric(10)
    for (indeks in seq_along(vecv))
      vecv[indeks] <- 2},
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
summary(microbenchmark(  # Assign values to vector two different ways
# Fast vectorized assignment loop performed in C using brackets "[]"
  brackets = {vecv <- numeric(10)
    vecv[4:7] <- rnorm(4)},
# Slow because loop is performed in R
  forloop = {vecv <- numeric(10)
    for (indeks in 4:7)
      vecv[indeks] <- rnorm(1)},
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Define function vectorized automatically
my_fun <- function(input, param) {
  param*input
}  # end my_fun
# "input" is vectorized
my_fun(input=1:3, param=2)
# "param" is vectorized
my_fun(input=10, param=2:4)
# Define vectors of parameters of rnorm()
stdevs <- structure(1:3, names=paste0("sd=", 1:3))
means <- structure(-1:1, names=paste0("mean=", -1:1))
# "sd" argument of rnorm() isn't vectorized
rnorm(1, sd=stdevs)
# "mean" argument of rnorm() isn't vectorized
rnorm(1, mean=means)
# Loop over stdevs produces vector output
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
sapply(stdevs, function(stdev) rnorm(n=2, sd=stdev))
# Same
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
sapply(stdevs, rnorm, n=2, mean=0)
# Loop over means
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
sapply(means, function(meanv) rnorm(n=2, mean=meanv))
# Same
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
sapply(means, rnorm, n=2)
# rnorm() vectorized with respect to "stdev"
vec_rnorm <- function(n, mean=0, sd=1) {
  if (NROW(sd)==1)
    rnorm(n=n, mean=mean, sd=sd)
  else
    sapply(sd, rnorm, n=n, mean=mean)
}  # end vec_rnorm
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
vec_rnorm(n=2, sd=stdevs)
# rnorm() vectorized with respect to "mean" and "sd"
vec_rnorm <- Vectorize(FUN=rnorm,
        vectorize.args=c("mean", "sd")
)  # end Vectorize
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
vec_rnorm(n=2, sd=stdevs)
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
vec_rnorm(n=2, mean=means)
str(sum)
# na.rm is bound by name
mapply(sum, 6:9, c(5, NA, 3), 2:6, na.rm=TRUE)
str(rnorm)
# mapply vectorizes both arguments "mean" and "sd"
mapply(rnorm, n=5, mean=means, sd=stdevs)
mapply(function(input, e_xp) input^e_xp,
 1:5, seq(from=1, by=0.2, length.out=5))
# rnorm() vectorized with respect to "mean" and "sd"
vec_rnorm <- function(n, mean=0, sd=1) {
  if (NROW(mean)==1 && NROW(sd)==1)
    rnorm(n=n, mean=mean, sd=sd)
  else
    mapply(rnorm, n=n, mean=mean, sd=sd)
}  # end vec_rnorm
# Call vec_rnorm() on vector of "sd"
vec_rnorm(n=2, sd=stdevs)
# Call vec_rnorm() on vector of "mean"
vec_rnorm(n=2, mean=means)
# Create two numeric vectors
vec1 <- sin(0.25*pi*1:20)
vec2 <- cos(0.25*pi*1:20)
# Create third vector using 'ifelse'
vec3 <- ifelse(vec1 > vec2, vec1, vec2)
# cbind all three together
vec3 <- cbind(vec1, vec2, vec3)
colnames(vec3)[3] <- "Max"
# Set plotting parameters
x11(width=6, height=7)
par(oma=c(0, 1, 1, 1), mar=c(0, 2, 2, 1),
    mgp=c(2, 1, 0), cex.lab=0.5, cex.axis=1.0, cex.main=1.8, cex.sub=0.5)
# Plot matrix
zoo::plot.zoo(vec3, lwd=2, ylim=c(-1, 1),
  xlab="", col=c("green", "blue", "red"),
  main="ifelse() Calculates The Max of Two Data Sets")
# Calculate cumulative sum of a vector
vecv <- runif(1e5)
# Use compiled function
cumsumv <- cumsum(vecv)
# Use for loop
cumsumv2 <- vecv
for (i in 2:NROW(vecv))
  cumsumv2[i] <- (vecv[i] + cumsumv2[i-1])
# Compare the outputs of the two methods
all.equal(cumsumv, cumsumv2)
# Microbenchmark the two methods
library(microbenchmark)
summary(microbenchmark(
  cumsum=cumsum(vecv), # Vectorized
  loop_alloc={cumsumv2 <- vecv # Allocate memory to cumsumv3
    for (i in 2:NROW(vecv))
cumsumv2[i] <- (vecv[i] + cumsumv2[i-1])
  },
  loop_nalloc={cumsumv3 <- vecv[1] # Doesn't allocate memory to cumsumv3
    for (i in 2:NROW(vecv))
cumsumv3[i] <- (vecv[i] + cumsumv3[i-1])
  }, times=10))[, c(1, 4, 5)]
library(parallel)  # Load package parallel
# Get short description
packageDescription("parallel")
# Load help page
help(package="parallel")
# List all objects in "parallel"
ls("package:parallel")
# Define function that pauses execution
paws <- function(x, sleep_time=0.01) {
  Sys.sleep(sleep_time)
  x
}  # end paws
library(parallel)  # Load package parallel
# Calculate number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
compclust <- makeCluster(ncores)
# Perform parallel loop under Windows
outv <- parLapply(compclust, 1:10, paws)
# Perform parallel loop under Mac-OSX or Linux
outv <- mclapply(1:10, paws, mc.cores=ncores)
library(microbenchmark)  # Load package microbenchmark
# Compare speed of lapply versus parallel computing
summary(microbenchmark(
  standard = lapply(1:10, paws),
  # parallel = parLapply(compclust, 1:10, paws),
  parallel = mclapply(1:10, paws, mc.cores=ncores),
  times=10)
)[, c(1, 4, 5)]
# Compare speed of lapply with parallel computing
runv <- 3:10
timev <- sapply(runv, function(nruns) {
    summary(microbenchmark(
standard = lapply(1:nruns, paws),
# parallel = parLapply(compclust, 1:nruns, paws),
parallel = mclapply(1:nruns, paws, mc.cores=ncores),
times=10))[, 4]
    })  # end sapply
timev <- t(timev)
colnames(timev) <- c("standard", "parallel")
rownames(timev) <- runv
# Stop R processes over cluster under Windows
stopCluster(compclust)
x11(width=6, height=5)
plot(x=rownames(timev),
     y=timev[, "standard"],
     type="l", lwd=2, col="blue",
     main="Compute times",
     xlab="Number of iterations in loop", ylab="",
     ylim=c(0, max(timev[, "standard"])))
lines(x=rownames(timev),
y=timev[, "parallel"], lwd=2, col="green")
legend(x="topleft", legend=colnames(timev),
 inset=0.1, cex=1.0, bty="n", bg="white",
 y.intersp=0.3, lwd=2, lty=1, col=c("blue", "green"))
library(parallel)  # Load package parallel
# Calculate number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
compclust <- makeCluster(ncores)
# Calculate matrix of random data
matv <- matrix(rnorm(1e5), ncol=100)
# Define aggregation function over column of matrix
aggfun <- function(column) {
  datav <- 0
  for (indeks in 1:NROW(column))
    datav <- datav + column[indeks]
  datav
}  # end aggfun
# Perform parallel aggregations over columns of matrix
aggs <- parCapply(compclust, matv, aggfun)
# Compare speed of apply with parallel computing
summary(microbenchmark(
  apply=apply(matv, MARGIN=2, aggfun),
  parapply=parCapply(compclust, matv, aggfun),
  times=10)
)[, c(1, 4, 5)]
# Stop R processes over cluster under Windows
stopCluster(compclust)
library(parallel)  # Load package parallel
# Calculate number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
compclust <- makeCluster(ncores)
basep <- 2
# Fails because child processes don't know basep:
parLapply(compclust, 2:4, function(exponent) basep^exponent)
# basep passed to child via dots ... argument:
parLapply(compclust, 2:4, function(exponent, basep) basep^exponent,
    basep=basep)
# basep passed to child via clusterExport:
clusterExport(compclust, "basep")
parLapply(compclust, 2:4, function(exponent) basep^exponent)
# Fails because child processes don't know zoo::index():
parSapply(compclust, c("VTI", "IEF", "DBC"), function(symbol)
  NROW(zoo::index(get(symbol, envir=rutils::etfenv))))
# zoo function referenced using "::" in child process:
parSapply(compclust, c("VTI", "IEF", "DBC"), function(symbol)
  NROW(zoo::index(get(symbol, envir=rutils::etfenv))))
# Package zoo loaded in child process:
parSapply(compclust, c("VTI", "IEF", "DBC"), function(symbol) {
  stopifnot("package:zoo" %in% search() || require("zoo", quietly=TRUE))
  NROW(zoo::index(get(symbol, envir=rutils::etfenv)))
})  # end parSapply
# Stop R processes over cluster under Windows
stopCluster(compclust)
library(parallel)  # Load package parallel
# Calculate number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
compclust <- makeCluster(ncores)
# Set seed for cluster under Windows
# Doesn't work: set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
clusterSetRNGStream(compclust, 1121)
# Perform parallel loop under Windows
datav <- parLapply(compclust, 1:10, rnorm, n=100)
sum(unlist(datav))
# Stop R processes over cluster under Windows
stopCluster(compclust)
# Perform parallel loop under Mac-OSX or Linux
datav <- mclapply(1:10, rnorm, mc.cores=ncores, n=100)
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# Sample from Standard Normal Distribution
nsimu <- 1000
datav <- rnorm(nsimu)
# Sample mean - MC estimate
mean(datav)
# Sample standard deviation - MC estimate
sd(datav)
# Monte Carlo estimate of cumulative probability
pnorm(-2)
sum(datav < (-2))/nsimu
# Monte Carlo estimate of quantile
confl <- 0.02
qnorm(confl)  # Exact value
cutoff <- confl*nsimu
datav <- sort(datav)
datav[cutoff]  # Naive Monte Carlo value
quantile(datav, probs=confl)
# Analyze the source code of quantile()
stats:::quantile.default
# Microbenchmark quantile
library(microbenchmark)
summary(microbenchmark(
  monte_carlo = datav[cutoff],
  quantv = quantile(datav, probs=confl),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Sample from Standard Normal Distribution
nsimu <- 1000; datav <- rnorm(nsimu)
# Sample mean and standard deviation
mean(datav); sd(datav)
# Bootstrap of sample mean and median
nboot <- 10000
bootd <- sapply(1:nboot, function(x) {
  # Sample from Standard Normal Distribution
  samplev <- rnorm(nsimu)
  c(mean=mean(samplev), median=median(samplev))
})  # end sapply
bootd[, 1:3]
bootd <- t(bootd)
# Standard error from formula
sd(datav)/sqrt(nsimu)
# Standard error of mean from bootstrap
sd(bootd[, "mean"])
# Standard error of median from bootstrap
sd(bootd[, "median"])
# Plot the densities of the bootstrap data
x11(width=6, height=5)
plot(density(bootd[, "mean"]), lwd=3, xlab="Estimator Value",
     main="Distribution of Bootstrapped Mean and Median", col="green")
lines(density(bootd[, "median"]), lwd=3, col="blue")
abline(v=mean(bootd[, "mean"]), lwd=2, col="red")
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("mean", "median"), bty="n", y.intersp=0.4,
 lwd=6, bg="white", col=c("green", "blue"))
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nsimu <- 1000
# Bootstrap of sample mean and median
nboot <- 100
bootd <- sapply(1:nboot, function(x) median(rnorm(nsimu)))
# Perform vectorized bootstrap
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# Calculate matrix of random data
samplev <- matrix(rnorm(nboot*nsimu), ncol=nboot)
bootv <- matrixStats::colMedians(samplev)
all.equal(bootd, bootv)
# Compare speed of loops with vectorized R code
library(microbenchmark)
summary(microbenchmark(
  loop = sapply(1:nboot, function(x) median(rnorm(nsimu))),
  cpp = {
    samplev <- matrix(rnorm(nboot*nsimu), ncol=nboot)
    matrixStats::colMedians(samplev)
    },
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
compclust <- makeCluster(ncores)  # Initialize compute cluster under Windows
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# Sample from Standard Normal Distribution
nsimu <- 1000
# Bootstrap mean and median under Windows
nboot <- 10000
bootd <- parLapply(compclust, 1:nboot, function(x, datav, nsimu) {
  samplev <- rnorm(nsimu)
  c(mean=mean(samplev), median=median(samplev))
}, datav=datav, nsimu=nsimu)  # end parLapply
# Bootstrap mean and median under Mac-OSX or Linux
bootd <- mclapply(1:nboot, function(x) {
  samplev <- rnorm(nsimu)
  c(mean=mean(samplev), median=median(samplev))
}, mc.cores=ncores)  # end mclapply
bootd <- rutils::do_call(rbind, bootd)
# Means and standard errors from bootstrap
apply(bootd, MARGIN=2, function(x) c(mean=mean(x), stderror=sd(x)))
# Standard error from formula
sd(datav)/sqrt(nsimu)
stopCluster(compclust)  # Stop R processes over cluster under Windows
nsimu <- 1000
datav <- rnorm(nsimu)
sd(datav); mad(datav)
median(abs(datav - median(datav)))
median(abs(datav - median(datav)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
nboot <- 10000
bootd <- sapply(1:nboot, function(x) {
  samplev <- rnorm(nsimu)
  c(sd=sd(samplev), mad=mad(samplev))
})  # end sapply
bootd <- t(bootd)
# Analyze bootstrapped variance
head(bootd)
sum(is.na(bootd))
# Means and standard errors from bootstrap
apply(bootd, MARGIN=2, function(x) c(mean=mean(x), stderror=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
compclust <- makeCluster(ncores)  # Initialize compute cluster
bootd <- parLapply(compclust, 1:nboot, function(x, datav) {
  samplev <- rnorm(nsimu)
  c(sd=sd(samplev), mad=mad(samplev))
}, datav=datav)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:nboot, function(x) {
  samplev <- rnorm(nsimu)
  c(sd=sd(samplev), mad=mad(samplev))
}, mc.cores=ncores)  # end mclapply
stopCluster(compclust)  # Stop R processes over cluster
bootd <- rutils::do_call(rbind, bootd)
# Means and standard errors from bootstrap
apply(bootd, MARGIN=2, function(x) c(mean=mean(x), stderror=sd(x)))
# Calculate time series of VTI returns
library(rutils)
retp <- rutils::etfenv$returns$VTI
retp <- na.omit(retp)
nrows <- NROW(retp)
# Sample from VTI returns
samplev <- retp[sample.int(nrows, replace=TRUE)]
c(sd=sd(samplev), mad=mad(samplev))
# sample.int() is a little faster than sample()
library(microbenchmark)
summary(microbenchmark(
  sample.int = sample.int(1e3),
  sample = sample(1e3),
  times=10))[, c(1, 4, 5)]
# Sample from time series of VTI returns
library(rutils)
retp <- rutils::etfenv$returns$VTI
retp <- na.omit(retp)
nrows <- NROW(retp)
# Bootstrap sd and MAD under Windows
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
compclust <- makeCluster(ncores)  # Initialize compute cluster under Windows
clusterSetRNGStream(compclust, 1121)  # Reset random number generator in all cores
nboot <- 10000
bootd <- parLapply(compclust, 1:nboot, function(x, retp, nsimu) {
  samplev <- retp[sample.int(nsimu, replace=TRUE)]
  c(sd=sd(samplev), mad=mad(samplev))
}, retp=retp, nsimu=nsimu)  # end parLapply
# Bootstrap sd and MAD under Mac-OSX or Linux
bootd <- mclapply(1:nboot, function(x) {
  samplev <- retp[sample.int(nsimu, replace=TRUE)]
  c(sd=sd(samplev), mad=mad(samplev))
}, mc.cores=ncores)  # end mclapply
stopCluster(compclust)  # Stop R processes over cluster under Windows
bootd <- rutils::do_call(rbind, bootd)
# Standard error of standard deviation assuming normal distribution of returns
sd(retp)/sqrt(nsimu)
# Means and standard errors from bootstrap
stderrors <- apply(bootd, MARGIN=2,
  function(x) c(mean=mean(x), stderror=sd(x)))
stderrors
# Relative standard errors
stderrors[2, ]/stderrors[1, ]
# Initialize random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# Define predictor and response variables
nsimu <- 100
predm <- rnorm(nsimu, mean=2)
noisev <- rnorm(nsimu)
respv <- (-3 + 2*predm + noisev)
desm <- cbind(respv, predm)
# Calculate alpha and beta regression coefficients
betac <- cov(desm[, 1], desm[, 2])/var(desm[, 2])
alphac <- mean(desm[, 1]) - betac*mean(desm[, 2])
x11(width=6, height=5)
plot(respv ~ predm, data=desm)
abline(a=alphac, b=betac, lwd=3, col="blue")
# Bootstrap of beta regression coefficient
nboot <- 100
bootd <- sapply(1:nboot, function(x) {
  samplev <- sample.int(nsimu, replace=TRUE)
  desm <- desm[samplev, ]
  cov(desm[, 1], desm[, 2])/var(desm[, 2])
})  # end sapply
x11(width=6, height=5)
par(oma=c(1, 2, 1, 0), mgp=c(2, 1, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
# Mean and standard error of beta regression coefficient
c(mean=mean(bootd), stderror=sd(bootd))
# Plot density of bootstrapped beta coefficients
plot(density(bootd), lwd=2, xlab="Regression slopes",
     main="Bootstrapped Regression Slopes")
# Add line for expected value
abline(v=mean(bootd), lwd=2, col="red")
text(x=mean(bootd)-0.01, y=1.0, labels="expected value",
     lwd=2, srt=90, pos=3)
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
compclust <- makeCluster(ncores)  # Initialize compute cluster under Windows
# Bootstrap of regression under Windows
bootd <- parLapply(compclust, 1:1000, function(x, desm) {
  samplev <- sample.int(nsimu, replace=TRUE)
  desm <- desm[samplev, ]
  cov(desm[, 1], desm[, 2])/var(desm[, 2])
}, desm=desm)  # end parLapply
# Bootstrap of regression under Mac-OSX or Linux
bootd <- mclapply(1:1000, function(x) {
  samplev <- sample.int(nsimu, replace=TRUE)
  desm <- desm[samplev, ]
  cov(desm[, 1], desm[, 2])/var(desm[, 2])
}, mc.cores=ncores)  # end mclapply
stopCluster(compclust)  # Stop R processes over cluster under Windows
# Collapse the bootstrap list into a vector
class(bootd)
bootd <- unlist(bootd)
# Mean and standard error of beta regression coefficient
c(mean=mean(bootd), stderror=sd(bootd))
# Plot density of bootstrapped beta coefficients
plot(density(bootd),
     lwd=2, xlab="Regression slopes",
     main="Bootstrapped Regression Slopes")
# Add line for expected value
abline(v=mean(bootd), lwd=2, col="red")
text(x=mean(bootd)-0.01, y=1.0, labels="expected value",
     lwd=2, srt=90, pos=3)
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
barl <- 20  # Barrier level
nsteps <- 1000  # Number of simulation steps
pathv <- numeric(nsteps)  # Allocate path vector
pathv[1] <- rnorm(1)  # Initialize path
it <- 2  # Initialize simulation index
while ((it <= nsteps) && (pathv[it - 1] < barl)) {
# Simulate next step
  pathv[it] <- pathv[it - 1] + rnorm(1)
  it <- it + 1  # Advance index
}  # end while
# Fill remaining path after it crosses barl
if (it <= nsteps)
  pathv[it:nsteps] <- pathv[it - 1]
# Plot the Brownian motion
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
plot(pathv, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=barl, lwd=3, col="red")
title(main="Brownian Motion Crossing a Barrier Level", line=0.5)
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
barl <- 20  # Barrier level
nsteps <- 1000  # Number of simulation steps
# Simulate path of Brownian motion
pathv <- cumsum(rnorm(nsteps))
# Find index when path crosses barl
crossp <- which(pathv > barl)
# Fill remaining path after it crosses barl
if (NROW(crossp) > 0) {
  pathv[(crossp[1]+1):nsteps] <- pathv[crossp[1]]
}  # end if
# Plot the Brownian motion
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
plot(pathv, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=barl, lwd=3, col="red")
title(main="Brownian Motion Crossing a Barrier Level", line=0.5)
# Define Brownian motion parameters
sigmav <- 1.0  # Volatility
drift <- 0.0  # Drift
nsteps <- 1000  # Number of simulation steps
npaths <- 100  # Number of simulation paths
# Simulate multiple paths of Brownian motion
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
pathm <- rnorm(npaths*nsteps, mean=drift, sd=sigmav)
pathm <- matrix(pathm, nc=npaths)
pathm <- matrixStats::colCumsums(pathm)
# Final distribution of paths
mean(pathm[nsteps, ]) ; sd(pathm[nsteps, ])
# Calculate option payout at maturity
strikep <- 50  # Strike price
payouts <- (pathm[nsteps, ] - strikep)
sum(payouts[payouts > 0])/npaths
# Calculate probability of crossing the barrier at any point
barl <- 50
crossi <- (colSums(pathm > barl) > 0)
sum(crossi)/npaths
# Plot in window
x11(width=6, height=5)
par(mar=c(4, 3, 2, 2), oma=c(0, 0, 0, 0), mgp=c(2.5, 1, 0))
# Select and plot full range of paths
ordern <- order(pathm[nsteps, ])
pathm[nsteps, ordern]
indeks <- ordern[seq(1, 100, 9)]
zoo::plot.zoo(pathm[, indeks], main="Paths of Brownian Motion",
  xlab="time steps", ylab=NA, plot.type="single")
abline(h=strikep, col="red", lwd=3)
text(x=(nsteps-60), y=strikep, labels="strike price", pos=3, cex=1)
# Calculate percentage returns from VTI prices
library(rutils)
pricev <- quantmod::Cl(rutils::etfenv$VTI)
prici <- as.numeric(pricev[1, ])
retp <- rutils::diffit(log(pricev))
class(retp); head(retp)
sum(is.na(retp))
nrows <- NROW(retp)
# Define barrier level with respect to prices
barl <- 1.5*max(pricev)
# Calculate single bootstrap sample
samplev <- retp[sample.int(nrows, replace=TRUE)]
# Calculate prices from percentage returns
samplev <- prici*exp(cumsum(samplev))
# Calculate if prices crossed barrier
sum(samplev > barl) > 0
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
compclust <- makeCluster(ncores)  # Initialize compute cluster under Windows
# Perform parallel bootstrap under Windows
clusterSetRNGStream(compclust, 1121)  # Reset random number generator in all cores
clusterExport(compclust, c("prici", "barl"))
nboot <- 10000
bootd <- parLapply(compclust, 1:nboot, function(x, retp, nrows) {
  samplev <- retp[sample.int(nrows, replace=TRUE)]
  # Calculate prices from percentage returns
  samplev <- prici*exp(cumsum(samplev))
  # Calculate if prices crossed barrier
  sum(samplev > barl) > 0
}, retp=retp, nrows=nrows)  # end parLapply
stopCluster(compclust)  # Stop R processes over cluster under Windows
# Perform parallel bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:nboot, function(x) {
  samplev <- retp[sample.int(nrows, replace=TRUE)]
  # Calculate prices from percentage returns
  samplev <- prici*exp(cumsum(samplev))
  # Calculate if prices crossed barrier
  sum(samplev > barl) > 0
}, mc.cores=ncores)  # end mclapply
bootd <- rutils::do_call(c, bootd)
# Calculate frequency of crossing barrier
sum(bootd)/nboot
# Calculate percentage returns from VTI prices
library(rutils)
ohlc <- rutils::etfenv$VTI
pricev <- as.numeric(ohlc[, 4])
prici <- pricev[1]
retp <- rutils::diffit(log(pricev))
nrows <- NROW(retp)
# Calculate difference of OHLC price columns
pricediff <- ohlc[, 1:3] - pricev
class(retp); head(retp)
# Calculate bootstrap prices from percentage returns
datav <- sample.int(nrows, replace=TRUE)
priceboot <- prici*exp(cumsum(retp[datav]))
ohlcboot <- pricediff + priceboot
ohlcboot <- cbind(ohlcboot, priceboot)
# Define barrier level with respect to prices
barl <- 1.5*max(pricev)
# Calculate if High bootstrapped prices crossed barrier level
sum(ohlcboot[, 2] > barl) > 0
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
compclust <- makeCluster(ncores)  # Initialize compute cluster under Windows
# Perform parallel bootstrap under Windows
clusterSetRNGStream(compclust, 1121)  # Reset random number generator in all cores
clusterExport(compclust, c("prici", "barl", "pricediff"))
nboot <- 10000
bootd <- parLapply(compclust, 1:nboot, function(x, retp, nrows) {
  # Calculate OHLC prices from percentage returns
  datav <- sample.int(nrows, replace=TRUE)
  priceboot <- prici*exp(cumsum(retp[datav]))
  ohlcboot <- pricediff + priceboot
  ohlcboot <- cbind(ohlcboot, priceboot)
  # Calculate statistic
  sum(ohlcboot[, 2] > barl) > 0
}, retp=retp, nrows=nrows)  # end parLapply
# Perform parallel bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:nboot, function(x) {
  # Calculate OHLC prices from percentage returns
  datav <- sample.int(nrows, replace=TRUE)
  priceboot <- prici*exp(cumsum(retp[datav]))
  ohlcboot <- pricediff + priceboot
  ohlcboot <- cbind(ohlcboot, priceboot)
  # Calculate statistic
  sum(ohlcboot[, 2] > barl) > 0
}, mc.cores=ncores)  # end mclapply
stopCluster(compclust)  # Stop R processes over cluster under Windows
bootd <- rutils::do_call(rbind, bootd)
# Calculate frequency of crossing barrier
sum(bootd)/nboot
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# Sample from Standard Normal Distribution
nsimu <- 1000
datav <- rnorm(nsimu)
# Estimate the 95% quantile
nboot <- 10000
bootd <- sapply(1:nboot, function(x) {
  samplev <- datav[sample.int(nsimu, replace=TRUE)]
  quantile(samplev, 0.95)
})  # end sapply
sd(bootd)
# Estimate the 95% quantile using antithetic sampling
bootd <- sapply(1:nboot, function(x) {
  samplev <- datav[sample.int(nsimu, replace=TRUE)]
  quantile(c(samplev, -samplev), 0.95)
})  # end sapply
# Standard error of quantile from bootstrap
sd(bootd)
sqrt(2)*sd(bootd)
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot a Normal probability distribution
curve(expr=dnorm, xlim=c(-3, 4),
main="Shifted Normal distribution function",
xlab="", ylab="", lwd=3, col="blue")
# Add shifted Normal probability distribution
curve(expr=dnorm(x, mean=1), add=TRUE, lwd=3, col="red")
# Add vertical dashed lines
abline(v=0, lwd=3, col="blue", lty="dashed")
abline(v=1, lwd=3, col="red", lty="dashed")
arrows(x0=0, y0=0.1, x1=1, y1=0.1, lwd=3,
 code=2, angle=20, length=grid::unit(0.2, "cm"))
text(x=0.3, 0.1, labels=bquote(lambda), pos=3, cex=2)
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection") # Reset random number generator
# Sample from Standard Normal Distribution
nsimu <- 1000
datav <- rnorm(nsimu)
# Cumulative probability from formula
quantv <- (-2)
pnorm(quantv)
integrate(dnorm, lower=-Inf, upper=quantv)
# Cumulative probability from Naive Monte Carlo
sum(datav < quantv)/nsimu
# Generate importance sample
lambdaf <- (-1.5)  # Tilt parameter
datat <- datav + lambdaf  # Tilt the random numbers
# Cumulative probability from importance sample - wrong!
sum(datat < quantv)/nsimu
# Cumulative probability from importance sample - correct
weightv <- exp(-lambdaf*datat + lambdaf^2/2)
sum((datat < quantv)*weightv)/nsimu
# Bootstrap of standard errors of cumulative probability
nboot <- 1000
bootd <- sapply(1:nboot, function(x) {
  datav <- rnorm(nsimu)
  naivemc <- sum(datav < quantv)/nsimu
  datav <- (datav + lambdaf)
  weightv <- exp(-lambdaf*datav + lambdaf^2/2)
  isample <- sum((datav < quantv)*weightv)/nsimu
  c(naivemc=naivemc, impsample=isample)
}) # end sapply
apply(bootd, MARGIN=1, function(x) c(mean=mean(x), sd=sd(x)))
# Quantile from Naive Monte Carlo
confl <- 0.02
qnorm(confl)  # Exact value
datav <- sort(datav)  # Must be sorted for importance sampling
cutoff <- nsimu*confl
datav[cutoff]  # Naive Monte Carlo value
# Importance sample weights
datat <- datav + lambdaf  # Tilt the random numbers
weightv <- exp(-lambdaf*datat + lambdaf^2/2)
# Cumulative probabilities using importance sample
cumprob <- cumsum(weightv)/nsimu
# Quantile from importance sample
datat[findInterval(confl, cumprob)]
# Bootstrap of standard errors of quantile
nboot <- 1000
bootd <- sapply(1:nboot, function(x) {
  datav <- sort(rnorm(nsimu))
  naivemc <- datav[cutoff]
  datat <- datav + lambdaf
  weightv <- exp(-lambdaf*datat + lambdaf^2/2)
  cumprob <- cumsum(weightv)/nsimu
  isample <- datat[findInterval(confl, cumprob)]
  c(naivemc=naivemc, impsample=isample)
}) # end sapply
apply(bootd, MARGIN=1, function(x) c(mean=mean(x), sd=sd(x)))
# VaR and CVaR from Naive Monte Carlo
varisk <- datav[cutoff]
sum((datav <= varisk)*datav)/sum((datav <= varisk))
# CVaR from importance sample
varisk <- datat[findInterval(confl, cumprob)]
sum((datat <= varisk)*datat*weightv)/sum((datat <= varisk)*weightv)
# CVaR from integration
integrate(function(x) x*dnorm(x), low=-Inf, up=varisk)$value/pnorm(varisk)
# Bootstrap of standard errors of CVaR
nboot <- 1000
bootd <- sapply(1:nboot, function(x) {
  datav <- sort(rnorm(nsimu))
  varisk <- datav[cutoff]
  naivemc <- sum((datav <= varisk)*datav)/sum((datav <= varisk))
  datat <- datav + lambdaf
  weightv <- exp(-lambdaf*datat + lambdaf^2/2)
  cumprob <- cumsum(weightv)/nsimu
  varisk <- datat[findInterval(confl, cumprob)]
  isample <- sum((datat <= varisk)*datat*weightv)/sum((datat <= varisk)*weightv)
  c(naivemc=naivemc, impsample=isample)
}) # end sapply
apply(bootd, MARGIN=1, function(x) c(mean=mean(x), sd=sd(x)))
# Calculate matrix of random data
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection") # Reset random number generator
nsimu <- 1000; nboot <- 100
datav <- matrix(rnorm(nboot*nsimu), ncol=nboot)
datav <- Rfast::colSort(datav)  # Sort the columns
# Bootstrap function for VaR (quantile) for a single tilt parameter
calc_vars <- function(lambdaf, confl=0.05) {
  datat <- datav + lambdaf  # Tilt the random numbers
  weightv <- exp(-lambdaf*datat + lambdaf^2/2)
  # Calculate quantiles for columns
  sapply(1:nboot, function(it) {
    cumprob <- cumsum(weightv[, it])/nsimu
    datat[findInterval(confl, cumprob), it]
  })  # end sapply
}  # end calc_vars
# Bootstrap vector of VaR for a single tilt parameter
bootd <- calc_vars(-1.5)
# Define vector of tilt parameters
lambdav <- seq(-3.0, -1.2, by=0.2)
# Calculate vector of VaR for vector of tilt parameters
varisk <- sapply(lambdav, calc_vars, confl=0.02)
# Calculate standard deviations of VaR for tilt parameters
stdevs <- apply(varisk, MARGIN=2, sd)
# Calculate the optimal tilt parameter
lambdav[which.min(stdevs)]
# Plot the standard deviations
x11(width=6, height=5)
plot(x=lambdav, y=stdevs,
     main="Standard Errors of Simulated VaR",
     xlab="tilt parameter", ylab="standard error",
     type="l", col="blue", lwd=2)
# Binomial sample
nsimu <- 1000
probv <- 0.1
datav <- rbinom(n=nsimu, size=1, probv)
head(datav, 33)
# Tilted binomial sample
lambdaf <- 5
probt <- lambdaf*probv/(1 + probv*(lambdaf - 1))
weightv <- (1 + probv*(lambdaf - 1))/lambdaf
datav <- rbinom(n=nsimu, size=1, probt)
head(datav, 33)
weightv*sum(datav)/nsimu
# Bootstrap of standard errors
nboot <- 1000
bootd <- sapply(1:nboot, function(x) {
  c(naivemc=sum(rbinom(n=nsimu, size=1, probv))/nsimu,
    impsample=weightv*sum(rbinom(n=nsimu, size=1, probt))/nsimu)
}) # end sapply
apply(bootd, MARGIN=1, function(x) c(mean=mean(x), sd=sd(x)))
# Define Brownian motion parameters
sigmav <- 1.0  # Volatility
drift <- 0.0  # Drift
nsteps <- 100  # Number of simulation steps
nsimu <- 10000  # Number of simulation paths
# Calculate matrix of normal variables
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
datav <- rnorm(nsimu*nsteps, mean=drift, sd=sigmav)
datav <- matrix(datav, nc=nsimu)
# Simulate paths of Brownian motion
pathm <- matrixStats::colCumsums(datav)
# Tilt the datav
lambdaf <- 0.04  # Tilt parameter
datat <- datav + lambdaf  # Tilt the random numbers
patht <- matrixStats::colCumsums(datat)
# Calculate path weights
weightm <- exp(-lambdaf*datat + lambdaf^2/2)
weightm <- matrixStats::colProds(weightm)
# Or
weightm <- exp(-lambdaf*colSums(datat) + nsteps*lambdaf^2/2)
# Calculate option payout using naive MC
strikep <- 10  # Strike price
payouts <- (pathm[nsteps, ] - strikep)
sum(payouts[payouts > 0])/nsimu
# Calculate option payout using importance sampling
payouts <- (patht[nsteps, ] - strikep)
sum((weightm*payouts)[payouts > 0])/nsimu
# Calculate crossing probability using naive MC
barl <- 10
crossi <- (colSums(pathm > barl) > 0)
sum(crossi)/nsimu
# Calculate crossing probability using importance sampling
crossi <- colSums(patht > barl) > 0
sum(weightm*crossi)/nsimu
