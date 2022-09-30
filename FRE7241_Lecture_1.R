# Display documentation on function "getwd"
help(getwd)
# Equivalent to "help(getwd)"
?getwd

# Open the hypertext documentation
help.start()

# Calculate cumulative sum of a vector
vectorv <- runif(1e5)
# Use compiled function
cumsumv <- cumsum(vectorv)
# Use for loop
cumsumv2 <- vectorv
for (i in 2:NROW(vectorv))
  cumsumv2[i] <- (vectorv[i] + cumsumv2[i-1])
# Compare the two methods
all.equal(cumsumv, cumsumv2)
# Microbenchmark the two methods
library(microbenchmark)
summary(microbenchmark(
  cumsum=cumsum(vectorv),
  loop_alloc={
    cumsumv2 <- vectorv
    for (i in 2:NROW(vectorv))
cumsumv2[i] <- (vectorv[i] + cumsumv2[i-1])
  },
  loop_nalloc={
    # Doesn't allocate memory to cumsumv3
    cumsumv3 <- vectorv[1]
    for (i in 2:NROW(vectorv))
# This command adds an extra element to cumsumv3
cumsumv3[i] <- (vectorv[i] + cumsumv3[i-1])
  },
  times=10))[, c(1, 4, 5)]

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
# Remove all objects starting with "v"
rm(list=ls(pattern=glob2rx("v*")))
save.image()  # Save workspace to file .RData in cwd
rm(var1)  # Remove object
ls()  # List objects
load(".RData")
ls()  # List objects
var2 <- 5  # Define another object
save(var1, var2,  # Save selected objects
     file="/Users/jerzy/Develop/lecture_slides/data/my_data.RData")
rm(list=ls())  # Remove all objects
ls()  # List objects
load_ed <- load(file="/Users/jerzy/Develop/lecture_slides/data/my_data.RData")
load_ed
ls()  # List objects

  q()  # quit R session

history(5)  # Display last 5 commands
savehistory(file="myfile")  # Default is ".Rhistory"
loadhistory(file="myfile")  # Default is ".Rhistory"

sessionInfo()  # Get R version and other session info

Sys.getenv()[5:7]  # List some environment variables

Sys.getenv("HOME")  # Get R user HOME directory

Sys.setenv(Home="/Users/jerzy/Develop/data")  # Set HOME directory

Sys.getenv("HOME")  # Get user HOME directory

Sys.getenv("R_HOME")  # Get R_HOME directory

R.home()  # Get R_HOME directory

R.home("etc")  # Get "etc" sub-directory of R_HOME

# ?options  # Long list of global options
# Interpret strings as characters, not factors
getOption("stringsAsFactors")  # Display option
options("stringsAsFactors")  # Display option
options(stringsAsFactors=FALSE)  # Set option
# Number of digits printed for numeric values
options(digits=3)
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
op_tions <- options()
# Restore all options from variable
options(op_tions)

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
new_env <- new.env()
# Get calling environment of new environment
parent.env(new_env)
# Assign Value to Name
assign("new_var1", 3, envir=new_env)
# Create object in new environment
new_env$new_var2 <- 11
# Get objects in new environment
ls(new_env)
# Get objects in current environment
ls(environment())
# Environments are subset like listv
new_env$new_var1
# Environments are subset like listv
new_env[["new_var1"]]

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
prices <- mget(symbolv, envir=rutils::etfenv)
# prices is a list of xts series
class(prices)
class(prices[[1]])
# Extract Close prices
prices <- lapply(prices, quantmod::Cl)
# Collapse list into time series the hard way
xts1 <- cbind(prices[[1]], prices[[2]], prices[[3]], prices[[4]])
class(xts1)
dim(xts1)
# Collapse list into time series using do.call()
prices <- do.call(cbind, prices)
all.equal(xts1, prices)
class(prices)
dim(prices)
# Extract and cbind in single step
prices <- do.call(cbind, lapply(
  mget(symbolv, envir=rutils::etfenv), quantmod::Cl))
# Or
# Extract and bind all data, subset by symbolv
prices <- lapply(symbolv, function(symbol) {
    quantmod::Cl(get(symbol, envir=rutils::etfenv))
})  # end lapply
# Same, but loop over etfenv without anonymous function
prices <- do.call(cbind,
  lapply(as.list(rutils::etfenv)[symbolv], quantmod::Cl))
# Same, but works only for OHLC series - produces error
prices <- do.call(cbind,
  eapply(rutils::etfenv, quantmod::Cl)[symbolv])

# Drop ".Close" from column names
colnames(prices[, 1:4])
do.call(rbind, strsplit(colnames(prices[, 1:4]), split="[.]"))[, 1]
colnames(prices) <- do.call(rbind, strsplit(colnames(prices), split="[.]"))[, 1]
# Or
colnames(prices) <- unname(sapply(colnames(prices),
    function(colname) strsplit(colname, split="[.]")[[1]][1]))
tail(prices, 3)
# Which objects in global environment are class xts?
unlist(eapply(globalenv(), is.xts))
# Save xts to csv file
write.zoo(prices,
  file="/Users/jerzy/Develop/lecture_slides/data/etf_series.csv", sep=",")
# Copy prices into etfenv
etfenv$etf_list <- etf_list
# Or
assign("prices", prices, envir=etfenv)
# Save to .RData file
save(etfenv, file="etf_data.RData")

# "trees" is in datasets base package
head(trees, 3)
colnames(trees)
mean(Girth)
mean(trees$Girth)
with(trees,
     c(mean(Girth), mean(Height), mean(Volume)))

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
# install package "babynamesv" from GitHub
install_github(repo="hadley/babynamesv")

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
pack_info <- installed.packages()
dim(pack_info)
# get all installed package names
sort(unname(pack_info[, "Package"]))
# get a few package names and their versions
pack_info[sample(x=1:100, 5), c("Package", "Version")]
# get info for package "xts"
t(pack_info["xts", ])

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
vectorv <- runif(1e6)
# sqrt() and "^0.5" are the same
all.equal(sqrt(vectorv), vectorv^0.5)
# sqrt() is much faster than "^0.5"
system.time(vectorv^0.5)
microbenchmark(
  power = vectorv^0.5,
  sqrt = sqrt(vectorv),
  times=10)

# Calculate matrix of random data with 5,000 rows
matrixv <- matrix(rnorm(10000), ncol=2)
# Allocate memory for row sums
rowsumv <- numeric(NROW(matrixv))
summary(microbenchmark(
  rowsums = rowSums(matrixv),  # end rowsumv
  apply = apply(matrixv, 1, sum),  # end apply
  lapply = lapply(1:NROW(matrixv), function(indeks)
    sum(matrixv[indeks, ])),  # end lapply
  vapply = vapply(1:NROW(matrixv), function(indeks)
    sum(matrixv[indeks, ]),
    FUN.VALUE = c(sum=0)),  # end vapply
  sapply = sapply(1:NROW(matrixv), function(indeks)
    sum(matrixv[indeks, ])),  # end sapply
  forloop = for (i in 1:NROW(matrixv)) {
    rowsumv[i] <- sum(matrixv[i,])
  },  # end for
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

vectorv <- rnorm(5000)
summary(microbenchmark(
# Allocate full memory for cumulative sum
  forloop = {cumsumv <- numeric(NROW(vectorv))
    cumsumv[1] <- vectorv[1]
    for (i in 2:NROW(vectorv)) {
      cumsumv[i] <- cumsumv[i-1] + vectorv[i]
    }},  # end for
# Allocate zero memory for cumulative sum
  grow_vec = {cumsumv <- numeric(0)
    cumsumv[1] <- vectorv[1]
    for (i in 2:NROW(vectorv)) {
# Add new element to "cumsumv" ("grow" it)
      cumsumv[i] <- cumsumv[i-1] + vectorv[i]
    }},  # end for
# Allocate zero memory for cumulative sum
  com_bine = {cumsumv <- numeric(0)
    cumsumv[1] <- vectorv[1]
    for (i in 2:NROW(vectorv)) {
# Add new element to "cumsumv" ("grow" it)
      cumsumv <- c(cumsumv, vectorv[i])
    }},  # end for
  times=10))[, c(1, 4, 5)]

vector1 <- rnorm(1000000)
vector2 <- rnorm(1000000)
big_vector <- numeric(1000000)
# Sum two vectors in two different ways
summary(microbenchmark(
  # Sum vectors using "for" loop
  rloop = (for (i in 1:NROW(vector1)) {
    big_vector[i] <- vector1[i] + vector2[i]
  }),
  # Sum vectors using vectorized "+"
  vectorvized = (vector1 + vector2),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Allocate memory for cumulative sum
cumsumv <- numeric(NROW(big_vector))
cumsumv[1] <- big_vector[1]
# Calculate cumulative sum in two different ways
summary(microbenchmark(
# Cumulative sum using "for" loop
  rloop = (for (i in 2:NROW(big_vector)) {
    cumsumv[i] <- cumsumv[i-1] + big_vector[i]
  }),
# Cumulative sum using "cumsum"
  vectorvized = cumsum(big_vector),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Calculate matrix of random data with 5,000 rows
matrixv <- matrix(rnorm(10000), ncol=2)
# Calculate row sums two different ways
all.equal(rowSums(matrixv),
  apply(matrixv, 1, sum))
summary(microbenchmark(
  rowsumv = rowSums(matrixv),
  applyloop = apply(matrixv, 1, sum),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

library(microbenchmark)
str(pmax)
# Calculate row maximums two different ways
summary(microbenchmark(
  pmax=do.call(pmax.int,
lapply(seq_along(matrixv[1, ]),
  function(indeks) matrixv[, indeks])),
applyloop=unlist(lapply(seq_along(matrixv[, 1]),
  function(indeks) max(matrixv[indeks, ]))),
  times=10))[, c(1, 4, 5)]

install.packages("matrixStats")  # Install package matrixStats
library(matrixStats)  # Load package matrixStats
# Calculate row min values three different ways
summary(microbenchmark(
  rowmins = rowMins(matrixv),
  pmin =
    do.call(pmin.int,
      lapply(seq_along(matrixv[1, ]),
             function(indeks)
               matrixv[, indeks])),
  as_dframe =
    do.call(pmin.int,
      as.data.frame.matrix(matrixv)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

install.packages("Rfast")  # Install package Rfast
library(Rfast)  # Load package Rfast
# Benchmark speed of calculating ranks
vectorv <- 1e3
all.equal(rank(vectorv), Rfast::Rank(vectorv))
library(microbenchmark)
summary(microbenchmark(
  Rcode = rank(vectorv),
  Rfast = Rfast::Rank(vectorv),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Benchmark speed of calculating column medians
matrixv <- matrix(1e4, nc=10)
all.equal(matrixStats::colMedians(matrixv), Rfast::colMedians(matrixv))
summary(microbenchmark(
  matrixStats = matrixStats::colMedians(matrixv),
  Rfast = Rfast::colMedians(matrixv),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

summary(microbenchmark(  # Assign values to vector three different ways
# Fast vectorized assignment loop performed in C using brackets "[]"
  brackets = {vectorv <- numeric(10)
    vectorv[] <- 2},
# Slow because loop is performed in R
  forloop = {vectorv <- numeric(10)
    for (indeks in seq_along(vectorv))
      vectorv[indeks] <- 2},
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
summary(microbenchmark(  # Assign values to vector two different ways
# Fast vectorized assignment loop performed in C using brackets "[]"
  brackets = {vectorv <- numeric(10)
    vectorv[4:7] <- rnorm(4)},
# Slow because loop is performed in R
  forloop = {vectorv <- numeric(10)
    for (indeks in 4:7)
      vectorv[indeks] <- rnorm(1)},
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
set.seed(1121)
sapply(stdevs, function(stdev) rnorm(n=2, sd=stdev))
# Same
set.seed(1121)
sapply(stdevs, rnorm, n=2, mean=0)
# Loop over means
set.seed(1121)
sapply(means, function(me_an) rnorm(n=2, mean=me_an))
# Same
set.seed(1121)
sapply(means, rnorm, n=2)

# rnorm() vectorized with respect to "stdev"
vec_rnorm <- function(n, mean=0, sd=1) {
  if (NROW(sd)==1)
    rnorm(n=n, mean=mean, sd=sd)
  else
    sapply(sd, rnorm, n=n, mean=mean)
}  # end vec_rnorm
set.seed(1121)
vec_rnorm(n=2, sd=stdevs)
# rnorm() vectorized with respect to "mean" and "sd"
vec_rnorm <- Vectorize(FUN=rnorm,
        vectorize.args=c("mean", "sd")
)  # end Vectorize
set.seed(1121)
vec_rnorm(n=2, sd=stdevs)
set.seed(1121)
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
vector1 <- sin(0.25*pi*1:20)
vector2 <- cos(0.25*pi*1:20)
# Create third vector using 'ifelse'
vector3 <- ifelse(vector1 > vector2, vector1, vector2)
# cbind all three together
vector3 <- cbind(vector1, vector2, vector3)
colnames(vector3)[3] <- "Max"
# Set plotting parameters
x11(width=6, height=7)
par(oma=c(0, 1, 1, 1), mar=c(0, 2, 2, 1),
    mgp=c(2, 1, 0), cex.lab=0.5, cex.axis=1.0, cex.main=1.8, cex.sub=0.5)
# Plot matrix
zoo::plot.zoo(vector3, lwd=2, ylim=c(-1, 1),
  xlab="", col=c("green", "blue", "red"),
  main="ifelse() Calculates The Max of Two Data Sets")

# Calculate cumulative sum of a vector
vectorv <- runif(1e5)
# Use compiled function
cumsumv <- cumsum(vectorv)
# Use for loop
cumsumv2 <- vectorv
for (i in 2:NROW(cumsumv2))
  cumsumv2[i] <- (cumsumv2[i] + cumsumv2[i-1])
# Compare the two methods
all.equal(cumsumv, cumsumv2)
# Microbenchmark the two methods
library(microbenchmark)
summary(microbenchmark(
  cumsum=cumsum(vectorv),
  loop_alloc={
    cumsumv2 <- vectorv
    for (i in 2:NROW(cumsumv2))
cumsumv2[i] <- (cumsumv2[i] + cumsumv2[i-1])
  },
  loop_nalloc={
    # Doesn't allocate memory to cumsumv3
    cumsumv3 <- vectorv[1]
    for (i in 2:NROW(vectorv))
# This command adds an extra element to cumsumv3
cumsumv3[i] <- (vectorv[i] + cumsumv3[i-1])
  },
  times=10))[, c(1, 4, 5)]

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
cluster <- makeCluster(ncores)
# Perform parallel loop under Windows
outv <- parLapply(cluster, 1:10, paws)
# Perform parallel loop under Mac-OSX or Linux
outv <- mclapply(1:10, paws, mc.cores=ncores)
library(microbenchmark)  # Load package microbenchmark
# Compare speed of lapply versus parallel computing
summary(microbenchmark(
  standard = lapply(1:10, paws),
  parallel = mclapply(1:10, paws, mc.cores=ncores),
  times=10)
)[, c(1, 4, 5)]

# Compare speed of lapply with parallel computing
iterations <- 3:10
compute_times <- sapply(iterations,
  function(max_iterations) {
    summary(microbenchmark(
standard = lapply(1:max_iterations, paws),
parallel = parLapply(cluster, 1:max_iterations, paws),
times=10))[, 4]
    })  # end sapply
compute_times <- t(compute_times)
colnames(compute_times) <- c("standard", "parallel")
rownames(compute_times) <- iterations
# Stop R processes over cluster under Windows
stopCluster(cluster)

x11(width=6, height=5)
plot(x=rownames(compute_times),
     y=compute_times[, "standard"],
     type="l", lwd=2, col="blue",
     main="Compute times",
     xlab="number of iterations in loop", ylab="",
     ylim=c(0, max(compute_times[, "standard"])))
lines(x=rownames(compute_times),
y=compute_times[, "parallel"], lwd=2, col="green")
legend(x="topleft", legend=colnames(compute_times),
 inset=0.1, cex=1.0, bg="white",
 lwd=2, lty=1, col=c("blue", "green"))

library(parallel)  # Load package parallel
# Calculate number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
cluster <- makeCluster(ncores)
# Calculate matrix of random data
matrixv <- matrix(rnorm(1e5), ncol=100)
# Define aggregation function over column of matrix
aggfun <- function(column) {
  output <- 0
  for (indeks in 1:NROW(column))
    output <- output + column[indeks]
  output
}  # end aggfun
# Perform parallel aggregations over columns of matrix
aggs <- parCapply(cluster, matrixv, aggfun)
# Compare speed of apply with parallel computing
summary(microbenchmark(
  applyloop=apply(matrixv, MARGIN=2, aggfun),
  parapplyloop=parCapply(cluster, matrixv, aggfun),
  times=10)
)[, c(1, 4, 5)]
# Stop R processes over cluster under Windows
stopCluster(cluster)

library(parallel)  # Load package parallel
# Calculate number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
cluster <- makeCluster(ncores)
basep <- 2
# Fails because child processes don't know basep:
parLapply(cluster, 2:4,
    function(exponent) basep^exponent)
# basep passed to child via dots ... argument:
parLapply(cluster, 2:4,
    function(exponent, basep) basep^exponent,
    basep=basep)
# basep passed to child via clusterExport:
clusterExport(cluster, "basep")
parLapply(cluster, 2:4,
    function(exponent) basep^exponent)
# Fails because child processes don't know zoo::index():
parSapply(cluster, c("VTI", "IEF", "DBC"),
    function(symbol)
      NROW(zoo::index(get(symbol, envir=rutils::etfenv))))
# zoo function referenced using "::" in child process:
parSapply(cluster, c("VTI", "IEF", "DBC"),
    function(symbol)
      NROW(zoo::index(get(symbol, envir=rutils::etfenv))))
# Package zoo loaded in child process:
parSapply(cluster, c("VTI", "IEF", "DBC"),
    function(symbol) {
      stopifnot("package:zoo" %in% search() || require("zoo", quietly=TRUE))
      NROW(zoo::index(get(symbol, envir=rutils::etfenv)))
    })  # end parSapply
# Stop R processes over cluster under Windows
stopCluster(cluster)

library(parallel)  # Load package parallel
# Calculate number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
cluster <- makeCluster(ncores)
# Set seed for cluster under Windows
# Doesn't work: set.seed(1121)
clusterSetRNGStream(cluster, 1121)
# Perform parallel loop under Windows
output <- parLapply(cluster, 1:70, rnorm, n=100)
sum(unlist(output))
# Stop R processes over cluster under Windows
stopCluster(cluster)
# Perform parallel loop under Mac-OSX or Linux
output <- mclapply(1:10, rnorm, mc.cores=ncores, n=100)

set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
nrows <- 1000
datav <- rnorm(nrows)
# Sample mean - MC estimate
mean(datav)
# Sample standard deviation - MC estimate
sd(datav)
# Monte Carlo estimate of cumulative probability
pnorm(-2)
sum(datav < (-2))/nrows
# Monte Carlo estimate of quantile
confl <- 0.02
qnorm(confl)  # Exact value
cutoff <- confl*nrows
datav <- sort(datav)
datav[cutoff]  # Naive Monte Carlo value
quantile(datav, probs=confl)
# Analyze the source code of quantile()
stats:::quantile.default
# Microbenchmark quantile
library(microbenchmark)
summary(microbenchmark(
  monte_carlo = datav[cutoff],
  quantilev = quantile(datav, probs=confl),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# Sample from Standard Normal Distribution
nrows <- 1000; datav <- rnorm(nrows)
# Sample mean and standard deviation
mean(datav); sd(datav)
# Bootstrap of sample mean and median
nboot <- 10000
bootd <- sapply(1:nboot, function(x) {
  # Sample from Standard Normal Distribution
  samplev <- rnorm(nrows)
  c(mean=mean(samplev), median=median(samplev))
})  # end sapply
bootd[, 1:3]
bootd <- t(bootd)
# Standard error from formula
sd(datav)/sqrt(nrows)
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
 leg=c("mean", "median"), bty="n",
 lwd=6, bg="white", col=c("green", "blue"))

set.seed(1121)  # Reset random number generator
nrows <- 1000
# Bootstrap of sample mean and median
nboot <- 100
bootd <- sapply(1:nboot, function(x) median(rnorm(nrows)))
# Perform vectorized bootstrap
set.seed(1121)  # Reset random number generator
# Calculate matrix of random data
samplev <- matrix(rnorm(nboot*nrows), ncol=nboot)
boot_vec <- matrixStats::colMedians(samplev)
all.equal(bootd, boot_vec)
# Compare speed of loops with vectorized R code
library(microbenchmark)
summary(microbenchmark(
  loop = sapply(1:nboot, function(x) median(rnorm(nrows))),
  cpp = {
    samplev <- matrix(rnorm(nboot*nrows), ncol=nboot)
    matrixStats::colMedians(samplev)
    },
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
cluster <- makeCluster(ncores)  # Initialize compute cluster under Windows
set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
nrows <- 1000
# Bootstrap mean and median under Windows
nboot <- 10000
bootd <- parLapply(cluster, 1:nboot,
  function(x, datav, nrows) {
  samplev <- rnorm(nrows)
  c(mean=mean(samplev), median=median(samplev))
  }, datav=datav, nrows*nrows)  # end parLapply
# Bootstrap mean and median under Mac-OSX or Linux
bootd <- mclapply(1:nboot,
  function(x) {
  samplev <- rnorm(nrows)
  c(mean=mean(samplev), median=median(samplev))
  }, mc.cores=ncores)  # end mclapply
bootd <- rutils::do_call(rbind, bootd)
# Means and standard errors from bootstrap
apply(bootd, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
# Standard error from formula
sd(datav)/sqrt(nrows)
stopCluster(cluster)  # Stop R processes over cluster under Windows

nrows <- 1000
datav <- rnorm(nrows)
sd(datav); mad(datav)
median(abs(datav - median(datav)))
median(abs(datav - median(datav)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
nboot <- 10000
bootd <- sapply(1:nboot, function(x) {
  samplev <- rnorm(nrows)
  c(sd=sd(samplev), mad=mad(samplev))
})  # end sapply
bootd <- t(bootd)
# Analyze bootstrapped variance
head(bootd)
sum(is.na(bootd))
# Means and standard errors from bootstrap
apply(bootd, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
cluster <- makeCluster(ncores)  # Initialize compute cluster
bootd <- parLapply(cluster, 1:nboot,
  function(x, datav) {
    samplev <- rnorm(nrows)
    c(sd=sd(samplev), mad=mad(samplev))
  }, datav=datav)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:nboot, function(x) {
  samplev <- rnorm(nrows)
  c(sd=sd(samplev), mad=mad(samplev))
}, mc.cores=ncores)  # end mclapply
stopCluster(cluster)  # Stop R processes over cluster
bootd <- rutils::do_call(rbind, bootd)
# Means and standard errors from bootstrap
apply(bootd, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))

# Calculate time series of VTI returns
library(rutils)
returns <- rutils::etfenv$returns$VTI
returns <- na.omit(returns)
nrows <- NROW(returns)
# Sample from VTI returns
samplev <- returns[sample.int(nrows, replace=TRUE)]
c(sd=sd(samplev), mad=mad(samplev))
# sample.int() is a little faster than sample()
library(microbenchmark)
summary(microbenchmark(
  sample.int = sample.int(1e3),
  sample = sample(1e3),
  times=10))[, c(1, 4, 5)]

# Sample from time series of VTI returns
library(rutils)
returns <- rutils::etfenv$returns$VTI
returns <- na.omit(returns)
nrows <- NROW(returns)
# Bootstrap sd and MAD under Windows
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
cluster <- makeCluster(ncores)  # Initialize compute cluster under Windows
clusterSetRNGStream(cluster, 1121)  # Reset random number generator in all cores
nboot <- 10000
bootd <- parLapply(cluster, 1:nboot,
  function(x, returns, nrows) {
    samplev <- returns[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, returns=returns, nrows*nrows)  # end parLapply
# Bootstrap sd and MAD under Mac-OSX or Linux
bootd <- mclapply(1:nboot, function(x) {
    samplev <- returns[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, mc.cores=ncores)  # end mclapply
stopCluster(cluster)  # Stop R processes over cluster under Windows
bootd <- rutils::do_call(rbind, bootd)
# Standard error assuming normal distribution of returns
sd(returns)/sqrt(nboot)
# Means and standard errors from bootstrap
stderrors <- apply(bootd, MARGIN=2,
  function(x) c(mean=mean(x), stderror=sd(x)))
stderrors
# Relative standard errors
stderrors[2, ]/stderrors[1, ]

# Initialize random number generator
set.seed(1121)
# Define explanatory and response variables
predictor <- rnorm(100, mean=2)
noise <- rnorm(100)
response <- (-3 + predictor + noise)
design <- cbind(response, predictor)
# Calculate alpha and beta regression coefficients
betav <- cov(design[, 1], design[, 2])/var(design[, 2])
alpha <- mean(design[, 1]) - betav*mean(design[, 2])
x11(width=6, height=5)
plot(response ~ predictor, data=design)
abline(a=alpha, b=betav, lwd=3, col="blue")
# Bootstrap of beta regression coefficient
nboot <- 100
bootd <- sapply(1:nboot, function(x) {
  samplev <- sample.int(NROW(design), replace=TRUE)
  design <- design[samplev, ]
  cov(design[, 1], design[, 2])/var(design[, 2])
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
cluster <- makeCluster(ncores)  # Initialize compute cluster under Windows
# Bootstrap of regression under Windows
bootd <- parLapply(cluster, 1:1000,
  function(x, design) {
    samplev <- sample.int(NROW(design), replace=TRUE)
    design <- design[samplev, ]
    cov(design[, 1], design[, 2])/var(design[, 2])
  }, design=design)  # end parLapply
# Bootstrap of regression under Mac-OSX or Linux
bootd <- mclapply(1:1000,
  function(x) {
    samplev <- sample.int(NROW(design), replace=TRUE)
    design <- design[samplev, ]
    cov(design[, 1], design[, 2])/var(design[, 2])
  }, mc.cores=ncores)  # end mclapply
stopCluster(cluster)  # Stop R processes over cluster under Windows

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

set.seed(1121)  # Reset random number generator
barl <- 20  # Barrier level
nrows <- 1000  # Number of simulation steps
paths <- numeric(nrows)  # Allocate path vector
paths[1] <- 0  # Initialize path
indeks <- 2  # Initialize simulation index
while ((indeks <= nrows) && (paths[indeks - 1] < barl)) {
# Simulate next step
  paths[indeks] <- paths[indeks - 1] + rnorm(1)
  indeks <- indeks + 1  # Advance indeks
}  # end while
# Fill remaining paths after it crosses barl
if (indeks <= nrows)
  paths[indeks:nrows] <- paths[indeks - 1]
# Plot the Brownian motion
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
plot(paths, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=barl, lwd=3, col="red")
title(main="Brownian Motion Crossing a Barrier Level", line=0.5)

set.seed(1121)  # Reset random number generator
barl <- 20  # Barrier level
nrows <- 1000  # Number of simulation steps
# Simulate path of Brownian motion
pathv <- cumsum(rnorm(nrows))
# Find index when pathv crosses barl
crossp <- which(pathv > barl)
# Fill remaining pathv after it crosses barl
if (NROW(crossp)>0) {
  pathv[(crossp[1]+1):nrows] <- pathv[crossp[1]]
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
nrows <- 1000  # Number of simulation steps
nsimu <- 100  # Number of simulations
# Simulate multiple paths of Brownian motion
set.seed(1121)
paths <- rnorm(nsimu*nrows, mean=drift, sd=sigmav)
paths <- matrix(paths, nc=nsimu)
paths <- matrixStats::colCumsums(paths)
# Final distribution of paths
mean(paths[nrows, ]) ; sd(paths[nrows, ])
# Calculate option payout at maturity
strikep <- 50  # Strike price
payouts <- (paths[nrows, ] - strikep)
sum(payouts[payouts > 0])/nsimu
# Calculate probability of crossing the barrier at any point
barl <- 50
crossi <- (colSums(paths > barl) > 0)
sum(crossi)/nsimu

# Plot in window
x11(width=6, height=5)
par(mar=c(4, 3, 2, 2), oma=c(0, 0, 0, 0), mgp=c(2.5, 1, 0))
# Select and plot full range of paths
ordern <- order(paths[nrows, ])
indeks <- ordern[seq(1, 100, 9)]
zoo::plot.zoo(paths[, indeks], main="Paths of Brownian Motion",
  xlab="time steps", ylab=NA, plot.type="single")
abline(h=strikep, col="red", lwd=3)
text(x=(nrows-60), y=strikep, labels="strike price", pos=3, cex=1)

# Calculate percentage returns from VTI prices
library(rutils)
prices <- quantmod::Cl(rutils::etfenv$VTI)
startd <- as.numeric(prices[1, ])
returns <- rutils::diffit(log(prices))
class(returns); head(returns)
sum(is.na(returns))
nrows <- NROW(returns)
# Define barrier level with respect to prices
barl <- 1.5*max(prices)
# Calculate single bootstrap sample
samplev <- returns[sample.int(nrows, replace=TRUE)]
# Calculate prices from percentage returns
samplev <- startd*exp(cumsum(samplev))
# Calculate if prices crossed barrier
sum(samplev > barl) > 0

library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
cluster <- makeCluster(ncores)  # Initialize compute cluster under Windows
# Perform parallel bootstrap under Windows
clusterSetRNGStream(cluster, 1121)  # Reset random number generator in all cores
clusterExport(cluster, c("startd", "barl"))
nboot <- 10000
bootd <- parLapply(cluster, 1:nboot,
  function(x, returns, nrows) {
    samplev <- returns[sample.int(nrows, replace=TRUE)]
    # Calculate prices from percentage returns
    samplev <- startd*exp(cumsum(samplev))
    # Calculate if prices crossed barrier
    sum(samplev > barl) > 0
  }, returns=returns, nrows*nrows)  # end parLapply
# Perform parallel bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:nboot, function(x) {
    samplev <- returns[sample.int(nrows, replace=TRUE)]
    # Calculate prices from percentage returns
    samplev <- startd*exp(cumsum(samplev))
    # Calculate if prices crossed barrier
    sum(samplev > barl) > 0
  }, mc.cores=ncores)  # end mclapply
stopCluster(cluster)  # Stop R processes over cluster under Windows
bootd <- rutils::do_call(rbind, bootd)
# Calculate frequency of crossing barrier
sum(bootd)/nboot

# Calculate percentage returns from VTI prices
library(rutils)
ohlc <- rutils::etfenv$VTI
prices <- as.numeric(ohlc[, 4])
startd <- prices[1]
returns <- rutils::diffit(log(prices))
nrows <- NROW(returns)
# Calculate difference of OHLC price columns
ohlc_diff <- ohlc[, 1:3] - prices
class(returns); head(returns)
# Calculate bootstrap prices from percentage returns
datav <- sample.int(nrows, replace=TRUE)
boot_prices <- startd*exp(cumsum(returns[datav]))
boot_ohlc <- ohlc_diff + boot_prices
boot_ohlc <- cbind(boot_ohlc, boot_prices)
# Define barrier level with respect to prices
barl <- 1.5*max(prices)
# Calculate if High bootstrapped prices crossed barrier level
sum(boot_ohlc[, 2] > barl) > 0

library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
cluster <- makeCluster(ncores)  # Initialize compute cluster under Windows
# Perform parallel bootstrap under Windows
clusterSetRNGStream(cluster, 1121)  # Reset random number generator in all cores
clusterExport(cluster, c("startd", "barl", "ohlc_diff"))
nboot <- 10000
bootd <- parLapply(cluster, 1:nboot,
  function(x, returns, nrows) {
    # Calculate OHLC prices from percentage returns
    datav <- sample.int(nrows, replace=TRUE)
    boot_prices <- startd*exp(cumsum(returns[datav]))
    boot_ohlc <- ohlc_diff + boot_prices
    boot_ohlc <- cbind(boot_ohlc, boot_prices)
    # Calculate statistic
    sum(boot_ohlc[, 2] > barl) > 0
  }, returns=returns, nrows*nrows)  # end parLapply
# Perform parallel bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:nboot, function(x) {
    # Calculate OHLC prices from percentage returns
    datav <- sample.int(nrows, replace=TRUE)
    boot_prices <- startd*exp(cumsum(returns[datav]))
    boot_ohlc <- ohlc_diff + boot_prices
    boot_ohlc <- cbind(boot_ohlc, boot_prices)
    # Calculate statistic
    sum(boot_ohlc[, 2] > barl) > 0
  }, mc.cores=ncores)  # end mclapply
stopCluster(cluster)  # Stop R processes over cluster under Windows
bootd <- rutils::do_call(rbind, bootd)
# Calculate frequency of crossing barrier
sum(bootd)/nboot

options(width=50, dev="pdf")
str(optimize)
# Objective function with multiple minima
objfun <- function(input, param1=0.01) {
  sin(0.25*pi*input) + param1*(input-1)^2
}  # end objfun
unlist(optimize(f=objfun, interval=c(-4, 2)))
unlist(optimize(f=objfun, interval=c(0, 8)))
options(width=80, dev="pdf")

par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot the objective function
curve(expr=objfun, type="l", xlim=c(-8, 9),
xlab="", ylab="", lwd=2)
# Add title
title(main="Objective Function", line=-1)

# Rastrigin function
rastrigin <- function(x, y, param=25) {
  x^2 + y^2 - param*(cos(x) + cos(y))
}  # end rastrigin
# Rastrigin function is vectorized!
rastrigin(c(-10, 5), c(-10, 5))
# Set rgl options and load package rgl
library(rgl)
options(rgl.useNULL=TRUE)
# Draw 3d surface plot of function
rgl::persp3d(x=rastrigin, xlim=c(-10, 10), ylim=c(-10, 10),
  col="green", axes=FALSE, param=15)
# Render the 3d surface plot of function
rgl::rglwidget(elementId="plot3drgl", width=400, height=400)

# Rastrigin function with vector argument for optimization
rastrigin <- function(vectorv, param=25) {
  sum(vectorv^2 - param*cos(vectorv))
}  # end rastrigin
vectorv <- c(pi/6, pi/6)
rastrigin(vectorv=vectorv)
# Draw 3d surface plot of Rastrigin function
rgl::persp3d(
  x=Vectorize(function(x, y) rastrigin(vectorv=c(x, y))),
  xlim=c(-10, 10), ylim=c(-10, 10),
  col="green", axes=FALSE, zlab="", main="rastrigin")
# Optimize with respect to vector argument
optiml <- optim(par=vectorv, fn=rastrigin,
        method="L-BFGS-B",
        upper=c(4*pi, 4*pi),
        lower=c(pi/2, pi/2),
        param=1)
# Optimal parameters and value
optiml$par
optiml$value
rastrigin(optiml$par, param=1)

# Sample of normal variables
datav <- rnorm(1000, mean=4, sd=2)
# Objective function is log-likelihood
objfun <- function(parv, datav) {
  sum(2*log(parv[2]) +
    ((datav - parv[1])/parv[2])^2)
}  # end objfun
# Objective function on parameter grid
par_mean <- seq(1, 6, length=50)
par_sd <- seq(0.5, 3.0, length=50)
objective_grid <- sapply(par_mean, function(m) {
  sapply(par_sd, function(sd) {
    objfun(c(m, sd), datav)
  })  # end sapply
})  # end sapply
# Perform grid search for minimum
objective_min <- which(
  objective_grid==min(objective_grid),
  arr.ind=TRUE)
objective_min
par_mean[objective_min[1]]  # mean
par_sd[objective_min[2]]  # sd
objective_grid[objective_min]
objective_grid[(objective_min[, 1] + -1:1),
       (objective_min[, 2] + -1:1)]
# Or create parameter grid using function outer()
objvecive <- Vectorize(
  FUN=function(mean, sd, datav)
    objfun(c(mean, sd), datav),
  vectorize.args=c("mean", "sd")
)  # end Vectorize
objective_grid <- outer(par_mean, par_sd,
objvecive, datav=datav)

par(cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
# Perspective plot of log-likelihood function
persp(z=-objective_grid,
theta=45, phi=30, shade=0.5,
border="green", zlab="objective",
main="objective function")
# Interactive perspective plot of log-likelihood function
library(rgl)  # Load package rgl
rgl::par3d(cex=2.0)  # Scale text by factor of 2
rgl::persp3d(z=-objective_grid, zlab="objective",
  col="green", main="objective function")

# Initial parameters
initp <- c(mean=0, sd=1)
# Perform optimization using optim()
optim_fit <- optim(par=initp,
  fn=objfun, # Log-likelihood function
  datav=datav,
  method="L-BFGS-B", # Quasi-Newton method
  upper=c(10, 10), # Upper constraint
  lower=c(-10, 0.1)) # Lower constraint
# Optimal parameters
optim_fit$par
# Perform optimization using MASS::fitdistr()
optim_fit <- MASS::fitdistr(datav, densfun="normal")
optim_fit$estimate
optim_fit$sd
# Plot histogram
histp <- hist(datav, plot=FALSE)
plot(histp, freq=FALSE, main="histogram of sample")
curve(expr=dnorm(x, mean=optim_fit$par["mean"], sd=optim_fit$par["sd"]),
add=TRUE, type="l", lwd=2, col="red")
legend("topright", inset=0.0, cex=0.8, title=NULL,
 leg="optimal parameters", lwd=2, bg="white", col="red")

# Sample from mixture of normal distributions
datav <- c(rnorm(100, sd=1.0),
      rnorm(100, mean=4, sd=1.0))
# Objective function is log-likelihood
objfun <- function(parv, datav) {
  likev <- parv[1]/parv[3] *
  dnorm((datav-parv[2])/parv[3]) +
  (1-parv[1])/parv[5]*dnorm((datav-parv[4])/parv[5])
  if (any(likev <= 0)) Inf else
    -sum(log(likev))
}  # end objfun
# Vectorize objective function
objvecive <- Vectorize(
  FUN=function(mean, sd, w, m1, s1, datav)
    objfun(c(w, m1, s1, mean, sd), datav),
  vectorize.args=c("mean", "sd")
)  # end Vectorize
# Objective function on parameter grid
par_mean <- seq(3, 5, length=50)
par_sd <- seq(0.5, 1.5, length=50)
objective_grid <- outer(par_mean, par_sd,
    objvecive, datav=datav,
    w=0.5, m1=2.0, s1=2.0)
rownames(objective_grid) <- round(par_mean, 2)
colnames(objective_grid) <- round(par_sd, 2)
objective_min <- which(objective_grid==
  min(objective_grid), arr.ind=TRUE)
objective_min
objective_grid[objective_min]
objective_grid[(objective_min[, 1] + -1:1),
         (objective_min[, 2] + -1:1)]

# Perspective plot of objective function
persp(par_mean, par_sd, -objective_grid,
theta=45, phi=30,
shade=0.5,
col=rainbow(50),
border="green",
main="objective function")

# Initial parameters
initp <- c(weight=0.5, m1=0, s1=1, m2=2, s2=1)
# Perform optimization
optim_fit <- optim(par=initp,
      fn=objfun,
      datav=datav,
      method="L-BFGS-B",
      upper=c(1,10,10,10,10),
      lower=c(0,-10,0.2,-10,0.2))
optim_fit$par
# Plot histogram
histp <- hist(datav, plot=FALSE)
plot(histp, freq=FALSE,
     main="histogram of sample")
fitfun <- function(x, parv) {
  parv["weight"]*dnorm(x, mean=parv["m1"], sd=parv["s1"]) +
  (1-parv["weight"])*dnorm(x, mean=parv["m2"], sd=parv["s2"])
}  # end fitfun
curve(expr=fitfun(x, parv=optim_fit$par), add=TRUE,
type="l", lwd=2, col="red")
legend("topright", inset=0.0, cex=0.8, title=NULL,
 leg="optimal parameters",
 lwd=2, bg="white", col="red")

# Rastrigin function with vector argument for optimization
rastrigin <- function(vectorv, param=25) {
  sum(vectorv^2 - param*cos(vectorv))
}  # end rastrigin
vectorv <- c(pi/6, pi/6)
rastrigin(vectorv=vectorv)
library(DEoptim)
# Optimize rastrigin using DEoptim
optiml <-  DEoptim(rastrigin,
  upper=c(6, 6), lower=c(-6, -6),
  DEoptim.control(trace=FALSE, itermax=50))
# Optimal parameters and value
optiml$optim$bestmem
rastrigin(optiml$optim$bestmem)
summary(optiml)
plot(optiml)
