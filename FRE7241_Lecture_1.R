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
setwd("C:/Develop/R")  # Set cwd
getwd()  # Get cwd
Sys.time()  # Get date and time
Sys.Date()  # Get date only
rm(list=ls())
setwd("C:/Develop/lecture_slides/data")
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
     file="C:/Develop/lecture_slides/data/my_data.RData")
rm(list=ls())  # Remove all objects
ls()  # List objects
load_ed <- load(file="C:/Develop/lecture_slides/data/my_data.RData")
load_ed
ls()  # List objects
  q()  # quit R session
history(5)  # Display last 5 commands
savehistory(file="myfile")  # Default is ".Rhistory"
loadhistory(file="myfile")  # Default is ".Rhistory"
sessionInfo()  # Get R version and other session info
Sys.getenv()[5:7]  # List some environment variables
Sys.getenv("HOME")  # Get R user HOME directory
Sys.setenv(Home="C:/Develop/data")  # Set HOME directory
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
# Control exponential scientific notation of print method
# Positive "scipen" values bias towards fixed notation
# Negative "scipen" values bias towards scientific notation
options(scipen=100)
# Maximum number of items printed to console
options(max.print=30)
# Warning levels options
# Negative - warnings are ignored
options(warn=-1)
# zero - warnings are stored and printed after top-level function has completed
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
glob_var <- 1
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
symbols <- c("VTI", "VEU", "IEF", "VNQ")
# Extract symbols from rutils::etfenv
prices <- mget(symbols, envir=rutils::etfenv)
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
  mget(symbols, envir=rutils::etfenv), quantmod::Cl))
# Or
# Extract and bind all data, subset by symbols
prices <- lapply(symbols, function(symbol) {
    quantmod::Cl(get(symbol, envir=rutils::etfenv))
})  # end lapply
# Same, but loop over etfenv without anonymous function
prices <- do.call(cbind,
  lapply(as.list(rutils::etfenv)[symbols], quantmod::Cl))
# Same, but works only for OHLC series - produces error
prices <- do.call(cbind,
  eapply(rutils::etfenv, quantmod::Cl)[symbols])
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
  file="C:/Develop/lecture_slides/data/etf_series.csv", sep=",")
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
row_sums <- numeric(NROW(matrixv))
summary(microbenchmark(
  row_sums = rowSums(matrixv),  # end row_sums
  ap_ply = apply(matrixv, 1, sum),  # end apply
  l_apply = lapply(1:NROW(matrixv), function(indeks)
    sum(matrixv[indeks, ])),  # end lapply
  v_apply = vapply(1:NROW(matrixv), function(indeks)
    sum(matrixv[indeks, ]),
    FUN.VALUE = c(sum=0)),  # end vapply
  s_apply = sapply(1:NROW(matrixv), function(indeks)
    sum(matrixv[indeks, ])),  # end sapply
  for_loop = for (i in 1:NROW(matrixv)) {
    row_sums[i] <- sum(matrixv[i,])
  },  # end for
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
vectorv <- rnorm(5000)
summary(microbenchmark(
# Allocate full memory for cumulative sum
  for_loop = {cumsumv <- numeric(NROW(vectorv))
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
  r_loop = (for (i in 1:NROW(vector1)) {
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
  r_loop = (for (i in 2:NROW(big_vector)) {
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
  row_sums = rowSums(matrixv),
  ap_ply = apply(matrixv, 1, sum),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
library(microbenchmark)
str(pmax)
# Calculate row maximums two different ways
summary(microbenchmark(
  p_max=do.call(pmax.int,
lapply(seq_along(matrixv[1, ]),
  function(indeks) matrixv[, indeks])),
  l_apply=unlist(lapply(seq_along(matrixv[, 1]),
  function(indeks) max(matrixv[indeks, ]))),
  times=10))[, c(1, 4, 5)]
install.packages("matrixStats")  # Install package matrixStats
library(matrixStats)  # Load package matrixStats
# Calculate row min values three different ways
summary(microbenchmark(
  row_mins = rowMins(matrixv),
  p_min =
    do.call(pmin.int,
      lapply(seq_along(matrixv[1, ]),
             function(indeks)
               matrixv[, indeks])),
  as_data_frame =
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
  brack_ets = {vectorv <- numeric(10)
    vectorv[] <- 2},
# Slow because loop is performed in R
  for_loop = {vectorv <- numeric(10)
    for (indeks in seq_along(vectorv))
      vectorv[indeks] <- 2},
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
summary(microbenchmark(  # Assign values to vector two different ways
# Fast vectorized assignment loop performed in C using brackets "[]"
  brack_ets = {vectorv <- numeric(10)
    vectorv[4:7] <- rnorm(4)},
# Slow because loop is performed in R
  for_loop = {vectorv <- numeric(10)
    for (indeks in 4:7)
      vectorv[indeks] <- rnorm(1)},
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Define function vectorized automatically
my_fun <- function(input, pa_ram) {
  pa_ram*input
}  # end my_fun
# "input" is vectorized
my_fun(input=1:3, pa_ram=2)
# "pa_ram" is vectorized
my_fun(input=10, pa_ram=2:4)
# Define vectors of parameters of rnorm()
stdevs <- structure(1:3, names=paste0("sd=", 1:3))
me_ans <- structure(-1:1, names=paste0("mean=", -1:1))
# "sd" argument of rnorm() isn't vectorized
rnorm(1, sd=stdevs)
# "mean" argument of rnorm() isn't vectorized
rnorm(1, mean=me_ans)
# Loop over stdevs produces vector output
set.seed(1121)
sapply(stdevs, function(stdev) rnorm(n=2, sd=stdev))
# Same
set.seed(1121)
sapply(stdevs, rnorm, n=2, mean=0)
# Loop over me_ans
set.seed(1121)
sapply(me_ans, function(me_an) rnorm(n=2, mean=me_an))
# Same
set.seed(1121)
sapply(me_ans, rnorm, n=2)
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
vec_rnorm(n=2, mean=me_ans)
str(sum)
# na.rm is bound by name
mapply(sum, 6:9, c(5, NA, 3), 2:6, na.rm=TRUE)
str(rnorm)
# mapply vectorizes both arguments "mean" and "sd"
mapply(rnorm, n=5, mean=me_ans, sd=stdevs)
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
vec_rnorm(n=2, mean=me_ans)
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
# list all objects in "parallel"
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
paw_s <- parLapply(cluster, 1:10, paws)
# Perform parallel loop under Mac-OSX or Linux
paw_s <- mclapply(1:10, paws, mc.cores=ncores)
library(microbenchmark)  # Load package microbenchmark
# Compare speed of lapply versus parallel computing
summary(microbenchmark(
  standard = lapply(1:10, paws),
  parallel = parLapply(cluster, 1:10, paws),
  times=10)
)[, c(1, 4, 5)]
# Compare speed of lapply with parallel computing
iter_ations <- 3:10
compute_times <- sapply(iter_ations,
  function(max_iterations) {
    summary(microbenchmark(
standard = lapply(1:max_iterations, paws),
parallel = parLapply(cluster, 1:max_iterations, paws),
times=10))[, 4]
    })  # end sapply
compute_times <- t(compute_times)
colnames(compute_times) <- c("standard", "parallel")
rownames(compute_times) <- iter_ations
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
agg_regate <- function(colnum) {
  output <- 0
  for (indeks in 1:NROW(colnum))
    output <- output + colnum[indeks]
  output
}  # end agg_regate
# Perform parallel aggregations over columns of matrix
agg_regations <- parCapply(cluster, matrixv, agg_regate)
# Compare speed of apply with parallel computing
summary(microbenchmark(
  ap_ply=apply(matrixv, MARGIN=2, agg_regate),
  parl_apply=parCapply(cluster, matrixv, agg_regate),
  times=10)
)[, c(1, 4, 5)]
# Stop R processes over cluster under Windows
stopCluster(cluster)
library(parallel)  # Load package parallel
# Calculate number of available cores
ncores <- detectCores() - 1
# Initialize compute cluster under Windows
cluster <- makeCluster(ncores)
ba_se <- 2
# Fails because child processes don't know ba_se:
parLapply(cluster, 2:4,
    function(exponent) ba_se^exponent)
# ba_se passed to child via dots ... argument:
parLapply(cluster, 2:4,
    function(exponent, ba_se) ba_se^exponent,
    ba_se=ba_se)
# ba_se passed to child via clusterExport:
clusterExport(cluster, "ba_se")
parLapply(cluster, 2:4,
    function(exponent) ba_se^exponent)
# Fails because child processes don't know zoo::index():
parSapply(cluster, c("VTI", "IEF", "DBC"),
    function(symbol)
      NROW(index(get(symbol, envir=rutils::etfenv))))
# zoo function referenced using "::" in child process:
parSapply(cluster, c("VTI", "IEF", "DBC"),
    function(symbol)
      NROW(zoo::index(get(symbol, envir=rutils::etfenv))))
# Package zoo loaded in child process:
parSapply(cluster, c("VTI", "IEF", "DBC"),
    function(symbol) {
      stopifnot("package:zoo" %in% search() || require("zoo", quietly=TRUE))
      NROW(index(get(symbol, envir=rutils::etfenv)))
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
barp <- 20  # Barrier level
nrows <- 1000  # Number of simulation steps
pa_th <- numeric(nrows)  # Allocate path vector
pa_th[1] <- 0  # Initialize path
indeks <- 2  # Initialize simulation index
while ((indeks <= nrows) && (pa_th[indeks - 1] < barp)) {
# Simulate next step
  pa_th[indeks] <- pa_th[indeks - 1] + rnorm(1)
  indeks <- indeks + 1  # Advance indeks
}  # end while
# Fill remaining pa_th after it crosses barp
if (indeks <= nrows)
  pa_th[indeks:nrows] <- pa_th[indeks - 1]
# Plot the Brownian motion
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
plot(pa_th, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=barp, lwd=3, col="red")
title(main="Brownian Motion Crossing a Barrier Level", line=0.5)
set.seed(1121)  # Reset random number generator
barp <- 20  # Barrier level
nrows <- 1000  # Number of simulation steps
# Simulate path of Brownian motion
pa_th <- cumsum(rnorm(nrows))
# Find index when pa_th crosses barp
cro_ss <- which(pa_th > barp)
# Fill remaining pa_th after it crosses barp
if (NROW(cro_ss)>0) {
  pa_th[(cro_ss[1]+1):nrows] <- pa_th[cro_ss[1]]
}  # end if
# Plot the Brownian motion
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
plot(pa_th, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=barp, lwd=3, col="red")
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
# Calculate option payout
strikep <- 50  # Strike price
payouts <- (paths[nrows, ] - strikep)
sum(payouts[payouts > 0])/nsimu
# Calculate probability of crossing a barrier
barp <- 50
didcross <- colSums(paths > barp) > 0
sum(didcross)/nsimu
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
set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
nrows <- 1000
datav <- rnorm(nrows)
# Sample mean - MC estimate
mean(datav)
# Sample standard deviation - MC estimate
sd(datav)
# Monte Carlo estimate of cumulative probability
pnorm(1)
sum(datav < 1)/nrows
# Monte Carlo estimate of quantile
confl <- 0.98
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
boot_data <- sapply(1:nboot, function(x) {
  # Sample from Standard Normal Distribution
  samplev <- rnorm(nrows)
  c(mean=mean(samplev), median=median(samplev))
})  # end sapply
boot_data[, 1:3]
boot_data <- t(boot_data)
# Standard error from formula
sd(datav)/sqrt(nrows)
# Standard error of mean from bootstrap
sd(boot_data[, "mean"])
# Standard error of median from bootstrap
sd(boot_data[, "median"])
# Plot the densities of the bootstrap data
x11(width=6, height=5)
plot(density(boot_data[, "mean"]), lwd=3, xlab="Estimator Value",
     main="Distribution of Bootstrapped Mean and Median", col="green")
lines(density(boot_data[, "median"]), lwd=3, col="blue")
abline(v=mean(boot_data[, "mean"]), lwd=2, col="red")
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("mean", "median"), bty="n",
 lwd=6, bg="white", col=c("green", "blue"))
set.seed(1121)  # Reset random number generator
nrows <- 1000
# Bootstrap of sample mean and median
nboot <- 100
boot_data <- sapply(1:nboot, function(x) median(rnorm(nrows)))
# Perform vectorized bootstrap
set.seed(1121)  # Reset random number generator
# Calculate matrix of random data
samplev <- matrix(rnorm(nboot*nrows), ncol=nboot)
boot_vec <- Rfast::colMedians(samplev)
all.equal(boot_data, boot_vec)
# Compare speed of loops with vectorized R code
library(microbenchmark)
summary(microbenchmark(
  loop = sapply(1:nboot, function(x) median(rnorm(nrows))),
  cpp = {
    samplev <- matrix(rnorm(nboot*nrows), ncol=nboot)
    Rfast::colMedians(samplev)
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
boot_data <- parLapply(cluster, 1:nboot,
  function(x, datav, nrows) {
  samplev <- rnorm(nrows)
  c(mean=mean(samplev), median=median(samplev))
  }, datav=datav, nrows=nrows)  # end parLapply
# Bootstrap mean and median under Mac-OSX or Linux
boot_data <- mclapply(1:nboot,
  function(x) {
  samplev <- rnorm(nrows)
  c(mean=mean(samplev), median=median(samplev))
  }, mc.cores=ncores)  # end mclapply
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
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
boot_data <- sapply(1:nboot, function(x) {
  samplev <- rnorm(nrows)
  c(sd=sd(samplev), mad=mad(samplev))
})  # end sapply
boot_data <- t(boot_data)
# Analyze bootstrapped variance
head(boot_data)
sum(is.na(boot_data))
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
cluster <- makeCluster(ncores)  # Initialize compute cluster
boot_data <- parLapply(cluster, 1:nboot,
  function(x, datav) {
    samplev <- rnorm(nrows)
    c(sd=sd(samplev), mad=mad(samplev))
  }, datav=datav)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:nboot, function(x) {
  samplev <- rnorm(nrows)
  c(sd=sd(samplev), mad=mad(samplev))
}, mc.cores=ncores)  # end mclapply
stopCluster(cluster)  # Stop R processes over cluster
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
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
boot_data <- parLapply(cluster, 1:nboot,
  function(x, returns, nrows) {
    samplev <- returns[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, returns=returns, nrows=nrows)  # end parLapply
# Bootstrap sd and MAD under Mac-OSX or Linux
boot_data <- mclapply(1:nboot, function(x) {
    samplev <- returns[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, mc.cores=ncores)  # end mclapply
stopCluster(cluster)  # Stop R processes over cluster under Windows
boot_data <- rutils::do_call(rbind, boot_data)
# Standard error assuming normal distribution of returns
sd(returns)/sqrt(nboot)
# Means and standard errors from bootstrap
stderrors <- apply(boot_data, MARGIN=2,
  function(x) c(mean=mean(x), stderror=sd(x)))
stderrors
# Relative standard errors
stderrors[2, ]/stderrors[1, ]
# Calculate percentage returns from VTI prices
library(rutils)
prices <- quantmod::Cl(rutils::etfenv$VTI)
startd <- as.numeric(prices[1, ])
returns <- rutils::diffit(log(prices))
class(returns); head(returns)
sum(is.na(returns))
nrows <- NROW(returns)
# Define barrier level with respect to prices
barp <- 1.5*max(prices)
# Calculate single bootstrap sample
samplev <- returns[sample.int(nrows, replace=TRUE)]
# Calculate prices from percentage returns
samplev <- startd*exp(cumsum(samplev))
# Calculate if prices crossed barrier
sum(samplev > barp) > 0
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
cluster <- makeCluster(ncores)  # Initialize compute cluster under Windows
# Perform parallel bootstrap under Windows
clusterSetRNGStream(cluster, 1121)  # Reset random number generator in all cores
clusterExport(cluster, c("startd", "barp"))
nboot <- 10000
boot_data <- parLapply(cluster, 1:nboot,
  function(x, returns, nrows) {
    samplev <- returns[sample.int(nrows, replace=TRUE)]
    # Calculate prices from percentage returns
    samplev <- startd*exp(cumsum(samplev))
    # Calculate if prices crossed barrier
    sum(samplev > barp) > 0
  }, returns=returns, nrows=nrows)  # end parLapply
# Perform parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:nboot, function(x) {
    samplev <- returns[sample.int(nrows, replace=TRUE)]
    # Calculate prices from percentage returns
    samplev <- startd*exp(cumsum(samplev))
    # Calculate if prices crossed barrier
    sum(samplev > barp) > 0
  }, mc.cores=ncores)  # end mclapply
stopCluster(cluster)  # Stop R processes over cluster under Windows
boot_data <- rutils::do_call(rbind, boot_data)
# Calculate frequency of crossing barrier
sum(boot_data)/nboot
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
barp <- 1.5*max(prices)
# Calculate if High bootstrapped prices crossed barrier level
sum(boot_ohlc[, 2] > barp) > 0
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
cluster <- makeCluster(ncores)  # Initialize compute cluster under Windows
# Perform parallel bootstrap under Windows
clusterSetRNGStream(cluster, 1121)  # Reset random number generator in all cores
clusterExport(cluster, c("startd", "barp", "ohlc_diff"))
nboot <- 10000
boot_data <- parLapply(cluster, 1:nboot,
  function(x, returns, nrows) {
    # Calculate OHLC prices from percentage returns
    datav <- sample.int(nrows, replace=TRUE)
    boot_prices <- startd*exp(cumsum(returns[datav]))
    boot_ohlc <- ohlc_diff + boot_prices
    boot_ohlc <- cbind(boot_ohlc, boot_prices)
    # Calculate statistic
    sum(boot_ohlc[, 2] > barp) > 0
  }, returns=returns, nrows=nrows)  # end parLapply
# Perform parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:nboot, function(x) {
    # Calculate OHLC prices from percentage returns
    datav <- sample.int(nrows, replace=TRUE)
    boot_prices <- startd*exp(cumsum(returns[datav]))
    boot_ohlc <- ohlc_diff + boot_prices
    boot_ohlc <- cbind(boot_ohlc, boot_prices)
    # Calculate statistic
    sum(boot_ohlc[, 2] > barp) > 0
  }, mc.cores=ncores)  # end mclapply
stopCluster(cluster)  # Stop R processes over cluster under Windows
boot_data <- rutils::do_call(rbind, boot_data)
# Calculate frequency of crossing barrier
sum(boot_data)/nboot
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
boot_data <- sapply(1:nboot, function(x) {
  samplev <- sample.int(NROW(design), replace=TRUE)
  design <- design[samplev, ]
  cov(design[, 1], design[, 2])/var(design[, 2])
})  # end sapply
x11(width=6, height=5)
par(oma=c(1, 2, 1, 0), mgp=c(2, 1, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
# Mean and standard error of beta regression coefficient
c(mean=mean(boot_data), stderror=sd(boot_data))
# Plot density of bootstrapped beta coefficients
plot(density(boot_data), lwd=2, xlab="Regression slopes",
     main="Bootstrapped Regression Slopes")
# Add line for expected value
abline(v=mean(boot_data), lwd=2, col="red")
text(x=mean(boot_data)-0.01, y=1.0, labels="expected value",
     lwd=2, srt=90, pos=3)
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
cluster <- makeCluster(ncores)  # Initialize compute cluster under Windows
# Bootstrap of regression under Windows
boot_data <- parLapply(cluster, 1:1000,
  function(x, design) {
    samplev <- sample.int(NROW(design), replace=TRUE)
    design <- design[samplev, ]
    cov(design[, 1], design[, 2])/var(design[, 2])
  }, design=design)  # end parLapply
# Bootstrap of regression under Mac-OSX or Linux
boot_data <- mclapply(1:1000,
  function(x) {
    samplev <- sample.int(NROW(design), replace=TRUE)
    design <- design[samplev, ]
    cov(design[, 1], design[, 2])/var(design[, 2])
  }, mc.cores=ncores)  # end mclapply
stopCluster(cluster)  # Stop R processes over cluster under Windows
# Collapse the bootstrap list into a vector
class(boot_data)
boot_data <- unlist(boot_data)
# Mean and standard error of beta regression coefficient
c(mean=mean(boot_data), stderror=sd(boot_data))
# Plot density of bootstrapped beta coefficients
plot(density(boot_data),
     lwd=2, xlab="Regression slopes",
     main="Bootstrapped Regression Slopes")
# Add line for expected value
abline(v=mean(boot_data), lwd=2, col="red")
text(x=mean(boot_data)-0.01, y=1.0, labels="expected value",
     lwd=2, srt=90, pos=3)
set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
nrows <- 1000
datav <- rnorm(nrows)
# Estimate the 95% quantile
nboot <- 10000
boot_data <- sapply(1:nboot, function(x) {
  samplev <- datav[sample.int(nrows, replace=TRUE)]
  quantile(samplev, 0.95)
})  # end sapply
sd(boot_data)
# Estimate the 95% quantile using antithetic sampling
boot_data <- sapply(1:nboot, function(x) {
  samplev <- datav[sample.int(nrows, replace=TRUE)]
  quantile(c(samplev, -samplev), 0.95)
})  # end sapply
# Standard error of quantile from bootstrap
sd(boot_data)
sqrt(2)*sd(boot_data)
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
set.seed(1121) # Reset random number generator
# Sample from Standard Normal Distribution
nrows <- 1000
datav <- rnorm(nrows)
# Cumulative probability from formula
quantilev <- (-2)
pnorm(quantilev)
integrate(dnorm, lower=-Inf, upper=quantilev)
# Cumulative probability from Naive Monte Carlo
sum(datav < quantilev)/nrows
# Generate importance sample
lambda <- (-1.5)  # Tilt parameter
data_tilt <- datav + lambda  # Tilt the random numbers
# Cumulative probability from importance sample
sum(data_tilt < quantilev)/nrows
weightv <- exp(-lambda*data_tilt + lambda^2/2)
sum((data_tilt < quantilev)*weightv)/nrows
# Bootstrap of standard errors of cumulative probability
nboot <- 1000
boot_data <- sapply(1:nboot, function(x) {
  datav <- rnorm(nrows)
  naivemc <- sum(datav < quantilev)/nrows
  datav <- (datav + lambda)
  weightv <- exp(-lambda*datav + lambda^2/2)
  im_port <- sum((datav < quantilev)*weightv)/nrows
  c(naive_mc=naivemc, importance=im_port)
}) # end sapply
apply(boot_data, MARGIN=1,
  function(x) c(mean=mean(x), sd=sd(x)))
# Quantile from Naive Monte Carlo
confl <- 0.02
qnorm(confl)  # Exact value
datav <- sort(datav)
cutoff <- nrows*confl
datav[cutoff]  # Naive Monte Carlo value
# Importance sample weights
data_tilt <- datav + lambda  # Tilt the random numbers
weightv <- exp(-lambda*data_tilt + lambda^2/2)
# Cumulative probabilities using importance sample
cum_prob <- cumsum(weightv)/nrows
# Quantile from importance sample
data_tilt[findInterval(confl, cum_prob)]
# Bootstrap of standard errors of quantile
nboot <- 1000
boot_data <- sapply(1:nboot, function(x) {
  datav <- sort(rnorm(nrows))
  naivemc <- datav[cutoff]
  data_tilt <- datav + lambda
  weightv <- exp(-lambda*data_tilt + lambda^2/2)
  cum_prob <- cumsum(weightv)/nrows
  im_port <- data_tilt[findInterval(confl, cum_prob)]
  c(naive_mc=naivemc, importance=im_port)
}) # end sapply
apply(boot_data, MARGIN=1,
  function(x) c(mean=mean(x), sd=sd(x)))
# CVaR from Naive Monte Carlo
va_r <- datav[cutoff]
sum((datav < va_r)*datav)/sum((datav < va_r))
# CVaR from importance sample
va_r <- data_tilt[findInterval(confl, cum_prob)]
sum((data_tilt < va_r)*data_tilt*weightv)/sum((data_tilt < va_r)*weightv)
# CVaR from integration
integrate(function(x) x*dnorm(x), low=-Inf, up=va_r)$value/pnorm(va_r)
# Bootstrap of standard errors of expected value
nboot <- 1000
boot_data <- sapply(1:nboot, function(x) {
  datav <- sort(rnorm(nrows))
  va_r <- datav[cutoff]
  naivemc <- sum((datav < va_r)*datav)/sum((datav < va_r))
  data_tilt <- datav + lambda
  weightv <- exp(-lambda*data_tilt + lambda^2/2)
  cum_prob <- cumsum(weightv)/nrows
  va_r <- data_tilt[findInterval(confl, cum_prob)]
  im_port <- sum((data_tilt < va_r)*data_tilt*weightv)/sum((data_tilt < va_r)*weightv)
  c(naive_mc=naivemc, importance=im_port)
}) # end sapply
apply(boot_data, MARGIN=1,
  function(x) c(mean=mean(x), sd=sd(x)))
# Calculate matrix of random data
set.seed(1121) # Reset random number generator
nrows <- 1000; nboot <- 100
datav <- matrix(rnorm(nboot*nrows), ncol=nboot)
datav <- Rfast::sort_mat(datav)  # Sort the columns
# Calculate vector of quantiles for tilt parameter
confl <- 0.02; cutoff <- confl*nrows
calc_quant <- function(lambda) {
  data_tilt <- datav + lambda  # Tilt the random numbers
  weightv <- exp(-lambda*data_tilt + lambda^2/2)
  # Calculate quantiles for columns
  sapply(1:nboot, function(boo_t) {
    cum_prob <- cumsum(weightv[, boo_t])/nrows
    data_tilt[findInterval(confl, cum_prob), boo_t]
  })  # end sapply
}  # end calc_quant
# Define vector of tilt parameters
lambda_s <- seq(-3.0, -1.2, by=0.2)
# Calculate vector of quantiles for tilt parameters
quantiles <- sapply(lambda_s, calc_quant)
# Calculate standard deviations of quantiles for tilt parameters
stdevs <- apply(quantiles, MARGIN=2, sd)
# Calculate the optimal tilt parameter
lambda_s[which.min(stdevs)]
# Plot the standard deviations
x11(width=6, height=5)
plot(x=lambda_s, y=stdevs,
     main="Standard Deviations of Simulated Quantiles",
     xlab="tilt parameter", ylab="standard deviation",
     type="l", col="blue", lwd=2)
# Binomial sample
nrows <- 1000
probv <- 0.1
datav <- rbinom(n=nrows, size=1, probv)
head(datav, 33)
fre_q <- sum(datav)/nrows
# Tilted binomial sample
lambda <- 5
p_tilted <- lambda*probv/(1 + probv*(lambda - 1))
weigh_t <- (1 + probv*(lambda - 1))/lambda
datav <- rbinom(n=nrows, size=1, p_tilted)
head(datav, 33)
weigh_t*sum(datav)/nrows
# Bootstrap of standard errors
nboot <- 1000
boot_data <- sapply(1:nboot, function(x) {
  c(naive_mc=sum(rbinom(n=nrows, size=1, probv))/nrows,
    importance=weigh_t*sum(rbinom(n=nrows, size=1, p_tilted))/nrows)
}) # end sapply
apply(boot_data, MARGIN=1,
  function(x) c(mean=mean(x), sd=sd(x)))
# Define Brownian motion parameters
sigmav <- 1.0  # Volatility
drift <- 0.0  # Drift
nrows <- 100  # Number of simulation steps
nsimu <- 10000  # Number of simulations
# Calculate matrix of normal variables
set.seed(1121)
datav <- rnorm(nsimu*nrows, mean=drift, sd=sigmav)
datav <- matrix(datav, nc=nsimu)
# Simulate paths of Brownian motion
paths <- matrixStats::colCumsums(datav)
# Tilt the datav
lambda <- 0.04  # Tilt parameter
data_tilt <- datav + lambda  # Tilt the random numbers
paths_tilt <- matrixStats::colCumsums(data_tilt)
# Calculate path weights
weightv <- exp(-lambda*data_tilt + lambda^2/2)
path_weights <- matrixStats::colProds(weightv)
# Or
path_weights <- exp(-lambda*colSums(data_tilt) + nrows*lambda^2/2)
# Calculate option payout using standard MC
strikep <- 10  # Strike price
payouts <- (paths[nrows, ] - strikep)
sum(payouts[payouts > 0])/nsimu
# Calculate option payout using importance sampling
payouts <- (paths_tilt[nrows, ] - strikep)
sum((path_weights*payouts)[payouts > 0])/nsimu
# Calculate crossing probability using standard MC
barp <- 10
didcross <- colSums(paths > barp) > 0
sum(didcross)/nsimu
# Calculate crossing probability using importance sampling
didcross <- colSums(paths_tilt > barp) > 0
sum(path_weights*didcross)/nsimu
# Load S&P500 constituent stock prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
prices <- eapply(sp500env, quantmod::Cl)
prices <- rutils::do_call(cbind, prices)
# Carry forward non-NA prices
prices <- zoo::na.locf(prices, na.rm=FALSE)
# Drop ".Close" from column names
colnames(prices[, 1:4])
colnames(prices) <- rutils::get_name(colnames(prices))
# Or
# colnames(prices) <- do.call(rbind,
#   strsplit(colnames(prices), split="[.]"))[, 1]
# Calculate percentage returns of the S&P500 constituent stocks
# returns <- xts::diff.xts(log(prices))
returns <- xts::diff.xts(prices)/
  rutils::lagit(prices, pad_zeros=FALSE)
set.seed(1121)
sam_ple <- sample(NCOL(returns), s=100, replace=FALSE)
prices100 <- prices[, sam_ple]
returns100 <- returns[, sam_ple]
save(prices, prices100,
  file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
save(returns, returns100,
  file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Calculate number of constituents without prices
datav <- rowSums(is.na(prices))
datav <- xts::xts(datav, order.by=index(prices))
dygraphs::dygraph(datav, main="Number of S&P 500 Constituents Without Prices") %>%
  dyOptions(colors="blue", strokeWidth=2) %>%
  dyAxis("y", valueRange=c(0, 300))
# Calculate price weighted index of constituent
ncols <- NCOL(prices)
indeks <- xts(rowSums(prices)/ncols, index(prices))
colnames(indeks) <- "index"
# Combine index with VTI
datav <- cbind(indeks[index(etfenv$VTI)], etfenv$VTI[, 4])
colnames <- c("index", "VTI")
colnames(datav) <- colnames
# Plot index with VTI
dygraphs::dygraph(datav,
  main="S&P 500 Price-weighted Index and VTI") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="red") %>%
  dySeries(name=colnames[2], axis="y2", col="blue")
# Select ETF symbols for asset allocation
symbols <- c("VTI", "VEU", "EEM", "XLY", "XLP", "XLE", "XLF",
  "XLV", "XLI", "XLB", "XLK", "XLU", "VYM", "IVW", "IWB", "IWD",
  "IWF", "IEF", "TLT", "VNQ", "DBC", "GLD", "USO", "VXX", "SVXY",
  "MTUM", "IVE", "VLUE", "QUAL", "VTV", "USMV")
# Read etf database into data frame
etf_list <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/etf_list.csv")
rownames(etf_list) <- etf_list$Symbol
# Select from etf_list only those ETF's in symbols
etf_list <- etf_list[symbols, ]
# Shorten names
etf_names <- sapply(etf_list$Name, function(name) {
  namesvplit <- strsplit(name, split=" ")[[1]]
  namesvplit <- namesvplit[c(-1, -NROW(namesvplit))]
  name_match <- match("Select", namesvplit)
  if (!is.na(name_match))
    namesvplit <- namesvplit[-name_match]
  paste(namesvplit, collapse=" ")
})  # end sapply
etf_list$Name <- etf_names
etf_list["IEF", "Name"] <- "10 year Treasury Bond Fund"
etf_list["TLT", "Name"] <- "20 plus year Treasury Bond Fund"
etf_list["XLY", "Name"] <- "Consumer Discr. Sector Fund"
etf_list["EEM", "Name"] <- "Emerging Market Stock Fund"
etf_list["MTUM", "Name"] <- "Momentum Factor Fund"
etf_list["SVXY", "Name"] <- "Short VIX Futures"
etf_list["VXX", "Name"] <- "Long VIX Futures"
etf_list["DBC", "Name"] <- "Commodity Futures Fund"
etf_list["USO", "Name"] <- "WTI Oil Futures Fund"
etf_list["GLD", "Name"] <- "Physical Gold Fund"
print(xtable::xtable(etf_list), comment=FALSE, size="tiny", include.rownames=FALSE)
library(rutils)  # Load package rutils
# Calculate VTI percentage returns
returns <- rutils::etfenv$returns$VTI
returns <- drop(coredata(na.omit(returns)))
nrows <- NROW(returns)
# Mean and standard deviation of returns
c(mean(returns), sd(returns))
# Calculate the smoothing bandwidth as the MAD of returns 10 points apart
returns <- sort(returns)
b_w <- 10*mad(rutils::diffit(returns, lagg=10))
# Calculate the kernel density
densityv <- sapply(1:nrows, function(i_d) {
  sum(dnorm(returns-returns[i_d], sd=b_w))
})  # end sapply
madv <- mad(returns)
plot(returns, densityv, xlim=c(-5*madv, 5*madv),
     t="l", col="blue", lwd=3,
     xlab="returns", ylab="density",
     main="Density of VTI Returns")
# Calculate the kernel density using density()
densityv <- density(returns, bw=b_w)
NROW(densityv$y)
x11(width=6, height=5)
plot(densityv, xlim=c(-5*madv, 5*madv),
     xlab="returns", ylab="density",
     col="blue", lwd=3, main="Density of VTI Returns")
# Interpolate the densityv vector into returns
densityv <- approx(densityv$x, densityv$y, xout=returns)
all.equal(densityv$x, returns)
plot(densityv, xlim=c(-5*madv, 5*madv),
     xlab="returns", ylab="density",
     t="l", col="blue", lwd=3,
     main="Density of VTI Returns")
# Plot histogram
histo_gram <- hist(returns, breaks=100, freq=FALSE,
  xlim=c(-5*madv, 5*madv), xlab="", ylab="",
  main="VTI Return Distribution")
# Draw kernel density of histogram
lines(densityv, col="red", lwd=2)
# Add density of normal distribution
curve(expr=dnorm(x, mean=mean(returns), sd=sd(returns)),
add=TRUE, lwd=2, col="blue")
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("VTI", "Normal"), bty="n",
 lwd=6, bg="white", col=c("red", "blue"))
# Create normal Q-Q plot
qqnorm(returns, ylim=c(-0.1, 0.1), main="VTI Q-Q Plot",
 xlab="Normal Quantiles")
# Fit a line to the normal quantiles
qqline(returns, col="red", lwd=2)
# Perform Shapiro-Wilk test
shapiro.test(as.numeric(returns))
# Boxplot method for formula
boxplot(formula=mpg ~ cyl, data=mtcars,
  main="Mileage by number of cylinders",
  xlab="Cylinders", ylab="Miles per gallon")
# Boxplot method for data frame of EuStockMarkets percentage returns
boxplot(x=diff(log(EuStockMarkets)))
# VTI percentage returns
returns <- na.omit(rutils::etfenv$returns$VTI)
# Number of observations
nrows <- NROW(returns)
# Mean of VTI returns
mean_rets <- mean(returns)
# Standard deviation of VTI returns
sd_rets <- sd(returns)
# Skewness of VTI returns
nrows/((nrows-1)*(nrows-2))*
  sum(((returns - mean_rets)/sd_rets)^3)
# Kurtosis of VTI returns
nrows*(nrows+1)/((nrows-1)^3)*
  sum(((returns - mean_rets)/sd_rets)^4)
# Random normal returns
returns <- rnorm(nrows, sd=sd_rets)
# Mean and standard deviation of random normal returns
mean_rets <- mean(returns)
sd_rets <- sd(returns)
# Skewness of random normal returns
nrows/((nrows-1)*(nrows-2))*
  sum(((returns - mean_rets)/sd_rets)^3)
# Kurtosis of random normal returns
nrows*(nrows+1)/((nrows-1)^3)*
  sum(((returns - mean_rets)/sd_rets)^4)
# calc_skew() calculates skew of returns
calc_skew <- function(returns) {
  returns <- na.omit(returns)
  sum(((returns - mean(returns))/sd(returns))^3)/NROW(returns)
}  # end calc_skew
# calc_kurt() calculates kurtosis of returns
calc_kurt <- function(returns) {
  returns <- na.omit(returns)
  sum(((returns - mean(returns))/sd(returns))^4)/NROW(returns)
}  # end calc_kurt
# Calculate skew and kurtosis of VTI returns
calc_skew(returns)
calc_kurt(returns)
# calcmom() calculates the moments of returns
calcmom <- function(returns, moment=3) {
  returns <- na.omit(returns)
  sum(((returns - mean(returns))/sd(returns))^moment)/NROW(returns)
}  # end calcmom
# Calculate skew and kurtosis of VTI returns
calcmom(returns, moment=3)
calcmom(returns, moment=4)
set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
nrows <- 1000
datav <- rnorm(nrows)
# Sample mean
mean(datav)
# Sample standard deviation
sd(datav)
# Standard error of sample mean
sd(datav)/sqrt(nrows)
x_var <- seq(-5, 7, length=100)
y_var <- dnorm(x_var, mean=1.0, sd=2.0)
plot(x_var, y_var, type="l", lty="solid", xlab="", ylab="")
title(main="Normal Density Function", line=0.5)
startd <- 3; fin_ish <- 5  # Set lower and upper bounds
# Plot polygon area
are_a <- ((x_var >= startd) & (x_var <= fin_ish))
polygon(c(startd, x_var[are_a], fin_ish),
  c(-1, y_var[are_a], -1), col="red")
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
sigmavs <- c(0.5, 1, 1.5, 2)  # Sigma values
# Create plot colors
colors <- c("red", "black", "blue", "green")
# Create legend labels
lab_els <- paste("sigma", sigmavs, sep="=")
for (indeks in 1:4) {  # Plot four curves
  curve(expr=dnorm(x, sd=sigmavs[indeks]),
  xlim=c(-4, 4), xlab="", ylab="", lwd=2,
  col=colors[indeks], add=as.logical(indeks-1))
}  # end for
# Add title
title(main="Normal Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, title="Sigmas",
 lab_els, cex=0.8, lwd=2, lty=1, bty="n", col=colors)
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
deg_free <- c(3, 6, 9)  # Df values
colors <- c("black", "red", "blue", "green")
lab_els <- c("normal", paste("df", deg_free, sep="="))
# Plot a Normal probability distribution
curve(expr=dnorm, xlim=c(-4, 4), xlab="", ylab="", lwd=2)
for (indeks in 1:3) {  # Plot three t-distributions
  curve(expr=dt(x, df=deg_free[indeks]), xlab="", ylab="",
lwd=2, col=colors[indeks+1], add=TRUE)
}  # end for
# Add title
title(main="t-distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n",
       title="Degrees\n of freedom", lab_els,
       cex=0.8, lwd=6, lty=1, col=colors)
# Mixture of two normal distributions with sd=1 and sd=2
nrows <- 1e5
returns <- c(rnorm(nrows/2), 2*rnorm(nrows/2))
returns <- (returns-mean(returns))/sd(returns)
# Kurtosis of normal
calc_kurt(rnorm(nrows))
# Kurtosis of mixture
calc_kurt(returns)
# Or
nrows*sum(returns^4)/(nrows-1)^2
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot the distributions
plot(density(returns), xlab="", ylab="",
  main="Mixture of Normal Returns",
  xlim=c(-3, 3), type="l", lwd=3, col="red")
curve(expr=dnorm, lwd=2, col="blue", add=TRUE)
curve(expr=dt(x, df=3), lwd=2, col="green", add=TRUE)
# Add legend
legend("topright", inset=0.05, lty=1, lwd=6, bty="n",
  legend=c("Mixture", "Normal", "t-distribution"),
  col=c("red", "blue", "green"))
# Objective function is log-likelihood
likeli_hood <- function(pa_r, free_dom, datav) {
  sum(
    -log(gamma((free_dom+1)/2) /
      (sqrt(pi*free_dom) * gamma(free_dom/2))) +
    log(pa_r[2]) +
    (free_dom+1)/2 * log(1 + ((datav - pa_r[1])/
                    pa_r[2])^2/free_dom))
}  # end likeli_hood
# Demonstrate equivalence with log(dt())
likeli_hood(c(1, 0.5), 2, 2:5)
-sum(log(dt(x=(2:5-1)/0.5, df=2)/0.5))
# Simpler objective function
likeli_hood <- function(pa_r, free_dom, datav) {
  -sum(log(dt(x=(datav-pa_r[1])/pa_r[2],
      df=free_dom)/pa_r[2]))
}  # end likeli_hood
# VTI percentage returns
returns <- na.omit(rutils::etfenv$returns$VTI)
# Initial parameters
par_init <- c(mean=0, scale=0.01)
# Fit distribution using optim()
optim_fit <- optim(par=par_init,
  fn=likeli_hood, # Log-likelihood function
  datav=returns,
  free_dom=2, # Degrees of freedom
  method="L-BFGS-B", # quasi-Newton method
  upper=c(1, 0.1), # upper constraint
  lower=c(-1, 1e-7)) # Lower constraint
# optimal parameters
lo_cation <- optim_fit$par["mean"]
scalit <- optim_fit$par["scale"]
# Fit VTI returns using MASS::fitdistr()
optim_fit <- MASS::fitdistr(returns,
  densfun="t", df=2)
optim_fit$estimate
optim_fit$sd
lo_cation <- optim_fit$estimate[1]
scalit <- optim_fit$estimate[2]
summary(optim_fit)
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot histogram of VTI returns
histo_gram <- hist(returns, col="lightgrey",
  xlab="returns", breaks=100, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE, main="VTI Returns Histogram")
lines(density(returns, adjust=1.5), lwd=3, col="blue")
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(returns),
  sd=sd(returns)), add=TRUE, lwd=3, col="green")
# Plot t-distribution function
curve(expr=dt((x-lo_cation)/scalit, df=2)/scalit,
type="l", lwd=3, col="red", add=TRUE)
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("density", "t-distr", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot histogram of VTI returns
histo_gram <- hist(returns, breaks=100, plot=FALSE)
plot(histo_gram, xlab="returns", ylab="frequency",
     col="lightgrey", freq=FALSE, main="VTI Left Tail Returns Histogram",
     xlim=c(min(returns), -0.02),
     ylim=c(0.0, histo_gram$density[findInterval(-0.02, histo_gram$breaks)]))
lines(density(returns, adjust=1.5), lwd=4, col="blue")
# Plot t-distribution function
curve(expr=dt((x-lo_cation)/scalit, df=2)/scalit, type="l", lwd=4, col="red", add=TRUE)
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(returns), sd=sd(returns)), add=TRUE, lwd=4, col="green")
# Add legend
legend("topleft", inset=0.05, bty="n",
  leg=c("density", "t-distr", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))
# Calculate VTI returns and trading volumes
ohlc <- rutils::etfenv$VTI
closep <- drop(coredata(quantmod::Cl(ohlc)))
returns <- rutils::diffit(log(closep))
volumes <- coredata(quantmod::Vo(ohlc))
# Calculate rolling variance
look_back <- 121
variance <- HighFreq::roll_var_ohlc(log(ohlc), method="close", look_back=look_back, scale=FALSE)
variance[1:look_back, ] <- variance[look_back+1, ]
# Calculate rolling average volume
volume_roll <- HighFreq::roll_vec(volumes, look_back=look_back)/look_back
# dygraph plot of VTI variance and trading volumes
datav <- xts::xts(cbind(variance, volume_roll), index(ohlc))
colnames <- c("variance", "volume")
colnames(datav) <- colnames
dygraphs::dygraph(datav, main="VTI Variance and Trading Volumes") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], strokeWidth=2, axis="y", col="blue") %>%
  dySeries(name=colnames[2], strokeWidth=2, axis="y2", col="red")
# Scale returns using volume (volume clock)
rets_scaled <- ifelse(volumes > 0, sqrt(volume_roll)*returns/sqrt(volumes), 0)
rets_scaled <- sd(returns)*rets_scaled/sd(rets_scaled)
# rets_scaled <- ifelse(volumes > 1e4, returns/volumes, 0)
# Calculate moments of scaled returns
nrows <- NROW(returns)
sapply(list(returns=returns, rets_scaled=rets_scaled),
  function(rets) {sapply(c(skew=3, kurt=4),
     function(x) sum((rets/sd(rets))^x)/nrows)
})  # end sapply
# x11(width=6, height=5)
dev.new(width=6, height=5, noRStudioGD=TRUE)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot densities of SPY returns
madv <- mad(returns)
# b_w <- mad(rutils::diffit(returns))
plot(density(returns, bw=madv/10), xlim=c(-5*madv, 5*madv),
     lwd=3, mgp=c(2, 1, 0), col="blue",
     xlab="returns (standardized)", ylab="frequency",
     main="Density of Volume-scaled VTI Returns")
lines(density(rets_scaled, bw=madv/10), lwd=3, col="red")
curve(expr=dnorm(x, mean=mean(returns), sd=sd(returns)),
add=TRUE, lwd=3, col="green")
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("unscaled", "scaled", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))
quartz.save("figure/vti_scaled.png", type="png", width=6, height=5)
# Calculate VTI percentage returns
library(rutils)
returns <- na.omit(rutils::etfenv$returns$VTI)
# Reset output digits
dig_its <- options(digits=5)
# Shapiro-Wilk test for normal distribution
shapiro.test(rnorm(NROW(returns)))
# Shapiro-Wilk test for VTI returns
shapiro.test(as.numeric(returns))
# Shapiro-Wilk test for uniform distribution
shapiro.test(runif(NROW(returns)))
# Restore output digits
options(digits=dig_its$digits)
library(tseries)  # Load package tseries
# Jarque-Bera test for normal distribution
jarque.bera.test(rnorm(NROW(returns)))
# Jarque-Bera test for VTI returns
jarque.bera.test(returns)
# Jarque-Bera test for uniform distribution
jarque.bera.test(runif(NROW(returns)))
# KS test for normal distribution
ks.test(rnorm(100), pnorm)
# KS test for uniform distribution
ks.test(runif(100), pnorm)
# KS test for two similar normal distributions
ks.test(rnorm(100), rnorm(100, mean=0.1))
# KS test for two different normal distributions
ks.test(rnorm(100), rnorm(100, mean=1.0))
# Fit t-dist into VTI returns
returns <- na.omit(rutils::etfenv$returns$VTI)
optim_fit <- MASS::fitdistr(returns, densfun="t", df=2)
lo_cation <- optim_fit$estimate[1]
scalit <- optim_fit$estimate[2]
# Perform Kolmogorov-Smirnov test on VTI returns
datav <- lo_cation + scalit*rt(NROW(returns), df=2)
ks.test(as.numeric(returns), datav)
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Degrees of freedom
deg_free <- c(2, 5, 8, 11)
# Plot four curves in loop
colors <- c("red", "black", "blue", "green")
for (indeks in 1:4) {
  curve(expr=dchisq(x, df=deg_free[indeks]),
  xlim=c(0, 20), ylim=c(0, 0.3),
  xlab="", ylab="", col=colors[indeks],
  lwd=2, add=as.logical(indeks-1))
}  # end for
# Add title
title(main="Chi-squared Distributions", line=0.5)
# Add legend
lab_els <- paste("df", deg_free, sep="=")
legend("topright", inset=0.05, bty="n",
       title="Degrees of freedom", lab_els,
       cex=0.8, lwd=6, lty=1, col=colors)
# Observed frequencies from random normal data
histo_gram <- hist(rnorm(1e3, mean=0), breaks=100, plot=FALSE)
freq_o <- histo_gram$counts
# Theoretical frequencies
freq_t <- rutils::diffit(pnorm(histo_gram$breaks))
# Perform Chi-squared test for normal data
chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
# Return p-value
chisq_test <- chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
chisq_test$p.value
# Observed frequencies from shifted normal data
histo_gram <- hist(rnorm(1e3, mean=2), breaks=100, plot=FALSE)
freq_o <- histo_gram$counts/sum(histo_gram$counts)
# Theoretical frequencies
freq_t <- rutils::diffit(pnorm(histo_gram$breaks))
# Perform Chi-squared test for shifted normal data
chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
# Calculate histogram of VTI returns
histo_gram <- hist(returns, breaks=100, plot=FALSE)
freq_o <- histo_gram$counts
# Calculate cumulative probabilities and then difference them
freq_t <- pt((histo_gram$breaks-lo_cation)/scalit, df=2)
freq_t <- rutils::diffit(freq_t)
# Perform Chi-squared test for VTI returns
chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
# Load package PerformanceAnalytics
library(PerformanceAnalytics)
# Get documentation for package PerformanceAnalytics
# Get short description
packageDescription("PerformanceAnalytics")
# Load help page
help(package="PerformanceAnalytics")
# List all objects in PerformanceAnalytics
ls("package:PerformanceAnalytics")
# List all datasets in PerformanceAnalytics
data(package="PerformanceAnalytics")
# Remove PerformanceAnalytics from search path
detach("package:PerformanceAnalytics")
perf_data <- unclass(data(
    package="PerformanceAnalytics"))$results[, -(1:2)]
apply(perf_data, 1, paste, collapse=" - ")
# Load "managers" data set
data(managers)
class(managers)
dim(managers)
head(managers, 3)
# Load package "PerformanceAnalytics"
library(PerformanceAnalytics)
# Calculate ETF returns
returns <- rutils::etfenv$returns[, c("VTI", "DBC", "IEF")]
returns <- na.omit(returns)
# Plot cumulative ETF returns
x11(width=6, height=5)
chart.CumReturns(returns, lwd=2, ylab="",
  legend.loc="topleft", main="ETF Cumulative Returns")
returns <- rutils::etfenv$returns$VTI
returns <- na.omit(returns)
x11(width=6, height=5)
chart.Histogram(returns, xlim=c(-0.04, 0.04),
  colorset = c("lightgray", "red", "blue"), lwd=3,
  main=paste("Distribution of", colnames(returns), "Returns"),
  methods = c("add.density", "add.normal"))
legend("topright", inset=0.05, bty="n",
 leg=c("VTI Density", "Normal"),
 lwd=6, lty=1, col=c("red", "blue"))
returns <- rutils::etfenv$returns[,
  c("VTI", "IEF", "IVW", "VYM", "IWB", "DBC", "VXX")]
x11(width=6, height=5)
chart.Boxplot(names=FALSE, returns)
par(cex.lab=0.8, cex.axis=0.8)
axis(side=2, at=(1:NCOL(returns))/7.5-0.05,labels=colnames(returns))
# Simulate normally distributed data
nrows <- 1000
datav <- rnorm(nrows)
sd(datav)
mad(datav)
median(abs(datav - median(datav)))
median(abs(datav - median(datav)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
boot_data <- sapply(1:10000, function(x) {
  samplev <- datav[sample.int(nrows, replace=TRUE)]
  c(sd=sd(samplev), mad=mad(samplev))
})  # end sapply
boot_data <- t(boot_data)
# Analyze bootstrapped variance
head(boot_data)
sum(is.na(boot_data))
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
cluster <- makeCluster(ncores)  # Initialize compute cluster
boot_data <- parLapply(cluster, 1:10000,
  function(x, datav) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, datav=datav)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:10000, function(x) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, mc.cores=ncores)  # end mclapply
stopCluster(cluster)  # Stop R processes over cluster
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
# VTI returns
returns <- rutils::etfenv$returns$VTI
returns <- na.omit(returns)
nrows <- NROW(returns)
sd(returns)
mad(returns)
# Bootstrap of sd and mad estimators
boot_data <- sapply(1:10000, function(x) {
  samplev <- returns[sample.int(nrows, replace=TRUE)]
  c(sd=sd(samplev), mad=mad(samplev))
})  # end sapply
boot_data <- t(boot_data)
# Means and standard errors from bootstrap
100*apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
cluster <- makeCluster(ncores)  # Initialize compute cluster
clusterExport(cluster, c("nrows", "returns"))
boot_data <- parLapply(cluster, 1:10000,
  function(x) {
    samplev <- returns[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  })  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:10000, function(x) {
    samplev <- returns[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, mc.cores=ncores)  # end mclapply
stopCluster(cluster)  # Stop R processes over cluster
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
library(PerformanceAnalytics)
# Define target rate of return of 50 bps
targetr <- 0.005
# Calculate the full downside returns
returns_sub <- (returns - targetr)
returns_sub <- ifelse(returns_sub < 0, returns_sub, 0)
nrows <- NROW(returns_sub)
# Calculate the downside deviation
all.equal(sqrt(sum(returns_sub^2)/nrows),
  drop(DownsideDeviation(returns, MAR=targetr, method="full")))
# Calculate the subset downside returns
returns_sub <- (returns - targetr)
returns_sub <- returns_sub[returns_sub < 0]
nrows <- NROW(returns_sub)
# Calculate the downside deviation
all.equal(sqrt(sum(returns_sub^2)/nrows),
  DownsideDeviation(returns, MAR=targetr, method="subset"))
# Calculate time series of VTI drawdowns
closep <- log(na.omit(rutils::etfenv$prices$VTI))
draw_downs <- (closep - cummax(closep))
# PerformanceAnalytics plot of VTI drawdowns
returns <- rutils::diffit(log(closep))
PerformanceAnalytics::chart.Drawdown(returns,
  ylab="", main="VTI Drawdowns")
# PerformanceAnalytics table of VTI drawdowns
PerformanceAnalytics::table.Drawdowns(returns)
# dygraph plot of VTI drawdowns
datav <- cbind(closep, draw_downs)
colnames <- c("VTI", "Drawdowns")
colnames(datav) <- colnames
dygraphs::dygraph(datav, main="VTI Drawdowns") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], valueRange=c(min(datav[, "Drawdowns"]), 5), independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", col="blue") %>%
  dySeries(name=colnames[2], axis="y2", col="red")
# Plot VTI drawdowns
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
x11(width=6, height=5)
quantmod::chart_Series(x=draw_downs, name="VTI Drawdowns", theme=plot_theme)
library(xtable)
library(PerformanceAnalytics)
closep <- log(na.omit(rutils::etfenv$prices$VTI))
returns <- rutils::diffit(log(closep))
# Calculate table of VTI drawdowns
tablev <- PerformanceAnalytics::table.Drawdowns(returns)
# Convert dates to strings
tablev <- cbind(sapply(tablev[, 1:3], as.character), tablev[, 4:7])
# Print table of VTI drawdowns
print(xtable(tablev), comment=FALSE, size="tiny", include.rownames=FALSE)
# Load "managers" data set
data(managers)
charts.PerformanceSummary(ham1,
  main="", lwd=2, ylog=TRUE)
x11(width=6, height=5)
par(mar=c(3, 2, 1, 0), oma=c(0, 0, 0, 0))
# VTI percentage returns
returns <- na.omit(rutils::etfenv$returns$VTI)
confl <- 0.1
va_r <- quantile(returns, confl)
c_var <- mean(returns[returns < va_r])
# Plot histogram of VTI returns
histo_gram <- hist(returns, col="lightgrey",
  xlab="returns", ylab="frequency", breaks=100,
  xlim=c(-0.05, 0.01), freq=FALSE, main="VTI Returns Histogram")
# Calculate density
densv <- density(returns, adjust=1.5)
# Plot density
lines(densv, lwd=3, col="blue")
# Plot line for VaR
abline(v=va_r, col="red", lwd=3)
text(x=va_r, y=20, labels="VaR", lwd=2, srt=90, pos=2)
# Plot polygon shading for CVaR
var_max <- -0.06
rangev <- (densv$x < va_r) &  (densv$x > var_max)
polygon(c(var_max, densv$x[rangev], va_r),
  c(0, densv$y[rangev], 0), col=rgb(1, 0, 0,0.5), border=NA)
text(x=va_r, y=3, labels="CVaR", lwd=2, pos=2)
# VTI percentage returns
returns <- na.omit(rutils::etfenv$returns$VTI)
confl <- 0.02
# Calculate VaR as quantile
va_r <- quantile(returns, confl)
# Or by sorting
sort_ed <- sort(as.numeric(returns))
indeks <- round(confl*NROW(returns))
va_r <- sort_ed[indeks]
# PerformanceAnalytics VaR
PerformanceAnalytics::VaR(returns,
  p=(1-confl), method="historical")
all.equal(unname(va_r),
  as.numeric(PerformanceAnalytics::VaR(returns,
  p=(1-confl), method="historical")))
x11(width=6, height=5)
par(mar=c(3, 2, 1, 0), oma=c(0, 0, 0, 0))
# Calculate VaR as quantile
va_r <- quantile(returns, confl)
# Calculate CVaR as expected loss
c_var <- mean(returns[returns < va_r])
# Or by sorting
sort_ed <- sort(as.numeric(returns))
indeks <- round(confl*NROW(returns))
va_r <- sort_ed[indeks]
c_var <- mean(sort_ed[1:indeks])
# PerformanceAnalytics VaR
PerformanceAnalytics::ETL(returns,
  p=(1-confl), method="historical")
all.equal(c_var,
  as.numeric(PerformanceAnalytics::ETL(returns,
  p=(1-confl), method="historical")))
# Calculate the risk-return statistics
risk_ret <-
  PerformanceAnalytics::table.Stats(rutils::etfenv$returns)
class(risk_ret)
# Transpose the data frame
risk_ret <- as.data.frame(t(risk_ret))
# Add Name column
risk_ret$Name <- rownames(risk_ret)
# Add Sharpe ratio column
risk_ret$Sharpe <- risk_ret$"Arithmetic Mean"/risk_ret$Stdev
# Sort on Sharpe ratio
risk_ret <- risk_ret[order(risk_ret$Sharpe, decreasing=TRUE), ]
# Copy from rutils to save time
risk_ret <- rutils::etfenv$riskstats
# Add Sharpe ratio column
risk_ret$Sharpe <- risk_ret$"Arithmetic Mean"/risk_ret$Stdev
# Sort on Sharpe ratio
risk_ret <- risk_ret[order(risk_ret$Sharpe, decreasing=TRUE), ]
# Print data frame
knitr::kable(risk_ret[, c("Sharpe", "Skewness", "Kurtosis")])
# Print data frame
knitr::kable(risk_ret[c("VXX", "SVXY"), c("Sharpe", "Skewness", "Kurtosis")])
# dygraph plot of VTI drawdowns
prices <- na.omit(rutils::etfenv$prices[, c("VXX", "SVXY")])
prices <- prices["2017/"]
colnames <- c("VXX", "SVXY")
colnames(prices) <- colnames
dygraphs::dygraph(prices, main="Prices of VXX and SVXY") %>%
  dyAxis("y", label=colnames[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnames[2], independentTicks=TRUE) %>%
  dySeries(name=colnames[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnames[2], axis="y2", strokeWidth=2, col="green") %>%
  dyLegend(show="always", width=500)
# Remove VIX volatility ETF data
risk_ret <- risk_ret[-match(c("VXX", "SVXY"), risk_ret$Name), ]
# Plot scatterplot of Sharpe vs Skewness
plot(Sharpe ~ Skewness, data=risk_ret,
     ylim=1.1*range(risk_ret$Sharpe),
     main="Sharpe vs Skewness")
# Add labels
text(x=risk_ret$Skewness, y=risk_ret$Sharpe,
    labels=risk_ret$Name, pos=3, cex=0.8)
# Plot scatterplot of Kurtosis vs Skewness
x11(width=6, height=5)
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
plot(Kurtosis ~ Skewness, data=risk_ret,
     ylim=c(1, max(risk_ret$Kurtosis)),
     main="Kurtosis vs Skewness")
# Add labels
text(x=risk_ret$Skewness, y=risk_ret$Kurtosis,
    labels=risk_ret$Name, pos=1, cex=0.8)
library(PerformanceAnalytics)
returns <- rutils::etfenv$returns[, c("VTI", "IEF")]
returns <- na.omit(returns)
# Calculate the Sharpe ratio
PerformanceAnalytics::SharpeRatio(returns)
# Calculate the Sortino ratio
PerformanceAnalytics::SortinoRatio(returns)
# Calculate the Calmar ratio
PerformanceAnalytics::CalmarRatio(returns)
# Calculate the returns statistics
tail(PerformanceAnalytics::table.Stats(returns), 4)
