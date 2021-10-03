# Display documentation on function "getwd"
help(getwd)
# Equivalent to "help(getwd)"
?getwd

# Open the hypertext documentation
help.start()

# Calculate cumulative sum of a vector
vec_tor <- runif(1e5)
# Use compiled function
cum_sum <- cumsum(vec_tor)
# Use for loop
cum_sum2 <- vec_tor
for (i in 2:NROW(vec_tor))
  cum_sum2[i] <- (vec_tor[i] + cum_sum2[i-1])
# Compare the two methods
all.equal(cum_sum, cum_sum2)
# Microbenchmark the two methods
library(microbenchmark)
summary(microbenchmark(
  cumsum=cumsum(vec_tor),
  loop_alloc={
    cum_sum2 <- vec_tor
    for (i in 2:NROW(vec_tor))
cum_sum2[i] <- (vec_tor[i] + cum_sum2[i-1])
  },
  loop_nalloc={
    # Doesn't allocate memory to cum_sum3
    cum_sum3 <- vec_tor[1]
    for (i in 2:NROW(vec_tor))
# This command adds an extra element to cum_sum3
cum_sum3[i] <- (vec_tor[i] + cum_sum3[i-1])
  },
  times=10))[, c(1, 4, 5)]

# "<-" and "=" are valid assignment operators
my_var <- 3

# Typing a symbol or expression evaluates it
my_var

# Text in quotes is interpreted as a string
my_var <- "Hello World!"

# Typing a symbol or expression evaluates it
my_var

my_var  # Text after hash is treated as comment

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
# Environments are subset like lists
new_env$new_var1
# Environments are subset like lists
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
sym_bols <- c("VTI", "VEU", "IEF", "VNQ")
# Extract sym_bols from rutils::etf_env
price_s <- mget(sym_bols, envir=rutils::etf_env)
# price_s is a list of xts series
class(price_s)
class(price_s[[1]])
# Extract Close prices
price_s <- lapply(price_s, quantmod::Cl)
# Collapse list into time series the hard way
xts_1 <- cbind(price_s[[1]], price_s[[2]], price_s[[3]], price_s[[4]])
class(xts_1)
dim(xts_1)
# Collapse list into time series using do.call()
price_s <- do.call(cbind, price_s)
all.equal(xts_1, price_s)
class(price_s)
dim(price_s)
# Extract and cbind in single step
price_s <- do.call(cbind, lapply(
  mget(sym_bols, envir=rutils::etf_env), quantmod::Cl))
# Or
# Extract and bind all data, subset by sym_bols
price_s <- lapply(sym_bols, function(sym_bol) {
    quantmod::Cl(get(sym_bol, envir=rutils::etf_env))
})  # end lapply
# Same, but loop over etf_env without anonymous function
price_s <- do.call(cbind,
  lapply(as.list(rutils::etf_env)[sym_bols], quantmod::Cl))
# Same, but works only for OHLC series - produces error
price_s <- do.call(cbind,
  eapply(rutils::etf_env, quantmod::Cl)[sym_bols])

# Drop ".Close" from column names
colnames(price_s[, 1:4])
do.call(rbind, strsplit(colnames(price_s[, 1:4]), split="[.]"))[, 1]
colnames(price_s) <- do.call(rbind, strsplit(colnames(price_s), split="[.]"))[, 1]
# Or
colnames(price_s) <- unname(sapply(colnames(price_s),
    function(col_name) strsplit(col_name, split="[.]")[[1]][1]))
tail(price_s, 3)
# Which objects in global environment are class xts?
unlist(eapply(globalenv(), is.xts))
# Save xts to csv file
write.zoo(price_s,
  file="C:/Develop/lecture_slides/data/etf_series.csv", sep=",")
# Copy price_s into etf_env
etf_env$etf_list <- etf_list
# Or
assign("price_s", price_s, envir=etf_env)
# Save to .RData file
save(etf_env, file="etf_data.RData")

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
v_wap <- TTR::VWAP(
  price=quantmod::Cl(rutils::etf_env$VTI),
  volume=quantmod::Vo(rutils::etf_env$VTI), n=10)

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
vec_tor <- runif(1e6)
# sqrt() and "^0.5" are the same
all.equal(sqrt(vec_tor), vec_tor^0.5)
# sqrt() is much faster than "^0.5"
system.time(vec_tor^0.5)
microbenchmark(
  power = vec_tor^0.5,
  sqrt = sqrt(vec_tor),
  times=10)

# Calculate matrix of random data with 5,000 rows
mat_rix <- matrix(rnorm(10000), ncol=2)
# Allocate memory for row sums
row_sums <- numeric(NROW(mat_rix))
summary(microbenchmark(
  row_sums = rowSums(mat_rix),  # end row_sums
  ap_ply = apply(mat_rix, 1, sum),  # end apply
  l_apply = lapply(1:NROW(mat_rix), function(in_dex)
    sum(mat_rix[in_dex, ])),  # end lapply
  v_apply = vapply(1:NROW(mat_rix), function(in_dex)
    sum(mat_rix[in_dex, ]),
    FUN.VALUE = c(sum=0)),  # end vapply
  s_apply = sapply(1:NROW(mat_rix), function(in_dex)
    sum(mat_rix[in_dex, ])),  # end sapply
  for_loop = for (i in 1:NROW(mat_rix)) {
    row_sums[i] <- sum(mat_rix[i,])
  },  # end for
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

vec_tor <- rnorm(5000)
summary(microbenchmark(
# Allocate full memory for cumulative sum
  for_loop = {cum_sum <- numeric(NROW(vec_tor))
    cum_sum[1] <- vec_tor[1]
    for (i in 2:NROW(vec_tor)) {
      cum_sum[i] <- cum_sum[i-1] + vec_tor[i]
    }},  # end for
# Allocate zero memory for cumulative sum
  grow_vec = {cum_sum <- numeric(0)
    cum_sum[1] <- vec_tor[1]
    for (i in 2:NROW(vec_tor)) {
# Add new element to "cum_sum" ("grow" it)
      cum_sum[i] <- cum_sum[i-1] + vec_tor[i]
    }},  # end for
# Allocate zero memory for cumulative sum
  com_bine = {cum_sum <- numeric(0)
    cum_sum[1] <- vec_tor[1]
    for (i in 2:NROW(vec_tor)) {
# Add new element to "cum_sum" ("grow" it)
      cum_sum <- c(cum_sum, vec_tor[i])
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
  vec_torized = (vector1 + vector2),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Allocate memory for cumulative sum
cum_sum <- numeric(NROW(big_vector))
cum_sum[1] <- big_vector[1]
# Calculate cumulative sum in two different ways
summary(microbenchmark(
# Cumulative sum using "for" loop
  r_loop = (for (i in 2:NROW(big_vector)) {
    cum_sum[i] <- cum_sum[i-1] + big_vector[i]
  }),
# Cumulative sum using "cumsum"
  vec_torized = cumsum(big_vector),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Calculate matrix of random data with 5,000 rows
mat_rix <- matrix(rnorm(10000), ncol=2)
# Calculate row sums two different ways
all.equal(rowSums(mat_rix),
  apply(mat_rix, 1, sum))
summary(microbenchmark(
  row_sums = rowSums(mat_rix),
  ap_ply = apply(mat_rix, 1, sum),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

library(microbenchmark)
str(pmax)
# Calculate row maximums two different ways
summary(microbenchmark(
  p_max=do.call(pmax.int,
lapply(seq_along(mat_rix[1, ]),
  function(in_dex) mat_rix[, in_dex])),
  l_apply=unlist(lapply(seq_along(mat_rix[, 1]),
  function(in_dex) max(mat_rix[in_dex, ]))),
  times=10))[, c(1, 4, 5)]

install.packages("matrixStats")  # Install package matrixStats
library(matrixStats)  # Load package matrixStats
# Calculate row min values three different ways
summary(microbenchmark(
  row_mins = rowMins(mat_rix),
  p_min =
    do.call(pmin.int,
      lapply(seq_along(mat_rix[1, ]),
             function(in_dex)
               mat_rix[, in_dex])),
  as_data_frame =
    do.call(pmin.int,
      as.data.frame.matrix(mat_rix)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

install.packages("Rfast")  # Install package Rfast
library(Rfast)  # Load package Rfast
# Benchmark speed of calculating ranks
vec_tor <- 1e3
all.equal(rank(vec_tor), Rfast::Rank(vec_tor))
library(microbenchmark)
summary(microbenchmark(
  Rcode = rank(vec_tor),
  Rfast = Rfast::Rank(vec_tor),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Benchmark speed of calculating column medians
mat_rix <- matrix(1e4, nc=10)
all.equal(matrixStats::colMedians(mat_rix), Rfast::colMedians(mat_rix))
summary(microbenchmark(
  matrixStats = matrixStats::colMedians(mat_rix),
  Rfast = Rfast::colMedians(mat_rix),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

summary(microbenchmark(  # Assign values to vector three different ways
# Fast vectorized assignment loop performed in C using brackets "[]"
  brack_ets = {vec_tor <- numeric(10)
    vec_tor[] <- 2},
# Slow because loop is performed in R
  for_loop = {vec_tor <- numeric(10)
    for (in_dex in seq_along(vec_tor))
      vec_tor[in_dex] <- 2},
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
summary(microbenchmark(  # Assign values to vector two different ways
# Fast vectorized assignment loop performed in C using brackets "[]"
  brack_ets = {vec_tor <- numeric(10)
    vec_tor[4:7] <- rnorm(4)},
# Slow because loop is performed in R
  for_loop = {vec_tor <- numeric(10)
    for (in_dex in 4:7)
      vec_tor[in_dex] <- rnorm(1)},
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Define function vectorized automatically
my_fun <- function(in_put, pa_ram) {
  pa_ram*in_put
}  # end my_fun
# "in_put" is vectorized
my_fun(in_put=1:3, pa_ram=2)
# "pa_ram" is vectorized
my_fun(in_put=10, pa_ram=2:4)
# Define vectors of parameters of rnorm()
std_devs <- structure(1:3, names=paste0("sd=", 1:3))
me_ans <- structure(-1:1, names=paste0("mean=", -1:1))
# "sd" argument of rnorm() isn't vectorized
rnorm(1, sd=std_devs)
# "mean" argument of rnorm() isn't vectorized
rnorm(1, mean=me_ans)

# Loop over std_devs produces vector output
set.seed(1121)
sapply(std_devs, function(std_dev) rnorm(n=2, sd=std_dev))
# Same
set.seed(1121)
sapply(std_devs, rnorm, n=2, mean=0)
# Loop over me_ans
set.seed(1121)
sapply(me_ans, function(me_an) rnorm(n=2, mean=me_an))
# Same
set.seed(1121)
sapply(me_ans, rnorm, n=2)

# rnorm() vectorized with respect to "std_dev"
vec_rnorm <- function(n, mean=0, sd=1) {
  if (NROW(sd)==1)
    rnorm(n=n, mean=mean, sd=sd)
  else
    sapply(sd, rnorm, n=n, mean=mean)
}  # end vec_rnorm
set.seed(1121)
vec_rnorm(n=2, sd=std_devs)
# rnorm() vectorized with respect to "mean" and "sd"
vec_rnorm <- Vectorize(FUN=rnorm,
        vectorize.args=c("mean", "sd")
)  # end Vectorize
set.seed(1121)
vec_rnorm(n=2, sd=std_devs)
set.seed(1121)
vec_rnorm(n=2, mean=me_ans)

str(sum)
# na.rm is bound by name
mapply(sum, 6:9, c(5, NA, 3), 2:6, na.rm=TRUE)
str(rnorm)
# mapply vectorizes both arguments "mean" and "sd"
mapply(rnorm, n=5, mean=me_ans, sd=std_devs)
mapply(function(in_put, e_xp) in_put^e_xp,
 1:5, seq(from=1, by=0.2, length.out=5))

# rnorm() vectorized with respect to "mean" and "sd"
vec_rnorm <- function(n, mean=0, sd=1) {
  if (NROW(mean)==1 && NROW(sd)==1)
    rnorm(n=n, mean=mean, sd=sd)
  else
    mapply(rnorm, n=n, mean=mean, sd=sd)
}  # end vec_rnorm
# Call vec_rnorm() on vector of "sd"
vec_rnorm(n=2, sd=std_devs)
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
vec_tor <- runif(1e5)
# Use compiled function
cum_sum <- cumsum(vec_tor)
# Use for loop
cum_sum2 <- vec_tor
for (i in 2:NROW(cum_sum2))
  cum_sum2[i] <- (cum_sum2[i] + cum_sum2[i-1])
# Compare the two methods
all.equal(cum_sum, cum_sum2)
# Microbenchmark the two methods
library(microbenchmark)
summary(microbenchmark(
  cumsum=cumsum(vec_tor),
  loop_alloc={
    cum_sum2 <- vec_tor
    for (i in 2:NROW(cum_sum2))
cum_sum2[i] <- (cum_sum2[i] + cum_sum2[i-1])
  },
  loop_nalloc={
    # Doesn't allocate memory to cum_sum3
    cum_sum3 <- vec_tor[1]
    for (i in 2:NROW(vec_tor))
# This command adds an extra element to cum_sum3
cum_sum3[i] <- (vec_tor[i] + cum_sum3[i-1])
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
n_cores <- detectCores() - 1
# Initialize compute cluster under Windows
clus_ter <- makeCluster(n_cores)
# Perform parallel loop under Windows
paw_s <- parLapply(clus_ter, 1:10, paws)
# Perform parallel loop under Mac-OSX or Linux
paw_s <- mclapply(1:10, paws, mc.cores=n_cores)
library(microbenchmark)  # Load package microbenchmark
# Compare speed of lapply versus parallel computing
summary(microbenchmark(
  standard = lapply(1:10, paws),
  parallel = parLapply(clus_ter, 1:10, paws),
  times=10)
)[, c(1, 4, 5)]

# Compare speed of lapply with parallel computing
iter_ations <- 3:10
compute_times <- sapply(iter_ations,
  function(max_iterations) {
    summary(microbenchmark(
standard = lapply(1:max_iterations, paws),
parallel = parLapply(clus_ter, 1:max_iterations, paws),
times=10))[, 4]
    })  # end sapply
compute_times <- t(compute_times)
colnames(compute_times) <- c("standard", "parallel")
rownames(compute_times) <- iter_ations
# Stop R processes over cluster under Windows
stopCluster(clus_ter)

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
n_cores <- detectCores() - 1
# Initialize compute cluster under Windows
clus_ter <- makeCluster(n_cores)
# Calculate matrix of random data
mat_rix <- matrix(rnorm(1e5), ncol=100)
# Define aggregation function over column of matrix
agg_regate <- function(col_umn) {
  out_put <- 0
  for (in_dex in 1:NROW(col_umn))
    out_put <- out_put + col_umn[in_dex]
  out_put
}  # end agg_regate
# Perform parallel aggregations over columns of matrix
agg_regations <- parCapply(clus_ter, mat_rix, agg_regate)
# Compare speed of apply with parallel computing
summary(microbenchmark(
  ap_ply=apply(mat_rix, MARGIN=2, agg_regate),
  parl_apply=parCapply(clus_ter, mat_rix, agg_regate),
  times=10)
)[, c(1, 4, 5)]
# Stop R processes over cluster under Windows
stopCluster(clus_ter)

library(parallel)  # Load package parallel
# Calculate number of available cores
n_cores <- detectCores() - 1
# Initialize compute cluster under Windows
clus_ter <- makeCluster(n_cores)
ba_se <- 2
# Fails because child processes don't know ba_se:
parLapply(clus_ter, 2:4,
    function(exponent) ba_se^exponent)
# ba_se passed to child via dots ... argument:
parLapply(clus_ter, 2:4,
    function(exponent, ba_se) ba_se^exponent,
    ba_se=ba_se)
# ba_se passed to child via clusterExport:
clusterExport(clus_ter, "ba_se")
parLapply(clus_ter, 2:4,
    function(exponent) ba_se^exponent)
# Fails because child processes don't know zoo::index():
parSapply(clus_ter, c("VTI", "IEF", "DBC"),
    function(sym_bol)
      NROW(index(get(sym_bol, envir=rutils::etf_env))))
# zoo function referenced using "::" in child process:
parSapply(clus_ter, c("VTI", "IEF", "DBC"),
    function(sym_bol)
      NROW(zoo::index(get(sym_bol, envir=rutils::etf_env))))
# Package zoo loaded in child process:
parSapply(clus_ter, c("VTI", "IEF", "DBC"),
    function(sym_bol) {
      stopifnot("package:zoo" %in% search() || require("zoo", quietly=TRUE))
      NROW(index(get(sym_bol, envir=rutils::etf_env)))
    })  # end parSapply
# Stop R processes over cluster under Windows
stopCluster(clus_ter)

library(parallel)  # Load package parallel
# Calculate number of available cores
n_cores <- detectCores() - 1
# Initialize compute cluster under Windows
clus_ter <- makeCluster(n_cores)
# Set seed for cluster under Windows
# Doesn't work: set.seed(1121)
clusterSetRNGStream(clus_ter, 1121)
# Perform parallel loop under Windows
out_put <- parLapply(clus_ter, 1:70, rnorm, n=100)
sum(unlist(out_put))
# Stop R processes over cluster under Windows
stopCluster(clus_ter)
# Perform parallel loop under Mac-OSX or Linux
out_put <- mclapply(1:10, rnorm, mc.cores=n_cores, n=100)

set.seed(1121)  # Reset random number generator
bar_rier <- 20  # Barrier level
n_rows <- 1000  # Number of simulation steps
pa_th <- numeric(n_rows)  # Allocate path vector
pa_th[1] <- 0  # Initialize path
in_dex <- 2  # Initialize simulation index
while ((in_dex <= n_rows) && (pa_th[in_dex - 1] < bar_rier)) {
# Simulate next step
  pa_th[in_dex] <- pa_th[in_dex - 1] + rnorm(1)
  in_dex <- in_dex + 1  # Advance in_dex
}  # end while
# Fill remaining pa_th after it crosses bar_rier
if (in_dex <= n_rows)
  pa_th[in_dex:n_rows] <- pa_th[in_dex - 1]
# Plot the Brownian motion
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
plot(pa_th, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=bar_rier, lwd=3, col="red")
title(main="Brownian Motion Crossing a Barrier Level", line=0.5)

set.seed(1121)  # Reset random number generator
bar_rier <- 20  # Barrier level
n_rows <- 1000  # Number of simulation steps
# Simulate path of Brownian motion
pa_th <- cumsum(rnorm(n_rows))
# Find index when pa_th crosses bar_rier
cro_ss <- which(pa_th > bar_rier)
# Fill remaining pa_th after it crosses bar_rier
if (NROW(cro_ss)>0) {
  pa_th[(cro_ss[1]+1):n_rows] <- pa_th[cro_ss[1]]
}  # end if
# Plot the Brownian motion
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
plot(pa_th, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=bar_rier, lwd=3, col="red")
title(main="Brownian Motion Crossing a Barrier Level", line=0.5)

# Define Brownian motion parameters
sig_ma <- 1.0  # Volatility
dri_ft <- 0.0  # Drift
n_rows <- 1000  # Number of simulation steps
n_simu <- 100  # Number of simulations
# Simulate multiple paths of Brownian motion
set.seed(1121)
path_s <- rnorm(n_simu*n_rows, mean=dri_ft, sd=sig_ma)
path_s <- matrix(path_s, nc=n_simu)
path_s <- matrixStats::colCumsums(path_s)
# Final distribution of paths
mean(path_s[n_rows, ]) ; sd(path_s[n_rows, ])
# Calculate option payout
strik_e <- 50  # Strike price
pay_outs <- (path_s[n_rows, ] - strik_e)
sum(pay_outs[pay_outs > 0])/n_simu
# Calculate probability of crossing a barrier
bar_rier <- 50
cross_ed <- colSums(path_s > bar_rier) > 0
sum(cross_ed)/n_simu

# Plot in window
x11(width=6, height=5)
par(mar=c(4, 3, 2, 2), oma=c(0, 0, 0, 0), mgp=c(2.5, 1, 0))
# Select and plot full range of paths
or_der <- order(path_s[n_rows, ])
in_dex <- or_der[seq(1, 100, 9)]
zoo::plot.zoo(path_s[, in_dex], main="Paths of Brownian Motion",
  xlab="time steps", ylab=NA, plot.type="single")
abline(h=strik_e, col="red", lwd=3)
text(x=(n_rows-60), y=strik_e, labels="strike price", pos=3, cex=1)

set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
n_rows <- 1000
da_ta <- rnorm(n_rows)
# Sample mean - MC estimate
mean(da_ta)
# Sample standard deviation - MC estimate
sd(da_ta)
# Monte Carlo estimate of cumulative probability
pnorm(1)
sum(da_ta < 1)/n_rows
# Monte Carlo estimate of quantile
conf_level <- 0.98
qnorm(conf_level)  # Exact value
cut_off <- conf_level*n_rows
da_ta <- sort(da_ta)
da_ta[cut_off]  # Naive Monte Carlo value
quantile(da_ta, probs=conf_level)
# Analyze the source code of quantile()
stats:::quantile.default
# Microbenchmark quantile
library(microbenchmark)
summary(microbenchmark(
  monte_carlo = da_ta[cut_off],
  quan_tile = quantile(da_ta, probs=conf_level),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# Sample from Standard Normal Distribution
n_rows <- 1000; da_ta <- rnorm(n_rows)
# Sample mean and standard deviation
mean(da_ta); sd(da_ta)
# Bootstrap of sample mean and median
n_boot <- 10000
boot_data <- sapply(1:n_boot, function(x) {
  # Sample from Standard Normal Distribution
  sampl_e <- rnorm(n_rows)
  c(mean=mean(sampl_e), median=median(sampl_e))
})  # end sapply
boot_data[, 1:3]
boot_data <- t(boot_data)
# Standard error from formula
sd(da_ta)/sqrt(n_rows)
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
n_rows <- 1000
# Bootstrap of sample mean and median
n_boot <- 100
boot_data <- sapply(1:n_boot, function(x) median(rnorm(n_rows)))
# Perform vectorized bootstrap
set.seed(1121)  # Reset random number generator
# Calculate matrix of random data
sampl_e <- matrix(rnorm(n_boot*n_rows), ncol=n_boot)
boot_vec <- Rfast::colMedians(sampl_e)
all.equal(boot_data, boot_vec)
# Compare speed of loops with vectorized R code
library(microbenchmark)
summary(microbenchmark(
  loop = sapply(1:n_boot, function(x) median(rnorm(n_rows))),
  cpp = {
    sampl_e <- matrix(rnorm(n_boot*n_rows), ncol=n_boot)
    Rfast::colMedians(sampl_e)
    },
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster under Windows
set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
n_rows <- 1000
# Bootstrap mean and median under Windows
n_boot <- 10000
boot_data <- parLapply(clus_ter, 1:n_boot,
  function(x, da_ta, n_rows) {
  sampl_e <- rnorm(n_rows)
  c(mean=mean(sampl_e), median=median(sampl_e))
  }, da_ta=da_ta, n_rows=n_rows)  # end parLapply
# Bootstrap mean and median under Mac-OSX or Linux
boot_data <- mclapply(1:n_boot,
  function(x) {
  sampl_e <- rnorm(n_rows)
  c(mean=mean(sampl_e), median=median(sampl_e))
  }, mc.cores=n_cores)  # end mclapply
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))
# Standard error from formula
sd(da_ta)/sqrt(n_rows)
stopCluster(clus_ter)  # Stop R processes over cluster under Windows

n_rows <- 1000
da_ta <- rnorm(n_rows)
sd(da_ta); mad(da_ta)
median(abs(da_ta - median(da_ta)))
median(abs(da_ta - median(da_ta)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
n_boot <- 10000
boot_data <- sapply(1:n_boot, function(x) {
  sampl_e <- rnorm(n_rows)
  c(sd=sd(sampl_e), mad=mad(sampl_e))
})  # end sapply
boot_data <- t(boot_data)
# Analyze bootstrapped variance
head(boot_data)
sum(is.na(boot_data))
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster
boot_data <- parLapply(clus_ter, 1:n_boot,
  function(x, da_ta) {
    sampl_e <- rnorm(n_rows)
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  }, da_ta=da_ta)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:n_boot, function(x) {
  sampl_e <- rnorm(n_rows)
  c(sd=sd(sampl_e), mad=mad(sampl_e))
}, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))

# Calculate time series of VTI returns
library(rutils)
re_turns <- rutils::etf_env$re_turns$VTI
re_turns <- na.omit(re_turns)
n_rows <- NROW(re_turns)
# Sample from VTI returns
sampl_e <- re_turns[sample.int(n_rows, replace=TRUE)]
c(sd=sd(sampl_e), mad=mad(sampl_e))
# sample.int() is a little faster than sample()
library(microbenchmark)
summary(microbenchmark(
  sample.int = sample.int(1e3),
  sample = sample(1e3),
  times=10))[, c(1, 4, 5)]

# Sample from time series of VTI returns
library(rutils)
re_turns <- rutils::etf_env$re_turns$VTI
re_turns <- na.omit(re_turns)
n_rows <- NROW(re_turns)
# Bootstrap sd and MAD under Windows
library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster under Windows
clusterSetRNGStream(clus_ter, 1121)  # Reset random number generator in all cores
n_boot <- 10000
boot_data <- parLapply(clus_ter, 1:n_boot,
  function(x, re_turns, n_rows) {
    sampl_e <- re_turns[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  }, re_turns=re_turns, n_rows=n_rows)  # end parLapply
# Bootstrap sd and MAD under Mac-OSX or Linux
boot_data <- mclapply(1:n_boot, function(x) {
    sampl_e <- re_turns[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster under Windows
boot_data <- rutils::do_call(rbind, boot_data)
# Standard error assuming normal distribution of returns
sd(re_turns)/sqrt(n_boot)
# Means and standard errors from bootstrap
std_errors <- apply(boot_data, MARGIN=2,
  function(x) c(mean=mean(x), std_error=sd(x)))
std_errors
# Relative standard errors
std_errors[2, ]/std_errors[1, ]

# Calculate percentage returns from VTI prices
library(rutils)
price_s <- quantmod::Cl(rutils::etf_env$VTI)
star_t <- as.numeric(price_s[1, ])
re_turns <- rutils::diff_it(log(price_s))
class(re_turns); head(re_turns)
sum(is.na(re_turns))
n_rows <- NROW(re_turns)
# Define barrier level with respect to price_s
bar_rier <- 1.5*max(price_s)
# Calculate single bootstrap sample
sampl_e <- re_turns[sample.int(n_rows, replace=TRUE)]
# Calculate prices from percentage returns
sampl_e <- star_t*exp(cumsum(sampl_e))
# Calculate if prices crossed barrier
sum(sampl_e > bar_rier) > 0

library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster under Windows
# Perform parallel bootstrap under Windows
clusterSetRNGStream(clus_ter, 1121)  # Reset random number generator in all cores
clusterExport(clus_ter, c("star_t", "bar_rier"))
n_boot <- 10000
boot_data <- parLapply(clus_ter, 1:n_boot,
  function(x, re_turns, n_rows) {
    sampl_e <- re_turns[sample.int(n_rows, replace=TRUE)]
    # Calculate prices from percentage returns
    sampl_e <- star_t*exp(cumsum(sampl_e))
    # Calculate if prices crossed barrier
    sum(sampl_e > bar_rier) > 0
  }, re_turns=re_turns, n_rows=n_rows)  # end parLapply
# Perform parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:n_boot, function(x) {
    sampl_e <- re_turns[sample.int(n_rows, replace=TRUE)]
    # Calculate prices from percentage returns
    sampl_e <- star_t*exp(cumsum(sampl_e))
    # Calculate if prices crossed barrier
    sum(sampl_e > bar_rier) > 0
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster under Windows
boot_data <- rutils::do_call(rbind, boot_data)
# Calculate frequency of crossing barrier
sum(boot_data)/n_boot

# Calculate percentage returns from VTI prices
library(rutils)
oh_lc <- rutils::etf_env$VTI
price_s <- as.numeric(oh_lc[, 4])
star_t <- price_s[1]
re_turns <- rutils::diff_it(log(price_s))
n_rows <- NROW(re_turns)
# Calculate difference of OHLC price columns
ohlc_diff <- oh_lc[, 1:3] - price_s
class(re_turns); head(re_turns)
# Calculate bootstrap prices from percentage returns
da_ta <- sample.int(n_rows, replace=TRUE)
boot_prices <- star_t*exp(cumsum(re_turns[da_ta]))
boot_ohlc <- ohlc_diff + boot_prices
boot_ohlc <- cbind(boot_ohlc, boot_prices)
# Define barrier level with respect to price_s
bar_rier <- 1.5*max(price_s)
# Calculate if High bootstrapped prices crossed barrier level
sum(boot_ohlc[, 2] > bar_rier) > 0

library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster under Windows
# Perform parallel bootstrap under Windows
clusterSetRNGStream(clus_ter, 1121)  # Reset random number generator in all cores
clusterExport(clus_ter, c("star_t", "bar_rier", "ohlc_diff"))
n_boot <- 10000
boot_data <- parLapply(clus_ter, 1:n_boot,
  function(x, re_turns, n_rows) {
    # Calculate OHLC prices from percentage returns
    da_ta <- sample.int(n_rows, replace=TRUE)
    boot_prices <- star_t*exp(cumsum(re_turns[da_ta]))
    boot_ohlc <- ohlc_diff + boot_prices
    boot_ohlc <- cbind(boot_ohlc, boot_prices)
    # Calculate statistic
    sum(boot_ohlc[, 2] > bar_rier) > 0
  }, re_turns=re_turns, n_rows=n_rows)  # end parLapply
# Perform parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:n_boot, function(x) {
    # Calculate OHLC prices from percentage returns
    da_ta <- sample.int(n_rows, replace=TRUE)
    boot_prices <- star_t*exp(cumsum(re_turns[da_ta]))
    boot_ohlc <- ohlc_diff + boot_prices
    boot_ohlc <- cbind(boot_ohlc, boot_prices)
    # Calculate statistic
    sum(boot_ohlc[, 2] > bar_rier) > 0
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster under Windows
boot_data <- rutils::do_call(rbind, boot_data)
# Calculate frequency of crossing barrier
sum(boot_data)/n_boot

# Initialize random number generator
set.seed(1121)
# Define explanatory and response variables
predic_tor <- rnorm(100, mean=2)
noise <- rnorm(100)
res_ponse <- (-3 + predic_tor + noise)
de_sign <- cbind(res_ponse, predic_tor)
# Calculate alpha and beta regression coefficients
be_ta <- cov(de_sign[, 1], de_sign[, 2])/var(de_sign[, 2])
al_pha <- mean(de_sign[, 1]) - be_ta*mean(de_sign[, 2])
x11(width=6, height=5)
plot(res_ponse ~ predic_tor, data=de_sign)
abline(a=al_pha, b=be_ta, lwd=3, col="blue")
# Bootstrap of beta regression coefficient
n_boot <- 100
boot_data <- sapply(1:n_boot, function(x) {
  sampl_e <- sample.int(NROW(de_sign), replace=TRUE)
  de_sign <- de_sign[sampl_e, ]
  cov(de_sign[, 1], de_sign[, 2])/var(de_sign[, 2])
})  # end sapply

x11(width=6, height=5)
par(oma=c(1, 2, 1, 0), mgp=c(2, 1, 0), mar=c(1, 1, 1, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
# Mean and standard error of beta regression coefficient
c(mean=mean(boot_data), std_error=sd(boot_data))
# Plot density of bootstrapped beta coefficients
plot(density(boot_data), lwd=2, xlab="Regression slopes",
     main="Bootstrapped Regression Slopes")
# Add line for expected value
abline(v=mean(boot_data), lwd=2, col="red")
text(x=mean(boot_data)-0.01, y=1.0, labels="expected value",
     lwd=2, srt=90, pos=3)

library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster under Windows
# Bootstrap of regression under Windows
boot_data <- parLapply(clus_ter, 1:1000,
  function(x, de_sign) {
    sampl_e <- sample.int(NROW(de_sign), replace=TRUE)
    de_sign <- de_sign[sampl_e, ]
    cov(de_sign[, 1], de_sign[, 2])/var(de_sign[, 2])
  }, de_sign=de_sign)  # end parLapply
# Bootstrap of regression under Mac-OSX or Linux
boot_data <- mclapply(1:1000,
  function(x) {
    sampl_e <- sample.int(NROW(de_sign), replace=TRUE)
    de_sign <- de_sign[sampl_e, ]
    cov(de_sign[, 1], de_sign[, 2])/var(de_sign[, 2])
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster under Windows

# Collapse the bootstrap list into a vector
class(boot_data)
boot_data <- unlist(boot_data)
# Mean and standard error of beta regression coefficient
c(mean=mean(boot_data), std_error=sd(boot_data))
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
n_rows <- 1000
da_ta <- rnorm(n_rows)
# Estimate the 95% quantile
n_boot <- 10000
boot_data <- sapply(1:n_boot, function(x) {
  sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
  quantile(sampl_e, 0.95)
})  # end sapply
sd(boot_data)
# Estimate the 95% quantile using antithetic sampling
boot_data <- sapply(1:n_boot, function(x) {
  sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
  quantile(c(sampl_e, -sampl_e), 0.95)
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
n_rows <- 1000
da_ta <- rnorm(n_rows)
# Cumulative probability from formula
quan_tile <- (-2)
pnorm(quan_tile)
integrate(dnorm, lower=-Inf, upper=quan_tile)
# Cumulative probability from Naive Monte Carlo
sum(da_ta < quan_tile)/n_rows
# Generate importance sample
lamb_da <- (-1.5)  # Tilt parameter
data_tilt <- da_ta + lamb_da  # Tilt the random numbers
# Cumulative probability from importance sample
sum(data_tilt < quan_tile)/n_rows
weight_s <- exp(-lamb_da*data_tilt + lamb_da^2/2)
sum((data_tilt < quan_tile)*weight_s)/n_rows
# Bootstrap of standard errors of cumulative probability
n_boot <- 1000
boot_data <- sapply(1:n_boot, function(x) {
  da_ta <- rnorm(n_rows)
  na_ive <- sum(da_ta < quan_tile)/n_rows
  da_ta <- (da_ta + lamb_da)
  weight_s <- exp(-lamb_da*da_ta + lamb_da^2/2)
  im_port <- sum((da_ta < quan_tile)*weight_s)/n_rows
  c(naive_mc=na_ive, importance=im_port)
}) # end sapply
apply(boot_data, MARGIN=1,
  function(x) c(mean=mean(x), sd=sd(x)))

# Quantile from Naive Monte Carlo
conf_level <- 0.02
qnorm(conf_level)  # Exact value
da_ta <- sort(da_ta)
cut_off <- n_rows*conf_level
da_ta[cut_off]  # Naive Monte Carlo value
# Importance sample weights
data_tilt <- da_ta + lamb_da  # Tilt the random numbers
weight_s <- exp(-lamb_da*data_tilt + lamb_da^2/2)
# Cumulative probabilities using importance sample
cum_prob <- cumsum(weight_s)/n_rows
# Quantile from importance sample
data_tilt[findInterval(conf_level, cum_prob)]
# Bootstrap of standard errors of quantile
n_boot <- 1000
boot_data <- sapply(1:n_boot, function(x) {
  da_ta <- sort(rnorm(n_rows))
  na_ive <- da_ta[cut_off]
  data_tilt <- da_ta + lamb_da
  weight_s <- exp(-lamb_da*data_tilt + lamb_da^2/2)
  cum_prob <- cumsum(weight_s)/n_rows
  im_port <- data_tilt[findInterval(conf_level, cum_prob)]
  c(naive_mc=na_ive, importance=im_port)
}) # end sapply
apply(boot_data, MARGIN=1,
  function(x) c(mean=mean(x), sd=sd(x)))

# CVaR from Naive Monte Carlo
va_r <- da_ta[cut_off]
sum((da_ta < va_r)*da_ta)/sum((da_ta < va_r))
# CVaR from importance sample
va_r <- data_tilt[findInterval(conf_level, cum_prob)]
sum((data_tilt < va_r)*data_tilt*weight_s)/sum((data_tilt < va_r)*weight_s)
# CVaR from integration
integrate(function(x) x*dnorm(x), low=-Inf, up=va_r)$value/pnorm(va_r)
# Bootstrap of standard errors of expected value
n_boot <- 1000
boot_data <- sapply(1:n_boot, function(x) {
  da_ta <- sort(rnorm(n_rows))
  va_r <- da_ta[cut_off]
  na_ive <- sum((da_ta < va_r)*da_ta)/sum((da_ta < va_r))
  data_tilt <- da_ta + lamb_da
  weight_s <- exp(-lamb_da*data_tilt + lamb_da^2/2)
  cum_prob <- cumsum(weight_s)/n_rows
  va_r <- data_tilt[findInterval(conf_level, cum_prob)]
  im_port <- sum((data_tilt < va_r)*data_tilt*weight_s)/sum((data_tilt < va_r)*weight_s)
  c(naive_mc=na_ive, importance=im_port)
}) # end sapply
apply(boot_data, MARGIN=1,
  function(x) c(mean=mean(x), sd=sd(x)))

# Calculate matrix of random data
set.seed(1121) # Reset random number generator
n_rows <- 1000; n_boot <- 100
da_ta <- matrix(rnorm(n_boot*n_rows), ncol=n_boot)
da_ta <- Rfast::sort_mat(da_ta)  # Sort the columns
# Calculate vector of quantiles for tilt parameter
conf_level <- 0.02; cut_off <- conf_level*n_rows
calc_quant <- function(lamb_da) {
  data_tilt <- da_ta + lamb_da  # Tilt the random numbers
  weight_s <- exp(-lamb_da*data_tilt + lamb_da^2/2)
  # Calculate quantiles for columns
  sapply(1:n_boot, function(boo_t) {
    cum_prob <- cumsum(weight_s[, boo_t])/n_rows
    data_tilt[findInterval(conf_level, cum_prob), boo_t]
  })  # end sapply
}  # end calc_quant

# Define vector of tilt parameters
lambda_s <- seq(-3.0, -1.2, by=0.2)
# Calculate vector of quantiles for tilt parameters
quantile_s <- sapply(lambda_s, calc_quant)
# Calculate standard deviations of quantiles for tilt parameters
std_devs <- apply(quantile_s, MARGIN=2, sd)
# Calculate the optimal tilt parameter
lambda_s[which.min(std_devs)]
# Plot the standard deviations
x11(width=6, height=5)
plot(x=lambda_s, y=std_devs,
     main="Standard Deviations of Simulated Quantiles",
     xlab="tilt parameter", ylab="standard deviation",
     type="l", col="blue", lwd=2)

# Binomial sample
n_rows <- 1000
pro_b <- 0.1
da_ta <- rbinom(n=n_rows, size=1, pro_b)
head(da_ta, 33)
fre_q <- sum(da_ta)/n_rows
# Tilted binomial sample
lamb_da <- 5
p_tilted <- lamb_da*pro_b/(1 + pro_b*(lamb_da - 1))
weigh_t <- (1 + pro_b*(lamb_da - 1))/lamb_da
da_ta <- rbinom(n=n_rows, size=1, p_tilted)
head(da_ta, 33)
weigh_t*sum(da_ta)/n_rows
# Bootstrap of standard errors
n_boot <- 1000
boot_data <- sapply(1:n_boot, function(x) {
  c(naive_mc=sum(rbinom(n=n_rows, size=1, pro_b))/n_rows,
    importance=weigh_t*sum(rbinom(n=n_rows, size=1, p_tilted))/n_rows)
}) # end sapply
apply(boot_data, MARGIN=1,
  function(x) c(mean=mean(x), sd=sd(x)))

# Define Brownian motion parameters
sig_ma <- 1.0  # Volatility
dri_ft <- 0.0  # Drift
n_rows <- 100  # Number of simulation steps
n_simu <- 10000  # Number of simulations
# Calculate matrix of normal variables
set.seed(1121)
da_ta <- rnorm(n_simu*n_rows, mean=dri_ft, sd=sig_ma)
da_ta <- matrix(da_ta, nc=n_simu)
# Simulate paths of Brownian motion
path_s <- matrixStats::colCumsums(da_ta)
# Tilt the da_ta
lamb_da <- 0.04  # Tilt parameter
data_tilt <- da_ta + lamb_da  # Tilt the random numbers
paths_tilt <- matrixStats::colCumsums(data_tilt)
# Calculate path weights
weight_s <- exp(-lamb_da*data_tilt + lamb_da^2/2)
path_weights <- matrixStats::colProds(weight_s)
# Or
path_weights <- exp(-lamb_da*colSums(data_tilt) + n_rows*lamb_da^2/2)
# Calculate option payout using standard MC
strik_e <- 10  # Strike price
pay_outs <- (path_s[n_rows, ] - strik_e)
sum(pay_outs[pay_outs > 0])/n_simu
# Calculate option payout using importance sampling
pay_outs <- (paths_tilt[n_rows, ] - strik_e)
sum((path_weights*pay_outs)[pay_outs > 0])/n_simu
# Calculate crossing probability using standard MC
bar_rier <- 10
cross_ed <- colSums(path_s > bar_rier) > 0
sum(cross_ed)/n_simu
# Calculate crossing probability using importance sampling
cross_ed <- colSums(paths_tilt > bar_rier) > 0
sum(path_weights*cross_ed)/n_simu

# Load S&P500 constituent stock prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
price_s <- eapply(sp500_env, quantmod::Cl)
price_s <- rutils::do_call(cbind, price_s)
# Carry forward non-NA prices
price_s <- zoo::na.locf(price_s, na.rm=FALSE)
# Drop ".Close" from column names
colnames(price_s[, 1:4])
colnames(price_s) <- rutils::get_name(colnames(price_s))
# Or
# colnames(price_s) <- do.call(rbind,
#   strsplit(colnames(price_s), split="[.]"))[, 1]
# Calculate percentage returns of the S&P500 constituent stocks
# re_turns <- xts::diff.xts(log(price_s))
re_turns <- xts::diff.xts(price_s)/
  rutils::lag_it(price_s, pad_zeros=FALSE)
set.seed(1121)
sam_ple <- sample(NCOL(re_turns), s=100, replace=FALSE)
prices_100 <- price_s[, sam_ple]
returns_100 <- re_turns[, sam_ple]
save(price_s, prices_100,
  file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
save(re_turns, returns_100,
  file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")

# Calculate number of constituents without prices
da_ta <- rowSums(is.na(price_s))
da_ta <- xts::xts(da_ta, order.by=index(price_s))
dygraphs::dygraph(da_ta, main="Number of S&P 500 Constituents Without Prices") %>%
  dyOptions(colors="blue", strokeWidth=2) %>%
  dyAxis("y", valueRange=c(0, 300))

# Calculate price weighted index of constituent
n_cols <- NCOL(price_s)
in_dex <- xts(rowSums(price_s)/n_cols, index(price_s))
colnames(in_dex) <- "index"
# Combine index with VTI
da_ta <- cbind(in_dex[index(etf_env$VTI)], etf_env$VTI[, 4])
col_names <- c("index", "VTI")
colnames(da_ta) <- col_names
# Plot index with VTI
dygraphs::dygraph(da_ta,
  main="S&P 500 Price-weighted Index and VTI") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red") %>%
  dySeries(name=col_names[2], axis="y2", col="blue")

# Select ETF symbols for asset allocation
sym_bols <- c("VTI", "VEU", "EEM", "XLY", "XLP", "XLE", "XLF",
  "XLV", "XLI", "XLB", "XLK", "XLU", "VYM", "IVW", "IWB", "IWD",
  "IWF", "IEF", "TLT", "VNQ", "DBC", "GLD", "USO", "VXX", "SVXY",
  "MTUM", "IVE", "VLUE", "QUAL", "VTV", "USMV")
# Read etf database into data frame
etf_list <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/etf_list.csv")
rownames(etf_list) <- etf_list$Symbol
# Select from etf_list only those ETF's in sym_bols
etf_list <- etf_list[sym_bols, ]
# Shorten names
etf_names <- sapply(etf_list$Name, function(name) {
  name_split <- strsplit(name, split=" ")[[1]]
  name_split <- name_split[c(-1, -NROW(name_split))]
  name_match <- match("Select", name_split)
  if (!is.na(name_match))
    name_split <- name_split[-name_match]
  paste(name_split, collapse=" ")
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
re_turns <- rutils::etf_env$re_turns$VTI
re_turns <- drop(coredata(na.omit(re_turns)))
n_rows <- NROW(re_turns)
# Mean and standard deviation of returns
c(mean(re_turns), sd(re_turns))
# Calculate the smoothing bandwidth as the MAD of returns 10 points apart
re_turns <- sort(re_turns)
b_w <- 10*mad(rutils::diff_it(re_turns, lagg=10))
# Calculate the kernel density
den_sity <- sapply(1:n_rows, function(i_d) {
  sum(dnorm(re_turns-re_turns[i_d], sd=b_w))
})  # end sapply
ma_d <- mad(re_turns)
plot(re_turns, den_sity, xlim=c(-5*ma_d, 5*ma_d),
     t="l", col="blue", lwd=3,
     xlab="returns", ylab="density",
     main="Density of VTI Returns")
# Calculate the kernel density using density()
den_sity <- density(re_turns, bw=b_w)
NROW(den_sity$y)
x11(width=6, height=5)
plot(den_sity, xlim=c(-5*ma_d, 5*ma_d),
     xlab="returns", ylab="density",
     col="blue", lwd=3, main="Density of VTI Returns")
# Interpolate the den_sity vector into re_turns
den_sity <- approx(den_sity$x, den_sity$y, xout=re_turns)
all.equal(den_sity$x, re_turns)
plot(den_sity, xlim=c(-5*ma_d, 5*ma_d),
     xlab="returns", ylab="density",
     t="l", col="blue", lwd=3,
     main="Density of VTI Returns")

# Plot histogram
histo_gram <- hist(re_turns, breaks=100, freq=FALSE,
  xlim=c(-5*ma_d, 5*ma_d), xlab="", ylab="",
  main="VTI Return Distribution")
# Draw kernel density of histogram
lines(den_sity, col="red", lwd=2)
# Add density of normal distribution
curve(expr=dnorm(x, mean=mean(re_turns), sd=sd(re_turns)),
add=TRUE, lwd=2, col="blue")
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("VTI", "Normal"), bty="n",
 lwd=6, bg="white", col=c("red", "blue"))

# Create normal Q-Q plot
qqnorm(re_turns, ylim=c(-0.1, 0.1), main="VTI Q-Q Plot",
 xlab="Normal Quantiles")
# Fit a line to the normal quantiles
qqline(re_turns, col="red", lwd=2)
# Perform Shapiro-Wilk test
shapiro.test(as.numeric(re_turns))

# Boxplot method for formula
boxplot(formula=mpg ~ cyl, data=mtcars,
  main="Mileage by number of cylinders",
  xlab="Cylinders", ylab="Miles per gallon")
# Boxplot method for data frame of EuStockMarkets percentage returns
boxplot(x=diff(log(EuStockMarkets)))

# VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
# Number of observations
n_rows <- NROW(re_turns)
# Mean of VTI returns
mean_rets <- mean(re_turns)
# Standard deviation of VTI returns
sd_rets <- sd(re_turns)
# Skewness of VTI returns
n_rows/((n_rows-1)*(n_rows-2))*
  sum(((re_turns - mean_rets)/sd_rets)^3)
# Kurtosis of VTI returns
n_rows*(n_rows+1)/((n_rows-1)^3)*
  sum(((re_turns - mean_rets)/sd_rets)^4)
# Random normal returns
re_turns <- rnorm(n_rows, sd=sd_rets)
# Mean and standard deviation of random normal returns
mean_rets <- mean(re_turns)
sd_rets <- sd(re_turns)
# Skewness of random normal returns
n_rows/((n_rows-1)*(n_rows-2))*
  sum(((re_turns - mean_rets)/sd_rets)^3)
# Kurtosis of random normal returns
n_rows*(n_rows+1)/((n_rows-1)^3)*
  sum(((re_turns - mean_rets)/sd_rets)^4)

# calc_skew() calculates skew of returns
calc_skew <- function(re_turns) {
  re_turns <- na.omit(re_turns)
  sum(((re_turns - mean(re_turns))/sd(re_turns))^3)/NROW(re_turns)
}  # end calc_skew
# calc_kurt() calculates kurtosis of returns
calc_kurt <- function(re_turns) {
  re_turns <- na.omit(re_turns)
  sum(((re_turns - mean(re_turns))/sd(re_turns))^4)/NROW(re_turns)
}  # end calc_kurt
# Calculate skew and kurtosis of VTI returns
calc_skew(re_turns)
calc_kurt(re_turns)
# calc_mom() calculates the moments of returns
calc_mom <- function(re_turns, mo_ment=3) {
  re_turns <- na.omit(re_turns)
  sum(((re_turns - mean(re_turns))/sd(re_turns))^mo_ment)/NROW(re_turns)
}  # end calc_mom
# Calculate skew and kurtosis of VTI returns
calc_mom(re_turns, mo_ment=3)
calc_mom(re_turns, mo_ment=4)

set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
n_rows <- 1000
da_ta <- rnorm(n_rows)
# Sample mean
mean(da_ta)
# Sample standard deviation
sd(da_ta)
# Standard error of sample mean
sd(da_ta)/sqrt(n_rows)

x_var <- seq(-5, 7, length=100)
y_var <- dnorm(x_var, mean=1.0, sd=2.0)
plot(x_var, y_var, type="l", lty="solid", xlab="", ylab="")
title(main="Normal Density Function", line=0.5)
star_t <- 3; fin_ish <- 5  # Set lower and upper bounds
# Plot polygon area
are_a <- ((x_var >= star_t) & (x_var <= fin_ish))
polygon(c(star_t, x_var[are_a], fin_ish),
  c(-1, y_var[are_a], -1), col="red")

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
sig_mas <- c(0.5, 1, 1.5, 2)  # Sigma values
# Create plot colors
col_ors <- c("red", "black", "blue", "green")
# Create legend labels
lab_els <- paste("sigma", sig_mas, sep="=")
for (in_dex in 1:4) {  # Plot four curves
  curve(expr=dnorm(x, sd=sig_mas[in_dex]),
  xlim=c(-4, 4), xlab="", ylab="", lwd=2,
  col=col_ors[in_dex], add=as.logical(in_dex-1))
}  # end for
# Add title
title(main="Normal Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, title="Sigmas",
 lab_els, cex=0.8, lwd=2, lty=1, bty="n", col=col_ors)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
deg_free <- c(3, 6, 9)  # Df values
col_ors <- c("black", "red", "blue", "green")
lab_els <- c("normal", paste("df", deg_free, sep="="))
# Plot a Normal probability distribution
curve(expr=dnorm, xlim=c(-4, 4), xlab="", ylab="", lwd=2)
for (in_dex in 1:3) {  # Plot three t-distributions
  curve(expr=dt(x, df=deg_free[in_dex]), xlab="", ylab="",
lwd=2, col=col_ors[in_dex+1], add=TRUE)
}  # end for

# Add title
title(main="t-distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n",
       title="Degrees\n of freedom", lab_els,
       cex=0.8, lwd=6, lty=1, col=col_ors)

# Mixture of two normal distributions with sd=1 and sd=2
n_rows <- 1e5
re_turns <- c(rnorm(n_rows/2), 2*rnorm(n_rows/2))
re_turns <- (re_turns-mean(re_turns))/sd(re_turns)
# Kurtosis of normal
calc_kurt(rnorm(n_rows))
# Kurtosis of mixture
calc_kurt(re_turns)
# Or
n_rows*sum(re_turns^4)/(n_rows-1)^2

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot the distributions
plot(density(re_turns), xlab="", ylab="",
  main="Mixture of Normal Returns",
  xlim=c(-3, 3), type="l", lwd=3, col="red")
curve(expr=dnorm, lwd=2, col="blue", add=TRUE)
curve(expr=dt(x, df=3), lwd=2, col="green", add=TRUE)
# Add legend
legend("topright", inset=0.05, lty=1, lwd=6, bty="n",
  legend=c("Mixture", "Normal", "t-distribution"),
  col=c("red", "blue", "green"))

# Objective function is log-likelihood
likeli_hood <- function(pa_r, free_dom, da_ta) {
  sum(
    -log(gamma((free_dom+1)/2) /
      (sqrt(pi*free_dom) * gamma(free_dom/2))) +
    log(pa_r[2]) +
    (free_dom+1)/2 * log(1 + ((da_ta - pa_r[1])/
                    pa_r[2])^2/free_dom))
}  # end likeli_hood
# Demonstrate equivalence with log(dt())
likeli_hood(c(1, 0.5), 2, 2:5)
-sum(log(dt(x=(2:5-1)/0.5, df=2)/0.5))
# Simpler objective function
likeli_hood <- function(pa_r, free_dom, da_ta) {
  -sum(log(dt(x=(da_ta-pa_r[1])/pa_r[2],
      df=free_dom)/pa_r[2]))
}  # end likeli_hood

# VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
# Initial parameters
par_init <- c(mean=0, scale=0.01)
# Fit distribution using optim()
optim_fit <- optim(par=par_init,
  fn=likeli_hood, # Log-likelihood function
  da_ta=re_turns,
  free_dom=2, # Degrees of freedom
  method="L-BFGS-B", # quasi-Newton method
  upper=c(1, 0.1), # upper constraint
  lower=c(-1, 1e-7)) # Lower constraint
# optimal parameters
lo_cation <- optim_fit$par["mean"]
scal_e <- optim_fit$par["scale"]
# Fit VTI returns using MASS::fitdistr()
optim_fit <- MASS::fitdistr(re_turns,
  densfun="t", df=2)
optim_fit$estimate
optim_fit$sd
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]
summary(optim_fit)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot histogram of VTI returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=100, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE, main="VTI Returns Histogram")
lines(density(re_turns, adjust=1.5), lwd=3, col="blue")
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(re_turns),
  sd=sd(re_turns)), add=TRUE, lwd=3, col="green")
# Plot t-distribution function
curve(expr=dt((x-lo_cation)/scal_e, df=2)/scal_e,
type="l", lwd=3, col="red", add=TRUE)
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("density", "t-distr", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot histogram of VTI returns
histo_gram <- hist(re_turns, breaks=100, plot=FALSE)
plot(histo_gram, xlab="returns", ylab="frequency",
     col="lightgrey", freq=FALSE, main="VTI Left Tail Returns Histogram",
     xlim=c(min(re_turns), -0.02),
     ylim=c(0.0, histo_gram$density[findInterval(-0.02, histo_gram$breaks)]))
lines(density(re_turns, adjust=1.5), lwd=4, col="blue")
# Plot t-distribution function
curve(expr=dt((x-lo_cation)/scal_e, df=2)/scal_e, type="l", lwd=4, col="red", add=TRUE)
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(re_turns), sd=sd(re_turns)), add=TRUE, lwd=4, col="green")
# Add legend
legend("topleft", inset=0.05, bty="n",
  leg=c("density", "t-distr", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))

# Calculate VTI returns and trading volumes
oh_lc <- rutils::etf_env$VTI
clos_e <- drop(coredata(quantmod::Cl(oh_lc)))
re_turns <- rutils::diff_it(log(clos_e))
vol_ume <- coredata(quantmod::Vo(oh_lc))
# Calculate rolling variance
look_back <- 121
vari_ance <- HighFreq::roll_var_ohlc(log(oh_lc), method="close", look_back=look_back, scale=FALSE)
vari_ance[1:look_back, ] <- vari_ance[look_back+1, ]
# Calculate rolling average volume
volume_roll <- HighFreq::roll_vec(vol_ume, look_back=look_back)/look_back
# dygraph plot of VTI variance and trading volumes
da_ta <- xts::xts(cbind(vari_ance, volume_roll), index(oh_lc))
col_names <- c("variance", "volume")
colnames(da_ta) <- col_names
dygraphs::dygraph(da_ta, main="VTI Variance and Trading Volumes") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], strokeWidth=2, axis="y", col="blue") %>%
  dySeries(name=col_names[2], strokeWidth=2, axis="y2", col="red")

# Scale returns using volume (volume clock)
rets_scaled <- ifelse(vol_ume > 0, sqrt(volume_roll)*re_turns/sqrt(vol_ume), 0)
rets_scaled <- sd(re_turns)*rets_scaled/sd(rets_scaled)
# rets_scaled <- ifelse(vol_ume > 1e4, re_turns/vol_ume, 0)
# Calculate moments of scaled returns
n_rows <- NROW(re_turns)
sapply(list(re_turns=re_turns, rets_scaled=rets_scaled),
  function(rets) {sapply(c(skew=3, kurt=4),
     function(x) sum((rets/sd(rets))^x)/n_rows)
})  # end sapply

# x11(width=6, height=5)
dev.new(width=6, height=5, noRStudioGD=TRUE)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot densities of SPY returns
ma_d <- mad(re_turns)
# b_w <- mad(rutils::diff_it(re_turns))
plot(density(re_turns, bw=ma_d/10), xlim=c(-5*ma_d, 5*ma_d),
     lwd=3, mgp=c(2, 1, 0), col="blue",
     xlab="returns (standardized)", ylab="frequency",
     main="Density of Volume-scaled VTI Returns")
lines(density(rets_scaled, bw=ma_d/10), lwd=3, col="red")
curve(expr=dnorm(x, mean=mean(re_turns), sd=sd(re_turns)),
add=TRUE, lwd=3, col="green")
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("unscaled", "scaled", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))
quartz.save("figure/vti_scaled.png", type="png", width=6, height=5)

# Calculate VTI percentage returns
library(rutils)
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
# Reset output digits
dig_its <- options(digits=5)
# Shapiro-Wilk test for normal distribution
shapiro.test(rnorm(NROW(re_turns)))
# Shapiro-Wilk test for VTI returns
shapiro.test(as.numeric(re_turns))
# Shapiro-Wilk test for uniform distribution
shapiro.test(runif(NROW(re_turns)))
# Restore output digits
options(digits=dig_its$digits)

library(tseries)  # Load package tseries
# Jarque-Bera test for normal distribution
jarque.bera.test(rnorm(NROW(re_turns)))
# Jarque-Bera test for VTI returns
jarque.bera.test(re_turns)
# Jarque-Bera test for uniform distribution
jarque.bera.test(runif(NROW(re_turns)))

# KS test for normal distribution
ks.test(rnorm(100), pnorm)
# KS test for uniform distribution
ks.test(runif(100), pnorm)
# KS test for two similar normal distributions
ks.test(rnorm(100), rnorm(100, mean=0.1))
# KS test for two different normal distributions
ks.test(rnorm(100), rnorm(100, mean=1.0))
# Fit t-dist into VTI returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
optim_fit <- MASS::fitdistr(re_turns, densfun="t", df=2)
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]
# Perform Kolmogorov-Smirnov test on VTI returns
da_ta <- lo_cation + scal_e*rt(NROW(re_turns), df=2)
ks.test(as.numeric(re_turns), da_ta)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Degrees of freedom
deg_free <- c(2, 5, 8, 11)
# Plot four curves in loop
col_ors <- c("red", "black", "blue", "green")
for (in_dex in 1:4) {
  curve(expr=dchisq(x, df=deg_free[in_dex]),
  xlim=c(0, 20), ylim=c(0, 0.3),
  xlab="", ylab="", col=col_ors[in_dex],
  lwd=2, add=as.logical(in_dex-1))
}  # end for

# Add title
title(main="Chi-squared Distributions", line=0.5)
# Add legend
lab_els <- paste("df", deg_free, sep="=")
legend("topright", inset=0.05, bty="n",
       title="Degrees of freedom", lab_els,
       cex=0.8, lwd=6, lty=1, col=col_ors)

# Observed frequencies from random normal data
histo_gram <- hist(rnorm(1e3, mean=0), breaks=100, plot=FALSE)
freq_o <- histo_gram$counts
# Theoretical frequencies
freq_t <- rutils::diff_it(pnorm(histo_gram$breaks))
# Perform Chi-squared test for normal data
chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
# Return p-value
chisq_test <- chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
chisq_test$p.value
# Observed frequencies from shifted normal data
histo_gram <- hist(rnorm(1e3, mean=2), breaks=100, plot=FALSE)
freq_o <- histo_gram$counts/sum(histo_gram$counts)
# Theoretical frequencies
freq_t <- rutils::diff_it(pnorm(histo_gram$breaks))
# Perform Chi-squared test for shifted normal data
chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
# Calculate histogram of VTI returns
histo_gram <- hist(re_turns, breaks=100, plot=FALSE)
freq_o <- histo_gram$counts
# Calculate cumulative probabilities and then difference them
freq_t <- pt((histo_gram$breaks-lo_cation)/scal_e, df=2)
freq_t <- rutils::diff_it(freq_t)
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
re_turns <- rutils::etf_env$re_turns[, c("VTI", "DBC", "IEF")]
re_turns <- na.omit(re_turns)
# Plot cumulative ETF returns
x11(width=6, height=5)
chart.CumReturns(re_turns, lwd=2, ylab="",
  legend.loc="topleft", main="ETF Cumulative Returns")

re_turns <- rutils::etf_env$re_turns$VTI
re_turns <- na.omit(re_turns)
x11(width=6, height=5)
chart.Histogram(re_turns, xlim=c(-0.04, 0.04),
  colorset = c("lightgray", "red", "blue"), lwd=3,
  main=paste("Distribution of", colnames(re_turns), "Returns"),
  methods = c("add.density", "add.normal"))
legend("topright", inset=0.05, bty="n",
 leg=c("VTI Density", "Normal"),
 lwd=6, lty=1, col=c("red", "blue"))

re_turns <- rutils::etf_env$re_turns[,
  c("VTI", "IEF", "IVW", "VYM", "IWB", "DBC", "VXX")]
x11(width=6, height=5)
chart.Boxplot(names=FALSE, re_turns)
par(cex.lab=0.8, cex.axis=0.8)
axis(side=2, at=(1:NCOL(re_turns))/7.5-0.05,labels=colnames(re_turns))

# Simulate normally distributed data
n_rows <- 1000
da_ta <- rnorm(n_rows)
sd(da_ta)
mad(da_ta)
median(abs(da_ta - median(da_ta)))
median(abs(da_ta - median(da_ta)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
boot_data <- sapply(1:10000, function(x) {
  sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
  c(sd=sd(sampl_e), mad=mad(sampl_e))
})  # end sapply
boot_data <- t(boot_data)
# Analyze bootstrapped variance
head(boot_data)
sum(is.na(boot_data))
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster
boot_data <- parLapply(clus_ter, 1:10000,
  function(x, da_ta) {
    sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  }, da_ta=da_ta)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:10000, function(x) {
    sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))

# VTI returns
re_turns <- rutils::etf_env$re_turns$VTI
re_turns <- na.omit(re_turns)
n_rows <- NROW(re_turns)
sd(re_turns)
mad(re_turns)
# Bootstrap of sd and mad estimators
boot_data <- sapply(1:10000, function(x) {
  sampl_e <- re_turns[sample.int(n_rows, replace=TRUE)]
  c(sd=sd(sampl_e), mad=mad(sampl_e))
})  # end sapply
boot_data <- t(boot_data)
# Means and standard errors from bootstrap
100*apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster
clusterExport(clus_ter, c("n_rows", "re_turns"))
boot_data <- parLapply(clus_ter, 1:10000,
  function(x) {
    sampl_e <- re_turns[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  })  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:10000, function(x) {
    sampl_e <- re_turns[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))

library(PerformanceAnalytics)
# Define target rate of return of 50 bps
tar_get <- 0.005
# Calculate the full downside returns
returns_sub <- (re_turns - tar_get)
returns_sub <- ifelse(returns_sub < 0, returns_sub, 0)
n_rows <- NROW(returns_sub)
# Calculate the downside deviation
all.equal(sqrt(sum(returns_sub^2)/n_rows),
  drop(DownsideDeviation(re_turns, MAR=tar_get, method="full")))
# Calculate the subset downside returns
returns_sub <- (re_turns - tar_get)
returns_sub <- returns_sub[returns_sub < 0]
n_rows <- NROW(returns_sub)
# Calculate the downside deviation
all.equal(sqrt(sum(returns_sub^2)/n_rows),
  DownsideDeviation(re_turns, MAR=tar_get, method="subset"))

# Calculate time series of VTI drawdowns
clos_e <- log(na.omit(rutils::etf_env$price_s$VTI))
draw_downs <- (clos_e - cummax(clos_e))
# PerformanceAnalytics plot of VTI drawdowns
re_turns <- rutils::diff_it(log(clos_e))
PerformanceAnalytics::chart.Drawdown(re_turns,
  ylab="", main="VTI Drawdowns")
# PerformanceAnalytics table of VTI drawdowns
PerformanceAnalytics::table.Drawdowns(re_turns)
# dygraph plot of VTI drawdowns
da_ta <- cbind(clos_e, draw_downs)
col_names <- c("VTI", "Drawdowns")
colnames(da_ta) <- col_names
dygraphs::dygraph(da_ta, main="VTI Drawdowns") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], valueRange=c(min(da_ta[, "Drawdowns"]), 5), independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue") %>%
  dySeries(name=col_names[2], axis="y2", col="red")
# Plot VTI drawdowns
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
x11(width=6, height=5)
quantmod::chart_Series(x=draw_downs, name="VTI Drawdowns", theme=plot_theme)

library(xtable)
library(PerformanceAnalytics)
clos_e <- log(na.omit(rutils::etf_env$price_s$VTI))
re_turns <- rutils::diff_it(log(clos_e))
# Calculate table of VTI drawdowns
tabl_e <- PerformanceAnalytics::table.Drawdowns(re_turns)
# Convert dates to strings
tabl_e <- cbind(sapply(tabl_e[, 1:3], as.character), tabl_e[, 4:7])
# Print table of VTI drawdowns
print(xtable(tabl_e), comment=FALSE, size="tiny", include.rownames=FALSE)

# Load "managers" data set
data(managers)
charts.PerformanceSummary(ham_1,
  main="", lwd=2, ylog=TRUE)

x11(width=6, height=5)
par(mar=c(3, 2, 1, 0), oma=c(0, 0, 0, 0))
# VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
conf_level <- 0.1
va_r <- quantile(re_turns, conf_level)
c_var <- mean(re_turns[re_turns < va_r])
# Plot histogram of VTI returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", ylab="frequency", breaks=100,
  xlim=c(-0.05, 0.01), freq=FALSE, main="VTI Returns Histogram")
# Calculate density
densi_ty <- density(re_turns, adjust=1.5)

# Plot density
lines(densi_ty, lwd=3, col="blue")
# Plot line for VaR
abline(v=va_r, col="red", lwd=3)
text(x=va_r, y=20, labels="VaR", lwd=2, srt=90, pos=2)
# Plot polygon shading for CVaR
var_max <- -0.06
rang_e <- (densi_ty$x < va_r) &  (densi_ty$x > var_max)
polygon(c(var_max, densi_ty$x[rang_e], va_r),
  c(0, densi_ty$y[rang_e], 0), col=rgb(1, 0, 0,0.5), border=NA)
text(x=va_r, y=3, labels="CVaR", lwd=2, pos=2)

# VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
conf_level <- 0.02
# Calculate VaR as quantile
va_r <- quantile(re_turns, conf_level)
# Or by sorting
sort_ed <- sort(as.numeric(re_turns))
in_dex <- round(conf_level*NROW(re_turns))
va_r <- sort_ed[in_dex]
# PerformanceAnalytics VaR
PerformanceAnalytics::VaR(re_turns,
  p=(1-conf_level), method="historical")
all.equal(unname(va_r),
  as.numeric(PerformanceAnalytics::VaR(re_turns,
  p=(1-conf_level), method="historical")))

x11(width=6, height=5)
par(mar=c(3, 2, 1, 0), oma=c(0, 0, 0, 0))
# Calculate VaR as quantile
va_r <- quantile(re_turns, conf_level)
# Calculate CVaR as expected loss
c_var <- mean(re_turns[re_turns < va_r])
# Or by sorting
sort_ed <- sort(as.numeric(re_turns))
in_dex <- round(conf_level*NROW(re_turns))
va_r <- sort_ed[in_dex]
c_var <- mean(sort_ed[1:in_dex])
# PerformanceAnalytics VaR
PerformanceAnalytics::ETL(re_turns,
  p=(1-conf_level), method="historical")
all.equal(c_var,
  as.numeric(PerformanceAnalytics::ETL(re_turns,
  p=(1-conf_level), method="historical")))

# Calculate the risk-return statistics
risk_ret <-
  PerformanceAnalytics::table.Stats(rutils::etf_env$re_turns)
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
risk_ret <- rutils::etf_env$risk_return
# Add Sharpe ratio column
risk_ret$Sharpe <- risk_ret$"Arithmetic Mean"/risk_ret$Stdev
# Sort on Sharpe ratio
risk_ret <- risk_ret[order(risk_ret$Sharpe, decreasing=TRUE), ]
# Print data frame
knitr::kable(risk_ret[, c("Sharpe", "Skewness", "Kurtosis")])

# Print data frame
knitr::kable(risk_ret[c("VXX", "SVXY"), c("Sharpe", "Skewness", "Kurtosis")])

# dygraph plot of VTI drawdowns
price_s <- na.omit(rutils::etf_env$price_s[, c("VXX", "SVXY")])
price_s <- price_s["2017/"]
col_names <- c("VXX", "SVXY")
colnames(price_s) <- col_names
dygraphs::dygraph(price_s, main="Prices of VXX and SVXY") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", strokeWidth=2, col="green") %>%
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
re_turns <- rutils::etf_env$re_turns[, c("VTI", "IEF")]
re_turns <- na.omit(re_turns)
# Calculate the Sharpe ratio
PerformanceAnalytics::SharpeRatio(re_turns)
# Calculate the Sortino ratio
PerformanceAnalytics::SortinoRatio(re_turns)
# Calculate the Calmar ratio
PerformanceAnalytics::CalmarRatio(re_turns)
# Calculate the returns statistics
tail(PerformanceAnalytics::table.Stats(re_turns), 4)
