# Get size of an object
vecv <- runif(1e6)
object.size(vecv)
format(object.size(vecv), units="MB")
# Get sizes of objects in workspace
sort(sapply(ls(), function(namev) {
  format(object.size(get(namev)), units="KB")}))
# Get sizes of all objects in workspace
sort(sapply(mget(ls()), object.size))
sort(sapply(mget(ls()), function(objectv) {
  format(object.size(objectv), units="KB")}
))
# Get total size of all objects in workspace
format(object.size(x=mget(ls())), units="MB")
# Get sizes of objects in rutils::etfenv environment
sort(sapply(ls(rutils::etfenv), function(namev) {
  object.size(get(namev, rutils::etfenv))}))
sort(sapply(mget(ls(rutils::etfenv), rutils::etfenv),
      object.size))
library(gdata)  # Load package gdata
# Get size of data frame columns
gdata::ll(unit="bytes", mtcars)
# Get namev, class, and size of objects in workspace
objframe <- gdata::ll(unit="bytes")
# Sort by memory size (descending)
objframe[order(objframe[, 2], decreasing=TRUE), ]
gdata::ll()[order(ll()$KB, decreasing=TRUE), ]
# Get sizes of objects in etfenv environment
gdata::ll(unit="bytes", etfenv)
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
# sum() is a compiled primitive function
sum
# mean() is a generic function
mean
vecv <- runif(1e6)
# sum() is much faster than mean()
all.equal(mean(vecv), sum(vecv)/NROW(vecv))
library(microbenchmark)
summary(microbenchmark(
  mean = mean(vecv),
  sum = sum(vecv)/NROW(vecv),
  times=10))[, c(1, 4, 5)]
# any() is a compiled primitive function
any
# any() is much faster than %in% wrapper for match()
all.equal(1 %in% vecv, any(vecv == 1))
summary(microbenchmark(
  inop = {1 %in% vecv},
  anyfun = any(vecv == 1),
  times=10))[, c(1, 4, 5)]
library(microbenchmark)
matv <- matrix(1:9, ncol=3, # Create matrix
  dimnames=list(paste0("row", 1:3),
          paste0("col", 1:3)))
# Create specialized function
matrix_to_dframe <- function(matv) {
  ncols <- ncol(matv)
  dframe <- vector("list", ncols)  # empty vector
  for (indeks in 1:ncols)  # Populate vector
    dframe <- matv[, indeks]
  attr(dframe, "row.names") <-  # Add attributes
    .set_row_names(NROW(matv))
  attr(dframe, "class") <- "data.frame"
  dframe  # Return data frame
}  # end matrix_to_dframe
# Compare speed of three methods
summary(microbenchmark(
  matrix_to_dframe(matv),
  as.data.frame.matrix(matv),
  as.data.frame(matv),
  times=10))[, c(1, 4, 5)]
# Calculate matrix of random data with 5,000 rows
matv <- matrix(rnorm(10000), ncol=2)
# Allocate memory for row sums
rowsumv <- numeric(NROW(matv))
summary(microbenchmark(
  rowsums = rowSums(matv),  # end rowsumv
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
# Compiled C++ function
  cpp = cumsum(vecv),  # end for
# Allocate full memory for cumulative sum
  forloop = {cumsumv <- numeric(NROW(vecv))
    cumsumv[1] <- vecv[1]
    for (i in 2:NROW(vecv)) {
      cumsumv[i] <- cumsumv[i-1] + vecv[i]
    }},  # end for
# Allocate zero memory for cumulative sum
  growvec = {cumsumv <- numeric(0)
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
# Disable JIT
jit_level <- compiler::enableJIT(0)
# Create inefficient function
meanfun <- function(x) {
  datav <- 0; nrows <- NROW(x)
  for(it in 1:nrows)
    datav <- datav + x[it]/nrows
  datav
}  # end meanfun
# Byte-compile function and inspect it
meanbyte <- compiler::cmpfun(meanfun)
meanbyte
# Test function
vecv <- runif(1e3)
all.equal(mean(vecv), meanbyte(vecv), meanfun(vecv))
# microbenchmark byte-compile function
summary(microbenchmark(
  mean(vecv),
  meanbyte(vecv),
  meanfun(vecv),
  times=10))[, c(1, 4, 5)]
# Create another inefficient function
sapply2 <- function(x, FUN, ...) {
  datav <- vector(length=NROW(x))
  for (it in seq_along(x))
    datav[it] <- FUN(x[it], ...)
  datav
}  # end sapply2
sapply2_comp <- compiler::cmpfun(sapply2)
all.equal(sqrt(vecv),
  sapply2(vecv, sqrt),
  sapply2_comp(vecv, sqrt))
summary(microbenchmark(
  sqrt(vecv),
  sapply2_comp(vecv, sqrt),
  sapply2(vecv, sqrt),
  times=10))[, c(1, 4, 5)]
# enable JIT
compiler::enableJIT(jit_level)
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
  pmax=do.call(pmax.int, lapply(1:NCOL(matv),
  function(indeks) matv[, indeks])),
  lapply=unlist(lapply(1:NROW(matv),
  function(indeks) max(matv[indeks, ]))),
  times=10))[, c(1, 4, 5)]
install.packages("matrixStats")  # Install package matrixStats
library(matrixStats)  # Load package matrixStats
# Calculate row mininmum values two different ways
all.equal(matrixStats::rowMins(matv), do.call(pmin.int, lapply(1:NCOL(matv),
    function(indeks) matv[, indeks])))
# Calculate row mininmum values three different ways
summary(microbenchmark(
  rowmins = matrixStats::rowMins(matv),
  pmin = do.call(pmin.int, lapply(1:NCOL(matv),
    function(indeks) matv[, indeks])),
  as_dframe = do.call(pmin.int, as.data.frame.matrix(matv)),
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
  brackets = {vecv <- numeric(10); vecv[] <- 2},
# Slow because loop is performed in R
  forloop = {vecv <- numeric(10)
    for (indeks in seq_along(vecv))
      vecv[indeks] <- 2},
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
summary(microbenchmark(  # Assign values to vector two different ways
# Fast vectorized assignment loop performed in C using brackets "[]"
  brackets = {vecv <- numeric(10); vecv[4:7] <- rnorm(4)},
# Slow because loop is performed in R
  forloop = {vecv <- numeric(10)
    for (indeks in 4:7)
      vecv[indeks] <- rnorm(1)},
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Define function vectorized automatically
myfun <- function(input, param) {
  param*input
}  # end myfun
# "input" is vectorized
myfun(input=1:3, param=2)
# "param" is vectorized
myfun(input=10, param=2:4)
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
for (i in 2:NROW(cumsumv2))
  cumsumv2[i] <- (cumsumv2[i] + cumsumv2[i-1])
# Compare the two methods
all.equal(cumsumv, cumsumv2)
# Microbenchmark the two methods
library(microbenchmark)
summary(microbenchmark(
  cumsum=cumsum(vecv),
  loop_alloc={
    cumsumv2 <- vecv
    for (i in 2:NROW(cumsumv2))
cumsumv2[i] <- (cumsumv2[i] + cumsumv2[i-1])
  },
  loop_nalloc={
    # Doesn't allocate memory to cumsumv3
    cumsumv3 <- vecv[1]
    for (i in 2:NROW(vecv))
# This command adds an extra element to cumsumv3
cumsumv3[i] <- (vecv[i] + cumsumv3[i-1])
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
}, retp=retp, nsimu=nrows)  # end parLapply
# Bootstrap sd and MAD under Mac-OSX or Linux
bootd <- mclapply(1:nboot, function(x) {
    samplev <- retp[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, mc.cores=ncores)  # end mclapply
stopCluster(compclust)  # Stop R processes over cluster under Windows
bootd <- rutils::do_call(rbind, bootd)
# Standard error of standard deviation assuming normal distribution of returns
sd(retp)/sqrt(nrows)
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
}, design=desm)  # end parLapply
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
nsimu <- 1000  # Number of simulation steps
pathv <- numeric(nsimu)  # Allocate path vector
pathv[1] <- rnorm(1)  # Initialize path
it <- 2  # Initialize simulation index
while ((it <= nsimu) && (pathv[it - 1] < barl)) {
# Simulate next step
  pathv[it] <- pathv[it - 1] + rnorm(1)
  it <- it + 1  # Advance index
}  # end while
# Fill remaining path after it crosses barl
if (it <= nsimu)
  pathv[it:nsimu] <- pathv[it - 1]
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
nsimu <- 1000  # Number of simulation steps
# Simulate path of Brownian motion
pathv <- cumsum(rnorm(nsimu))
# Find index when path crosses barl
crossp <- which(pathv > barl)
# Fill remaining path after it crosses barl
if (NROW(crossp) > 0) {
  pathv[(crossp[1]+1):nsimu] <- pathv[crossp[1]]
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
nsimu <- 100  # Number of simulation paths
# Simulate multiple paths of Brownian motion
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
pathm <- rnorm(nsimu*nsteps, mean=drift, sd=sigmav)
pathm <- matrix(pathm, nc=nsimu)
pathm <- matrixStats::colCumsums(pathm)
# Final distribution of paths
mean(pathm[nsteps, ]) ; sd(pathm[nsteps, ])
# Calculate option payout at maturity
strikep <- 50  # Strike price
payouts <- (pathm[nsteps, ] - strikep)
sum(payouts[payouts > 0])/nsimu
# Calculate probability of crossing the barrier at any point
barl <- 50
crossi <- (colSums(pathm > barl) > 0)
sum(crossi)/nsimu
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
startd <- as.numeric(pricev[1, ])
retp <- rutils::diffit(log(pricev))
class(retp); head(retp)
sum(is.na(retp))
nrows <- NROW(retp)
# Define barrier level with respect to prices
barl <- 1.5*max(pricev)
# Calculate single bootstrap sample
samplev <- retp[sample.int(nrows, replace=TRUE)]
# Calculate prices from percentage returns
samplev <- startd*exp(cumsum(samplev))
# Calculate if prices crossed barrier
sum(samplev > barl) > 0
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
compclust <- makeCluster(ncores)  # Initialize compute cluster under Windows
# Perform parallel bootstrap under Windows
clusterSetRNGStream(compclust, 1121)  # Reset random number generator in all cores
clusterExport(compclust, c("startd", "barl"))
nboot <- 10000
bootd <- parLapply(compclust, 1:nboot, function(x, retp, nsimu) {
  samplev <- retp[sample.int(nsimu, replace=TRUE)]
  # Calculate prices from percentage returns
  samplev <- startd*exp(cumsum(samplev))
  # Calculate if prices crossed barrier
  sum(samplev > barl) > 0
}, retp=retp, nsimu=nrows)  # end parLapply
stopCluster(compclust)  # Stop R processes over cluster under Windows
# Perform parallel bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:nboot, function(x) {
    samplev <- retp[sample.int(nrows, replace=TRUE)]
    # Calculate prices from percentage returns
    samplev <- startd*exp(cumsum(samplev))
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
startd <- pricev[1]
retp <- rutils::diffit(log(pricev))
nrows <- NROW(retp)
# Calculate difference of OHLC price columns
pricediff <- ohlc[, 1:3] - pricev
class(retp); head(retp)
# Calculate bootstrap prices from percentage returns
datav <- sample.int(nrows, replace=TRUE)
priceboot <- startd*exp(cumsum(retp[datav]))
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
clusterExport(compclust, c("startd", "barl", "pricediff"))
nboot <- 10000
bootd <- parLapply(compclust, 1:nboot, function(x, retp, nsimu) {
  # Calculate OHLC prices from percentage returns
  datav <- sample.int(nsimu, replace=TRUE)
  priceboot <- startd*exp(cumsum(retp[datav]))
  ohlcboot <- pricediff + priceboot
  ohlcboot <- cbind(ohlcboot, priceboot)
  # Calculate statistic
  sum(ohlcboot[, 2] > barl) > 0
}, retp=retp, nsimu=nrows)  # end parLapply
# Perform parallel bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:nboot, function(x) {
    # Calculate OHLC prices from percentage returns
    datav <- sample.int(nrows, replace=TRUE)
    priceboot <- startd*exp(cumsum(retp[datav]))
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
lambda <- (-1.5)  # Tilt parameter
datat <- datav + lambda  # Tilt the random numbers
# Cumulative probability from importance sample - wrong!
sum(datat < quantv)/nsimu
# Cumulative probability from importance sample - correct
weightv <- exp(-lambda*datat + lambda^2/2)
sum((datat < quantv)*weightv)/nsimu
# Bootstrap of standard errors of cumulative probability
nboot <- 1000
bootd <- sapply(1:nboot, function(x) {
  datav <- rnorm(nsimu)
  naivemc <- sum(datav < quantv)/nsimu
  datav <- (datav + lambda)
  weightv <- exp(-lambda*datav + lambda^2/2)
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
datat <- datav + lambda  # Tilt the random numbers
weightv <- exp(-lambda*datat + lambda^2/2)
# Cumulative probabilities using importance sample
cumprob <- cumsum(weightv)/nsimu
# Quantile from importance sample
datat[findInterval(confl, cumprob)]
# Bootstrap of standard errors of quantile
nboot <- 1000
bootd <- sapply(1:nboot, function(x) {
  datav <- sort(rnorm(nsimu))
  naivemc <- datav[cutoff]
  datat <- datav + lambda
  weightv <- exp(-lambda*datat + lambda^2/2)
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
  datat <- datav + lambda
  weightv <- exp(-lambda*datat + lambda^2/2)
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
calc_vars <- function(lambda, confl=0.05) {
  datat <- datav + lambda  # Tilt the random numbers
  weightv <- exp(-lambda*datat + lambda^2/2)
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
lambda <- 0.04  # Tilt parameter
datat <- datav + lambda  # Tilt the random numbers
patht <- matrixStats::colCumsums(datat)
# Calculate path weights
weightm <- exp(-lambda*datat + lambda^2/2)
weightm <- matrixStats::colProds(weightm)
# Or
weightm <- exp(-lambda*colSums(datat) + nsteps*lambda^2/2)
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
# Calculate random default probabilities
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nbonds <- 100
probv <- runif(nbonds, max=0.2)
mean(probv)
# Simulate number of defaults
unifv <- runif(nbonds)
sum(unifv < probv)
# Simulate average number of defaults using for() loop (inefficient way)
nsimu <- 1000
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
defaultv <- numeric(nsimu)
for (i in 1:nsimu) {  # Perform loop
  unifv <- runif(nbonds)
  defaultv[i] <- sum(unifv < probv)
}  # end for
# Calculate average number of defaults
mean(defaultv)
# Simulate using vectorized functions (efficient way)
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
unifm <- matrix(runif(nsimu*nbonds), ncol=nsimu)
defaultv <- colSums(unifm < probv)
mean(defaultv)
# Plot the distribution of defaults
x11(width=6, height=5)
plot(density(defaultv), main="Distribution of Defaults",
     xlab="number of defaults", ylab="frequency")
abline(v=mean(defaultv), lwd=3, col="red")
# Calculate default thresholds and asset values
threshv <- qnorm(probv)
assetm <-qnorm(unifm)
# Simulate defaults
defaultv <- colSums(assetm < threshv)
mean(defaultv)
# Plot Standard Normal distribution
x11(width=6, height=5)
xlim <- 4; threshv <- qnorm(0.025)
curve(expr=dnorm(x), type="l", xlim=c(-xlim, xlim),
xlab="asset value", ylab="", lwd=3,
col="blue", main="Distribution of Asset Values")
abline(v=threshv, col="red", lwd=3)
text(x=threshv-0.1, y=0.15, labels="default threshold",
 lwd=2, srt=90, pos=3)
# Plot polygon area
xvar <- seq(-xlim, xlim, length=100)
yvar <- dnorm(xvar)
intail <- ((xvar >= (-xlim)) & (xvar <= threshv))
polygon(c(xlim, xvar[intail], threshv),
  c(-1, yvar[intail], -1), col="red")
# Define correlation parameters
rho <- 0.2
rhos <- sqrt(rho) ; rhosm <- sqrt(1-rho)
nbonds <- 5 ; nsimu <- 10000
# Calculate vector of systematic and idiosyncratic factors
sysv <- rnorm(nsimu)
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
isync <- rnorm(nsimu*nbonds)
dim(isync) <- c(nbonds, nsimu)
# Simulate asset values using vectorized functions (efficient way)
assetm <- t(rhos*sysv + t(rhosm*isync))
# Asset values are standard normally distributed
apply(assetm, MARGIN=1, function(x) c(mean=mean(x), sd=sd(x)))
# Calculate correlations between asset values
cor(t(assetm))
# Simulate asset values using for() loop (inefficient way)
# Allocate matrix of assets
assetn <- matrix(nrow=nbonds, ncol=nsimu)
# Simulate asset values using for() loop
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
for (i in 1:nsimu) {  # Perform loop
  assetn[, i] <- rhos*sysv[i] + rhosm*rnorm(nbonds)
}  # end for
all.equal(assetn, assetm)
# benchmark the speed of the two methods
library(microbenchmark)
summary(microbenchmark(
  forloop={for (i in 1:nsimu) {
    rhos*sysv[i] + rhosm*rnorm(nbonds)}},
  vectorized={t(rhos*sysv + t(rhosm*isync))},
  times=10))[, c(1, 4, 5)]
# Calculate random default probabilities
nbonds <- 5
probv <- runif(nbonds, max=0.2)
mean(probv)
# Calculate default thresholds
threshv <- qnorm(probv)
# Calculate number of defaults using vectorized functions (efficient way)
# Calculate vector of number of defaults
rowMeans(assetm < threshv)
probv
# Calculate number of defaults using for() loop (inefficient way)
# Allocate matrix of defaultm
defaultm <- matrix(nrow=nbonds, ncol=nsimu)
# Simulate asset values using for() loop
for (i in 1:nsimu) {  # Perform loop
  defaultm[, i] <- (assetm[, i] < threshv)
}  # end for
rowMeans(defaultm)
rowMeans(assetm < threshv)
# Calculate correlations between defaults
cor(t(defaultm))
# Define default probabilities
nbonds <- 2
defprob <- 0.2
threshv <- qnorm(defprob)
# Define correlation parameters
rho <- 0.2
rhos <- sqrt(rho) ; rhosm <- sqrt(1-rho)
# Calculate vector of systematic factors
nsimu <- 1000
sysv <- rnorm(nsimu)
isync <- rnorm(nsimu*nbonds)
dim(isync) <- c(nbonds, nsimu)
# Simulate asset values using vectorized functions
assetm <- t(rhos*sysv + t(rhosm*isync))
# Calculate number of defaults using vectorized functions
defaultm <- (assetm < threshv)
# Calculate average number of defaults and compare to defprob
rowMeans(defaultm)
defprob
# Calculate correlations between assets
cor(t(assetm))
# Calculate correlations between defaults
cor(t(defaultm))
# Define cumulative default distribution function
cumdefdistr <- function(x, threshv=(-2), rho=0.2)
  pnorm((sqrt(1-rho)*qnorm(x) - threshv)/sqrt(rho))
defprob <- 0.4; threshv <- qnorm(defprob)
cumdefdistr(x=0.2, threshv=qnorm(defprob), rho=rho)
# Plot cumulative default distribution function
curve(expr=cumdefdistr(x, threshv=threshv, rho=0.05),
xlim=c(0, 0.999), lwd=3, xlab="percent default", ylab="probability",
col="green", main="Cumulative Default Probabilities")
# Plot default distribution with higher correlation
curve(expr=cumdefdistr(x, threshv=threshv, rho=0.2),
    xlim=c(0, 0.999), add=TRUE, lwd=3, col="blue", main="")
# Add legend
legend(x="topleft",
   legend=c("high correlation", "low correlation"),
   title=NULL, inset=0.05, cex=1.0, bg="white",
   bty="n", lwd=6, lty=1, col=c("blue", "green"))
# Add unconditional default probability
abline(v=defprob, col="red", lwd=3)
text(x=defprob, y=0.0, labels="default probability",
 lwd=2, srt=90, pos=4)
# Define default probability density function
defdistr <- function(x, threshv=(-2), rho=0.2)
  sqrt((1-rho)/rho)*exp(-(sqrt(1-rho)*qnorm(x) -
  threshv)^2/(2*rho) + qnorm(x)^2/2)
# Define parameters
rho <- 0.2 ; rhos <- sqrt(rho) ; rhosm <- sqrt(1-rho)
defprob <- 0.3; threshv <- qnorm(defprob)
defdistr(0.03, threshv=threshv, rho=rho)
# Plot probability distribution of defaults
curve(expr=defdistr(x, threshv=threshv, rho=0.1),
xlim=c(0, 1.0), lwd=3,
xlab="Default percentage", ylab="Density",
col="green", main="Distribution of Defaults")
# Plot default distribution with higher correlation
curve(expr=defdistr(x, threshv=threshv, rho=0.3),
xlab="default percentage", ylab="",
add=TRUE, lwd=3, col="blue", main="")
# Add legend
legend(x="topright",
   legend=c("high correlation", "low correlation"),
   title=NULL, inset=0.05, cex=1.0, bg="white",
   bty="n", lwd=6, lty=1, col=c("blue", "green"))
# Add unconditional default probability
abline(v=defprob, col="red", lwd=3)
text(x=defprob, y=2, labels="default probability",
 lwd=2, srt=90, pos=2)
# Plot default distribution with low correlation
curve(expr=defdistr(x, threshv=threshv, rho=0.01),
  xlab="default percentage", ylab="", lwd=2,
  col="green", main="Distribution of Defaults")
# Plot default distribution with high correlation
curve(expr=defdistr(x, threshv=threshv, rho=0.99),
  xlab="percentage of defaults", ylab="density",
  add=TRUE, lwd=2, n=10001, col="blue", main="")
# Add legend
legend(x="top", legend=c("high correlation", "low correlation"),
   title=NULL, inset=0.1, cex=1.0, bg="white",
   bty="n", lwd=6, lty=1, col=c("blue", "green"))
# Add unconditional default probability
abline(v=0.1, col="red", lwd=2)
text(x=0.1, y=10, lwd=2, pos=4, labels="default probability")
# Get help for integrate()
?integrate
# Calculate slowly converging integral
func <- function(x) {1/((x+1)*sqrt(x))}
integrate(func, lower=0, upper=10)
integrate(func, lower=0, upper=Inf)
# Integrate function with parameter lambda
func <- function(x, lambda=1) {
  exp(-x*lambda)
}  # end func
integrate(func, lower=0, upper=Inf)
integrate(func, lower=0, upper=Inf, lambda=2)
# Cumulative probability over normal distribution
pnorm(-2)
integrate(dnorm, low=2, up=Inf)
str(dnorm)
pnorm(-1)
integrate(dnorm, low=2, up=Inf, mean=1)
# Expected value over normal distribution
integrate(function(x) x*dnorm(x), low=2, up=Inf)
# Vasicek model parameters
rho <- 0.1; lgd <- 0.4
defprob <- 0.05; threshv <- qnorm(defprob)
# Define Vasicek cumulative loss distribution
cumlossdistr <- function(x, threshv=(-2), rho=0.2, lgd=0.4)
  pnorm((sqrt(1-rho)*qnorm(x/lgd) - threshv)/sqrt(rho))
# Define Vasicek loss distribution function
lossdistr <- function(x, threshv=(-2), rho=0.2, lgd=0.4)
  sqrt((1-rho)/rho)*exp(-(sqrt(1-rho)*qnorm(x/lgd) - threshv)^2/(2*rho) + qnorm(x/lgd)^2/2)/lgd
integrate(lossdistr, low=0, up=lgd, threshv=(-2), rho=rho, lgd=lgd)
# Plot probability distribution of losses
x11(width=6, height=5)
curve(expr=lossdistr(x, threshv=threshv, rho=rho),
cex.main=1.8, cex.lab=1.8, cex.axis=1.5,
type="l", xlim=c(0, 0.06),
xlab="loss percentage", ylab="density", lwd=3,
col="blue", main="Portfolio Loss Density")
# Add line for expected loss
abline(v=lgd*defprob, col="red", lwd=3)
text(x=lgd*defprob-0.001, y=35, labels="expected loss", lwd=3, pos=4, cex=1.8)
# Define Vasicek cumulative loss distribution
# (with error handling for x)
cumlossdistr <- function(x, threshv=(-2), rho=0.2, lgd=0.4) {
  qnormv <- ifelse(x/lgd < 0.999, qnorm(x/lgd), 3.1)
  pnorm((sqrt(1-rho)*qnormv - threshv)/sqrt(rho))
}  # end cumlossdistr
# Define Vasicek loss distribution function
# (vectorized version with error handling for x)
lossdistr <- function(x, threshv=(-2), rho=0.1, lgd=0.4) {
  qnormv <- ifelse(x/lgd < 0.999, qnorm(x/lgd), 3.1)
  sqrt((1-rho)/rho)*exp(-(sqrt(1-rho)*qnormv - threshv)^2/(2*rho) + qnormv^2/2)/lgd
}  # end lossdistr
defprob <- 0.2; threshv <- qnorm(defprob)
rho <- 0.1; lgd <- 0.4
attachp <- 0.15; detachp <- 0.2
# Expected tranche loss is sum of two terms
tranchel <-
  # Loss between attachp and detachp
  integrate(function(x, attachp) (x-attachp)*lossdistr(x,
threshv=threshv, rho=rho, lgd=lgd),
low=attachp, up=detachp, attachp=attachp)$value/(detachp-attachp) +
  # Loss in excess of detachp
  (1-cumlossdistr(x=detachp, threshv=threshv, rho=rho, lgd=lgd))
# Plot probability distribution of losses
curve(expr=lossdistr(x, threshv=threshv, rho=rho),
cex.main=1.8, cex.lab=1.8, cex.axis=1.5,
type="l", xlim=c(0, 3*lgd*defprob),
xlab="loss percentage", ylab="density", lwd=3,
col="orange", main="CDO Tranche Losses")
# Add line for expected loss
abline(v=lgd*defprob, col="red", lwd=3)
text(x=lgd*defprob-0.001, y=4, labels="expected loss",
 lwd=2, srt=90, pos=3, cex=1.8)
# Add lines for attach and detach
abline(v=attachp, col="blue", lwd=3)
text(x=attachp-0.001, y=4, labels="attach",
 lwd=2, srt=90, pos=3, cex=1.8)
abline(v=detachp, col="green", lwd=3)
text(x=detachp-0.001, y=4, labels="detach",
 lwd=2, srt=90, pos=3, cex=1.8)
# Add shading for CDO tranche
vars <- seq(attachp, detachp, length=100)
densv <- sapply(vars, lossdistr, threshv=threshv, rho=rho)
# Draw shaded polygon
polygon(c(attachp, vars, detachp), density=20,
  c(-1, densv, -1), col="red", border=NA)
text(x=0.5*(attachp+detachp), y=0, labels="CDO tranche", cex=1.8, lwd=2, pos=3)
# Add lines for unexpected loss
abline(v=0.04, col="blue", lwd=3)
arrows(x0=0.02, y0=35, x1=0.04, y1=35, code=3, lwd=3, cex=0.5)
text(x=0.03, y=36, labels="unexpected loss", lwd=2, pos=3)
# Add lines for VaR
abline(v=0.055, col="red", lwd=3)
arrows(x0=0.0, y0=25, x1=0.055, y1=25, code=3, lwd=3, cex=0.5)
text(x=0.03, y=26, labels="VaR", lwd=2, pos=3)
text(x=0.055-0.001, y=10, labels="VaR", lwd=2, srt=90, pos=3)
varisk <- 0.04; varmax <- 4*lgd*defprob
# Calculate CVaR
cvar <- integrate(function(x) x*lossdistr(x, threshv=threshv,
  rho=rho, lgd=lgd), low=varisk, up=lgd)$value
cvar <- cvar/integrate(lossdistr, low=varisk, up=lgd,
   threshv=threshv, rho=rho, lgd=lgd)$value
# Plot probability distribution of losses
curve(expr=lossdistr(x, threshv=threshv, rho=rho),
type="l", xlim=c(0, 0.06),
xlab="loss percentage", ylab="density", lwd=3,
col="blue", main="Conditional Value at Risk")
# Add line for expected loss
abline(v=lgd*defprob, col="red", lwd=3)
text(x=lgd*defprob-0.001, y=10, labels="expected loss", lwd=2, srt=90, pos=3)
# Add lines for VaR
abline(v=varisk, col="red", lwd=3)
text(x=varisk-0.001, y=10, labels="VaR",
 lwd=2, srt=90, pos=3)
# Add shading for CVaR
vars <- seq(varisk, varmax, length=100)
densv <- sapply(vars, lossdistr,
  threshv=threshv, rho=rho)
# Draw shaded polygon
polygon(c(varisk, vars, varmax), density=20,
  c(-1, densv, -1), col="red", border=NA)
text(x=varisk+0.005, y=0, labels="CVaR", lwd=2, pos=3)
# VaR (quantile of the loss distribution)
varfun <- function(x, threshv=qnorm(0.1), rho=0.1, lgd=0.4)
  lgd*pnorm((sqrt(rho)*qnorm(x) + threshv)/sqrt(1-rho))
varfun(x=0.99, threshv=threshv, rho=rho, lgd=lgd)
# Plot VaR
curve(expr=varfun(x, threshv=threshv, rho=rho, lgd=lgd),
type="l", xlim=c(0, 0.999), xlab="confidence level", ylab="VaR", lwd=3,
col="orange", main="VaR versus Confidence Level")
# Add line for expected loss
abline(h=lgd*defprob, col="red", lwd=3)
text(x=0.2, y=lgd*defprob, labels="expected loss", lwd=2, pos=3)
# Integrate lossdistr() over full range
integrate(lossdistr, low=0.0, up=lgd,
    threshv=threshv, rho=rho, lgd=lgd)
# Calculate expected losses using lossdistr()
integrate(function(x) x*lossdistr(x, threshv=threshv,
  rho=rho, lgd=lgd), low=0.0, up=lgd)
# Calculate confidence levels corresponding to VaR values
vars <- seq(0.07, 0.12, 0.001)
confls <- sapply(vars, function(varisk) {
  integrate(lossdistr, low=varisk, up=lgd,
      threshv=threshv, rho=rho, lgd=lgd)
})  # end sapply
confls <- cbind(as.numeric(t(confls)[, 1]), vars)
colnames(confls) <- c("levels", "VaRs")
# Calculate 95% confidence level VaR value
confls[match(TRUE, confls[, "levels"] < 0.05), "VaRs"]
plot(x=1-confls[, "levels"],
     y=confls[, "VaRs"], lwd=2,
     xlab="confidence level", ylab="VaRs",
     t="l", main="VaR Values and Confidence Levels")
# Calculate CVaR values
cvars <- sapply(vars, function(varisk) {
  integrate(function(x) x*lossdistr(x, threshv=threshv,
rho=rho, lgd=lgd), low=varisk, up=lgd)})  # end sapply
confls <- cbind(confls, as.numeric(t(cvars)[, 1]))
colnames(confls)[3] <- "CVaRs"
# Divide CVaR by confidence level
confls[, "CVaRs"] <- confls[, "CVaRs"]/confls[, "levels"]
# Calculate 95% confidence level CVaR value
confls[match(TRUE, confls[, "levels"] < 0.05), "CVaRs"]
# Plot CVaRs
plot(x=1-confls[, "levels"], y=confls[, "CVaRs"],
     t="l", col="red", lwd=2,
     ylim=range(confls[, c("VaRs", "CVaRs")]),
     xlab="confidence level", ylab="CVaRs",
     main="CVaR Values and Confidence Levels")
# Add VaRs
lines(x=1-confls[, "levels"], y=confls[, "VaRs"], lwd=2)
# Add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"),
   title="default probability = 5%
correlation = 10%
loss given default = 40%",
   inset=0.1, cex=1.0, bg="white", bty="n",
   lwd=6, lty=1, col=c("red", "black"))
# Define model parameters
nbonds <- 300; nsimu <- 1000; lgd <- 0.4
# Define correlation parameters
rho <- 0.2; rhos <- sqrt(rho); rhosm <- sqrt(1-rho)
# Calculate default probabilities and thresholds
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
probv <- runif(nbonds, max=0.2)
threshv <- qnorm(probv)
# Simulate losses under the Vasicek model
sysv <- rnorm(nsimu)
assetm <- matrix(rnorm(nsimu*nbonds), ncol=nsimu)
assetm <- t(rhos*sysv + t(rhosm*assetm))
lossm <- lgd*colSums(assetm < threshv)/nbonds
# Calculate VaR from confidence level
confl <- 0.95
varisk <- quantile(lossm, confl)
# Calculate the CVaR as the mean losses in excess of VaR
cvar <- mean(lossm[lossm > varisk])
# Plot the density of portfolio losses
densv <- density(lossm, from=0)
plot(densv, xlab="loss percentage", ylab="density",
   cex.main=1.0, cex.lab=1.0, cex.axis=1.0,
   lwd=3, col="blue", main="Portfolio Loss Distribution")
# Add vertical line for expected loss
exploss <- lgd*mean(probv)
abline(v=exploss, col="red", lwd=3)
xmax <- max(densv$x); ymax <- max(densv$y)
text(x=exploss, y=(6*ymax/7), labels="expected loss",
     lwd=2, pos=4, cex=1.0)
# Add vertical line for VaR
abline(v=varisk, col="red", lwd=3)
text(x=varisk, y=4*ymax/5, labels="VaR", lwd=2, pos=4, cex=1.0)
# Draw shaded polygon for CVaR
intail <- (densv$x > varisk)
xvar <- c(min(densv$x[intail]), densv$x[intail], max(densv$x))
polygon(xvar, c(-1, densv$y[intail], -1), col="red", border=NA, density=10)
# Add text for CVaR
text(x=5*varisk/4, y=(ymax/7), labels="CVaR", lwd=2, pos=4, cex=1.0)
# Add text with data
text(xmax, ymax, labels=paste0(
   "Expected Loss = ", format(100*exploss, digits=3), "%", "\n",
   "Loss severity = ", format(100*lgd, digits=3), "%", "\n",
   "Correlation = ", format(100*rho, digits=3), "%", "\n",
   "VaR = ", format(100*varisk, digits=3), "%", "\n",
   "CVaR = ", format(100*cvar, digits=3), "%"),
   adj=c(1, 1), cex=1.0, lwd=2)
# Calculate VaRs from confidence levels
confls <- seq(0.93, 0.99, 0.01)
vars <- quantile(lossm, probs=confls)
plot(x=confls, y=vars, t="l", lwd=2,
   xlab="confidence level", ylab="VaRs",
   main="Simulated VaR and Confidence Levels")
# Calculate CVaRs
cvars <- sapply(vars, function(varisk) {
  mean(lossm[lossm >= varisk])
})  # end sapply
cvars <- cbind(cvars, vars)
# Alternative CVaR calculation using frequency table
# first calculate frequency table of losses
# tablev <- table(lossm)/nsimu
# Calculate CVaRs from frequency table
# cvars <- sapply(vars, function(varisk) {
#   tailrisk <- tablev[names(tablev) > varisk]
#   tailrisk %*% as.numeric(names(tailrisk)) / sum(tailrisk)
# })  # end sapply
# Plot CVaRs
plot(x=confls, y=cvars[, "cvars"],
   t="l", col="red", lwd=2, ylim=range(cvars),
   xlab="confidence level", ylab="CVaRs",
   main="Simulated CVaR and Confidence Levels")
# Add VaRs
lines(x=confls, y=cvars[, "vars"], lwd=2)
# Add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
   title=NULL, inset=0.05, cex=1.0, bg="white",
   y.intersp=0.1, lwd=6, lty=1, col=c("red", "black"))
calc_var <- function(threshv, # Default thresholds
   lgd=0.6, # loss given default
   rhos, rhosm, # asset correlation
   nsimu=1000, # number of simulations
   confls=seq(0.93, 0.99, 0.01) # Confidence levels
   ) {
  # Define model parameters
  nbonds <- NROW(threshv)
  # Simulate losses under the Vasicek model
  sysv <- rnorm(nsimu)
  assetm <- matrix(rnorm(nsimu*nbonds), ncol=nsimu)
  assetm <- t(rhos*sysv + t(rhosm*assetm))
  lossm <- lgd*colSums(assetm < threshv)/nbonds
  # Calculate VaRs and CVaRs
  vars <- quantile(lossm, probs=confls)
  cvars <- sapply(vars, function(varisk) {
    mean(lossm[lossm >= varisk])
  })  # end sapply
  names(vars) <- confls
  names(cvars) <- confls
  c(vars, cvars)
}  # end calc_var
# Define model parameters
nbonds <- 300; nsimu <- 1000; lgd <- 0.4
rho <- 0.2; rhos <- sqrt(rho); rhosm <- sqrt(1-rho)
# Calculate default probabilities and thresholds
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
probv <- runif(nbonds, max=0.2)
threshv <- qnorm(probv)
confls <- seq(0.93, 0.99, 0.01)
# Define number of bootstrap simulations
nboot <- 500
# Perform bootstrap of calc_var
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
bootd <- sapply(rep(lgd, nboot), calc_var,
  threshv=threshv,
  rhos=rhos, rhosm=rhosm,
  nsimu=nsimu, confls=confls)  # end sapply
bootd <- t(bootd)
# Calculate standard errors of VaR and CVaR from bootd data
varsd <- apply(bootd[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
cvarsd <- apply(bootd[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
varsds <- varsd[2, ]/varsd[1, ]
cvarsds <- cvarsd[2, ]/cvarsd[1, ]
# Plot the scaled standard errors of VaRs and CVaRs
plot(x=names(varsds), y=varsds,
  t="l", lwd=2, ylim=range(c(varsds, cvarsds)),
  xlab="confidence level", ylab="standard error",
  main="Scaled Standard Errors of CVaR and VaR")
lines(x=names(cvarsds), y=cvarsds, lwd=2, col="red")
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
   title=NULL, inset=0.05, cex=1.0, bg="white",
   y.intersp=0.1, lwd=6, lty=1, col=c("red", "black"))
library(parallel)  # load package parallel
ncores <- detectCores() - 1  # number of cores
compclust <- makeCluster(ncores)  # Initialize compute cluster
# Perform bootstrap of calc_var for Windows
clusterSetRNGStream(compclust, 1121)
bootd <- parLapply(compclust, rep(lgd, nboot),
  fun=calc_var, threshv=threshv,
  rhos=rhos, rhosm=rhosm,
  nsimu=nsimu, confls=confls)  # end parLapply
stopCluster(compclust)  # Stop R processes over cluster
# Bootstrap under Mac-OSX or Linux
bootd <- mclapply(rep(lgd, nboot),
  FUN=calc_var, threshv=threshv,
  rhos=rhos, rhosm=rhosm,
  nsimu=nsimu, confls=confls)  # end mclapply
bootd <- rutils::do_call(rbind, bootd)
# Calculate standard errors of VaR and CVaR from bootd data
varsd <- apply(bootd[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
cvarsd <- apply(bootd[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
varsds <- varsd[2, ]/varsd[1, ]
cvarsds <- cvarsd[2, ]/cvarsd[1, ]
# Plot the standard errors of VaRs and CVaRs
plot(x=names(varsds), y=varsds, t="l", lwd=2,
  ylim=range(c(varsds, cvarsds)),
  xlab="confidence level", ylab="standard error",
  main="Scaled Standard Errors of CVaR and VaR")
lines(x=names(cvarsds), y=cvarsds, lwd=2, col="red")
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
   title=NULL, inset=0.05, cex=1.0, bg="white",
   y.intersp=0.1, lwd=6, lty=1, col=c("red", "black"))
calc_var <- function(probv, # Default probabilities
   lgd=0.6, # loss given default
   rhos, rhosm, # asset correlation
   nsimu=1000, # number of simulations
   confls=seq(0.93, 0.99, 0.01) # Confidence levels
   ) {
  # Calculate random default thresholds
  threshv <- qnorm(runif(1, min=0.5, max=1.5)*probv)
  # Simulate losses under the Vasicek model
  nbonds <- NROW(probv)
  sysv <- rnorm(nsimu)
  assetm <- matrix(rnorm(nsimu*nbonds), ncol=nsimu)
  assetm <- t(rhos*sysv + t(rhosm*assetm))
  lossm <- lgd*colSums(assetm < threshv)/nbonds
  # Calculate VaRs and CVaRs
  vars <- quantile(lossm, probs=confls)
  cvars <- sapply(vars, function(varisk) {
    mean(lossm[lossm >= varisk])
  })  # end sapply
  names(vars) <- confls
  names(cvars) <- confls
  c(vars, cvars)
}  # end calc_var
library(parallel)  # load package parallel
ncores <- detectCores() - 1  # number of cores
compclust <- makeCluster(ncores)  # Initialize compute cluster
# Perform bootstrap of calc_var for Windows
clusterSetRNGStream(compclust, 1121)
bootd <- parLapply(compclust, rep(lgd, nboot),
  fun=calc_var, probv=probv,
  rhos=rhos, rhosm=rhosm,
  nsimu=nsimu, confls=confls)  # end parLapply
stopCluster(compclust)  # Stop R processes over cluster
# Bootstrap under Mac-OSX or Linux
bootd <- mclapply(rep(lgd, nboot),
  FUN=calc_var, probv=probv,
  rhos=rhos, rhosm=rhosm,
  nsimu=nsimu, confls=confls)  # end mclapply
bootd <- rutils::do_call(rbind, bootd)
# Calculate standard errors of VaR and CVaR from bootd data
varsd <- apply(bootd[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
cvarsd <- apply(bootd[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
varsdsu <- varsd[2, ]/varsd[1, ]
cvarsdsu <- cvarsd[2, ]/cvarsd[1, ]
# Plot the standard errors of VaRs under uncertain default probabilities
plot(x=colnames(varsd), y=varsds, t="l",
 col="black", lwd=2, ylim=range(c(varsds, varsdsu)),
  xlab="confidence level", ylab="standard error",
  main="Standard Errors of VaR
  with Random Default Probabilities")
lines(x=colnames(varsd), y=varsdsu, lwd=2, col="red")
legend(x="topleft",
   legend=c("VaR Fixed Def Probs", "VaR Random Def Probs"),
   bty="n", title=NULL, inset=0.05, cex=1.0, bg="white",
   y.intersp=0.3, lwd=6, lty=1, col=c("black", "red"))
NA
# Plot the standard errors of VaRs and CVaRs
plot(x=colnames(varsd), y=varsdsu, t="l", lwd=2,
  ylim=range(c(varsdsu, cvarsdsu)),
  xlab="confidence level", ylab="standard error",
  main="Relative Standard Errors of VaR and CVaR
  with Uncertain Default Probabilities")
lines(x=colnames(varsd), y=cvarsdsu, lwd=2, col="red")
legend(x="topright", legend=c("CVaR", "VaR"), bty="n",
   title=NULL, inset=0.05, cex=1.0, bg="white",
   y.intersp=0.1, lwd=6, lty=1, col=c("red", "black"))
