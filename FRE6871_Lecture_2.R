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
  vecvized = (vector1 + vector2),
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
  vecvized = cumsum(big_vector),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Calculate matrix of random data with 5,000 rows
matv <- matrix(rnorm(10000), ncol=2)
# Calculate row sums two different ways
all.equal(rowSums(matv), apply(matv, 1, sum))
summary(microbenchmark(
  rowsumv = rowSums(matv),
  apply = apply(matv, 1, sum),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

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
set.seed(1121)
sapply(stdevs, function(stdev) rnorm(n=2, sd=stdev))
# Same
set.seed(1121)
sapply(stdevs, rnorm, n=2, mean=0)
# Loop over means
set.seed(1121)
sapply(means, function(meanv) rnorm(n=2, mean=meanv))
# Same
set.seed(1121)
sapply(means, rnorm, n=2)

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
  # parallel = parLapply(cluster, 1:10, paws),
  parallel = mclapply(1:10, paws, mc.cores=ncores),
  times=10)
)[, c(1, 4, 5)]

# Compare speed of lapply with parallel computing
runv <- 3:10
timev <- sapply(runv, function(nruns) {
    summary(microbenchmark(
standard = lapply(1:nruns, paws),
# parallel = parLapply(cluster, 1:nruns, paws),
parallel = mclapply(1:nruns, paws, mc.cores=ncores),
times=10))[, 4]
    })  # end sapply
timev <- t(timev)
colnames(timev) <- c("standard", "parallel")
rownames(timev) <- runv
# Stop R processes over cluster under Windows
stopCluster(cluster)

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
cluster <- makeCluster(ncores)
# Calculate matrix of random data
matv <- matrix(rnorm(1e5), ncol=100)
# Define aggregation function over column of matrix
aggfun <- function(column) {
  output <- 0
  for (indeks in 1:NROW(column))
    output <- output + column[indeks]
  output
}  # end aggfun
# Perform parallel aggregations over columns of matrix
aggs <- parCapply(cluster, matv, aggfun)
# Compare speed of apply with parallel computing
summary(microbenchmark(
  apply=apply(matv, MARGIN=2, aggfun),
  parapply=parCapply(cluster, matv, aggfun),
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
parLapply(cluster, 2:4, function(exponent) basep^exponent)
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

set.seed(1121)  # Reset random number generator
nsimu <- 1000
# Bootstrap of sample mean and median
nboot <- 100
bootd <- sapply(1:nboot, function(x) median(rnorm(nsimu)))
# Perform vectorized bootstrap
set.seed(1121)  # Reset random number generator
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
cluster <- makeCluster(ncores)  # Initialize compute cluster under Windows
set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
nsimu <- 1000
# Bootstrap mean and median under Windows
nboot <- 10000
bootd <- parLapply(cluster, 1:nboot,
  function(x, datav, nsimu) {
  samplev <- rnorm(nsimu)
  c(mean=mean(samplev), median=median(samplev))
  }, datav=datav, nsimu*nsimu)  # end parLapply
# Bootstrap mean and median under Mac-OSX or Linux
bootd <- mclapply(1:nboot,
  function(x) {
  samplev <- rnorm(nsimu)
  c(mean=mean(samplev), median=median(samplev))
  }, mc.cores=ncores)  # end mclapply
bootd <- rutils::do_call(rbind, bootd)
# Means and standard errors from bootstrap
apply(bootd, MARGIN=2, function(x) c(mean=mean(x), stderror=sd(x)))
# Standard error from formula
sd(datav)/sqrt(nsimu)
stopCluster(cluster)  # Stop R processes over cluster under Windows

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
cluster <- makeCluster(ncores)  # Initialize compute cluster
bootd <- parLapply(cluster, 1:nboot,
  function(x, datav) {
    samplev <- rnorm(nsimu)
    c(sd=sd(samplev), mad=mad(samplev))
  }, datav=datav)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:nboot, function(x) {
  samplev <- rnorm(nsimu)
  c(sd=sd(samplev), mad=mad(samplev))
}, mc.cores=ncores)  # end mclapply
stopCluster(cluster)  # Stop R processes over cluster
bootd <- rutils::do_call(rbind, bootd)
# Means and standard errors from bootstrap
apply(bootd, MARGIN=2, function(x) c(mean=mean(x), stderror=sd(x)))

# Calculate time series of VTI returns
library(rutils)
retp <- rutils::etfenv$returns$VTI
retp <- na.omit(retp)
nsimu <- NROW(retp)
# Sample from VTI returns
samplev <- retp[sample.int(nsimu, replace=TRUE)]
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
nsimu <- NROW(retp)
# Bootstrap sd and MAD under Windows
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
cluster <- makeCluster(ncores)  # Initialize compute cluster under Windows
clusterSetRNGStream(cluster, 1121)  # Reset random number generator in all cores
nboot <- 10000
bootd <- parLapply(cluster, 1:nboot,
  function(x, retp, nsimu) {
    samplev <- retp[sample.int(nsimu, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, retp=retp, nsimu*nsimu)  # end parLapply
# Bootstrap sd and MAD under Mac-OSX or Linux
bootd <- mclapply(1:nboot, function(x) {
    samplev <- retp[sample.int(nsimu, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, mc.cores=ncores)  # end mclapply
stopCluster(cluster)  # Stop R processes over cluster under Windows
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
set.seed(1121)
# Define predictor and response variables
nsimu <- 100
predm <- rnorm(nsimu, mean=2)
noisev <- rnorm(nsimu)
respv <- (-3 + 2*predm + noisev)
desm <- cbind(respv, predm)
# Calculate alpha and beta regression coefficients
betav <- cov(desm[, 1], desm[, 2])/var(desm[, 2])
alpha <- mean(desm[, 1]) - betav*mean(desm[, 2])
x11(width=6, height=5)
plot(respv ~ predm, data=desm)
abline(a=alpha, b=betav, lwd=3, col="blue")
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
cluster <- makeCluster(ncores)  # Initialize compute cluster under Windows
# Bootstrap of regression under Windows
bootd <- parLapply(cluster, 1:1000,
  function(x, desm) {
    samplev <- sample.int(nsimu, replace=TRUE)
    desm <- desm[samplev, ]
    cov(desm[, 1], desm[, 2])/var(desm[, 2])
  }, design=desm)  # end parLapply
# Bootstrap of regression under Mac-OSX or Linux
bootd <- mclapply(1:1000,
  function(x) {
    samplev <- sample.int(nsimu, replace=TRUE)
    desm <- desm[samplev, ]
    cov(desm[, 1], desm[, 2])/var(desm[, 2])
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

set.seed(1121)  # Reset random number generator
barl <- 20  # Barrier level
nsimu <- 1000  # Number of simulation steps
# Simulate path of Brownian motion
pathv <- cumsum(rnorm(nsimu))
# Find index when path crosses barl
crossp <- which(pathv > barl)
# Fill remaining path after it crosses barl
if (NROW(crossp)>0) {
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
set.seed(1121)
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
ordern <- order(pathm[nsimu, ])
pathm[nsimu, ordern]
indeks <- ordern[seq(1, 100, 9)]
zoo::plot.zoo(pathm[, indeks], main="Paths of Brownian Motion",
  xlab="time steps", ylab=NA, plot.type="single")
abline(h=strikep, col="red", lwd=3)
text(x=(nsimu-60), y=strikep, labels="strike price", pos=3, cex=1)

set.seed(1121)  # Reset random number generator
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

set.seed(1121) # Reset random number generator
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
set.seed(1121) # Reset random number generator
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
set.seed(1121)
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
