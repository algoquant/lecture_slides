numv <- 0.3/3
numv  # Printed as "0.1"
numv - 0.1  # numv is not equal to "0.1"
numv == 0.1  # numv is not equal to "0.1"
print(numv, digits=10)
print(numv, digits=16)
# numv is equal to "0.1" within machine precision
all.equal(numv, 0.1)
numv <- (3-2.9)
print(numv, digits=20)
# Info machine precision of computer R is running on
# ?.Machine
# Machine precision
.Machine$double.eps
numv <- sqrt(2)
numv^2  # Printed as "2"
numv^2 == 2  # numv^2 is not equal to "2"
print(numv^2, digits=20)
# numv^2 is equal to "2" within machine precision
all.equal(numv^2, 2)
# Numbers with precision 0.1
0.1*(1:10)
# Round to precision 0.1
round(3.675, 1)
# Round to precision 1.0
round(3.675)
# Round to nearest even number
c(round(2.5), round(3.5), round(4.5))
round(4:20/2)  # Round to nearest even number
trunc(3.675)  # Truncate
numv <- 2
numv==2
identical(numv, 2)
identical(numv, NULL)
# This doesn't work:
# numv==NULL
is.null(numv)
vecv <- c(2, 4, 6)
vecv == 2
identical(vecv, 2)
# numv is equal to "1.0" within machine precision
numv <- 1.0 + 2*sqrt(.Machine$double.eps)
all.equal(numv, 1.0)
# Info machine precision of computer R is running on
# ?.Machine
# Machine precision
.Machine$double.eps
4.7 %/% 0.5  # Modulo division
4.7 %% 0.5  # Remainder of modulo division
# Reversing modulo division usually
# returns the original number
(4.7 %% 0.5) + 0.5 * (4.7 %/% 0.5)
# Modulo division of non-integer numbers can
# produce incorrect results
0.6 %/% 0.2  # Produces 2 instead of 3
6 %/% 2  # Use integers to get correct result
# 0.2 stored as binary number
# Slightly larger than 0.2
print(0.2, digits=22)
# Get help for integrate()
?integrate
# Calculate slowly converging integral
func <- function(x) {1/((x+1)*sqrt(x))}
integrate(func, lower=0, upper=10)
integrate(func, lower=0, upper=Inf)
# Integrate function with parameter lambdaf
func <- function(x, lambdaf=1) {
  exp(-x*lambdaf)
}  # end func
integrate(func, lower=0, upper=Inf)
integrate(func, lower=0, upper=Inf, lambdaf=2)
# Cumulative probability over normal distribution
pnorm(-2)
integrate(dnorm, low=2, up=Inf)
str(dnorm)
pnorm(-1)
integrate(dnorm, low=2, up=Inf, mean=1)
# Expected value over normal distribution
integrate(function(x) x*dnorm(x),
    low=2, up=Inf)
library(rutils)  # Load package rutils
# Calculate VTI percentage returns
retp <- rutils::etfenv$returns$VTI
retp <- drop(coredata(na.omit(retp)))
nrows <- NROW(retp)
# Mean and standard deviation of returns
c(mean(retp), sd(retp))
# Calculate the MAD of returns 10 points apart
retp <- sort(retp)
bwidth <- 10*mad(rutils::diffit(retp, lagg=10))
# Calculate the kernel density
densv <- sapply(1:nrows, function(it) {
  sum(dnorm(retp-retp[it], sd=bwidth))
})  # end sapply
madv <- mad(retp)
plot(retp, densv, xlim=c(-5*madv, 5*madv),
     t="l", col="blue", lwd=3,
     xlab="returns", ylab="density",
     main="Density of VTI Returns")
# Calculate the kernel density using density()
densv <- density(retp, bw=bwidth)
NROW(densv$y)
x11(width=6, height=5)
plot(densv, xlim=c(-5*madv, 5*madv),
     xlab="returns", ylab="density",
     col="blue", lwd=3, main="Density of VTI Returns")
# Interpolate the densv vector into returns
densv <- approx(densv$x, densv$y, xout=retp)
all.equal(densv$x, retp)
plot(densv, xlim=c(-5*madv, 5*madv),
     xlab="returns", ylab="density",
     t="l", col="blue", lwd=3,
     main="Density of VTI Returns")
# Plot histogram
histp <- hist(retp, breaks=100, freq=FALSE,
  xlim=c(-5*madv, 5*madv), xlab="", ylab="",
  main="VTI Return Distribution")
# Draw kernel density of histogram
lines(densv, col="red", lwd=2)
# Add density of normal distribution
curve(expr=dnorm(x, mean=mean(retp), sd=sd(retp)),
add=TRUE, type="l", lwd=2, col="blue")
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("VTI", "Normal"), bty="n", y.intersp=0.4,
 lwd=6, bg="white", col=c("red", "blue"))
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
library(SOAR)  # Load package SOAR
# Get sizes of objects in workspace
sort(sapply(mget(ls()), object.size))
Store(etf_list)  # Store in object cache
# Get sizes of objects in workspace
sort(sapply(mget(ls()), object.size))
search()  # Get search path for R objects
ls()  # List object cache
find("etf_list")  # Find object on search path
# Get R memory
vcells <- gc()["Vcells", "used"]
# Create vector with 1,000,000 elements
numv <- numeric(1000000)
# Get extra R memory
gc()["Vcells", "used"] - vcells
# Get total size of all objects in workspace
print(object.size(x=mget(ls())), units="MB")
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
# Define functions for profiling
profun <- function() {fastfun(); slowfun()}
fastfun <- function() Sys.sleep(0.1)
slowfun <- function() Sys.sleep(0.2)
# Turn on profiling
Rprof(filename="/Users/jerzy/Develop/data_def/profile.out")
# Run code for profiling
replicate(n=10, profun())
# Turn off profiling
Rprof(NULL)
# Compile summary of profiling from file
summaryRprof("/Users/jerzy/Develop/data_def/profile.out")
# Profile plotting of regression
profvis::profvis({
  plot(price ~ carat, data=ggplot2::diamonds)
  model <- lm(price ~ carat, data=ggplot2::diamonds)
  abline(model, col="red")
})  # end profvis
# Four methods of calculating column means of matrix
matv <- matrix(rnorm(1e5), ncol=5e4)
profvis::profvis({
  meanv <- apply(matv, 2, mean)
  meanv <- colMeans(matv)
  meanv <- lapply(matv, mean)
  meanv <- vapply(matv, mean, numeric(1))
})  # end profvis
# Four methods of calculating data frame column means
dframe <- as.data.frame(matv)
profvis::profvis({
  meanv <- apply(dframe, 2, mean)
  meanv <- colMeans(dframe)
  meanv <- lapply(dframe, mean)
  meanv <- vapply(dframe, mean, numeric(1))
})  # end profvis
# Profile a shiny app
profvis::profvis(
  shiny::runExample(example="06_tabsets",
            display.mode="normal")
)  # end profvis
# Calculate cumulative sum of a vector
vecv <- runif(1e5)
# Use compiled function
cumsumv <- cumsum(vecv)
# Use for loop
cumsumv2 <- vecv
for (i in 2:NROW(vecv))
  cumsumv2[i] <- (vecv[i] + cumsumv2[i-1])
# Compare the two methods
all.equal(cumsumv, cumsumv2)
# Microbenchmark the two methods
library(microbenchmark)
summary(microbenchmark(
  cumsum=cumsum(vecv),
  loop_alloc={
    cumsumv2 <- vecv
    for (i in 2:NROW(vecv))
cumsumv2[i] <- (vecv[i] + cumsumv2[i-1])
  },
  loop_nalloc={
    # Doesn't allocate memory to cumsumv3
    cumsumv3 <- vecv[1]
    for (i in 2:NROW(vecv))
# This command adds an extra element to cumsumv3
cumsumv3[i] <- (vecv[i] + cumsumv3[i-1])
  },
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
mapply(function(input, expv) input^expv,
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
stderr <- apply(bootd, MARGIN=2,
  function(x) c(mean=mean(x), stderror=sd(x)))
stderr
# Relative standard errors
stderr[2, ]/stderr[1, ]
# Calculate log returns from VTI prices
library(rutils)
pricev <- quantmod::Cl(rutils::etfenv$VTI)
pricev <- log(as.numeric(pricev))
nrows <- NROW(pricev)
prici <- pricev[1]
retp <- rutils::diffit(pricev)
class(retp); head(retp)
sum(is.na(retp))
# Define barrier level with respect to prices
barl <- 2*max(pricev)
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
  samplev <- prici*cumsum(samplev)
  # Calculate if prices crossed barrier
  sum(samplev > barl) > 0
}, retp=retp, nrows=nrows)  # end parLapply
stopCluster(compclust)  # Stop R processes over cluster under Windows
# Perform parallel bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:nboot, function(x) {
  samplev <- retp[sample.int(nrows, replace=TRUE)]
  # Calculate prices from percentage returns
  samplev <- prici*cumsum(samplev)
  # Calculate if prices crossed barrier
  sum(samplev > barl) > 0
}, mc.cores=ncores)  # end mclapply
bootd <- rutils::do_call(c, bootd)
# Calculate frequency of crossing barrier
sum(bootd)/nboot
# Define barrier level with respect to the prices
barl <- 0.1*max(pricev)
# Define time horizon of 1 year in days
holdp <- 252
# Sample the start dates for the bootstrap
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
startd <- sample.int(nrows-holdp, nboot, replace=TRUE)
# Bootstrap the cumulative returns
samplev <- sapply(startd, function(x) {
  pricev[x+holdp-1] - pricev[x]
})  # end sapply
# Faster way to calculate the cumulative returns
samplev <- pricev[startd+holdp-1] - pricev[startd]
# Calculate if any of the returns are above the barrier
sum(samplev > barl) > 0
# Plot the cumulative returns
densv <- density(samplev)
plot(densv, xlab="return", main="Return density")
abline(v=barl, col="red", lwd=2)
text(x=barl, y=0.7*max(densv$y), pos=2, "barrier")
# Bootstrap the whole paths of cumulative returns
samplev <- sapply(startd, function(x) {
  pricev[x:(x+holdp-1)] - pricev[x]
})  # end sapply
dim(samplev)
samplev[1:5, 1:5]
# Calculate which of the paths crossed the barrier
apply(samplev, 2, function(x) {sum(x > barl) > 0})
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
nsimu <- 1000  # Number of simulation paths
# Calculate matrix of normal variables
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
datav <- rnorm(nsimu*nsteps, mean=drift, sd=sigmav)
datav <- matrix(datav, nc=nsimu)
# Simulate paths of Brownian motion
pathm <- matrixStats::colCumsums(datav)
# Tilt the datav
lambdaf <- 0.1  # Tilt parameter
datat <- datav + lambdaf  # Tilt the random numbers
patht <- matrixStats::colCumsums(datat)
zoo::plot.zoo(patht[, sample(nsimu, 20)], main="Paths of Brownian Motion", xlab="time steps", ylab=NA, plot.type="single")
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
# Display the structure of optimize()
str(optimize)
# Objective function with multiple minima
objfun <- function(input, param1=0.01) {
  sin(0.25*pi*input) + param1*(input-1)^2
}  # end objfun
optiml <- optimize(f=objfun, interval=c(-4, 2))
class(optiml)
unlist(optiml)
# Find minimum in different interval
unlist(optimize(f=objfun, interval=c(0, 8)))
# Find minimum with less accuracy
accl <- 1e4*.Machine$double.eps^0.25
unlist(optimize(f=objfun, interval=c(0, 8), tol=accl))
# Microbenchmark optimize() with less accuracy
library(microbenchmark)
summary(microbenchmark(
  more_accurate = optimize(f=objfun, interval=c(0, 8)),
  less_accurate = optimize(f=objfun, interval=c(0, 8), tol=accl),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
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
rastrigin <- function(vecv, param=25) {
  sum(vecv^2 - param*cos(vecv))
}  # end rastrigin
vecv <- c(pi, pi/4)
rastrigin(vecv=vecv)
# Draw 3d surface plot of Rastrigin function
rgl::persp3d(
  x=Vectorize(function(x, y) rastrigin(vecv=c(x, y))),
  xlim=c(-10, 10), ylim=c(-10, 10),
  col="green", axes=FALSE, zlab="", main="rastrigin")
# Render the 3d surface plot of function
rgl::rglwidget(elementId="plot3drgl", width=400, height=400)
# Optimize with respect to vector argument
optiml <- optim(par=vecv, fn=rastrigin,
        method="L-BFGS-B",
        upper=c(14*pi, 14*pi),
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
  sum(2*log(parv[2]) + ((datav - parv[1])/parv[2])^2)
}  # end objfun
# Objective function on parameter grid
parmean <- seq(1, 6, length=50)
parsd <- seq(0.5, 3.0, length=50)
objgrid <- sapply(parmean, function(m) {
  sapply(parsd, function(sd) {
    objfun(c(m, sd), datav)
  })  # end sapply
})  # end sapply
# Perform grid search for minimum
objmin <- which(objgrid == min(objgrid), arr.ind=TRUE)
objmin
parmean[objmin[1]]  # mean
parsd[objmin[2]]  # sd
objgrid[objmin]
objgrid[(objmin[, 1] + -1:1), (objmin[, 2] + -1:1)]
# Or create parameter grid using function outer()
objfunv <- Vectorize(
  FUN=function(mean, sd, datav) objfun(c(mean, sd), datav),
  vectorize.args=c("mean", "sd")
)  # end Vectorize
objgrid <- outer(parmean, parsd, objfunv, datav=datav)
# Perspective plot of log-likelihood function
persp(z=-objgrid,
      theta=45, phi=30, shade=0.5,
      border="green", zlab="objective",
      main="objective function")
# Interactive perspective plot of log-likelihood function
library(rgl)  # Load package rgl
rgl::par3d(cex=2.0)  # Scale text by factor of 2
rgl::persp3d(z=-objgrid, zlab="objective",
col="green", main="objective function")
# Render the 3d surface plot of function
rgl::rglwidget(elementId="plot3drgl", width=400, height=400)
# Initial parameters
initp <- c(mean=0, sd=1)
# Perform optimization using optim()
optiml <- optim(par=initp,
  fn=objfun, # Log-likelihood function
  datav=datav,
  method="L-BFGS-B", # Quasi-Newton method
  upper=c(10, 10), # Upper constraint
  lower=c(-10, 0.1)) # Lower constraint
# Optimal parameters
optiml$par
# Perform optimization using MASS::fitdistr()
optiml <- MASS::fitdistr(datav, densfun="normal")
optiml$estimate
optiml$sd
# Plot histogram
histp <- hist(datav, plot=FALSE)
plot(histp, freq=FALSE, main="histogram of sample")
curve(expr=dnorm(x, mean=optiml$par["mean"], sd=optiml$par["sd"]),
add=TRUE, type="l", lwd=2, col="red")
legend("topright", leg="optimal parameters",
   inset=0.0, cex=0.8, title=NULL, y.intersp=0.4,
   bty="n", lwd=2, bg="white", col="red")
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
objfunv <- Vectorize(
  FUN=function(mean, sd, w, m1, s1, datav)
    objfun(c(w, m1, s1, mean, sd), datav),
  vectorize.args=c("mean", "sd")
)  # end Vectorize
# Objective function on parameter grid
parmean <- seq(3, 5, length=50)
parsd <- seq(0.5, 1.5, length=50)
objgrid <- outer(parmean, parsd,
    objfunv, datav=datav,
    w=0.5, m1=2.0, s1=2.0)
rownames(objgrid) <- round(parmean, 2)
colnames(objgrid) <- round(parsd, 2)
objmin <- which(objgrid==
  min(objgrid), arr.ind=TRUE)
objmin
objgrid[objmin]
objgrid[(objmin[, 1] + -1:1),
         (objmin[, 2] + -1:1)]
# Perspective plot of objective function
persp(parmean, parsd, -objgrid,
theta=45, phi=30,
shade=0.5,
col=rainbow(50),
border="green",
main="objective function")
# Initial parameters
initp <- c(weight=0.5, m1=0, s1=1, m2=2, s2=1)
# Perform optimization
optiml <- optim(par=initp,
      fn=objfun,
      datav=datav,
      method="L-BFGS-B",
      upper=c(1,10,10,10,10),
      lower=c(0,-10,0.2,-10,0.2))
optiml$par
# Plot histogram
histp <- hist(datav, plot=FALSE)
plot(histp, freq=FALSE,
     main="histogram of sample")
fitfun <- function(x, parv) {
  parv["weight"]*dnorm(x, mean=parv["m1"], sd=parv["s1"]) +
  (1-parv["weight"])*dnorm(x, mean=parv["m2"], sd=parv["s2"])
}  # end fitfun
curve(expr=fitfun(x, parv=optiml$par), add=TRUE,
type="l", lwd=2, col="red")
legend("topright", leg="optimal parameters", inset=0.0,
 cex=0.8, title=NULL, y.intersp=0.4, bty="n",
 lwd=2, bg="white", col="red")
# Rastrigin function with vector argument for optimization
rastrigin <- function(vecv, param=25) {
  sum(vecv^2 - param*cos(vecv))
}  # end rastrigin
vecv <- c(pi/6, pi/6)
rastrigin(vecv=vecv)
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
# Rastrigin function with vector argument for optimization
rastrigin <- function(vecv, param=25) {
  sum(vecv^2 - param*cos(vecv))
}  # end rastrigin
vecv <- c(pi/6, pi/6)
rastrigin(vecv=vecv)
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
# Verify that Rtools or XCode are working properly:
devtools::find_rtools()  # Under Windows
devtools::has_devel()
# Install the packages Rcpp and RcppArmadillo
install.packages(c("Rcpp", "RcppArmadillo"))
# Load package Rcpp
library(Rcpp)
# Get documentation for package Rcpp
# Get short description
packageDescription("Rcpp")
# Load help page
help(package="Rcpp")
# List all datasets in "Rcpp"
data(package="Rcpp")
# List all objects in "Rcpp"
ls("package:Rcpp")
# Remove Rcpp from search path
detach("package:Rcpp")
# Define Rcpp function
Rcpp::cppFunction("
  int times_two(int x)
    { return 2 * x;}
  ")  # end cppFunction
# Run Rcpp function
times_two(3)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/mult_rcpp.cpp")
# Multiply two numbers
mult_rcpp(2, 3)
mult_rcpp(1:3, 6:4)
# Multiply two vectors
mult_vec_rcpp(2, 3)
mult_vec_rcpp(1:3, 6:4)
# Define Rcpp function with loop
Rcpp::cppFunction("
double inner_mult(NumericVector x, NumericVector y) {
int xsize = x.size();
int ysize = y.size();
if (xsize != ysize) {
    return 0;
  } else {
    double total = 0;
    for(int i = 0; i < xsize; ++i) {
total += x[i] * y[i];
  }
  return total;
  }
}")  # end cppFunction
# Run Rcpp function
inner_mult(1:3, 6:4)
inner_mult(1:3, 6:3)
# Define Rcpp Sugar function with loop
Rcpp::cppFunction("
double inner_sugar(NumericVector x, NumericVector y) {
  return sum(x * y);
}")  # end cppFunction
# Run Rcpp Sugar function
inner_sugar(1:3, 6:4)
inner_sugar(1:3, 6:3)
# Define R function with loop
inner_multr <- function(x, y) {
    sumv <- 0
    for(i in 1:NROW(x)) {
sumv <- sumv + x[i] * y[i]
    }
    sumv
}  # end inner_multr
# Run R function
inner_multr(1:3, 6:4)
inner_multr(1:3, 6:3)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=inner_multr(1:10000, 1:10000),
  innerp=1:10000 %*% 1:10000,
  Rcpp=inner_mult(1:10000, 1:10000),
  sugar=inner_sugar(1:10000, 1:10000),
  times=10))[, c(1, 4, 5)]
# Define Ornstein-Uhlenbeck function in R
nboot <- 1000
bootd <- function(datav, nboot=nboot) {
  bootd <- sapply(1:nboot, function(x) {
    samplev <- datav[sample.int(nsimu, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  })  # end sapply
  bootd <- t(bootd)
  # Analyze bootstrapped variance
  head(bootd)
  sum(is.na(bootd))
  # Means and standard errors from bootstrap
  apply(bootd, MARGIN=2,
    function(x) c(mean=mean(x), stderror=sd(x)))
  retp <- numeric(nsimu)
  pricev <- numeric(nsimu)
  pricev[1] <- priceq
  for (i in 2:nsimu) {
    retp[i] <- thetav*(priceq - pricev[i-1]) + volat*rnorm(1)
    pricev[i] <- pricev[i-1] + retp[i]
  }  # end for
  pricev
}  # end bootd
# Define Ornstein-Uhlenbeck function in R
sim_our <- function(nrows=1000, priceq=5.0,
              volat=0.01, theta=0.01) {
  retp <- numeric(nrows)
  pricev <- numeric(nrows)
  pricev[1] <- priceq
  for (i in 2:nrows) {
    retp[i] <- theta*(priceq - pricev[i-1]) + volat*rnorm(1)
    pricev[i] <- pricev[i-1] + retp[i]
  }  # end for
  pricev
}  # end sim_our
# Simulate Ornstein-Uhlenbeck process in R
priceq <- 5.0; sigmav <- 0.01
thetav <- 0.01; nrows <- 1000
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
ousim <- sim_our(nrows, priceq=priceq, volat=sigmav, theta=thetav)
# Define Ornstein-Uhlenbeck function in Rcpp
Rcpp::cppFunction("
NumericVector sim_oucpp(double priceq,
                  double volat,
                  double thetav,
                  NumericVector innov) {
  int nrows = innov.size();
  NumericVector pricev(nrows);
  NumericVector retv(nrows);
  pricev[0] = priceq;
  for (int it = 1; it < nrows; it++) {
    retv[it] = thetav*(priceq - pricev[it-1]) + volat*innov[it-1];
    pricev[it] = pricev[it-1] + retv[it];
  }  // end for
  return pricev;
}")  # end cppFunction
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
oucpp <- sim_oucpp(priceq=priceq,
  volat=sigmav, theta=thetav, innov=rnorm(nrows))
all.equal(ousim, oucpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=sim_our(nrows, priceq=priceq, volat=sigmav, theta=thetav),
  Rcpp=sim_oucpp(priceq=priceq, volat=sigmav, theta=thetav, innov=rnorm(nrows)),
  times=10))[, c(1, 4, 5)]
# Source Rcpp function for Ornstein-Uhlenbeck process from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_ou.cpp")
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
oucpp <- sim_oucpp(priceq=priceq,
  volat=sigmav,
  theta=thetav,
  innov=rnorm(nrows))
all.equal(ousim, oucpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  rcode=sim_our(nrows, priceq=priceq, volat=sigmav, theta=thetav),
  Rcpp=sim_oucpp(priceq=priceq, volat=sigmav, theta=thetav, innov=rnorm(nrows)),
  times=10))[, c(1, 4, 5)]
# Calculate uniformly distributed pseudo-random sequence
unifun <- function(seedv, nrows=10) {
  datav <- numeric(nrows)
  datav[1] <- seedv
  for (i in 2:nrows) {
    datav[i] <- 4*datav[i-1]*(1-datav[i-1])
  }  # end for
  acos(1-2*datav)/pi
}  # end unifun
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/unifun.cpp")
# Microbenchmark Rcpp code
library(microbenchmark)
summary(microbenchmark(
  rcode=runif(1e5),
  rloop=unifun(0.3, 1e5),
  Rcpp=unifuncpp(0.3, 1e5),
  times=10))[, c(1, 4, 5)]
# Define Ornstein-Uhlenbeck function in R
bootd <- function(datav, nboot=1000) {
  bootd <- sapply(1:nboot, function(x) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  })  # end sapply
  bootd <- t(bootd)
  # Analyze bootstrapped variance
  head(bootd)
  sum(is.na(bootd))
  # Means and standard errors from bootstrap
  apply(bootd, MARGIN=2,
    function(x) c(mean=mean(x), stderror=sd(x)))
  retp <- numeric(nrows)
  pricev <- numeric(nrows)
  pricev[1] <- priceq
  for (i in 2:nrows) {
    retp[i] <- thetav*(priceq - pricev[i-1]) + volat*rnorm(1)
    pricev[i] <- pricev[i-1] + retp[i]
  }  # end for
  pricev
}  # end bootd
# Simulate Ornstein-Uhlenbeck process in R
priceq <- 5.0; sigmav <- 0.01
thetav <- 0.01; nrows <- 1000
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
ousim <- sim_our(nrows, priceq=priceq, volat=sigmav, theta=thetav)
# Define Ornstein-Uhlenbeck function in R
bootd <- function(datav, nboot=1000) {
  bootd <- sapply(1:nboot, function(x) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  })  # end sapply
  bootd <- t(bootd)
  # Analyze bootstrapped variance
  head(bootd)
  sum(is.na(bootd))
  # Means and standard errors from bootstrap
  apply(bootd, MARGIN=2,
  function(x) c(mean=mean(x), stderror=sd(x)))
  retp <- numeric(nrows)
  pricev <- numeric(nrows)
  pricev[1] <- priceq
  for (i in 2:nrows) {
    retp[i] <- thetav*(priceq - pricev[i-1]) + volat*rnorm(1)
    pricev[i] <- pricev[i-1] + retp[i]
  }  # end for
  pricev
}  # end bootd
# Simulate Ornstein-Uhlenbeck process in R
priceq <- 5.0; sigmav <- 0.01
thetav <- 0.01; nrows <- 1000
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
ousim <- sim_our(nrows, priceq=priceq, volat=sigmav, theta=thetav)
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillo_functions.cpp")
vec1 <- runif(1e5)
vec2 <- runif(1e5)
inner_vec(vec1, vec2)
vec1 %*% vec2
# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcpp = inner_vec(vec1, vec2),
  rcode = (vec1 %*% vec2),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Microbenchmark shows:
# inner_vec() is several times faster than %*%, especially for longer vectors.
#     expr     mean   median
# 1 inner_vec 110.7067 110.4530
# 2 rcode 585.5127 591.3575
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_arima.cpp")
# Define AR(2) coefficients
coeff <- c(0.9, 0.09)
nrows <- 1e4
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
innov <- rnorm(nrows)
# Simulate ARIMA using filter()
arimar <- filter(x=innov, filter=coeff, method="recursive")
# Simulate ARIMA using sim_ar()
innov <- matrix(innov)
coeff <- matrix(coeff)
arimav <- sim_ar(coeff, innov)
all.equal(drop(arimav), as.numeric(arimar))
# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcpp = sim_ar(coeff, innov),
  filter = filter(x=innov, filter=coeff, method="recursive"),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillo_functions.cpp")
matv <- matrix(runif(1e5), nc=1e3)
# Center matrix columns using apply()
matd <- apply(matv, 2, function(x) (x-mean(x)))
# Center matrix columns in place using Rcpp demeanr()
demeanr(matv)
all.equal(matd, matv)
# Microbenchmark \emph{RcppArmadillo} code
library(microbenchmark)
summary(microbenchmark(
  rcode = (apply(matv, 2, mean)),
  rcpp = demeanr(matv),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Perform matrix inversion
# Create random positive semi-definite matrix
matv <- matrix(runif(25), nc=5)
matv <- t(matv) %*% matv
# Invert the matrix
matrixinv <- solve(matv)
inv_mat(matv)
all.equal(matrixinv, matv)
# Microbenchmark \emph{RcppArmadillo} code
summary(microbenchmark(
  rcode = solve(matv),
  rcpp = inv_mat(matv),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/HighFreq.cpp")
# Calculate matrix of random returns
matv <- matrix(rnorm(300), nc=5)
# Reduced inverse of correlation matrix
dimax <- 4
cormat <- cor(matv)
eigend <- eigen(cormat)
invmat <- eigend$vectors[, 1:dimax] %*%
  (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])
# Reduced inverse using \emph{RcppArmadillo}
invarma <- calc_inv(cormat, dimax=dimax)
all.equal(invmat, invarma)
# Microbenchmark \emph{RcppArmadillo} code
library(microbenchmark)
summary(microbenchmark(
  rcode = {eigend <- eigen(cormat)
eigend$vectors[, 1:dimax] %*% (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])},
  rcpp = calc_inv(cormat, dimax=dimax),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Install package reticulate
install.packages("reticulate")
# Start Python session
reticulate::repl_python()
# Exit Python session
exit
