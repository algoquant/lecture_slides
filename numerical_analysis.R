






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

vectorv <- c(2, 4, 6)
vectorv == 2
identical(vectorv, 2)

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
# Integrate function with parameter lambda
func <- function(x, lambda=1) {
  exp(-x*lambda)
}  # end func
integrate(func, lower=0, upper=Inf)
integrate(func, lower=0, upper=Inf,
    lambda=2)
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
returns <- rutils::etfenv$returns$VTI
returns <- drop(coredata(na.omit(returns)))
nrows <- NROW(returns)
# Mean and standard deviation of returns
c(mean(returns), sd(returns))
# Calculate the MAD of returns 10 points apart
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
histp <- hist(returns, breaks=100, freq=FALSE,
  xlim=c(-5*madv, 5*madv), xlab="", ylab="",
  main="VTI Return Distribution")
# Draw kernel density of histogram
lines(densityv, col="red", lwd=2)
# Add density of normal distribution
curve(expr=dnorm(x, mean=mean(returns), sd=sd(returns)),
add=TRUE, type="l", lwd=2, col="blue")
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("VTI", "Normal"), bty="n",
 lwd=6, bg="white", col=c("red", "blue"))

# Get size of an object
vectorv <- runif(1e6)
object.size(vectorv)
format(object.size(vectorv), units="MB")
# Get sizes of objects in workspace
sort(sapply(ls(), function(ob_ject) {
  format(object.size(get(ob_ject)), units="KB")}))
# Get sizes of all objects in workspace
sort(sapply(mget(ls()), object.size))
sort(sapply(mget(ls()), function(ob_ject) {
  format(object.size(ob_ject), units="KB")}
))
# Get total size of all objects in workspace
format(object.size(x=mget(ls())), units="MB")
# Get sizes of objects in rutils::etfenv environment
sort(sapply(ls(rutils::etfenv), function(ob_ject) {
  object.size(get(ob_ject, rutils::etfenv))}))
sort(sapply(mget(ls(rutils::etfenv), rutils::etfenv),
      object.size))
library(gdata)  # Load package gdata
# Get size of data frame columns
gdata::ll(unit="bytes", mtcars)
# Get namesv, class, and size of objects in workspace
ob_jects <- gdata::ll(unit="bytes")
# Sort by memory size (descending)
ob_jects[order(ob_jects[, 2], decreasing=TRUE), ]
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
Ls()  # List object cache
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
vectorv <- runif(1e6)
# sqrt() and "^0.5" are the same
all.equal(sqrt(vectorv), vectorv^0.5)
# sqrt() is much faster than "^0.5"
system.time(vectorv^0.5)
microbenchmark(
  power = vectorv^0.5,
  sqrt = sqrt(vectorv),
  times=10)

library(microbenchmark)
# sum() is a compiled primitive function
sum
# mean() is a generic function
mean
vectorv <- runif(1e6)
# sum() is much faster than mean()
all.equal(mean(vectorv), sum(vectorv)/NROW(vectorv))
summary(microbenchmark(
  mean_fun = mean(vectorv),
  sum_fun = sum(vectorv)/NROW(vectorv),
  times=10))[, c(1, 4, 5)]
# any() is a compiled primitive function
any
# any() is much faster than %in% wrapper for match()
all.equal(1 %in% vectorv, any(vectorv == 1))
summary(microbenchmark(
  in_fun = {1 %in% vectorv},
  any_fun = any(vectorv == 1),
  times=10))[, c(1, 4, 5)]

library(microbenchmark)
matrixv <- matrix(1:9, ncol=3, # Create matrix
  dimnames=list(paste0("row", 1:3),
          paste0("col", 1:3)))
# Create specialized function
matrix_to_dframe <- function(matrixv) {
  ncols <- ncol(matrixv)
  dframe <- vector("list", ncols)  # empty vector
  for (indeks in 1:ncols)  # Populate vector
    dframe <- matrixv[, indeks]
  attr(dframe, "row.names") <-  # Add attributes
    .set_row_names(NROW(matrixv))
  attr(dframe, "class") <- "data.frame"
  dframe  # Return data frame
}  # end matrix_to_dframe
# Compare speed of three methods
summary(microbenchmark(
  matrix_to_dframe(matrixv),
  as.data.frame.matrix(matrixv),
  as.data.frame(matrixv),
  times=10))[, c(1, 4, 5)]

# Calculate matrix of random data with 5,000 rows
matrixv <- matrix(rnorm(10000), ncol=2)
# Allocate memory for row sums
rowsumv <- numeric(NROW(matrixv))
summary(microbenchmark(
  rowsumv = rowSums(matrixv),  # end rowsumv
  applyloop = apply(matrixv, 1, sum),  # end apply
  applyloop = lapply(1:NROW(matrixv), function(indeks)
    sum(matrixv[indeks, ])),  # end lapply
  v_apply = vapply(1:NROW(matrixv), function(indeks)
    sum(matrixv[indeks, ]),
    FUN.VALUE = c(sum=0)),  # end vapply
  s_apply = sapply(1:NROW(matrixv), function(indeks)
    sum(matrixv[indeks, ])),  # end sapply
  forloop = for (i in 1:NROW(matrixv)) {
    rowsumv[i] <- sum(matrixv[i,])
  },  # end for
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

vectorv <- rnorm(5000)
summary(microbenchmark(
# Compiled C++ function
  cpp = cumsum(vectorv),  # end for
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

# Disable JIT
jit_confl <- compiler::enableJIT(0)
# Create inefficient function
my_mean <- function(x) {
  output <- 0; n_elem <- NROW(x)
  for(it in 1:n_elem)
    output <- output + x[it]/n_elem
  output
}  # end my_mean
# Byte-compile function and inspect it
mymeancomp <- compiler::cmpfun(my_mean)
mymeancomp
# Test function
vectorv <- runif(1e3)
all.equal(mean(vectorv), mymeancomp(vectorv), my_mean(vectorv))
# microbenchmark byte-compile function
summary(microbenchmark(
  mean(vectorv),
  mymeancomp(vectorv),
  my_mean(vectorv),
  times=10))[, c(1, 4, 5)]
# Create another inefficient function
sapply2 <- function(x, FUN, ...) {
  output <- vector(length=NROW(x))
  for (it in seq_along(x))
    output[it] <- FUN(x[it], ...)
  output
}  # end sapply2
sapply2_comp <- compiler::cmpfun(sapply2)
all.equal(sqrt(vectorv),
  sapply2(vectorv, sqrt),
  sapply2_comp(vectorv, sqrt))
summary(microbenchmark(
  sqrt(vectorv),
  sapply2_comp(vectorv, sqrt),
  sapply2(vectorv, sqrt),
  times=10))[, c(1, 4, 5)]
# enable JIT
compiler::enableJIT(jit_confl)

# Define functions for profiling
out_er <- function() {fa_st(); sl_ow()}
fa_st <- function() Sys.sleep(0.1)
sl_ow <- function() Sys.sleep(0.2)
# Turn on profiling
Rprof(filename="/Users/jerzy/Develop/data_def/profile.out")
# Run code for profiling
replicate(n=10, out_er())
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
matrixv <- matrix(rnorm(1e5), ncol=5e4)
profvis::profvis({
  mean_s <- apply(matrixv, 2, mean)
  mean_s <- colMeans(matrixv)
  mean_s <- lapply(matrixv, mean)
  mean_s <- vapply(matrixv, mean, numeric(1))
})  # end profvis
# Four methods of calculating data frame column means
data_frame <- as.data.frame(matrixv)
profvis::profvis({
  mean_s <- apply(data_frame, 2, mean)
  mean_s <- colMeans(data_frame)
  mean_s <- lapply(data_frame, mean)
  mean_s <- vapply(data_frame, mean, numeric(1))
})  # end profvis
# Profile a shiny app
profvis::profvis(
  shiny::runExample(example="06_tabsets",
            display.mode="normal")
)  # end profvis

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
  rowsumv = rowSums(matrixv),  # end rowsumv
  applyloop = apply(matrixv, 1, sum),  # end apply
  applyloop = lapply(1:NROW(matrixv), function(indeks)
    sum(matrixv[indeks, ])),  # end lapply
  v_apply = vapply(1:NROW(matrixv), function(indeks)
    sum(matrixv[indeks, ]),
    FUN.VALUE = c(sum=0)),  # end vapply
  s_apply = sapply(1:NROW(matrixv), function(indeks)
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
  parallel = parLapply(cluster, 1:10, paws),
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
barp <- 20  # Barrier level
nrows <- 1000  # Number of simulation steps
paths <- numeric(nrows)  # Allocate path vector
paths[1] <- 0  # Initialize path
indeks <- 2  # Initialize simulation index
while ((indeks <= nrows) && (paths[indeks - 1] < barp)) {
# Simulate next step
  paths[indeks] <- paths[indeks - 1] + rnorm(1)
  indeks <- indeks + 1  # Advance indeks
}  # end while
# Fill remaining paths after it crosses barp
if (indeks <= nrows)
  paths[indeks*nrows] <- paths[indeks - 1]
# Plot the Brownian motion
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
plot(paths, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=barp, lwd=3, col="red")
title(main="Brownian Motion Crossing a Barrier Level", line=0.5)

set.seed(1121)  # Reset random number generator
barp <- 20  # Barrier level
nrows <- 1000  # Number of simulation steps
# Simulate path of Brownian motion
paths <- cumsum(rnorm(nrows))
# Find index when paths crosses barp
crossp <- which(paths > barp)
# Fill remaining paths after it crosses barp
if (NROW(crossp)>0) {
  paths[(crossp[1]+1)/nrows] <- paths[crossp[1]]
}  # end if
# Plot the Brownian motion
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
plot(paths, type="l", col="black",
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
# Calculate option payout at maturity
strikep <- 50  # Strike price
payouts <- (paths[nrows, ] - strikep)
sum(payouts[payouts > 0])/nsimu
# Calculate probability of crossing the barrier at any point
barp <- 50
crossi <- (colSums(paths > barp) > 0)
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
cutoff <- confl/nrows
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
  }, datav=datav, nrows*nrows)  # end parLapply
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
  }, returns=returns, nrows*nrows)  # end parLapply
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
  }, returns=returns, nrows*nrows)  # end parLapply
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
  }, returns=returns, nrows*nrows)  # end parLapply
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
weights <- exp(-lambda*data_tilt + lambda^2/2)
sum((data_tilt < quantilev)*weights)/nrows
# Bootstrap of standard errors of cumulative probability
nboot <- 1000
boot_data <- sapply(1:nboot, function(x) {
  datav <- rnorm(nrows)
  naivemc <- sum(datav < quantilev)/nrows
  datav <- (datav + lambda)
  weights <- exp(-lambda*datav + lambda^2/2)
  isample <- sum((datav < quantilev)*weights)/nrows
  c(naivemc=naivemc, importmc=isample)
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
weights <- exp(-lambda*data_tilt + lambda^2/2)
# Cumulative probabilities using importance sample
cumprob <- cumsum(weights)/nrows
# Quantile from importance sample
data_tilt[findInterval(confl, cumprob)]
# Bootstrap of standard errors of quantile
nboot <- 1000
boot_data <- sapply(1:nboot, function(x) {
  datav <- sort(rnorm(nrows))
  naivemc <- datav[cutoff]
  data_tilt <- datav + lambda
  weights <- exp(-lambda*data_tilt + lambda^2/2)
  cumprob <- cumsum(weights)/nrows
  isample <- data_tilt[findInterval(confl, cumprob)]
  c(naivemc=naivemc, importmc=isample)
}) # end sapply
apply(boot_data, MARGIN=1,
  function(x) c(mean=mean(x), sd=sd(x)))

# VaR and CVaR from Naive Monte Carlo
varisk <- datav[cutoff]
sum((datav < varisk)*datav)/sum((datav < varisk))
# CVaR from importance sample
varisk <- data_tilt[findInterval(confl, cumprob)]
sum((data_tilt < varisk)*data_tilt*weights)/sum((data_tilt < varisk)*weights)
# CVaR from integration
integrate(function(x) x*dnorm(x), low=-Inf, up=varisk)$value/pnorm(varisk)
# Bootstrap of standard errors of expected value
nboot <- 1000
boot_data <- sapply(1:nboot, function(x) {
  datav <- sort(rnorm(nrows))
  varisk <- datav[cutoff]
  naivemc <- sum((datav < varisk)*datav)/sum((datav < varisk))
  data_tilt <- datav + lambda
  weights <- exp(-lambda*data_tilt + lambda^2/2)
  cumprob <- cumsum(weights)/nrows
  varisk <- data_tilt[findInterval(confl, cumprob)]
  isample <- sum((data_tilt < varisk)*data_tilt*weights)/sum((data_tilt < varisk)*weights)
  c(naivemc=naivemc, importmc=isample)
}) # end sapply
apply(boot_data, MARGIN=1,
  function(x) c(mean=mean(x), sd=sd(x)))

# Calculate matrix of random data
set.seed(1121) # Reset random number generator
nrows <- 1000; nboot <- 100
datav <- matrix(rnorm(nboot*nrows), ncol=nboot)
datav <- Rfast::sort_mat(datav)  # Sort the columns
# Calculate vector of quantiles for tilt parameter
confl <- 0.02; cutoff <- confl/nrows
calc_quant <- function(lambda) {
  data_tilt <- datav + lambda  # Tilt the random numbers
  weights <- exp(-lambda*data_tilt + lambda^2/2)
  # Calculate quantiles for columns
  sapply(1:nboot, function(boo_t) {
    cumprob <- cumsum(weights[, boo_t])/nrows
    data_tilt[findInterval(confl, cumprob), boo_t]
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
  c(naivemc=sum(rbinom(n=nrows, size=1, probv))/nrows,
    importmc=weigh_t*sum(rbinom(n=nrows, size=1, p_tilted))/nrows)
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
weights <- exp(-lambda*data_tilt + lambda^2/2)
path_weights <- matrixStats::colProds(weights)
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
crossi <- (colSums(paths > barp) > 0)
sum(crossi)/nsimu
# Calculate crossing probability using importance sampling
crossi <- colSums(paths_tilt > barp) > 0
sum(path_weights*crossi)/nsimu

options(width=50, dev="pdf")
str(optimize)
# Objective function with multiple minima
object <- function(input, param1=0.01) {
  sin(0.25*pi*input) + param1*(input-1)^2
}  # end object
unlist(optimize(f=object, interval=c(-4, 2)))
unlist(optimize(f=object, interval=c(0, 8)))


par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot the objective function
curve(expr=object, type="l", xlim=c(-8, 9),
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
object <- function(parv, datav) {
  sum(2*log(parv[2]) +
    ((datav - parv[1])/parv[2])^2)
}  # end object
# Objective function on parameter grid
par_mean <- seq(1, 6, length=50)
par_sd <- seq(0.5, 3.0, length=50)
objective_grid <- sapply(par_mean, function(m) {
  sapply(par_sd, function(sd) {
    object(c(m, sd), datav)
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
    object(c(mean, sd), datav),
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
  fn=object, # Log-likelihood function
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
object <- function(parv, datav) {
  likelihood <- parv[1]/parv[3] *
  dnorm((datav-parv[2])/parv[3]) +
  (1-parv[1])/parv[5]*dnorm((datav-parv[4])/parv[5])
  if (any(likelihood <= 0)) Inf else
    -sum(log(likelihood))
}  # end object
# Vectorize objective function
objvecive <- Vectorize(
  FUN=function(mean, sd, w, m1, s1, datav)
    object(c(w, m1, s1, mean, sd), datav),
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
      fn=object,
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

# Verify that rtools are working properly:
devtools::find_rtools()
devtools::has_devel()

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
int x_size = x.size();
int y_size = y.size();
if (x_size != y_size) {
    return 0;
  } else {
    double total = 0;
    for(int i = 0; i < x_size; ++i) {
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
double inner_lagmugar(NumericVector x, NumericVector y) {
  return sum(x * y);
}")  # end cppFunction
# Run Rcpp Sugar function
inner_lagmugar(1:3, 6:4)
inner_lagmugar(1:3, 6:3)

# Define R function with loop
inner_mult_r <- function(x, y) {
    to_tal <- 0
    for(i in 1:NROW(x)) {
to_tal <- to_tal + x[i] * y[i]
    }
    to_tal
}  # end inner_mult_r
# Run R function
inner_mult_r(1:3, 6:4)
inner_mult_r(1:3, 6:3)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=inner_mult_r(1:10000, 1:10000),
  inner_r=1:10000 %*% 1:10000,
  r_cpp=inner_mult(1:10000, 1:10000),
  r_cpp_sugar=inner_lagmugar(1:10000, 1:10000),
  times=10))[, c(1, 4, 5)]

# Define Ornstein-Uhlenbeck function in R
nboot <- 1000
boot_data <- function(datav, nboot=nboot) {
  boot_data <- sapply(1:nboot, function(x) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  })  # end sapply
  boot_data <- t(boot_data)
  # Analyze bootstrapped variance
  head(boot_data)
  sum(is.na(boot_data))
  # Means and standard errors from bootstrap
  apply(boot_data, MARGIN=2,
    function(x) c(mean=mean(x), stderror=sd(x)))

  returns <- numeric(nrows)
  prices <- numeric(nrows)
  prices[1] <- eq_price
  for (i in 2:nrows) {
    returns[i] <- thetav*(eq_price - prices[i-1]) + volat*rnorm(1)
    prices[i] <- prices[i-1] + returns[i]
  }  # end for
  prices
}  # end boot_data
# Simulate Ornstein-Uhlenbeck process in R
eq_price <- 5.0; sigmav <- 0.01
thetav <- 0.01; nrows <- 1000
set.seed(1121)  # Reset random numbers
ou_sim <- sim_ou(nrows*nrows, eq_price=eq_price, volat=sigmav, theta=thetav)

# Define Ornstein-Uhlenbeck function in R
sim_ou <- function(nrows=1000, eq_price=5.0,
              volat=0.01, theta=0.01) {
  returns <- numeric(nrows)
  prices <- numeric(nrows)
  prices[1] <- eq_price
  for (i in 2:nrows) {
    returns[i] <- theta*(eq_price - prices[i-1]) + volat*rnorm(1)
    prices[i] <- prices[i-1] + returns[i]
  }  # end for
  prices
}  # end sim_ou
# Simulate Ornstein-Uhlenbeck process in R
eq_price <- 5.0; sigmav <- 0.01
thetav <- 0.01; nrows <- 1000
set.seed(1121)  # Reset random numbers
ou_sim <- sim_ou(nrows*nrows, eq_price=eq_price, volat=sigmav, theta=thetav)

# Define Ornstein-Uhlenbeck function in Rcpp
Rcpp::cppFunction("
NumericVector sim_ou_rcpp(double eq_price,
                double volat,
                double theta,
                NumericVector innov) {
  int(nrows = innov.size();
  NumericVector prices*nrows);
  NumericVector returns*nrows);
  prices[0] = eq_price;
  for (int it = 1; it <.n_rows; it++) {
    returns[it] = theta*(eq_price - prices[it-1]) + volat*innov[it-1];
    prices[it] = prices[it-1] + returns[it];
  }  // end for
  return prices;
}")  # end cppFunction
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121)  # Reset random numbers
ou_sim_rcpp <- sim_ou_rcpp(eq_price=eq_price,
  volat=sigmav,
  theta=thetav,
  innov=rnorm(nrows))
all.equal(ou_sim, ou_sim_rcpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=sim_ou(nrows*nrows, eq_price=eq_price, volat=sigmav, theta=thetav),
  r_cpp=sim_ou_rcpp(eq_price=eq_price, volat=sigmav, theta=thetav, innov=rnorm(nrows)),
  times=10))[, c(1, 4, 5)]

# Source Rcpp function for Ornstein-Uhlenbeck process from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_ou.cpp")
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121)  # Reset random numbers
ou_sim_rcpp <- sim_ou_rcpp(eq_price=eq_price,
  volat=sigmav,
  theta=thetav,
  innov=rnorm(nrows))
all.equal(ou_sim, ou_sim_rcpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=sim_ou(nrows*nrows, eq_price=eq_price, volat=sigmav, theta=thetav),
  r_cpp=sim_ou_rcpp(eq_price=eq_price, volat=sigmav, theta=thetav, innov=rnorm(nrows)),
  times=10))[, c(1, 4, 5)]

# Calculate uniformly distributed pseudo-random sequence
unifun <- function(seedv, nrows=10) {
  output <- numeric(nrows)
  output[1] <- seedv
  for (i in 2:nrows) {
    output[i] <- 4*output[i-1]*(1-output[i-1])
  }  # end for
  acos(1-2*output)/pi
}  # end unifun

# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/unifun.cpp")
# Microbenchmark Rcpp code
library(microbenchmark)
summary(microbenchmark(
  pure_r=runif(1e5),
  rloop=unifun(0.3, 1e5),
  r_cpp=uniform_rcpp(0.3, 1e5),
  times=10))[, c(1, 4, 5)]

# Define Ornstein-Uhlenbeck function in R
boot_data <- function(datav, nboot=1000) {
  boot_data <- sapply(1:nboot, function(x) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  })  # end sapply
  boot_data <- t(boot_data)
  # Analyze bootstrapped variance
  head(boot_data)
  sum(is.na(boot_data))
  # Means and standard errors from bootstrap
  apply(boot_data, MARGIN=2,
    function(x) c(mean=mean(x), stderror=sd(x)))

  returns <- numeric(nrows)
  prices <- numeric(nrows)
  prices[1] <- eq_price
  for (i in 2:nrows) {
    returns[i] <- thetav*(eq_price - prices[i-1]) + volat*rnorm(1)
    prices[i] <- prices[i-1] + returns[i]
  }  # end for
  prices
}  # end boot_data
# Simulate Ornstein-Uhlenbeck process in R
eq_price <- 5.0; sigmav <- 0.01
thetav <- 0.01; nrows <- 1000
set.seed(1121)  # Reset random numbers
ou_sim <- sim_ou(nrows*nrows, eq_price=eq_price, volat=sigmav, theta=thetav)

# Define Ornstein-Uhlenbeck function in R
boot_data <- function(datav, nboot=1000) {
  boot_data <- sapply(1:nboot, function(x) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  })  # end sapply
  boot_data <- t(boot_data)
  # Analyze bootstrapped variance
  head(boot_data)
  sum(is.na(boot_data))
  # Means and standard errors from bootstrap
  apply(boot_data, MARGIN=2,
  function(x) c(mean=mean(x), stderror=sd(x)))

  returns <- numeric(nrows)
  prices <- numeric(nrows)
  prices[1] <- eq_price
  for (i in 2:nrows) {
    returns[i] <- thetav*(eq_price - prices[i-1]) + volat*rnorm(1)
    prices[i] <- prices[i-1] + returns[i]
  }  # end for
  prices
}  # end boot_data
# Simulate Ornstein-Uhlenbeck process in R
eq_price <- 5.0; sigmav <- 0.01
thetav <- 0.01; nrows <- 1000
set.seed(1121)  # Reset random numbers
ou_sim <- sim_ou(nrows*nrows, eq_price=eq_price, volat=sigmav, theta=thetav)

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillofuntions.cpp")
vec1 <- runif(1e5)
vec2 <- runif(1e5)
inner_vec(vec1, vec2)
vec1 %*% vec2

# Microbenchmark RcppArmadillo code
summary(microbenchmark(
  inner_vec = inner_vec(vec1, vec2),
  r_code = (vec1 %*% vec2),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Microbenchmark shows:
# inner_vec() is several times faster than %*%, especially for longer vectors.
#     expr     mean   median
# 1 inner_vec 110.7067 110.4530
# 2 r_code 585.5127 591.3575

# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_arima.cpp")
# Define AR(2) coefficients
coeff <- c(0.9, 0.09)
nrows <- 1e4
set.seed(1121)
innov <- rnorm(nrows)
# Simulate ARIMA using filter()
arima_filter <- filter(x=innov,
  filter=coeff, method="recursive")
# Simulate ARIMA using sim_arima()
arimav <- sim_arima(innov, rev(coeff))
all.equal(drop(arimav),
  as.numeric(arima_filter))
# Microbenchmark RcppArmadillo code
summary(microbenchmark(
  sim_arima = sim_arima(innov, rev(coeff)),
  filter = filter(x=innov, filter=coeff, method="recursive"),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillofuntions.cpp")
matrixv <- matrix(runif(1e5), nc=1e3)
# De-mean using apply()
newmapt <- apply(matrixv, 2, function(x) (x-mean(x)))
# De-mean using demean_mat()
demean_mat(matrixv)
all.equal(newmapt, matrixv)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  apply = (apply(matrixv, 2, mean)),
  demean_mat = demean_mat(matrixv),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Perform matrix inversion
# Create random positive semi-definite matrix
matrixv <- matrix(runif(25), nc=5)
matrixv <- t(matrixv) %*% matrixv
# Invert the matrix
matrix_inv <- solve(matrixv)
inv_mat(matrixv)
all.equal(matrix_inv, matrixv)
# Microbenchmark RcppArmadillo code
summary(microbenchmark(
  solve = solve(matrixv),
  inv_mat = inv_mat(matrixv),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/calc_weights.cpp")
# Calculate matrix of random returns
matrixv <- matrix(rnorm(300), nc=5)
# Regularized inverse of correlation matrix
eigen_max <- 4
cormat <- cor(matrixv)
eigend <- eigen(cormat)
inverse <- eigend$vectors[, 1:eigen_max] %*%
  (t(eigend$vectors[, 1:eigen_max]) / eigend$values[1:eigen_max])
# Regularized inverse using RcppArmadillo
inverse_arma <- calc_inv(cormat, eigen_max=eigen_max)
all.equal(inverse, inverse_arma)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  Rcode = {eigend <- eigen(cormat)
eigend$vectors[, 1:eigen_max] %*% (t(eigend$vectors[, 1:eigen_max]) / eigend$values[1:eigen_max])},
  Rcpp = calc_inv(cormat, eigen_max=eigen_max),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# wipp
# Install package reticulate
install.packages("reticulate")
# Load package reticulate
library(reticulate)
# Get documentation for package reticulate
# Get short description
packageDescription("reticulate")
# Load help page
help(package="reticulate")
# List all datasets in "reticulate"
data(package="reticulate")
# List all objects in "reticulate"
ls("package:reticulate")
# Remove reticulate from search path
detach("package:reticulate")
