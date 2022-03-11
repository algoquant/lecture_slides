va_r <- 0.3/3
va_r  # Printed as "0.1"
va_r - 0.1  # va_r is not equal to "0.1"
va_r == 0.1  # va_r is not equal to "0.1"
print(va_r, digits=10)
print(va_r, digits=16)
# va_r is equal to "0.1" within machine precision
all.equal(va_r, 0.1)
va_r <- (3-2.9)
print(va_r, digits=20)
# Info machine precision of computer R is running on
# ?.Machine
# Machine precision
.Machine$double.eps
va_r <- sqrt(2)
va_r^2  # Printed as "2"
va_r^2 == 2  # va_r^2 is not equal to "2"
print(va_r^2, digits=20)
# va_r^2 is equal to "2" within machine precision
all.equal(va_r^2, 2)
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
num_var <- 2
num_var==2
identical(num_var, 2)
identical(num_var, NULL)
# This doesn't work:
# num_var==NULL
is.null(num_var)
vec_tor <- c(2, 4, 6)
vec_tor == 2
identical(vec_tor, 2)
# num_ber is equal to "1.0" within machine precision
num_ber <- 1.0 + 2*sqrt(.Machine$double.eps)
all.equal(num_ber, 1.0)
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
func_tion <- function(x) {1/((x+1)*sqrt(x))}
integrate(func_tion, lower=0, upper=10)
integrate(func_tion, lower=0, upper=Inf)
# Integrate function with parameter lamb_da
func_tion <- function(x, lamb_da=1) {
  exp(-x*lamb_da)
}  # end func_tion
integrate(func_tion, lower=0, upper=Inf)
integrate(func_tion, lower=0, upper=Inf,
    lamb_da=2)
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
re_turns <- rutils::etfenv$re_turns$VTI
re_turns <- drop(coredata(na.omit(re_turns)))
n_rows <- NROW(re_turns)
# Mean and standard deviation of returns
c(mean(re_turns), sd(re_turns))
# Calculate the MAD of returns 10 points apart
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
add=TRUE, type="l", lwd=2, col="blue")
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("VTI", "Normal"), bty="n",
 lwd=6, bg="white", col=c("red", "blue"))
# Get size of an object
vec_tor <- runif(1e6)
object.size(vec_tor)
format(object.size(vec_tor), units="MB")
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
# Get names, class, and size of objects in workspace
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
v_cells <- gc()["Vcells", "used"]
# Create vector with 1,000,000 cells
va_r_bar <- numeric(1000000)
# Get extra R memory
gc()["Vcells", "used"] - v_cells
# Get total size of all objects in workspace
print(object.size(x=mget(ls())), units="MB")
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
library(microbenchmark)
# sum() is a compiled primitive function
sum
# mean() is a generic function
mean
vec_tor <- runif(1e6)
# sum() is much faster than mean()
all.equal(mean(vec_tor), sum(vec_tor)/NROW(vec_tor))
summary(microbenchmark(
  mean_fun = mean(vec_tor),
  sum_fun = sum(vec_tor)/NROW(vec_tor),
  times=10))[, c(1, 4, 5)]
# any() is a compiled primitive function
any
# any() is much faster than %in% wrapper for match()
all.equal(1 %in% vec_tor, any(vec_tor == 1))
summary(microbenchmark(
  in_fun = {1 %in% vec_tor},
  any_fun = any(vec_tor == 1),
  times=10))[, c(1, 4, 5)]
library(microbenchmark)
mat_rix <- matrix(1:9, ncol=3, # Create matrix
  dimnames=list(paste0("row", 1:3),
          paste0("col", 1:3)))
# Create specialized function
matrix_to_dframe <- function(mat_rix) {
  n_cols <- ncol(mat_rix)
  dframe <- vector("list", n_cols)  # empty vector
  for (in_dex in 1:n_cols)  # Populate vector
    dframe <- mat_rix[, in_dex]
  attr(dframe, "row.names") <-  # Add attributes
    .set_row_names(NROW(mat_rix))
  attr(dframe, "class") <- "data.frame"
  dframe  # Return data frame
}  # end matrix_to_dframe
# Compare speed of three methods
summary(microbenchmark(
  matrix_to_dframe(mat_rix),
  as.data.frame.matrix(mat_rix),
  as.data.frame(mat_rix),
  times=10))[, c(1, 4, 5)]
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
# Compiled C++ function
  cpp = cumsum(vec_tor),  # end for
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
# Disable JIT
jit_level <- compiler::enableJIT(0)
# Create inefficient function
my_mean <- function(x) {
  out_put <- 0; n_elem <- NROW(x)
  for(it in 1:n_elem)
    out_put <- out_put + x[it]/n_elem
  out_put
}  # end my_mean
# Byte-compile function and inspect it
mymean_comp <- compiler::cmpfun(my_mean)
mymean_comp
# Test function
vec_tor <- runif(1e3)
all.equal(mean(vec_tor), mymean_comp(vec_tor), my_mean(vec_tor))
# microbenchmark byte-compile function
summary(microbenchmark(
  mean(vec_tor),
  mymean_comp(vec_tor),
  my_mean(vec_tor),
  times=10))[, c(1, 4, 5)]
# Create another inefficient function
sapply2 <- function(x, FUN, ...) {
  out_put <- vector(length=NROW(x))
  for (it in seq_along(x))
    out_put[it] <- FUN(x[it], ...)
  out_put
}  # end sapply2
sapply2_comp <- compiler::cmpfun(sapply2)
all.equal(sqrt(vec_tor),
  sapply2(vec_tor, sqrt),
  sapply2_comp(vec_tor, sqrt))
summary(microbenchmark(
  sqrt(vec_tor),
  sapply2_comp(vec_tor, sqrt),
  sapply2(vec_tor, sqrt),
  times=10))[, c(1, 4, 5)]
# enable JIT
compiler::enableJIT(jit_level)
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
  mod_el <- lm(price ~ carat, data=ggplot2::diamonds)
  abline(mod_el, col="red")
})  # end profvis
# Four methods of calculating column means of matrix
mat_rix <- matrix(rnorm(1e5), ncol=5e4)
profvis::profvis({
  mean_s <- apply(mat_rix, 2, mean)
  mean_s <- colMeans(mat_rix)
  mean_s <- lapply(mat_rix, mean)
  mean_s <- vapply(mat_rix, mean, numeric(1))
})  # end profvis
# Four methods of calculating data frame column means
data_frame <- as.data.frame(mat_rix)
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
# List all objects in "parallel"
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
      NROW(index(get(sym_bol, envir=rutils::etfenv))))
# zoo function referenced using "::" in child process:
parSapply(clus_ter, c("VTI", "IEF", "DBC"),
    function(sym_bol)
      NROW(zoo::index(get(sym_bol, envir=rutils::etfenv))))
# Package zoo loaded in child process:
parSapply(clus_ter, c("VTI", "IEF", "DBC"),
    function(sym_bol) {
      stopifnot("package:zoo" %in% search() || require("zoo", quietly=TRUE))
      NROW(index(get(sym_bol, envir=rutils::etfenv)))
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
# Calculate option payout at maturity
strik_e <- 50  # Strike price
pay_outs <- (path_s[n_rows, ] - strik_e)
sum(pay_outs[pay_outs > 0])/n_simu
# Calculate probability of crossing the barrier at any point
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
pnorm(-2)
sum(da_ta < (-2))/n_rows
# Monte Carlo estimate of quantile
conf_level <- 0.02
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
re_turns <- rutils::etfenv$re_turns$VTI
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
re_turns <- rutils::etfenv$re_turns$VTI
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
price_s <- quantmod::Cl(rutils::etfenv$VTI)
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
oh_lc <- rutils::etfenv$VTI
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
# Extract the density
da_ta <- den_sity$x
den_sity <- den_sity$y
# Importance sample weights
lamb_da <- (-1.5*sd(da_ta))  # Tilt parameter
data_tilt <- da_ta + lamb_da  # Tilt the data
shif_t <- (-findInterval(lamb_da, da_ta))
# data_tilt <- rutils::lag_it(da_ta, shif_t, pad_zeros=FALSE)  # Tilt the data
density_tilt <- rutils::lag_it(den_sity, shif_t, pad_zeros=FALSE)  # Tilt the density
# Interpolate the den_sity vector into re_turns
density_base <- approx(da_ta, den_sity, xout=data_tilt)
all.equal(density_base$x, data_tilt)
density_base$y <- na.locf(density_base$y, fromLast=TRUE)
weight_s <- density_base$y/density_tilt
weight_s <- weight_s/sum(weight_s)
# Calculate time series of VTI returns
# Estimate of VaR (quantile - extreme loss)
conf_level <- 0.02; cut_off <- conf_level*n_rows
# da_ta <- sort(da_ta)
va_r <- da_ta[cut_off]  # Naive Monte Carlo value
sd(da_ta)*qnorm(conf_level)  # Assuming normal distribution
# Cumulative probabilities using importance sample
cum_prob <- cumsum(weight_s)
# Quantile from importance sample
va_r <- data_tilt[findInterval(conf_level, cum_prob)]
# Estimate of CVaR (expected loss)
cva_r <- sum((da_ta < va_r)*da_ta)/sum((da_ta < va_r))
# Estimate of cumulative probability
pnorm(1)
sum(da_ta < 1)/n_rows
# Generate importance sample
# Sample from VTI returns
sampl_e <- re_turns[sample.int(n_rows, replace=TRUE)]
c(sd=sd(sampl_e), mad=mad(sampl_e))
# sample.int() is a little faster than sample()
library(microbenchmark)
summary(microbenchmark(
  sample.int = sample.int(1e3),
  sample = sample(1e3),
  times=10))[, c(1, 4, 5)]
# Quantile from Naive Monte Carlo
conf_level <- 0.02
qnorm(conf_level)  # Exact value
da_ta <- sort(da_ta)
quantile(da_ta, probs=conf_level)
cut_off <- conf_level*n_rows
da_ta[cut_off]  # Naive Monte Carlo value
# Bootstrap of standard errors of quantile
n_boot <- 1000
boot_data <- sapply(1:n_boot, function(x) {
  da_ta <- sort(rnorm(n_rows))
  na_ive <- da_ta[cut_off]
  data_tilt <- da_ta + lamb_da
  weight_s <- exp(-lamb_da*data_tilt + lamb_da^2/2)
  cum_prob <- cumsum(weight_s)/n_rows
  im_port <- data_tilt[n_rows -
    findInterval(1-conf_level, cum_prob)]
  c(naive_mc=na_ive, importance=im_port)
}) # end sapply
apply(boot_data, MARGIN=1,
  function(x) c(mean=mean(x), sd=sd(x)))
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
options(width=50, dev="pdf")
str(optimize)
# Objective function with multiple minima
objec_tive <- function(in_put, param1=0.01) {
  sin(0.25*pi*in_put) + param1*(in_put-1)^2
}  # end objec_tive
unlist(optimize(f=objec_tive, interval=c(-4, 2)))
unlist(optimize(f=objec_tive, interval=c(0, 8)))
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot the objective function
curve(expr=objec_tive, type="l", xlim=c(-8, 9),
xlab="", ylab="", lwd=2)
# Add title
title(main="Objective Function", line=-1)
# Rastrigin function
rastri_gin <- function(x, y, pa_ram=25) {
  x^2 + y^2 - pa_ram*(cos(x) + cos(y))
}  # end rastri_gin
# Rastrigin function is vectorized!
rastri_gin(c(-10, 5), c(-10, 5))
# Set rgl options and load package rgl
options(rgl.useNULL=TRUE)
library(rgl)
# Draw 3d surface plot of function
rgl::persp3d(x=rastri_gin, xlim=c(-10, 10), ylim=c(-10, 10),
  col="green", axes=FALSE, pa_ram=15)
# Render the 3d surface plot of function
rgl::rglwidget(elementId="plot3drgl", width=400, height=400)
# Rastrigin function with vector argument for optimization
rastri_gin <- function(vec_tor, pa_ram=25) {
  sum(vec_tor^2 - pa_ram*cos(vec_tor))
}  # end rastri_gin
vec_tor <- c(pi/6, pi/6)
rastri_gin(vec_tor=vec_tor)
# Draw 3d surface plot of Rastrigin function
rgl::persp3d(
  x=Vectorize(function(x, y) rastri_gin(vec_tor=c(x, y))),
  xlim=c(-10, 10), ylim=c(-10, 10),
  col="green", axes=FALSE, zlab="", main="rastri_gin")
# Optimize with respect to vector argument
op_tim <- optim(par=vec_tor, fn=rastri_gin,
        method="L-BFGS-B",
        upper=c(4*pi, 4*pi),
        lower=c(pi/2, pi/2),
        pa_ram=1)
# Optimal parameters and value
op_tim$par
op_tim$value
rastri_gin(op_tim$par, pa_ram=1)
# Sample of normal variables
da_ta <- rnorm(1000, mean=4, sd=2)
# Objective function is log-likelihood
objec_tive <- function(pa_r, da_ta) {
  sum(2*log(pa_r[2]) +
    ((da_ta - pa_r[1])/pa_r[2])^2)
}  # end objec_tive
# Objective function on parameter grid
par_mean <- seq(1, 6, length=50)
par_sd <- seq(0.5, 3.0, length=50)
objective_grid <- sapply(par_mean, function(m) {
  sapply(par_sd, function(sd) {
    objec_tive(c(m, sd), da_ta)
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
vec_objective <- Vectorize(
  FUN=function(mean, sd, da_ta)
    objec_tive(c(mean, sd), da_ta),
  vectorize.args=c("mean", "sd")
)  # end Vectorize
objective_grid <- outer(par_mean, par_sd,
vec_objective, da_ta=da_ta)
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
par_init <- c(mean=0, sd=1)
# Perform optimization using optim()
optim_fit <- optim(par=par_init,
  fn=objec_tive, # Log-likelihood function
  da_ta=da_ta,
  method="L-BFGS-B", # Quasi-Newton method
  upper=c(10, 10), # Upper constraint
  lower=c(-10, 0.1)) # Lower constraint
# Optimal parameters
optim_fit$par
# Perform optimization using MASS::fitdistr()
optim_fit <- MASS::fitdistr(da_ta, densfun="normal")
optim_fit$estimate
optim_fit$sd
# Plot histogram
histo_gram <- hist(da_ta, plot=FALSE)
plot(histo_gram, freq=FALSE, main="histogram of sample")
curve(expr=dnorm(x, mean=optim_fit$par["mean"], sd=optim_fit$par["sd"]),
add=TRUE, type="l", lwd=2, col="red")
legend("topright", inset=0.0, cex=0.8, title=NULL,
 leg="optimal parameters", lwd=2, bg="white", col="red")
# Sample from mixture of normal distributions
da_ta <- c(rnorm(100, sd=1.0),
      rnorm(100, mean=4, sd=1.0))
# Objective function is log-likelihood
objec_tive <- function(pa_r, da_ta) {
  likelihood <- pa_r[1]/pa_r[3] *
  dnorm((da_ta-pa_r[2])/pa_r[3]) +
  (1-pa_r[1])/pa_r[5]*dnorm((da_ta-pa_r[4])/pa_r[5])
  if (any(likelihood <= 0)) Inf else
    -sum(log(likelihood))
}  # end objec_tive
# Vectorize objective function
vec_objective <- Vectorize(
  FUN=function(mean, sd, w, m1, s1, da_ta)
    objec_tive(c(w, m1, s1, mean, sd), da_ta),
  vectorize.args=c("mean", "sd")
)  # end Vectorize
# Objective function on parameter grid
par_mean <- seq(3, 5, length=50)
par_sd <- seq(0.5, 1.5, length=50)
objective_grid <- outer(par_mean, par_sd,
    vec_objective, da_ta=da_ta,
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
par_init <- c(weight=0.5, m1=0, s1=1, m2=2, s2=1)
# Perform optimization
optim_fit <- optim(par=par_init,
      fn=objec_tive,
      da_ta=da_ta,
      method="L-BFGS-B",
      upper=c(1,10,10,10,10),
      lower=c(0,-10,0.2,-10,0.2))
optim_fit$par
# Plot histogram
histo_gram <- hist(da_ta, plot=FALSE)
plot(histo_gram, freq=FALSE,
     main="histogram of sample")
fit_func <- function(x, pa_r) {
  pa_r["weight"]*dnorm(x, mean=pa_r["m1"], sd=pa_r["s1"]) +
  (1-pa_r["weight"])*dnorm(x, mean=pa_r["m2"], sd=pa_r["s2"])
}  # end fit_func
curve(expr=fit_func(x, pa_r=optim_fit$par), add=TRUE,
type="l", lwd=2, col="red")
legend("topright", inset=0.0, cex=0.8, title=NULL,
 leg="optimal parameters",
 lwd=2, bg="white", col="red")
# Rastrigin function with vector argument for optimization
rastri_gin <- function(vec_tor, pa_ram=25) {
  sum(vec_tor^2 - pa_ram*cos(vec_tor))
}  # end rastri_gin
vec_tor <- c(pi/6, pi/6)
rastri_gin(vec_tor=vec_tor)
library(DEoptim)
# Optimize rastri_gin using DEoptim
op_tim <-  DEoptim(rastri_gin,
  upper=c(6, 6), lower=c(-6, -6),
  DEoptim.control(trace=FALSE, itermax=50))
# Optimal parameters and value
op_tim$optim$bestmem
rastri_gin(op_tim$optim$bestmem)
summary(op_tim)
plot(op_tim)
# Rastrigin function with vector argument for optimization
rastri_gin <- function(vec_tor, pa_ram=25) {
  sum(vec_tor^2 - pa_ram*cos(vec_tor))
}  # end rastri_gin
vec_tor <- c(pi/6, pi/6)
rastri_gin(vec_tor=vec_tor)
library(DEoptim)
# Optimize rastri_gin using DEoptim
op_tim <-  DEoptim(rastri_gin,
  upper=c(6, 6), lower=c(-6, -6),
  DEoptim.control(trace=FALSE, itermax=50))
# Optimal parameters and value
op_tim$optim$bestmem
rastri_gin(op_tim$optim$bestmem)
summary(op_tim)
plot(op_tim)
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
double inner_mult_sugar(NumericVector x, NumericVector y) {
  return sum(x * y);
}")  # end cppFunction
# Run Rcpp Sugar function
inner_mult_sugar(1:3, 6:4)
inner_mult_sugar(1:3, 6:3)
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
  r_cpp_sugar=inner_mult_sugar(1:10000, 1:10000),
  times=10))[, c(1, 4, 5)]
# Define Ornstein-Uhlenbeck function in R
n_boot <- 1000
boot_data <- function(da_ta, n_boot=n_boot) {
  boot_data <- sapply(1:n_boot, function(x) {
    sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  })  # end sapply
  boot_data <- t(boot_data)
  # Analyze bootstrapped variance
  head(boot_data)
  sum(is.na(boot_data))
  # Means and standard errors from bootstrap
  apply(boot_data, MARGIN=2,
    function(x) c(mean=mean(x), std_error=sd(x)))
  re_turns <- numeric(n_rows)
  price_s <- numeric(n_rows)
  price_s[1] <- eq_price
  for (i in 2:n_rows) {
    re_turns[i] <- the_ta*(eq_price - price_s[i-1]) + vol_at*rnorm(1)
    price_s[i] <- price_s[i-1] + re_turns[i]
  }  # end for
  price_s
}  # end boot_data
# Simulate Ornstein-Uhlenbeck process in R
eq_price <- 5.0; sig_ma <- 0.01
the_ta <- 0.01; n_rows <- 1000
set.seed(1121)  # Reset random numbers
ou_sim <- sim_ou(n_rows=n_rows, eq_price=eq_price, vol_at=sig_ma, theta=the_ta)
# Define Ornstein-Uhlenbeck function in R
sim_ou <- function(n_rows=1000, eq_price=5.0,
              volat=0.01, theta=0.01) {
  re_turns <- numeric(n_rows)
  price_s <- numeric(n_rows)
  price_s[1] <- eq_price
  for (i in 2:n_rows) {
    re_turns[i] <- theta*(eq_price - price_s[i-1]) + volat*rnorm(1)
    price_s[i] <- price_s[i-1] + re_turns[i]
  }  # end for
  price_s
}  # end sim_ou
# Simulate Ornstein-Uhlenbeck process in R
eq_price <- 5.0; sig_ma <- 0.01
the_ta <- 0.01; n_rows <- 1000
set.seed(1121)  # Reset random numbers
ou_sim <- sim_ou(n_rows=n_rows, eq_price=eq_price, vol_at=sig_ma, theta=the_ta)
# Define Ornstein-Uhlenbeck function in Rcpp
Rcpp::cppFunction("
NumericVector sim_ou_rcpp(double eq_price,
                double volat,
                double theta,
                NumericVector innov) {
  int n_rows = innov.size();
  NumericVector price_s(n_rows);
  NumericVector re_turns(n_rows);
  price_s[0] = eq_price;
  for (int it = 1; it < n_rows; it++) {
    re_turns[it] = theta*(eq_price - price_s[it-1]) + volat*innov[it-1];
    price_s[it] = price_s[it-1] + re_turns[it];
  }  // end for
  return price_s;
}")  # end cppFunction
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121)  # Reset random numbers
ou_sim_rcpp <- sim_ou_rcpp(eq_price=eq_price,
  volat=sig_ma,
  theta=the_ta,
  innov=rnorm(n_rows))
all.equal(ou_sim, ou_sim_rcpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=sim_ou(n_rows=n_rows, eq_price=eq_price, vol_at=sig_ma, theta=the_ta),
  r_cpp=sim_ou_rcpp(eq_price=eq_price, vol_at=sig_ma, theta=the_ta, innov=rnorm(n_rows)),
  times=10))[, c(1, 4, 5)]
# Source Rcpp function for Ornstein-Uhlenbeck process from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_ou.cpp")
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121)  # Reset random numbers
ou_sim_rcpp <- sim_ou_rcpp(eq_price=eq_price,
  vol_at=sig_ma,
  theta=the_ta,
  innov=rnorm(n_rows))
all.equal(ou_sim, ou_sim_rcpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=sim_ou(n_rows=n_rows, eq_price=eq_price, vol_at=sig_ma, theta=the_ta),
  r_cpp=sim_ou_rcpp(eq_price=eq_price, vol_at=sig_ma, theta=the_ta, innov=rnorm(n_rows)),
  times=10))[, c(1, 4, 5)]
# Calculate uniformly distributed pseudo-random sequence
uni_form <- function(see_d, n_rows=10) {
  out_put <- numeric(n_rows)
  out_put[1] <- see_d
  for (i in 2:n_rows) {
    out_put[i] <- 4*out_put[i-1]*(1-out_put[i-1])
  }  # end for
  acos(1-2*out_put)/pi
}  # end uni_form
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/uni_form.cpp")
# Microbenchmark Rcpp code
library(microbenchmark)
summary(microbenchmark(
  pure_r=runif(1e5),
  r_loop=uni_form(0.3, 1e5),
  r_cpp=uniform_rcpp(0.3, 1e5),
  times=10))[, c(1, 4, 5)]
# Define Ornstein-Uhlenbeck function in R
boot_data <- function(da_ta, n_boot=1000) {
  boot_data <- sapply(1:n_boot, function(x) {
    sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  })  # end sapply
  boot_data <- t(boot_data)
  # Analyze bootstrapped variance
  head(boot_data)
  sum(is.na(boot_data))
  # Means and standard errors from bootstrap
  apply(boot_data, MARGIN=2,
    function(x) c(mean=mean(x), std_error=sd(x)))
  re_turns <- numeric(n_rows)
  price_s <- numeric(n_rows)
  price_s[1] <- eq_price
  for (i in 2:n_rows) {
    re_turns[i] <- the_ta*(eq_price - price_s[i-1]) + vol_at*rnorm(1)
    price_s[i] <- price_s[i-1] + re_turns[i]
  }  # end for
  price_s
}  # end boot_data
# Simulate Ornstein-Uhlenbeck process in R
eq_price <- 5.0; sig_ma <- 0.01
the_ta <- 0.01; n_rows <- 1000
set.seed(1121)  # Reset random numbers
ou_sim <- sim_ou(n_rows=n_rows, eq_price=eq_price, vol_at=sig_ma, theta=the_ta)
# Define Ornstein-Uhlenbeck function in R
boot_data <- function(da_ta, n_boot=1000) {
  boot_data <- sapply(1:n_boot, function(x) {
    sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  })  # end sapply
  boot_data <- t(boot_data)
  # Analyze bootstrapped variance
  head(boot_data)
  sum(is.na(boot_data))
  # Means and standard errors from bootstrap
  apply(boot_data, MARGIN=2,
  function(x) c(mean=mean(x), std_error=sd(x)))
  re_turns <- numeric(n_rows)
  price_s <- numeric(n_rows)
  price_s[1] <- eq_price
  for (i in 2:n_rows) {
    re_turns[i] <- the_ta*(eq_price - price_s[i-1]) + vol_at*rnorm(1)
    price_s[i] <- price_s[i-1] + re_turns[i]
  }  # end for
  price_s
}  # end boot_data
# Simulate Ornstein-Uhlenbeck process in R
eq_price <- 5.0; sig_ma <- 0.01
the_ta <- 0.01; n_rows <- 1000
set.seed(1121)  # Reset random numbers
ou_sim <- sim_ou(n_rows=n_rows, eq_price=eq_price, vol_at=sig_ma, theta=the_ta)
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillo_functions.cpp")
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
co_eff <- c(0.9, 0.09)
n_rows <- 1e4
set.seed(1121)
in_nov <- rnorm(n_rows)
# Simulate ARIMA using filter()
arima_filter <- filter(x=in_nov,
  filter=co_eff, method="recursive")
# Simulate ARIMA using sim_arima()
ari_ma <- sim_arima(in_nov, rev(co_eff))
all.equal(drop(ari_ma),
  as.numeric(arima_filter))
# Microbenchmark RcppArmadillo code
summary(microbenchmark(
  sim_arima = sim_arima(in_nov, rev(co_eff)),
  filter = filter(x=in_nov, filter=co_eff, method="recursive"),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/armadillo_functions.cpp")
mat_rix <- matrix(runif(1e5), nc=1e3)
# De-mean using apply()
new_mat <- apply(mat_rix, 2, function(x) (x-mean(x)))
# De-mean using demean_mat()
demean_mat(mat_rix)
all.equal(new_mat, mat_rix)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  apply = (apply(mat_rix, 2, mean)),
  demean_mat = demean_mat(mat_rix),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Perform matrix inversion
# Create random positive semi-definite matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- t(mat_rix) %*% mat_rix
# Invert the matrix
matrix_inv <- solve(mat_rix)
inv_mat(mat_rix)
all.equal(matrix_inv, mat_rix)
# Microbenchmark RcppArmadillo code
summary(microbenchmark(
  solve = solve(mat_rix),
  inv_mat = inv_mat(mat_rix),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/calc_weights.cpp")
# Calculate matrix of random returns
mat_rix <- matrix(rnorm(300), nc=5)
# Regularized inverse of correlation matrix
eigen_max <- 4
cor_mat <- cor(mat_rix)
ei_gen <- eigen(cor_mat)
in_verse <- ei_gen$vectors[, 1:eigen_max] %*%
  (t(ei_gen$vectors[, 1:eigen_max]) / ei_gen$values[1:eigen_max])
# Regularized inverse using RcppArmadillo
inverse_arma <- calc_inv(cor_mat, eigen_max=eigen_max)
all.equal(in_verse, inverse_arma)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  Rcode = {ei_gen <- eigen(cor_mat)
ei_gen$vectors[, 1:eigen_max] %*% (t(ei_gen$vectors[, 1:eigen_max]) / ei_gen$values[1:eigen_max])},
  Rcpp = calc_inv(cor_mat, eigen_max=eigen_max),
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
