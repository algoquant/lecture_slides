library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(digits=3)
options(width=60, dev='pdf')
thm <- knit_theme$get("acid")
knit_theme$set(thm)

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
vec_tor==2
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
6 %/% 2  # use integers to get correct result
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
# Get sizes of objects in rutils::etf_env environment
sort(sapply(ls(rutils::etf_env), function(ob_ject) {
  object.size(get(ob_ject, rutils::etf_env))}))
sort(sapply(mget(ls(rutils::etf_env), rutils::etf_env),
      object.size))
library(gdata)  # Load package gdata
# Get size of data frame columns
gdata::ll(unit="bytes", mtcars)
# Get names, class, and size of objects in workspace
ob_jects <- gdata::ll(unit="bytes")
# Sort by memory size (descending)
ob_jects[order(ob_jects[, 2], decreasing=TRUE), ]
gdata::ll()[order(ll()$KB, decreasing=TRUE), ]
# Get sizes of objects in etf_env environment
gdata::ll(unit="bytes", etf_env)

library(SOAR)  # Load package SOAR
# Get sizes of objects in workspace
sort(sapply(mget(ls()), object.size))
Store(etf_list)  # Store in object cache
# Get sizes of objects in workspace
sort(sapply(mget(ls()), object.size))
search()  # Get search path for R objects
Ls()  # list object cache
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
va_r <- runif(1e6)
system.time(va_r^0.5)
microbenchmark(sqrt(va_r), va_r^0.5, times=10)

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
system.time(vec_tor^0.5)
summary(
  microbenchmark(sqrt(vec_tor), vec_tor^0.5, times=10)
  )[, c(1, 4, 5)]

library(microbenchmark)
# sum() is a compiled primitive function
sum
# mean() is a generic function
mean
va_r <- runif(1e6)
# sum() is much faster than mean()
summary(
  microbenchmark(sum(va_r), mean(va_r), times=10)
  )[, c(1, 4, 5)]
# any() is a compiled primitive function
any
# any() is much faster than %in% wrapper for match()
summary(
  microbenchmark(any(va_r == 1), {1 %in% va_r}, times=10)
  )[, c(1, 4, 5)]

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

# Matrix with 5,000 rows
mat_rix <- matrix(rnorm(10000), ncol=2)
# Allocate memory for row sums
row_sums <- numeric(NROW(mat_rix))
summary(microbenchmark(
  row_sums=rowSums(mat_rix),  # end row_sums
  ap_ply=apply(mat_rix, 1, sum),  # end apply
  l_apply=lapply(1:NROW(mat_rix), function(in_dex)
    sum(mat_rix[in_dex, ])),  # end lapply
  v_apply=vapply(1:NROW(mat_rix), function(in_dex)
    sum(mat_rix[in_dex, ]),
    FUN.VALUE=c(sum=0)),  # end vapply
  s_apply=sapply(1:NROW(mat_rix), function(in_dex)
    sum(mat_rix[in_dex, ])),  # end sapply
  for_loop=for (i in 1:NROW(mat_rix)) {
    row_sums[i] <- sum(mat_rix[i,])
  },  # end for
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

big_vector <- rnorm(5000)
summary(microbenchmark(
# Allocate full memory for cumulative sum
  for_loop={cum_sum <- numeric(NROW(big_vector))
    cum_sum[1] <- big_vector[1]
    for (i in 2:NROW(big_vector)) {
      cum_sum[i] <- cum_sum[i-1] + big_vector[i]
    }},  # end for
# Allocate zero memory for cumulative sum
  grow_vec={cum_sum <- numeric(0)
    cum_sum[1] <- big_vector[1]
    for (i in 2:NROW(big_vector)) {
# Add new element to "cum_sum" ("grow" it)
      cum_sum[i] <- cum_sum[i-1] + big_vector[i]
    }},  # end for
# Allocate zero memory for cumulative sum
  com_bine={cum_sum <- numeric(0)
    cum_sum[1] <- big_vector[1]
    for (i in 2:NROW(big_vector)) {
# Add new element to "cum_sum" ("grow" it)
      cum_sum <- c(cum_sum, big_vector[i])
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
Rprof(filename="C:/Develop/data_def/profile.out")
# Run code for profiling
replicate(n=10, out_er())
# Turn off profiling
Rprof(NULL)
# Compile summary of profiling from file
summaryRprof("C:/Develop/data_def/profile.out")

# Profile plotting of regression
profvis::profvis({
  plot(price ~ carat, data=ggplot2::diamonds)
  mod_el <- lm(price ~ carat, data=ggplot2::diamonds)
  abline(mod_el, col="red")
})  # end profvis
# Four methods of calculating matrix column means
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

vec_tor1 <- rnorm(1000000)
vec_tor2 <- rnorm(1000000)
big_vector <- numeric(1000000)
# Sum two vectors in two different ways
summary(microbenchmark(
  # Sum vectors using "for" loop
  r_loop=(for (i in 1:NROW(vec_tor1)) {
    big_vector[i] <- vec_tor1[i] + vec_tor2[i]
  }),
  # Sum vectors using vectorized "+"
  vec_torized=(vec_tor1 + vec_tor2),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Allocate memory for cumulative sum
cum_sum <- numeric(NROW(big_vector))
cum_sum[1] <- big_vector[1]
# Calculate cumulative sum in two different ways
summary(microbenchmark(
# Cumulative sum using "for" loop
  r_loop=(for (i in 2:NROW(big_vector)) {
    cum_sum[i] <- cum_sum[i-1] + big_vector[i]
  }),
# Cumulative sum using "cumsum"
  vec_torized=cumsum(big_vector),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# Matrix with 5,000 rows
mat_rix <- matrix(rnorm(10000), ncol=2)
# Calculate row sums two different ways
all.equal(rowSums(mat_rix),
  apply(mat_rix, 1, sum))
summary(microbenchmark(
  row_sums=rowSums(mat_rix),
  ap_ply=apply(mat_rix, 1, sum),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

library(microbenchmark)
str(pmax)
# Calculate row maximums two different ways
summary(microbenchmark(
  p_max=
    do.call(pmax.int,
lapply(seq_along(mat_rix[1, ]),
  function(in_dex) mat_rix[, in_dex])),
  l_apply=unlist(
    lapply(seq_along(mat_rix[, 1]),
  function(in_dex) max(mat_rix[in_dex, ]))),
  times=10))[, c(1, 4, 5)]

install.packages("matrixStats")  # Install package matrixStats
library(matrixStats)  # Load package matrixStats
# Calculate row min values three different ways
summary(microbenchmark(
  row_mins=rowMins(mat_rix),
  p_min=
    do.call(pmin.int,
      lapply(seq_along(mat_rix[1, ]),
             function(in_dex)
               mat_rix[, in_dex])),
  as_data_frame=
    do.call(pmin.int,
      as.data.frame.matrix(mat_rix)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

install.packages("Rfast")  # Install package Rfast
library(Rfast)  # Load package Rfast
# Benchmark speed of calculating ranks
va_r <- 1e3
all.equal(rank(va_r), Rfast::Rank(va_r))
summary(microbenchmark(
  r=rank(va_r),
  fast=Rank(va_r),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Benchmark speed of calculating column medians
va_r <- matrix(1e4, nc=10)
all.equal(matrixStats::colMedians(va_r), Rfast::colMedians(va_r))
summary(microbenchmark(
  r=matrixStats::colMedians(va_r),
  fast=Rfast::colMedians(va_r),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

summary(microbenchmark(  # Assign values to vector three different ways
# Fast vectorized assignment loop performed in C using brackets "[]"
  brack_ets={vec_tor <- numeric(10)
    vec_tor[] <- 2},
# Slow because loop is performed in R
  for_loop={vec_tor <- numeric(10)
    for (in_dex in seq_along(vec_tor))
      vec_tor[in_dex] <- 2},
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
summary(microbenchmark(  # Assign values to vector two different ways
# Fast vectorized assignment loop performed in C using brackets "[]"
  brack_ets={vec_tor <- numeric(10)
    vec_tor[4:7] <- rnorm(4)},
# Slow because loop is performed in R
  for_loop={vec_tor <- numeric(10)
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
std_devs <-
  structure(1:3, names=paste0("sd=", 1:3))
me_ans <-
  structure(-1:1, names=paste0("mean=", -1:1))
# "sd" argument of rnorm() isn't vectorized
rnorm(1, sd=std_devs)
# "mean" argument of rnorm() isn't vectorized
rnorm(1, mean=me_ans)

# sapply produces desired vector output
set.seed(1121)
sapply(std_devs, function(std_dev) rnorm(n=2, sd=std_dev))
set.seed(1121)
sapply(std_devs, rnorm, n=2, mean=0)
set.seed(1121)
sapply(me_ans,
 function(me_an) rnorm(n=2, mean=me_an))
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
vec_tor1 <- sin(0.25*pi*1:10)
vec_tor2 <- cos(0.25*pi*1:10)
# Create third vector using 'ifelse'
vec_tor3 <- ifelse(vec_tor1 > vec_tor2,
          vec_tor1, vec_tor2)
# cbind all three together
vec_tor4 <- cbind(vec_tor1, vec_tor2, vec_tor3)

# Set plotting parameters
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0),
    cex.lab=0.8, cex.axis=0.8, cex.main=0.8,
    cex.sub=0.5)
# Plot matrix
matplot(vec_tor4, type="l", lty="solid",
col=c("green", "blue", "red"),
lwd=2, xlab="", ylab="")
# Add legend
legend(x="bottomright", legend=colnames(vec_tor4),
       title="", inset=0.05, cex=0.8, lwd=2,
       lty=1, col=c("green", "blue", "red"))

set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
len_gth <- 1000
da_ta <- rnorm(len_gth)
# Sample mean - MC estimate
mean(da_ta)
# Sample standard deviation - MC estimate
sd(da_ta)
# Monte Carlo estimate of cumulative probability
da_ta <- sort(da_ta)
pnorm(1)
sum(da_ta<1)/len_gth
# Monte Carlo estimate of quantile
conf_level <- 0.99
qnorm(conf_level)
cut_off <- conf_level*len_gth
da_ta[cut_off]
quantile(da_ta, probs=conf_level)
# Analyze the source code of quantile()
stats:::quantile.default
# Microbenchmark quantile
library(microbenchmark)
summary(microbenchmark(
  monte_carlo=da_ta[cut_off],
  quan_tile=quantile(da_ta, probs=conf_level),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

x11(width=6, height=5)
par(oma=c(1, 1, 1, 1), mar=c(2, 2, 2, 1), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # Reset random number generator
bar_rier <- 20  # Barrier level
len_gth <- 1000  # Number of simulation steps
pa_th <- numeric(len_gth)  # Allocate path vector
pa_th[1] <- 0  # Initialize path
in_dex <- 2  # Initialize simulation index
while ((in_dex <= len_gth) &&
 (pa_th[in_dex - 1] < bar_rier)) {
# Simulate next step
  pa_th[in_dex] <-
    pa_th[in_dex - 1] + rnorm(1)
  in_dex <- in_dex + 1  # Advance in_dex
}  # end while
# Fill remaining pa_th after it crosses bar_rier
if (in_dex <= len_gth)
  pa_th[in_dex:len_gth] <- pa_th[in_dex - 1]
# Create daily time series starting 2011
ts_path <- ts(data=pa_th, frequency=365, start=c(2011, 1))
plot(ts_path, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=bar_rier, lwd=2, col="red")
title(main="Brownian motion crossing a barrier level",
      line=0.5)

x11(width=6, height=5)
par(oma=c(1, 1, 1, 1), mar=c(2, 2, 2, 1), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # Reset random number generator
bar_rier <- 20  # Barrier level
len_gth <- 1000  # Number of simulation steps
# Simulate path of Brownian motion
pa_th <- cumsum(rnorm(len_gth))
# Find index when pa_th crosses bar_rier
cro_ss <- which(pa_th > bar_rier)
# Fill remaining pa_th after it crosses bar_rier
if (NROW(cro_ss)>0) {
  pa_th[(cro_ss[1]+1):len_gth] <-
    pa_th[cro_ss[1]]
}  # end if
# Create daily time series starting 2011
ts_path <- ts(data=pa_th, frequency=365,
     start=c(2011, 1))
# Create plot with horizontal line
plot(ts_path, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=bar_rier, lwd=2, col="red")
title(main="Brownian motion crossing a barrier level",
      line=0.5)

set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
len_gth <- 1000; da_ta <- rnorm(len_gth)
# Sample mean and standard deviation
mean(da_ta); sd(da_ta)
# Bootstrap of sample mean and median
boot_data <- sapply(1:10000, function(x) {
  boot_sample <- da_ta[sample.int(len_gth,
                              replace=TRUE)]
  c(mean=mean(boot_sample), median=median(boot_sample))
})  # end sapply
boot_data <- t(boot_data)

boot_data[1:3, ]
# Standard error from formula
sd(da_ta)/sqrt(len_gth)
# Standard error of mean from bootstrap
sd(boot_data[, "mean"])
# Standard error of median from bootstrap
sd(boot_data[, "median"])
plot(density(boot_data[, "median"]),
     lwd=2, xlab="estimate of median",
     main="Distribution of Bootstrapped Median")
abline(v=mean(boot_data[, "median"]),
 lwd=2, col="red")

set.seed(1121)  # Reset random number generator
len_gth <- 1000
da_ta <- rnorm(len_gth)
# Bootstrap of sample mean and median
boot_data <- sapply(1:10000, function(x) {
  # Boot_sample from Standard Normal Distribution
  boot_sample <- rnorm(len_gth)
  c(mean=mean(boot_sample),
    median=median(boot_sample))
})  # end sapply
boot_data[, 1:3]
# Standard error from formula
sd(da_ta)/sqrt(len_gth)
# Standard error of mean from bootstrap
sd(boot_data["mean", ])
# Standard error of median from bootstrap
sd(boot_data["median", ])

library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster under Windows
set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
len_gth <- 1000
da_ta <- rnorm(len_gth)
# Bootstrap mean and median under Windows
boot_data <- parLapply(clus_ter, 1:10000,
  function(x, da_ta, len_gth) {
  boot_sample <- da_ta[sample.int(len_gth, replace=TRUE)]
  c(mean=mean(boot_sample), median=median(boot_sample))
  }, da_ta=da_ta, len_gth=len_gth)  # end parLapply
# Bootstrap mean and median under Mac-OSX or Linux
boot_data <- mclapply(1:10000,
  function(x) {
  boot_sample <- da_ta[sample.int(len_gth, replace=TRUE)]
  c(mean=mean(boot_sample), median=median(boot_sample))
  }, mc.cores=n_cores)  # end mclapply
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))
# Standard error from formula
sd(da_ta)/sqrt(len_gth)
stopCluster(clus_ter)  # Stop R processes over cluster under Windows

len_gth <- 1000
da_ta <- rnorm(len_gth)
sd(da_ta)
mad(da_ta)
median(abs(da_ta - median(da_ta)))
median(abs(da_ta - median(da_ta)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
boot_data <- sapply(1:10000, function(x) {
  boot_sample <-
    da_ta[sample.int(len_gth, replace=TRUE)]
  c(sd=sd(boot_sample), mad=mad(boot_sample))
})  # end sapply
boot_data <- t(boot_data)
# Analyze bootstrapped variance
head(boot_data)
sum(is.na(boot_data))
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster
boot_data <- parLapply(clus_ter, 1:10000,
  function(x, da_ta) {
    boot_sample <- da_ta[sample.int(len_gth, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, da_ta=da_ta)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:10000,
  function(x) {
    boot_sample <- da_ta[sample.int(len_gth, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))

# Sample from time series of ETF returns
re_turns <- rutils::etf_env$re_turns[, "VTI"]
re_turns <- na.omit(re_turns)
len_gth <- NROW(re_turns)
# Bootstrap sd and MAD under Windows
library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster under Windows
clusterSetRNGStream(clus_ter, 1121)  # Reset random number generator in all cores
n_boot <- 1e4
boot_data <- parLapply(clus_ter, 1:n_boot,
  function(x, re_turns, len_gth) {
    boot_sample <- re_turns[sample.int(len_gth, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, re_turns=re_turns, len_gth=len_gth)  # end parLapply
# Bootstrap sd and MAD under Mac-OSX or Linux
boot_data <- mclapply(1:n_boot,
  function(x) {
    boot_sample <- re_turns[sample.int(len_gth, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
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
price_s <- quantmod::Ad(rutils::etf_env$VTI)
star_t <- as.numeric(price_s[1, ])
re_turns <- rutils::diff_it(log(price_s))
class(re_turns); head(re_turns)
sum(is.na(re_turns))
len_gth <- NROW(re_turns)
# Define barrier level with respect to price_s
bar_rier <- 1.5*max(price_s)
# Calculate single bootstrap sample
boot_sample <- re_turns[sample.int(len_gth, replace=TRUE)]
# Calculate prices from percentage returns
boot_sample <- star_t*exp(cumsum(boot_sample))
# Calculate statistic
sum(boot_sample > bar_rier) > 0

library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster under Windows
# Perform parallel bootstrap under Windows
clusterSetRNGStream(clus_ter, 1121)  # Reset random number generator in all cores
clusterExport(clus_ter, c("star_t", "bar_rier"))
n_boot <- 1e4
boot_data <- parLapply(clus_ter, 1:n_boot,
  function(x, re_turns, len_gth) {
    boot_sample <- re_turns[sample.int(len_gth, replace=TRUE)]
    # Calculate prices from percentage returns
    boot_sample <- star_t*exp(cumsum(boot_sample))
    # Calculate statistic
    sum(boot_sample > bar_rier) > 0
  }, re_turns=re_turns, len_gth=len_gth)  # end parLapply
# Perform parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:n_boot,
  function(x) {
    boot_sample <- re_turns[sample.int(len_gth, replace=TRUE)]
    # Calculate prices from percentage returns
    boot_sample <- star_t*exp(cumsum(boot_sample))
    # Calculate statistic
    sum(boot_sample > bar_rier) > 0
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster under Windows
boot_data <- rutils::do_call(rbind, boot_data)
# Calculate frequency of crossing barrier
sum(boot_data)/n_boot

# Calculate percentage returns from VTI prices
oh_lc <- rutils::etf_env$VTI
price_s <- as.numeric(oh_lc[, 4])
star_t <- price_s[1]
re_turns <- rutils::diff_it(log(price_s))
len_gth <- NROW(re_turns)
# Calculate difference of OHLC price columns
ohlc_diff <- oh_lc[, 1:3] - price_s
class(re_turns); head(re_turns)
# Calculate bootstrap prices from percentage returns
da_ta <- sample.int(len_gth, replace=TRUE)
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
n_boot <- 1e4
boot_data <- parLapply(clus_ter, 1:n_boot,
  function(x, re_turns, len_gth) {
    # Calculate OHLC prices from percentage returns
    da_ta <- sample.int(len_gth, replace=TRUE)
    boot_prices <- star_t*exp(cumsum(re_turns[da_ta]))
    boot_ohlc <- ohlc_diff + boot_prices
    boot_ohlc <- cbind(boot_ohlc, boot_prices)
    # Calculate statistic
    sum(boot_ohlc[, 2] > bar_rier) > 0
  }, re_turns=re_turns, len_gth=len_gth)  # end parLapply
# Perform parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:n_boot,
  function(x) {
    # Calculate OHLC prices from percentage returns
    da_ta <- sample.int(len_gth, replace=TRUE)
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

set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
len_gth <- 1000
da_ta <- rnorm(len_gth)
# Estimate the 95% quantile
boot_data <- sapply(1:10000, function(x) {
  boot_sample <- da_ta[sample.int(len_gth,
                              replace=TRUE)]
  quantile(boot_sample, 0.95)
})  # end sapply
sd(boot_data)
# Estimate the 95% quantile using antithetic sampling
boot_data <- sapply(1:10000, function(x) {
  boot_sample <- da_ta[sample.int(len_gth,
                              replace=TRUE)]
  quantile(c(boot_sample, -boot_sample), 0.95)
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
curve(expr=dnorm(x, mean=1), add=TRUE,
lwd=3, col="red")
# Add vertical dashed lines
abline(v=0, lwd=3, col="blue", lty="dashed")
abline(v=1, lwd=3, col="red", lty="dashed")
arrows(x0=0, y0=0.1, x1=1, y1=0.1, lwd=3,
 code=2, angle=20,
 length=grid::unit(0.2, "cm"))
text(x=0.3, 0.1, labels=bquote(lambda),
     pos=3, cex=2)

set.seed(1121) # Reset random number generator
# Sample from Standard Normal Distribution
len_gth <- 1000
da_ta <- rnorm(len_gth)
# Cumulative probability from Naive Monte Carlo
quan_tile <- 2
pnorm(-quan_tile)
integrate(dnorm, low=quan_tile, up=Inf)
sum(da_ta > quan_tile)/len_gth
# Generate importance sample
lamb_da <- 1.5
data_is <- da_ta + lamb_da
# Cumulative probability from importance sample
sum(data_is > quan_tile)/len_gth
weight_s <- exp(-lamb_da*data_is + lamb_da^2/2)
sum((data_is > quan_tile)*weight_s)/len_gth
# Bootstrap of standard errors of cumulative probability
boot_data <- sapply(1:1000, function(x) {
  da_ta <- rnorm(len_gth)
  na_ive <- sum(da_ta > quan_tile)/len_gth
  da_ta <- (da_ta + lamb_da)
  weight_s <- exp(-lamb_da*da_ta + lamb_da^2/2)
  im_port <- sum((da_ta > quan_tile)*weight_s)/len_gth
  c(naive_mc=na_ive, importance=im_port)
}) # end sapply
apply(boot_data, MARGIN=1,
function(x) c(mean=mean(x), sd=sd(x)))

# Quantile from Naive Monte Carlo
conf_level <- 0.98
qnorm(conf_level)
da_ta <- sort(da_ta)
quantile(da_ta, probs=conf_level)
da_ta[conf_level*len_gth]
# Quantile from importance sample
wei_ght <- exp(-lamb_da*conf_level + lamb_da^2/2)
conf_import <- wei_ght*conf_level
quant_import <-
  da_ta[conf_import*len_gth] + lamb_da
# Bootstrap of standard errors of quantile
boot_data <- sapply(1:1000, function(x) {
  da_ta <- sort(rnorm(len_gth))
  na_ive <- da_ta[conf_level*len_gth]
  im_port <- da_ta[conf_level*exp(-lamb_da*conf_level + lamb_da^2/2)*len_gth] + lamb_da
  c(naive_mc=na_ive, importance=im_port)
}) # end sapply
apply(boot_data, MARGIN=1,
function(x) c(mean=mean(x), sd=sd(x)))

# Quantile from importance sample
quant_import <-
  da_ta[conf_import*len_gth] + lamb_da
# Expected value from Naive Monte Carlo
integrate(function(x) x*dnorm(x), low=quant_import, up=Inf)
sum((da_ta > quant_import)*da_ta)/len_gth
# Expected value from importance sample
data_is <- da_ta + lamb_da
weight_s <- exp(-lamb_da*data_is + lamb_da^2/2)
sum((data_is > quant_import)*data_is*weight_s)/len_gth
# Bootstrap of standard errors of expected value
boot_data <- sapply(1:1000, function(x) {
  da_ta <- rnorm(len_gth)
  na_ive <- sum((da_ta > quant_import)*da_ta)/len_gth
  da_ta <- (da_ta + lamb_da)
  weight_s <- exp(-lamb_da*da_ta + lamb_da^2/2)
  im_port <- sum((da_ta > quant_import)*da_ta*weight_s)/len_gth
  c(naive_mc=na_ive, importance=im_port)
}) # end sapply
apply(boot_data, MARGIN=1,
function(x) c(mean=mean(x), sd=sd(x)))

set.seed(1121) # Reset random number generator
# Sample from Standard Normal Distribution
len_gth <- 1000
da_ta <- rnorm(len_gth)
da_ta <- sort(da_ta)
# Monte Carlo cumulative probability
pnorm(-2)
integrate(dnorm, low=2, up=Inf)
sum(da_ta > 2)/len_gth
# Generate importance sample
lamb_da <- 1.5
data_is <- da_ta + lamb_da
# Importance cumulative probability
sum(data_is > 2)/len_gth
weight_s <- exp(-lamb_da*data_is + lamb_da^2/2)
sum((data_is > 2)*weight_s)/len_gth
# Bootstrap of standard errors of cumulative probability
boot_data <- sapply(1:1000, function(x) {
  da_ta <- rnorm(len_gth)
  na_ive <- sum(da_ta > 2)/len_gth
  da_ta <- (da_ta + lamb_da)
  weight_s <- exp(-lamb_da*da_ta + lamb_da^2/2)
  im_port <- sum((da_ta > 2)*weight_s)/len_gth
  c(naive_mc=na_ive, importance=im_port)
}) # end sapply
apply(boot_data, MARGIN=1,
function(x) c(mean=mean(x), sd=sd(x)))
# Monte Carlo expected value
integrate(function(x) x*dnorm(x), low=2, up=Inf)
sum((da_ta > 2)*da_ta)/len_gth
# Importance expected value
sum((data_is > 2)*data_is)/len_gth
sum((data_is > 2)*data_is*weight_s)/len_gth
# Bootstrap of standard errors of expected value
boot_data <- sapply(1:1000, function(x) {
  da_ta <- rnorm(len_gth)
  na_ive <- sum((da_ta > 2)*da_ta)/len_gth
  da_ta <- (da_ta + lamb_da)
  weight_s <- exp(-lamb_da*da_ta + lamb_da^2/2)
  im_port <- sum((da_ta > 2)*da_ta*weight_s)/len_gth
  c(naive_mc=na_ive, importance=im_port)
}) # end sapply
apply(boot_data, MARGIN=1,
function(x) c(mean=mean(x), sd=sd(x)))

# Binomial sample
len_gth <- 1000
pro_b <- 0.1
da_ta <- rbinom(n=len_gth, size=1, pro_b)
head(da_ta, 33)
fre_q <- sum(da_ta)/len_gth
# Tilted binomial sample
lamb_da <- 5
p_tilted <- lamb_da*pro_b/(1 + pro_b*(lamb_da - 1))
weigh_t <- (1 + pro_b*(lamb_da - 1))/lamb_da
da_ta <- rbinom(n=len_gth, size=1, p_tilted)
head(da_ta, 33)
weigh_t*sum(da_ta)/len_gth
# Bootstrap of standard errors
boot_data <- sapply(1:1000, function(x) {
  c(naive_mc=sum(rbinom(n=len_gth, size=1, pro_b))/len_gth,
    importance=weigh_t*sum(rbinom(n=len_gth, size=1, p_tilted))/len_gth)
}) # end sapply
apply(boot_data, MARGIN=1,
function(x) c(mean=mean(x), sd=sd(x)))

set.seed(1121)  # Initialize random number generator
# Define explanatory and response variables
de_sign <- rnorm(100, mean=2)
noise <- rnorm(100)
res_ponse <- -3 + de_sign + noise
# Define design matrix and regression formula
de_sign <- data.frame(res_ponse, de_sign)
for_mula <- paste(colnames(de_sign)[1],
  paste(colnames(de_sign)[-1], collapse="+"),
  sep=" ~ ")
# Bootstrap of regression
boot_data <- sapply(1:100, function(x) {
  boot_sample <- sample.int(NROW(de_sign),
                      replace=TRUE)
  mod_el <- lm(for_mula,
          data=de_sign[boot_sample, ])
  mod_el$coefficients
})  # end sapply

par(oma=c(1, 2, 1, 0), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
x11(width=6, height=6)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=1,
      function(x) c(mean=mean(x), std_error=sd(x)))
# Means and standard errors from regression summary
mod_el <- lm(for_mula, data=de_sign)
mod_el$coefficients
summary(mod_el)$coefficients[, "Std. Error"]
plot(density(boot_data["de_sign", ]),
     lwd=2, xlab="regression slopes",
     main="Bootstrapped regression slopes")
abline(v=mean(boot_data["de_sign", ]),
       lwd=2, col="red")

library(parallel)  # Load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster under Windows
# Bootstrap of regression under Windows
boot_data <- parLapply(clus_ter, 1:1000,
  function(x, for_mula, de_sign) {
    boot_sample <-
sample.int(NROW(de_sign), replace=TRUE)
    mod_el <- lm(for_mula,
data=de_sign[boot_sample, ])
    mod_el$coefficients
  },
  for_mula=for_mula,
  de_sign=de_sign)  # end parLapply
# Bootstrap of regression under Mac-OSX or Linux
boot_data <- mclapply(1:1000,
  function(x) {
    boot_sample <-
sample.int(NROW(de_sign), replace=TRUE)
    lm(for_mula,
data=de_sign[boot_sample, ])$coefficients
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster under Windows
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))
x11(width=6, height=6)
plot(density(boot_data[, "de_sign"]),
     lwd=2, xlab="regression slopes",
     main="Bootstrapped regression slopes")
abline(v=mean(boot_data[, "de_sign"]),
 lwd=2, col="red")

library(parallel)  # Load package parallel
# Get short description
packageDescription("parallel")
# Load help page
help(package="parallel")
# list all objects in "parallel"
ls("package:parallel")

library(parallel)  # Load package parallel
# Calculate number of available cores
n_cores <- detectCores() - 1
# Define function that pauses execution
paws <- function(x, sleep_time) {
  Sys.sleep(sleep_time)
  x
}  # end paws
# Perform parallel loop under Mac-OSX or Linux
paw_s <- mclapply(1:10, paws, mc.cores=n_cores,
          sleep_time=0.01)
# Initialize compute cluster under Windows
clus_ter <- makeCluster(n_cores)
# Perform parallel loop under Windows
paw_s <- parLapply(clus_ter, 1:10, paws,
           sleep_time=0.01)
library(microbenchmark)  # Load package microbenchmark
# Compare speed of lapply versus parallel computing
summary(microbenchmark(
  l_apply=lapply(1:10, paws, sleep_time=0.01),
  parl_apply=
    parLapply(clus_ter, 1:10, paws, sleep_time=0.01),
  times=10)
)[, c(1, 4, 5)]
# Stop R processes over cluster under Windows
stopCluster(clus_ter)

library(parallel)  # Load package parallel
# Calculate number of available cores
n_cores <- detectCores() - 1
# Initialize compute cluster under Windows
clus_ter <- makeCluster(n_cores)
# Define function that pauses execution
paws <- function(x, sleep_time) {
  Sys.sleep(sleep_time)
  x
}  # end paws
# Compare speed of lapply with parallel computing
iter_ations <- 3:10
compute_times <- sapply(iter_ations,
  function(max_iterations, sleep_time) {
    out_put <- summary(microbenchmark(
lapply=lapply(1:max_iterations, paws,
              sleep_time=sleep_time),
parallel=parLapply(clus_ter, 1:max_iterations,
        paws, sleep_time=sleep_time),
times=10))[, c(1, 4)]
    structure(out_put[, 2],
        names=as.vector(out_put[, 1]))
    }, sleep_time=0.01)
compute_times <- t(compute_times)
rownames(compute_times) <- iter_ations

library(parallel)  # Load package parallel
plot(x=rownames(compute_times),
     y=compute_times[, "lapply"],
     type="l", lwd=2, col="blue",
     main="Compute times",
     xlab="number of iterations in loop", ylab="",
     ylim=c(0, max(compute_times[, "lapply"])))
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
# Define large matrix
mat_rix <- matrix(rnorm(7*10^5), ncol=7)
# Define aggregation function over column of matrix
agg_regate <- function(col_umn) {
  out_put <- 0
  for (in_dex in 1:NROW(col_umn))
    out_put <- out_put + col_umn[in_dex]
  out_put
}  # end agg_regate
# Perform parallel aggregations over columns of matrix
agg_regations <-
  parCapply(clus_ter, mat_rix, agg_regate)
# Compare speed of apply with parallel computing
summary(microbenchmark(
  ap_ply=apply(mat_rix, MARGIN=2, agg_regate),
  parl_apply=
    parCapply(clus_ter, mat_rix, agg_regate),
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

options(width=50, dev='pdf')
str(optimize)
# Objective function with multiple minima
object_ive <- function(in_put, param1=0.01) {
  sin(0.25*pi*in_put) + param1*(in_put-1)^2
}  # end object_ive
unlist(optimize(f=object_ive, interval=c(-4, 2)))
unlist(optimize(f=object_ive, interval=c(0, 8)))
options(width=60, dev='pdf')

par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(5, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot the objective function
curve(expr=object_ive, type="l", xlim=c(-8, 9),
xlab="", ylab="", lwd=2)
# Add title
title(main="Objective Function", line=-1)

library(rgl)  # Load rgl
# Define function of two variables
sur_face <- function(x, y) y*sin(x)
# Draw 3d surface plot of function
persp3d(x=sur_face, xlim=c(-5, 5), ylim=c(-5, 5),
  col="green", axes=FALSE)
# Draw 3d surface plot of matrix
x_lim <- seq(from=-5, to=5, by=0.1)
y_lim <- seq(from=-5, to=5, by=0.1)
persp3d(z=outer(x_lim, y_lim, FUN=sur_face),
  xlab="x", ylab="y", zlab="sur_face",
  col="green")
# Save current view to png file
rgl.snapshot("surface_plot.png")
# Define function of two variables and two parameters
sur_face <- function(x, y, par_1=1, par_2=1)
  sin(par_1*x)*sin(par_2*y)
# Draw 3d surface plot of function
persp3d(x=sur_face, xlim=c(-5, 5), ylim=c(-5, 5),
  col="green", axes=FALSE,
  par_1=1, par_2=2)

# Rastrigin function with vector argument for optimization
rastri_gin <- function(vec_tor, pa_ram=25){
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
object_ive <- function(pa_r, da_ta) {
  sum(2*log(pa_r[2]) +
    ((da_ta - pa_r[1])/pa_r[2])^2)
}  # end object_ive
# Vectorize objective function
vec_objective <- Vectorize(
  FUN=function(mean, sd, da_ta)
    object_ive(c(mean, sd), da_ta),
  vectorize.args=c("mean", "sd")
)  # end Vectorize
# Objective function on parameter grid
par_mean <- seq(1, 6, length=50)
par_sd <- seq(0.5, 3.0, length=50)
objective_grid <- outer(par_mean, par_sd,
vec_objective, da_ta=da_ta)
objective_min <- which(  # grid search
  objective_grid==min(objective_grid),
  arr.ind=TRUE)
objective_min
par_mean[objective_min[1]]  # mean
par_sd[objective_min[2]]  # sd
objective_grid[objective_min]
objective_grid[(objective_min[, 1] + -1:1),
       (objective_min[, 2] + -1:1)]

par(cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
# Perspective plot of log-likelihood function
persp(z=-objective_grid,
theta=45, phi=30, shade=0.5,
border="green", zlab="objective",
main="objective function")
# Interactive perspective plot of log-likelihood function
library(rgl)  # Load package rgl
par3d(cex=2.0)  # Scale text by factor of 2
persp3d(z=-objective_grid, zlab="objective",
  col="green", main="objective function")

# Initial parameters
par_init <- c(mean=0, sd=1)
# Perform optimization using optim()
optim_fit <- optim(par=par_init,
  fn=object_ive, # log-likelihood function
  da_ta=da_ta,
  method="L-BFGS-B", # quasi-Newton method
  upper=c(10, 10), # upper constraint
  lower=c(-10, 0.1)) # lower constraint
# Optimal parameters
optim_fit$par
# Perform optimization using MASS::fitdistr()
optim_fit <- MASS::fitdistr(da_ta, densfun="normal")
optim_fit$estimate
optim_fit$sd

# Plot histogram
histo_gram <- hist(da_ta, plot=FALSE)
plot(histo_gram, freq=FALSE,
     main="histogram of sample")
curve(expr=dnorm(x, mean=optim_fit$par["mean"],
           sd=optim_fit$par["sd"]),
add=TRUE, type="l", lwd=2, col="red")
legend("topright", inset=0.0, cex=0.8, title=NULL,
 leg="optimal parameters",
 lwd=2, bg="white", col="red")

# Sample from mixture of normal distributions
da_ta <- c(rnorm(100, sd=1.0),
      rnorm(100, mean=4, sd=1.0))
# Objective function is log-likelihood
object_ive <- function(pa_r, da_ta) {
  likelihood <- pa_r[1]/pa_r[3] *
  dnorm((da_ta-pa_r[2])/pa_r[3]) +
  (1-pa_r[1])/pa_r[5]*dnorm((da_ta-pa_r[4])/pa_r[5])
  if (any(likelihood <= 0)) Inf else
    -sum(log(likelihood))
}  # end object_ive
# Vectorize objective function
vec_objective <- Vectorize(
  FUN=function(mean, sd, w, m1, s1, da_ta)
    object_ive(c(w, m1, s1, mean, sd), da_ta),
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
      fn=object_ive,
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
  pa_r["weight"] *
    dnorm(x, mean=pa_r["m1"], sd=pa_r["s1"]) +
  (1-pa_r["weight"]) *
    dnorm(x, mean=pa_r["m2"], sd=pa_r["s2"])
}  # end fit_func
curve(expr=fit_func(x, pa_r=optim_fit$par), add=TRUE,
type="l", lwd=2, col="red")
legend("topright", inset=0.0, cex=0.8, title=NULL,
 leg="optimal parameters",
 lwd=2, bg="white", col="red")

# Rastrigin function with vector argument for optimization
rastri_gin <- function(vec_tor, pa_ram=25){
  sum(vec_tor^2 - pa_ram*cos(vec_tor))
}  # end rastri_gin
vec_tor <- c(pi/6, pi/6)
rastri_gin(vec_tor=vec_tor)
library(DEoptim)
Optimize rastri_gin using DEoptim
op_tim <-  DEoptim(rastri_gin,
  upper=c(6, 6), lower=c(-6, -6),
  DEoptim.control(trace=FALSE, itermax=50))
# Optimal parameters and value
op_tim$optim$bestmem
rastri_gin(op_tim$optim$bestmem)
summary(op_tim)
plot(op_tim)

# Rastrigin function with vector argument for optimization
rastri_gin <- function(vec_tor, pa_ram=25){
  sum(vec_tor^2 - pa_ram*cos(vec_tor))
}  # end rastri_gin
vec_tor <- c(pi/6, pi/6)
rastri_gin(vec_tor=vec_tor)
library(DEoptim)
Optimize rastri_gin using DEoptim
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
# list all datasets in "Rcpp"
data(package="Rcpp")
# list all objects in "Rcpp"
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
Rcpp::sourceCpp(file="C:/Develop/lecture_slides/scripts/mult_rcpp.cpp")
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
boot_data <- function(da_ta, n_boot=1000) {
  boot_data <- sapply(1:10000, function(x) {
    boot_sample <-
da_ta[sample.int(len_gth, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  })  # end sapply
  boot_data <- t(boot_data)
  # Analyze bootstrapped variance
  head(boot_data)
  sum(is.na(boot_data))
  # Means and standard errors from bootstrap
  apply(boot_data, MARGIN=2,
  function(x) c(mean=mean(x), std_error=sd(x)))

  re_turns <- numeric(len_gth)
  price_s <- numeric(len_gth)
  price_s[1] <- eq_price
  for (i in 2:len_gth) {
    re_turns[i] <- the_ta*(eq_price - price_s[i-1]) + vol_at*rnorm(1)
    price_s[i] <- price_s[i-1] * exp(re_turns[i])
  }  # end for
  price_s
}  # end boot_data
# Simulate Ornstein-Uhlenbeck process in R
eq_price <- 5.0; sigma_r <- 0.01
the_ta <- 0.01; len_gth <- 1000
set.seed(1121)  # Reset random numbers
ou_sim <- sim_ou(len_gth=len_gth, eq_price=eq_price, vol_at=sigma_r, the_ta=the_ta)

# Define Ornstein-Uhlenbeck function in R
sim_ou <- function(len_gth=1000, eq_price=5.0,
              vol_at=0.01, the_ta=0.01) {
  re_turns <- numeric(len_gth)
  price_s <- numeric(len_gth)
  price_s[1] <- eq_price
  for (i in 2:len_gth) {
    re_turns[i] <- the_ta*(eq_price - price_s[i-1]) + vol_at*rnorm(1)
    price_s[i] <- price_s[i-1] * exp(re_turns[i])
  }  # end for
  price_s
}  # end sim_ou
# Simulate Ornstein-Uhlenbeck process in R
eq_price <- 5.0; sigma_r <- 0.01
the_ta <- 0.01; len_gth <- 1000
set.seed(1121)  # Reset random numbers
ou_sim <- sim_ou(len_gth=len_gth, eq_price=eq_price, vol_at=sigma_r, the_ta=the_ta)

# Define Ornstein-Uhlenbeck function in Rcpp
Rcpp::cppFunction("
NumericVector sim_ou_rcpp(double eq_price,
                double vol_at,
                double the_ta,
                NumericVector in_nov) {
  int len_gth = in_nov.size();
  NumericVector price_s(len_gth);
  NumericVector re_turns(len_gth);
  price_s[0] = eq_price;
  for (int it = 1; it < len_gth; it++) {
    re_turns[it] = the_ta*(eq_price - price_s[it-1]) + vol_at*in_nov[it-1];
    price_s[it] = price_s[it-1] * exp(re_turns[it]);
  }  // end for
  return price_s;
}")  # end cppFunction
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121)  # Reset random numbers
ou_sim_rcpp <- sim_ou_rcpp(eq_price=eq_price,
  vol_at=sigma_r,
  the_ta=the_ta,
  in_nov=rnorm(len_gth))
all.equal(ou_sim, ou_sim_rcpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=sim_ou(len_gth=len_gth, eq_price=eq_price, vol_at=sigma_r, the_ta=the_ta),
  r_cpp=sim_ou_rcpp(eq_price=eq_price, vol_at=sigma_r, the_ta=the_ta, in_nov=rnorm(len_gth)),
  times=10))[, c(1, 4, 5)]

# Source Rcpp function for Ornstein-Uhlenbeck process from file
Rcpp::sourceCpp(file="C:/Develop/lecture_slides/scripts/sim_ou.cpp")
# Simulate Ornstein-Uhlenbeck process in Rcpp
set.seed(1121)  # Reset random numbers
ou_sim_rcpp <- sim_ou_rcpp(eq_price=eq_price,
  vol_at=sigma_r,
  the_ta=the_ta,
  in_nov=rnorm(len_gth))
all.equal(ou_sim, ou_sim_rcpp)
# Compare speed of Rcpp and R
library(microbenchmark)
summary(microbenchmark(
  pure_r=sim_ou(len_gth=len_gth, eq_price=eq_price, vol_at=sigma_r, the_ta=the_ta),
  r_cpp=sim_ou_rcpp(eq_price=eq_price, vol_at=sigma_r, the_ta=the_ta, in_nov=rnorm(len_gth)),
  times=10))[, c(1, 4, 5)]

# Calculate uniformly distributed pseudo-random sequence
uni_form <- function(see_d, len_gth=10) {
  out_put <- numeric(len_gth)
  out_put[1] <- see_d
  for (i in 2:len_gth) {
    out_put[i] <- 4*out_put[i-1]*(1-out_put[i-1])
  }  # end for
  acos(1-2*out_put)/pi
}  # end uni_form

# Source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/lecture_slides/scripts/uni_form.cpp")
# Microbenchmark Rcpp code
library(microbenchmark)
summary(microbenchmark(
  pure_r=runif(1e5),
  r_loop=uni_form(0.3, 1e5),
  r_cpp=uniform_rcpp(0.3, 1e5),
  times=10))[, c(1, 4, 5)]

# Define Ornstein-Uhlenbeck function in R
boot_data <- function(da_ta, n_boot=1000) {
  boot_data <- sapply(1:10000, function(x) {
    boot_sample <-
da_ta[sample.int(len_gth, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  })  # end sapply
  boot_data <- t(boot_data)
  # Analyze bootstrapped variance
  head(boot_data)
  sum(is.na(boot_data))
  # Means and standard errors from bootstrap
  apply(boot_data, MARGIN=2,
  function(x) c(mean=mean(x), std_error=sd(x)))

  re_turns <- numeric(len_gth)
  price_s <- numeric(len_gth)
  price_s[1] <- eq_price
  for (i in 2:len_gth) {
    re_turns[i] <- the_ta*(eq_price - price_s[i-1]) + vol_at*rnorm(1)
    price_s[i] <- price_s[i-1] * exp(re_turns[i])
  }  # end for
  price_s
}  # end boot_data
# Simulate Ornstein-Uhlenbeck process in R
eq_price <- 5.0; sigma_r <- 0.01
the_ta <- 0.01; len_gth <- 1000
set.seed(1121)  # Reset random numbers
ou_sim <- sim_ou(len_gth=len_gth, eq_price=eq_price, vol_at=sigma_r, the_ta=the_ta)

# Define Ornstein-Uhlenbeck function in R
boot_data <- function(da_ta, n_boot=1000) {
  boot_data <- sapply(1:10000, function(x) {
    boot_sample <-
da_ta[sample.int(len_gth, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  })  # end sapply
  boot_data <- t(boot_data)
  # Analyze bootstrapped variance
  head(boot_data)
  sum(is.na(boot_data))
  # Means and standard errors from bootstrap
  apply(boot_data, MARGIN=2,
  function(x) c(mean=mean(x), std_error=sd(x)))

  re_turns <- numeric(len_gth)
  price_s <- numeric(len_gth)
  price_s[1] <- eq_price
  for (i in 2:len_gth) {
    re_turns[i] <- the_ta*(eq_price - price_s[i-1]) + vol_at*rnorm(1)
    price_s[i] <- price_s[i-1] * exp(re_turns[i])
  }  # end for
  price_s
}  # end boot_data
# Simulate Ornstein-Uhlenbeck process in R
eq_price <- 5.0; sigma_r <- 0.01
the_ta <- 0.01; len_gth <- 1000
set.seed(1121)  # Reset random numbers
ou_sim <- sim_ou(len_gth=len_gth, eq_price=eq_price, vol_at=sigma_r, the_ta=the_ta)

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/lecture_slides/scripts/armadillo_functions.cpp")
vec1 <- runif(1e5)
vec2 <- runif(1e5)
vec_in(vec1, vec2)
vec1 %*% vec2

# Microbenchmark RcppArmadillo code
summary(microbenchmark(
  vec_in=vec_in(vec1, vec2),
  r_code=(vec1 %*% vec2),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Microbenchmark shows:
# vec_in() is several times faster than %*%, especially for longer vectors.
#     expr     mean   median
# 1 vec_in 110.7067 110.4530
# 2 r_code 585.5127 591.3575

# Source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/lecture_slides/scripts/sim_arima.cpp")
# Define AR(2) coefficients
co_eff <- c(0.9, 0.09)
len_gth <- 1e4
set.seed(1121)
in_nov <- rnorm(len_gth)
# Simulate ARIMA using filter()
arima_filter <- filter(x=in_nov,
  filter=co_eff, method="recursive")
# Simulate ARIMA using sim_arima()
ari_ma <- sim_arima(in_nov, rev(co_eff))
all.equal(drop(ari_ma),
  as.numeric(arima_filter))
# Microbenchmark RcppArmadillo code
summary(microbenchmark(
  filter=filter(x=in_nov, filter=co_eff, method="recursive"),
  sim_arima=sim_arima(in_nov, rev(co_eff)),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp(file="C:/Develop/lecture_slides/scripts/armadillo_functions.cpp")
mat_rix <- matrix(runif(1e5), nc=1e3)
# De-mean using apply()
new_mat <- apply(mat_rix, 2,
  function(x) (x-mean(x)))
# De-mean using demean_mat()
demean_mat(mat_rix)
all.equal(new_mat, mat_rix)
# Microbenchmark RcppArmadillo code
summary(microbenchmark(
  demean_mat=demean_mat(mat_rix),
  apply=(apply(mat_rix, 2, mean)),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Microbenchmark shows:
# Demean_mat() is over 70 times faster than apply()
#         expr       mean   median
# 1 demean_mat   127.7539  125.604
# 2      apply 10781.7534 9291.674

# Perform matrix inversion
# Create random positive semi-definite matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- t(mat_rix) %*% mat_rix
# Invert the matrix
matrix_inv <- solve(mat_rix)
inv_mat(mat_rix)
all.equal(inv_mat, mat_rix)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  inv_mat=inv_mat(mat_rix),
  solve=solve(mat_rix),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary
# Microbenchmark shows:
# inv_mat() is over 10 times faster than solve()
#      expr     mean median
# 1 inv_mat  3.42669  2.933
# 2 solve   32.00254 31.280

library(RcppArmadillo)
# Source Rcpp functions from file
Rcpp::sourceCpp("C:/Develop/lecture_slides/scripts/calc_weights.cpp")
# Create random matrix of returns
mat_rix <- matrix(rnorm(300), nc=5)
# Regularized inverse of covariance matrix
max_eigen <- 4
ei_gen <- eigen(cov(mat_rix))
cov_inv <- ei_gen$vectors[, 1:max_eigen] %*%
  (t(ei_gen$vectors[, 1:max_eigen]) / ei_gen$values[1:max_eigen])
# Regularized inverse using RcppArmadillo
cov_inv_arma <- calc_inv(mat_rix, max_eigen)
all.equal(cov_inv, cov_inv_arma)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  pure_r={
    ei_gen <- eigen(cov(mat_rix))
    ei_gen$vectors[, 1:max_eigen] %*%
(t(ei_gen$vectors[, 1:max_eigen]) / ei_gen$values[1:max_eigen])
  },
  r_cpp=calc_inv(mat_rix, max_eigen),
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
# list all datasets in "reticulate"
data(package="reticulate")
# list all objects in "reticulate"
ls("package:reticulate")
# Remove reticulate from search path
detach("package:reticulate")
