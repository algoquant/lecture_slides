# Calculate the square root of a vector
vec_tor <- runif(1e6)
all.equal(sqrt(vec_tor), vec_tor^0.5)
# Microbenchmark the two methods
library(microbenchmark)
summary(microbenchmark(
  sqrt(vec_tor),
  vec_tor^0.5,
  times=10))[, c(1, 4, 5)]
library(microbenchmark)
va_r <- runif(1e6)
system.time(va_r^0.5)
microbenchmark(sqrt(va_r), va_r^0.5, times=10)
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
library(matrixStats)  # load package matrixStats
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
library(Rfast)  # load package Rfast
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
library(parallel)  # load package parallel
# Get short description
packageDescription("parallel")
# load help page
help(package="parallel")
# list all objects in "parallel"
ls("package:parallel")
library(parallel)  # load package parallel
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
library(microbenchmark)  # load package microbenchmark
# Compare speed of lapply versus parallel computing
summary(microbenchmark(
  l_apply=lapply(1:10, paws, sleep_time=0.01),
  parl_apply=
    parLapply(clus_ter, 1:10, paws, sleep_time=0.01),
  times=10)
)[, c(1, 4, 5)]
# Stop R processes over cluster under Windows
stopCluster(clus_ter)
library(parallel)  # load package parallel
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
library(parallel)  # load package parallel
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
library(parallel)  # load package parallel
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
library(parallel)  # load package parallel
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
library(parallel)  # load package parallel
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
# Sample from Standard Normal Distribution
len_gth <- 1000; r_norm <- rnorm(len_gth)
# Sample mean and standard deviation
mean(r_norm); sd(r_norm)
# Bootstrap of sample mean and median
boot_strap <- sapply(1:10000, function(x) {
  boot_sample <- r_norm[sample.int(len_gth,
                              replace=TRUE)]
  c(mean=mean(boot_sample), median=median(boot_sample))
})  # end sapply
boot_strap <- t(boot_strap)
boot_strap[1:3, ]
# Standard error from formula
sd(r_norm)/sqrt(len_gth)
# Standard error of mean from bootstrap
sd(boot_strap[, "mean"])
# Standard error of median from bootstrap
sd(boot_strap[, "median"])
plot(density(boot_strap[, "median"]),
     lwd=2, xlab="regression slopes",
     main="Distribution of Bootstrapped Median")
abline(v=mean(boot_strap[, "median"]),
 lwd=2, col="red")
set.seed(1121)  # Reset random number generator
len_gth <- 1000
r_norm <- rnorm(len_gth)
# Bootstrap of sample mean and median
boot_strap <- sapply(1:10000, function(x) {
  # Boot_sample from Standard Normal Distribution
  boot_sample <- rnorm(len_gth)
  c(mean=mean(boot_sample),
    median=median(boot_sample))
})  # end sapply
boot_strap[, 1:3]
# Standard error from formula
sd(r_norm)/sqrt(len_gth)
# Standard error of mean from bootstrap
sd(boot_strap["mean", ])
# Standard error of median from bootstrap
sd(boot_strap["median", ])
library(parallel)  # load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster under Windows
set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
len_gth <- 1000
r_norm <- rnorm(len_gth)
# Bootstrap mean and median under Windows
boot_strap <- parLapply(clus_ter, 1:10000,
  function(x, r_norm, len_gth) {
  boot_sample <- r_norm[sample.int(len_gth, replace=TRUE)]
  c(mean=mean(boot_sample), median=median(boot_sample))
  }, r_norm=r_norm, len_gth=len_gth)  # end parLapply
# Bootstrap mean and median under Mac-OSX or Linux
boot_strap <- mclapply(1:10000,
  function(x) {
  boot_sample <- r_norm[sample.int(len_gth, replace=TRUE)]
  c(mean=mean(boot_sample), median=median(boot_sample))
  }, mc.cores=n_cores)  # end mclapply
boot_strap <- rutils::do_call(rbind, boot_strap)
# Means and standard errors from bootstrap
apply(boot_strap, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))
# Standard error from formula
sd(r_norm)/sqrt(len_gth)
stopCluster(clus_ter)  # Stop R processes over cluster under Windows
len_gth <- 1000
r_norm <- rnorm(len_gth)
sd(r_norm)
mad(r_norm)
median(abs(r_norm - median(r_norm)))
median(abs(r_norm - median(r_norm)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
boot_strap <- sapply(1:10000, function(x) {
  boot_sample <-
    r_norm[sample.int(len_gth, replace=TRUE)]
  c(sd=sd(boot_sample), mad=mad(boot_sample))
})  # end sapply
boot_strap <- t(boot_strap)
# Analyze bootstrapped variance
head(boot_strap)
sum(is.na(boot_strap))
# Means and standard errors from bootstrap
apply(boot_strap, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster
boot_strap <- parLapply(clus_ter, 1:10000,
  function(x, r_norm) {
    boot_sample <- r_norm[sample.int(len_gth, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, r_norm=r_norm)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
boot_strap <- mclapply(1:10000,
  function(x) {
    boot_sample <- r_norm[sample.int(len_gth, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster
boot_strap <- rutils::do_call(rbind, boot_strap)
# Means and standard errors from bootstrap
apply(boot_strap, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))
# Sample from time series of ETF returns
re_turns <- rutils::etf_env$re_turns[, "VTI"]
re_turns <- na.omit(re_turns)
len_gth <- NROW(re_turns)
# Bootstrap sd and MAD under Windows
library(parallel)  # load package parallel
n_cores <- detectCores() - 1  # Number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster under Windows
clusterSetRNGStream(clus_ter, 1121)  # Reset random number generator in all cores
n_boot <- 1e4
boot_strap <- parLapply(clus_ter, 1:n_boot,
  function(x, re_turns, len_gth) {
    boot_sample <- re_turns[sample.int(len_gth, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, re_turns=re_turns, len_gth=len_gth)  # end parLapply
# Bootstrap sd and MAD under Mac-OSX or Linux
boot_strap <- mclapply(1:n_boot,
  function(x) {
    boot_sample <- re_turns[sample.int(len_gth, replace=TRUE)]
    c(sd=sd(boot_sample), mad=mad(boot_sample))
  }, mc.cores=n_cores)  # end mclapply
boot_strap <- rutils::do_call(rbind, boot_strap)
# Means and standard errors from bootstrap
apply(boot_strap, MARGIN=2,
function(x) c(mean=mean(x), std_error=sd(x)))
# Standard error assuming normal distribution of returns
sd(re_turns)/sqrt(n_boot)
stopCluster(clus_ter)  # Stop R processes over cluster under Windows
# Calculate random default probabilities
n_assets <- 100
def_probs <- runif(n_assets, max=0.2)
mean(def_probs)
# Calculate number of defaults
uni_form <- runif(n_assets)
sum(uni_form < def_probs)
# Simulate average number of defaults
n_simu <- 1000
de_faults <- numeric(n_simu)
# Simulate using for() loop (inefficient way)
for (i in 1:n_simu) {  # perform loop
  uni_form <- runif(n_assets)
  de_faults[i] <- sum(uni_form < def_probs)
}  # end for
# Calculate average number of defaults
mean(de_faults)
# Simulate using vectorized functions  (efficient way)
uni_form <- matrix(runif(n_simu*n_assets),
             ncol=n_simu)
sum(uni_form < def_probs)/n_simu
# Plot Standard Normal distribution
curve(expr=dnorm(x),
type="l", xlim=c(-4, 4),
xlab="asset value", ylab="", lwd=2,
col="blue", main="Distribution of Asset Values")
abline(v=qnorm(0.025), col="red", lwd=2)
text(x=qnorm(0.025)-0.1, y=0.15,
 labels="default threshold",
 lwd=2, srt=90, pos=3)
# Define correlation parameters
rh_o <- 0.2
rho_sqrt <- sqrt(rh_o) ; rho_sqrtm <- sqrt(1-rh_o)
n_assets <- 5 ; n_simu <- 10000
# Calculate vector of systematic factors
system_atic <- rnorm(n_simu)
# Simulate asset values using vectorized functions (efficient way)
asset_s <- rho_sqrt*system_atic +
  rho_sqrtm*rnorm(n_simu*n_assets)
dim(asset_s) <- c(n_simu, n_assets)
# Calculate correlations between asset values
cor(asset_s)
# Simulate asset values using for() loop (inefficient way)
# allocate matrix of assets
asset_s <- matrix(nr=n_simu, nc=n_assets)
# Simulate asset values using for() loop
for (i in 1:n_simu) {  # perform loop
  asset_s[i, ] <-
    rho_sqrt*system_atic[i] +
    rho_sqrtm*rnorm(n_assets)
}  # end for
cor(asset_s)
# benchmark the speed of the two methods
library(microbenchmark)
summary(microbenchmark(
  for_loop={for (i in 1:n_simu) {
    rho_sqrt*system_atic[i] +
    rho_sqrtm*rnorm(n_assets)}},
  vector_ized={rho_sqrt*system_atic +
        rho_sqrtm*rnorm(n_simu*n_assets)},
  times=10))[, c(1, 4, 5)]
# Calculate random default probabilities
n_assets <- 5
def_probs <- runif(n_assets, max=0.2)
mean(def_probs)
# Calculate default thresholds
def_thresh <- qnorm(def_probs)
# Calculate number of defaults using vectorized functions (efficient way)
# Calculate vector of number of defaults
de_faults <-
  colSums(t(t(asset_s) < def_thresh))
de_faults / n_simu
def_probs
# Calculate number of defaults using for() loop (inefficient way)
# allocate matrix of de_faults
de_faults <- matrix(nr=n_simu, nc=n_assets)
# Simulate asset values using for() loop
for (i in 1:n_simu) {  # perform loop
  de_faults[i, ] <-
    (asset_s[i, ] < def_thresh)
}  # end for
colSums(de_faults) / n_simu
def_probs
# Calculate correlations between defaults
cor(de_faults)
# Define default probabilities
n_assets <- 2
def_prob <- 0.2
def_thresh <- qnorm(def_prob)
# Define correlation parameters
rh_o <- 0.2
rho_sqrt <- sqrt(rh_o) ; rho_sqrtm <- sqrt(1-rh_o)
# Calculate vector of systematic factors
n_simu <- 1000
system_atic <- rnorm(n_simu)
# Simulate asset values using vectorized functions
asset_s <- rho_sqrt*system_atic +
  rho_sqrtm*rnorm(n_simu*n_assets)
dim(asset_s) <- c(n_simu, n_assets)
# Calculate number of defaults using vectorized functions
de_faults <- t(t(asset_s) < def_thresh)
# Calculate correlations between defaults
cor(de_faults)
# Calculate average number of defaults and compare to def_prob
colSums(de_faults) / n_simu
def_prob
# Define cumulative default probability function
def_cumdistr <- function(x, def_thresh=(-2), rh_o=0.2)
  pnorm((sqrt(1-rh_o)*qnorm(x) - def_thresh)/sqrt(rh_o))
def_cumdistr(x=0.2, def_thresh=qnorm(def_prob), rh_o=rh_o)
# Plot cumulative default probability function
def_prob <- 0.4; def_thresh <- qnorm(def_prob)
curve(expr=def_cumdistr(x, def_thresh=def_thresh, rh_o=0.05),
xlim=c(0, 0.999), lwd=3,
xlab="percent default", ylab="probability",
col="green", main="Cumulative Default Probabilities")
# Plot default distribution with higher correlation
curve(expr=def_cumdistr(x, def_thresh=def_thresh, rh_o=0.2),
xlim=c(0, 0.999), add=TRUE, lwd=3,
col="blue", main="")
# Add legend
legend(x="topleft",
 legend=c("high correlation", "low correlation"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 bty="n", lwd=6, lty=1, col=c("blue", "green"))
# Add unconditional default probability
abline(v=def_prob, col="red", lwd=3)
text(x=def_prob, y=0.0,
 labels="default probability",
 lwd=2, srt=90, pos=4)
# Define default probability density function
def_distr <- function(x, def_thresh=(-2), rh_o=0.2)
  sqrt((1-rh_o)/rh_o)*exp(-(sqrt(1-rh_o)*qnorm(x) -
  def_thresh)^2/(2*rh_o) + qnorm(x)^2/2)
# Define parameters
rh_o <- 0.2 ; rho_sqrt <- sqrt(rh_o) ; rho_sqrtm <- sqrt(1-rh_o)
def_prob <- 0.3; def_thresh <- qnorm(def_prob)
def_distr(0.03, def_thresh=def_thresh, rh_o=rh_o)
# Plot probability distribution of defaults
curve(expr=def_distr(x, def_thresh=def_thresh, rh_o=0.1),
xlim=c(0, 1.0), lwd=3,
xlab="percentage of defaults", ylab="density",
col="green", main="Distribution of Defaults")
# Plot default distribution with higher correlation
curve(expr=def_distr(x, def_thresh=def_thresh, rh_o=0.3),
xlab="default percentage", ylab="",
add=TRUE, lwd=3, col="blue", main="")
# Add legend
legend(x="topright",
 legend=c("high correlation", "low correlation"),
 title=NULL, inset=0.05, cex=0.8, bg="white",
 bty="n", lwd=6, lty=1, col=c("blue", "green"))
# Add unconditional default probability
abline(v=def_prob, col="red", lwd=3)
text(x=def_prob, y=2,
 labels="default probability",
 lwd=2, srt=90, pos=2)
# Plot default distribution with low correlation
curve(expr=def_distr(x, def_thresh=def_thresh, rh_o=0.01),
xlab="default percentage", ylab="", lwd=2,
col="green", main="Distribution of Defaults")
# Plot default distribution with high correlation
curve(expr=def_distr(x, def_thresh=def_thresh, rh_o=0.99),
xlab="percentage of defaults", ylab="density",
add=TRUE, lwd=2, n=10001, col="blue", main="")
# Add legend
legend(x="top",
 legend=c("high correlation", "low correlation"),
 title=NULL, inset=0.1, cex=0.8, bg="white",
 bty="n", lwd=6, lty=1, col=c("blue", "green"))
# Add unconditional default probability
abline(v=0.1, col="red", lwd=2)
text(x=0.1, y=10, lwd=2, pos=4,
 labels="default probability")
rh_o <- 0.1; l_gd <- 0.4
# Define Vasicek loss distribution density function
loss_distr <- function(x, def_thresh=(-2), rh_o=0.2, l_gd=0.4)
  sqrt((1-rh_o)/rh_o)*exp(-(sqrt(1-rh_o)*qnorm(x/l_gd) - def_thresh)^2/(2*rh_o) + qnorm(x/l_gd)^2/2)/l_gd
integrate(loss_distr, low=0, up=l_gd,
  def_thresh=(-2), rh_o=rh_o, l_gd=l_gd)
# Plot probability distribution of losses
def_prob <- 0.05; def_thresh <- qnorm(def_prob)
curve(expr=loss_distr(x, def_thresh=def_thresh, rh_o=rh_o),
type="l", xlim=c(0, 0.06),
xlab="loss percentage", ylab="density", lwd=3,
col="orange", main="Distribution of Losses")
# Add line for expected loss
abline(v=l_gd*def_prob, col="red", lwd=3)
text(x=l_gd*def_prob-0.001, y=10, labels="expected loss",
 lwd=2, srt=90, pos=3)
# Add lines for unexpected loss
abline(v=0.04, col="blue", lwd=3)
arrows(x0=0.02, y0=35, x1=0.04, y1=35,
 code=3, lwd=3, cex=0.5)
text(x=0.03, y=36, labels="unexpected loss",
     lwd=2, pos=3)
# Add lines for VaR
abline(v=0.055, col="red", lwd=3)
arrows(x0=0.0, y0=25, x1=0.055, y1=25,
 code=3, lwd=3, cex=0.5)
text(x=0.03, y=26, labels="VaR", lwd=2, pos=3)
text(x=0.055-0.001, y=10, labels="VaR",
 lwd=2, srt=90, pos=3)
va_r <- 0.04; var_max <- 4*l_gd*def_prob
# Calculate CVaR
c_var <- integrate(function(x, ...) x*loss_distr(x, ...),
  low=va_r, up=l_gd, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)$value
c_var <- c_var/integrate(loss_distr, low=va_r, up=l_gd, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)$value
# Plot probability distribution of losses
curve(expr=loss_distr(x, def_thresh=def_thresh, rh_o=rh_o),
type="l", xlim=c(0, 0.06),
xlab="loss percentage", ylab="density", lwd=3,
col="orange", main="Conditional Value at Risk")
# Add line for expected loss
abline(v=l_gd*def_prob, col="red", lwd=3)
text(x=l_gd*def_prob-0.001, y=10, labels="expected loss",
 lwd=2, srt=90, pos=3)
# Add lines for VaR
abline(v=va_r, col="red", lwd=3)
text(x=va_r-0.001, y=10, labels="VaR",
 lwd=2, srt=90, pos=3)
# Add shading for CVaR
var_s <- seq(va_r, var_max, length=100)
dens_ity <- sapply(var_s, loss_distr,
  def_thresh=def_thresh, rh_o=rh_o)
# Draw shaded polygon
polygon(c(va_r, var_s, var_max), density=20,
  c(-1, dens_ity, -1), col="red", border=NA)
text(x=va_r+0.005, y=0, labels="CVaR", lwd=2, pos=3)
# VaR (quantile of the loss distribution)
var_func <- function(x, def_thresh=qnorm(0.1), rh_o=0.1, l_gd=0.4)
  l_gd*pnorm((sqrt(rh_o)*qnorm(x) + def_thresh)/sqrt(1-rh_o))
var_func(x=0.99, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)
# Plot VaR
curve(expr=var_func(x, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd),
type="l", xlim=c(0, 0.999),
xlab="confidence level", ylab="VaR", lwd=3,
col="orange", main="VaR versus Confidence Level")
# Add line for expected loss
abline(h=l_gd*def_prob, col="red", lwd=3)
text(x=0.2, y=l_gd*def_prob, labels="expected loss",
     lwd=2, pos=3)
# integrate loss_distr() over full range
integrate(loss_distr, low=0.0, up=l_gd,
    def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)
# Calculate expected losses using loss_distr()
integrate(function(x, ...) x*loss_distr(x, ...),
    low=0.0, up=l_gd,
    def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)
# Calculate confidence levels corresponding to VaR values
var_s <- seq(0.07, 0.12, 0.001)
level_s <- sapply(var_s, function(va_r, ...) {
  integrate(loss_distr, low=va_r, up=l_gd, ...)
}, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)  # end sapply
level_s <- cbind(as.numeric(t(level_s)[, 1]), var_s)
colnames(level_s) <- c("level_s", "VaRs")
# Calculate 95% confidence level VaR value
level_s[
  match(TRUE, level_s[, "level_s"] < 0.05), "VaRs"]
plot(x=1-level_s[, "level_s"],
     y=level_s[, "VaRs"], lwd=2,
     xlab="Confidence Levels", ylab="VaRs",
     t="l", main="VaR Values and Confidence Levels")
# Calculate CVaR values
cvar_s <- sapply(var_s, function(va_r, ...) {
  integrate(function(x, ...) x*loss_distr(x, ...),
      low=va_r, up=l_gd, ...)
}, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)  # end sapply
level_s <- cbind(level_s, as.numeric(t(cvar_s)[, 1]))
colnames(level_s)[3] <- "CVaRs"
# Divide CVaR by confidence level
level_s[, "CVaRs"] <-
  level_s[, "CVaRs"]/level_s[, "level_s"]
# Calculate 95% confidence level CVaR value
level_s[match(TRUE,
  level_s[, "level_s"] < 0.05), "CVaRs"]
# Plot CVaRs
plot(x=1-level_s[, "level_s"],
     y=level_s[, "CVaRs"],
     t="l", col="red", lwd=2,
     ylim=range(level_s[, c("VaRs", "CVaRs")]),
     xlab="Confidence Levels", ylab="CVaRs",
     main="CVaR Values and Confidence Levels")
# Add VaRs
lines(x=1-level_s[, "level_s"],
y=level_s[, "VaRs"], lwd=2)
# Add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"),
 title="default probability = 5%
correlation = 10%
loss given default = 40%",
 inset=0.1, cex=0.8, bg="white", bty="n",
 lwd=6, lty=1, col=c("red", "black"))
# Define model parameters
n_assets <- 300; n_simu <- 1000; l_gd <- 0.4
# Define correlation parameters
rh_o <- 0.2; rho_sqrt <- sqrt(rh_o); rho_sqrtm <- sqrt(1-rh_o)
# Calculate default probabilities and thresholds
set.seed(1121)
def_probs <- runif(n_assets, max=0.2)
def_thresh <- qnorm(def_probs)
# Calculate vector of systematic factors
system_atic <- rnorm(n_simu)
# Simulate losses under Vasicek model
asset_s <-
  matrix(rnorm(n_simu*n_assets), ncol=n_simu)
asset_s <-
  t(rho_sqrt*system_atic + t(rho_sqrtm*asset_s))
loss_es <-
  l_gd*colSums(asset_s < def_thresh)/n_assets
# Calculate VaRs
level_s <- seq(0.93, 0.99, 0.01)
var_s <- quantile(loss_es, probs=level_s)
plot(x=level_s, y=var_s, t="l", lwd=2,
     xlab="Confidence Levels", ylab="VaRs",
     main="Simulated VaR and Confidence Levels")
# Calculate CVaRs
cvar_s <- sapply(var_s, function(va_r) {
  mean(loss_es[loss_es >=va_r])
})  # end sapply
cvar_s <- cbind(cvar_s, var_s)
# Alternative CVaR calculation using frequency table
# first calculate frequency table of loss_es
# ta_ble <- table(loss_es)/n_simu
# Calculate CVaRs from frequency table
# Cvar_s <- sapply(var_s, function(va_r) {
#   tai_l <- ta_ble[names(ta_ble) > va_r]
#   tai_l %*% as.numeric(names(tai_l)) / sum(tai_l)
# })  # end sapply
# Plot CVaRs
plot(x=level_s, y=cvar_s[, "cvar_s"],
     t="l", col="red", lwd=2,
     ylim=range(cvar_s),
     xlab="Confidence Levels", ylab="CVaRs",
     main="Simulated CVaR and Confidence Levels")
# Add VaRs
lines(x=level_s, y=cvar_s[, "var_s"], lwd=2)
# Add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=1, col=c("red", "black"))
calc_var <- function(def_thresh, # Default thresholds
               l_gd=0.6, # loss given default
               rho_sqrt, rho_sqrtm, # asset correlation
               n_simu=1000, # number of simulations
               level_s=seq(0.93, 0.99, 0.01) # Confidence levels
               ) {
  # Define model parameters
  n_assets <- NROW(def_thresh)
  # Simulate losses under Vasicek model
  system_atic <- rnorm(n_simu)
  asset_s <- matrix(rnorm(n_simu*n_assets), ncol=n_simu)
  asset_s <- t(rho_sqrt*system_atic + t(rho_sqrtm*asset_s))
  loss_es <- l_gd*colSums(asset_s < def_thresh)/n_assets
  # Calculate VaRs and CVaRs
  var_s <- quantile(loss_es, probs=level_s)
  cvar_s <- sapply(var_s, function(va_r) {
    mean(loss_es[loss_es >= va_r])
  })  # end sapply
  names(var_s) <- level_s
  names(cvar_s) <- level_s
  c(var_s, cvar_s)
}  # end calc_var
# Define model parameters
n_assets <- 300; n_simu <- 1000; l_gd <- 0.4
rh_o <- 0.2; rho_sqrt <- sqrt(rh_o); rho_sqrtm <- sqrt(1-rh_o)
# Calculate default probabilities and thresholds
set.seed(1121)
def_probs <- runif(n_assets, max=0.2)
def_thresh <- qnorm(def_probs)
# Define number of bootstrap simulations
n_boot <- 500
# Perform bootstrap of calc_var
set.seed(1121)
boot_strap <- sapply(rep(l_gd, n_boot), calc_var,
  def_thresh=def_thresh,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end sapply
boot_strap <- t(boot_strap)
# Calculate vectors of standard errors of VaR and CVaR from boot_strap data
std_error_var <- apply(boot_strap[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
std_error_cvar <- apply(boot_strap[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
std_error_var[2, ] <- std_error_var[2, ]/std_error_var[1, ]
std_error_cvar[2, ] <- std_error_cvar[2, ]/std_error_cvar[1, ]
# Plot the standard errors of VaRs and CVaRs
plot(x=colnames(std_error_cvar),
  y=std_error_cvar[2, ], t="l", col="red", lwd=2,
  ylim=range(c(std_error_var[2, ], std_error_cvar[2, ])),
  xlab="level_s", ylab="CVaRs",
  main="Scaled standard errors of CVaR and VaR")
lines(x=colnames(std_error_var), y=std_error_var[2, ], lwd=2)
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=1, col=c("red", "black"))
NA
library(parallel)  # load package parallel
n_cores <- detectCores() - 1  # number of cores
clus_ter <- makeCluster(n_cores)  # initialize compute cluster
# Perform bootstrap of calc_var for Windows
clusterSetRNGStream(clus_ter, 1121)
boot_strap <- parLapply(clus_ter, rep(l_gd, n_boot),
  fun=calc_var, def_thresh=def_thresh,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end parLapply
# Bootstrap under Mac-OSX or Linux
boot_strap <- mclapply(rep(l_gd, n_boot),
  FUN=calc_var, def_thresh=def_thresh,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end mclapply
boot_strap <- rutils::do_call(rbind, boot_strap)
stopCluster(clus_ter)  # Stop R processes over cluster
# Calculate vectors of standard errors of VaR and CVaR from boot_strap data
std_error_var <- apply(boot_strap[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
std_error_cvar <- apply(boot_strap[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
std_error_var[2, ] <- std_error_var[2, ]/std_error_var[1, ]
std_error_cvar[2, ] <- std_error_cvar[2, ]/std_error_cvar[1, ]
# Plot the standard errors of VaRs and CVaRs
plot(x=colnames(std_error_cvar),
  y=std_error_cvar[2, ], t="l", col="red", lwd=2,
  ylim=range(c(std_error_var[2, ], std_error_cvar[2, ])),
  xlab="level_s", ylab="CVaRs",
  main="Scaled standard errors of CVaR and VaR")
lines(x=colnames(std_error_var), y=std_error_var[2, ], lwd=2)
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=1, col=c("red", "black"))
calc_var <- function(def_probs, # Default probabilities
               l_gd=0.6, # loss given default
               rho_sqrt, rho_sqrtm, # asset correlation
               n_simu=1000, # number of simulations
               level_s=seq(0.93, 0.99, 0.01) # Confidence levels
               ) {
  # Calculate default thresholds
  def_thresh <- qnorm(runif(1, min=0.5, max=1.5)*def_probs)
  # Simulate losses under Vasicek model
  n_assets <- NROW(def_probs)
  system_atic <- rnorm(n_simu)
  asset_s <- matrix(rnorm(n_simu*n_assets), ncol=n_simu)
  asset_s <- t(rho_sqrt*system_atic + t(rho_sqrtm*asset_s))
  loss_es <- l_gd*colSums(asset_s < def_thresh)/n_assets
  # Calculate VaRs and CVaRs
  var_s <- quantile(loss_es, probs=level_s)
  cvar_s <- sapply(var_s, function(va_r) {
    mean(loss_es[loss_es >=va_r])
  })  # end sapply
  names(var_s) <- level_s
  names(cvar_s) <- level_s
  c(var_s, cvar_s)
}  # end calc_var
library(parallel)  # load package parallel
n_cores <- detectCores() - 1  # number of cores
clus_ter <- makeCluster(n_cores)  # initialize compute cluster
# Perform bootstrap of calc_var for Windows
clusterSetRNGStream(clus_ter, 1121)
boot_strap <- parLapply(clus_ter, rep(l_gd, n_boot),
  fun=calc_var, def_probs=def_probs,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end parLapply
# Bootstrap under Mac-OSX or Linux
boot_strap <- mclapply(rep(l_gd, n_boot),
  FUN=calc_var, def_probs=def_probs,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end mclapply
boot_strap <- rutils::do_call(rbind, boot_strap)
stopCluster(clus_ter)  # Stop R processes over cluster
# Calculate vectors of standard errors of VaR and CVaR from boot_strap data
std_error_var <- apply(boot_strap[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
std_error_cvar <- apply(boot_strap[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Plot the standard errors of VaRs and CVaRs
plot(x=colnames(std_error_cvar),
  y=std_error_cvar[2, ], t="l", col="red", lwd=2,
  ylim=range(c(std_error_var[2, ], std_error_cvar[2, ])),
  xlab="Confidence Levels", ylab="CVaRs",
  main="Standard Errors of CVaR and VaR
  with Uncertain Default Probabilities")
lines(x=colnames(std_error_var), y=std_error_var[2, ], lwd=2)
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=1.0, bg="white",
 lwd=6, lty=1, col=c("red", "black"))
# Define cumulative default probability function
cum_loss <- function(x, def_thresh=(-2), rh_o=0.2, l_gd=0.4)
  pnorm((sqrt(1-rh_o)*qnorm(x/l_gd) - def_thresh)/sqrt(rh_o))
# Define Vasicek loss distribution density function
# (vectorized version with error handling for x)
loss_distr <- function(x, def_thresh=-2, rh_o=0.1, l_gd=0.4) {
  q_norm <- ifelse(x/l_gd < 0.999, qnorm(x/l_gd), 3.1)
  sqrt((1-rh_o)/rh_o)*exp(-(sqrt(1-rh_o)*q_norm - def_thresh)^2/(2*rh_o) + q_norm^2/2)/l_gd
}  # end loss_distr
def_prob <- 0.2; def_thresh <- qnorm(def_prob)
rh_o <- 0.1; l_gd <- 0.4
at_tach <- 0.15; de_tach <- 0.2
# Expected tranche loss is sum of two terms
tranche_loss <-
  # Loss between at_tach and de_tach
  integrate(function(x, at_tach) (x-at_tach)*loss_distr(x,
def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd),
low=at_tach, up=de_tach, at_tach=at_tach)$value / (de_tach-at_tach) +
  # Loss in excess of de_tach
  (1-cum_loss(x=de_tach, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd))
# Plot probability distribution of losses
curve(expr=loss_distr(x, def_thresh=def_thresh, rh_o=rh_o),
type="l", xlim=c(0, 3*l_gd*def_prob),
xlab="loss percentage", ylab="density", lwd=3,
col="orange", main="CDO Tranche Losses")
# Add line for expected loss
abline(v=l_gd*def_prob, col="red", lwd=3)
text(x=l_gd*def_prob-0.001, y=4, labels="expected loss",
 lwd=2, srt=90, pos=3)
# Add lines for attach and detach
abline(v=at_tach, col="blue", lwd=3)
text(x=at_tach-0.001, y=4, labels="attach",
 lwd=2, srt=90, pos=3)
abline(v=de_tach, col="green", lwd=3)
text(x=de_tach-0.001, y=4, labels="detach",
 lwd=2, srt=90, pos=3)
# Add shading for CDO tranche
var_s <- seq(at_tach, de_tach, length=100)
dens_ity <- sapply(var_s, loss_distr,
  def_thresh=def_thresh, rh_o=rh_o)
# Draw shaded polygon
polygon(c(at_tach, var_s, de_tach), density=20,
  c(-1, dens_ity, -1), col="red", border=NA)
text(x=0.5*(at_tach+de_tach), y=0, labels="CDO tranche", cex=0.9, lwd=2, pos=3)
