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

set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
n_rows <- 1000; da_ta <- rnorm(n_rows)
# Sample mean and standard deviation
mean(da_ta); sd(da_ta)
# Bootstrap of sample mean and median
n_boot <- 10000
boot_data <- sapply(1:n_boot, function(x) {
  sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
  c(mean=mean(sampl_e), median=median(sampl_e))
})  # end sapply
boot_data <- t(boot_data)

boot_data[1:3, ]
# Standard error from formula
sd(da_ta)/sqrt(n_rows)
# Standard error of mean from bootstrap
sd(boot_data[, "mean"])
# Standard error of median from bootstrap
sd(boot_data[, "median"])
plot(density(boot_data[, "median"]), lwd=2, xlab="estimate of median",
     main="Distribution of Bootstrapped Median")
abline(v=mean(boot_data[, "median"]), lwd=2, col="red")

set.seed(1121)  # Reset random number generator
n_rows <- 1000
# Bootstrap of sample mean and median
n_boot <- 10000
boot_data <- sapply(1:n_boot, function(x) {
  # Sample from Standard Normal Distribution
  sampl_e <- rnorm(n_rows)
  c(mean=mean(sampl_e), median=median(sampl_e))
})  # end sapply
boot_data[, 1:3]
# Standard error from formula
1/sqrt(n_rows)
# Standard error of mean from bootstrap
sd(boot_data["mean", ])
# Standard error of median from bootstrap
sd(boot_data["median", ])

set.seed(1121)  # Reset random number generator
n_rows <- 1000
# Bootstrap of sample mean and median
n_boot <- 100
boot_data <- sapply(1:n_boot, function(x) {
  median(rnorm(n_rows))
})  # end sapply
# Perform vectorized bootstrap
set.seed(1121)  # Reset random number generator
# Calculate matrix of random data
sampl_e <- matrix(rnorm(n_boot*n_rows), ncol=n_boot)
boot_vec <- Rfast::colMedians(sampl_e)
all.equal(boot_data, boot_vec)
# Compare speed of loops with vectorized R code
library(microbenchmark)
summary(microbenchmark(
  loop = {
    sapply(1:n_boot, function(x) {
median(rnorm(n_rows))
    })  # end sapply
  },
  vector_ized = {
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
da_ta <- rnorm(n_rows)
# Bootstrap mean and median under Windows
n_boot <- 10000
boot_data <- parLapply(clus_ter, 1:n_boot,
  function(x, da_ta, n_rows) {
  sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
  c(mean=mean(sampl_e), median=median(sampl_e))
  }, da_ta=da_ta, n_rows=n_rows)  # end parLapply
# Bootstrap mean and median under Mac-OSX or Linux
boot_data <- mclapply(1:n_boot,
  function(x) {
  sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
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
sd(da_ta)
mad(da_ta)
median(abs(da_ta - median(da_ta)))
median(abs(da_ta - median(da_ta)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
n_boot <- 10000
boot_data <- sapply(1:n_boot, function(x) {
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
boot_data <- parLapply(clus_ter, 1:n_boot,
  function(x, da_ta) {
    sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  }, da_ta=da_ta)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:n_boot, function(x) {
    sampl_e <- da_ta[sample.int(n_rows, replace=TRUE)]
    c(sd=sd(sampl_e), mad=mad(sampl_e))
  }, mc.cores=n_cores)  # end mclapply
stopCluster(clus_ter)  # Stop R processes over cluster
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), std_error=sd(x)))

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
curve(expr=dnorm(x, mean=1), add=TRUE,
lwd=3, col="red")
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

# Calculate random default probabilities
set.seed(1121)
n_assets <- 100
def_probs <- runif(n_assets, max=0.2)
mean(def_probs)
# Simulate number of defaults
uni_form <- runif(n_assets)
sum(uni_form < def_probs)
# Simulate average number of defaults using for() loop (inefficient way)
n_simu <- 1000
set.seed(1121)
de_faults <- numeric(n_simu)
for (i in 1:n_simu) {  # Perform loop
  uni_form <- runif(n_assets)
  de_faults[i] <- sum(uni_form < def_probs)
}  # end for
# Calculate average number of defaults
mean(de_faults)
# Simulate using vectorized functions  (efficient way)
set.seed(1121)
uni_form <- matrix(runif(n_simu*n_assets), ncol=n_simu)
de_faults <- colSums(uni_form < def_probs)
mean(de_faults)
# Plot the distribution of defaults
x11(width=6, height=5)
plot(density(de_faults), main="Distribution of Defaults",
     xlab="number of defaults", ylab="frequqncy")
abline(v=mean(de_faults), lwd=3, col="red")

# Calculate default thresholds and asset values
def_thresh <- qnorm(def_probs)
asset_s <- qnorm(uni_form)
# Simulate defaults
de_faults <- colSums(asset_s < def_thresh)
mean(de_faults)

# Plot Standard Normal distribution
x11(width=6, height=5)
x_lim <- 4; def_thresh <- qnorm(0.025)
curve(expr=dnorm(x), type="l", xlim=c(-x_lim, x_lim),
xlab="asset value", ylab="", lwd=3,
col="blue", main="Distribution of Asset Values")
abline(v=def_thresh, col="red", lwd=3)
text(x=def_thresh-0.1, y=0.15, labels="default threshold",
 lwd=2, srt=90, pos=3)
# Plot polygon area
x_var <- seq(-x_lim, x_lim, length=100)
y_var <- dnorm(x_var)
are_a <- ((x_var >= (-x_lim)) & (x_var <= def_thresh))
polygon(c(x_lim, x_var[are_a], def_thresh),
  c(-1, y_var[are_a], -1), col="red")

# Define correlation parameters
rh_o <- 0.2
rho_sqrt <- sqrt(rh_o) ; rho_sqrtm <- sqrt(1-rh_o)
n_assets <- 5 ; n_simu <- 10000
# Calculate vector of systematic and idiosyncratic factors
system_atic <- rnorm(n_simu)
idio_syncratic <- rnorm(n_simu*n_assets)
# Simulate asset values using vectorized functions (efficient way)
asset_s <- rho_sqrt*system_atic + rho_sqrtm*idio_syncratic
dim(asset_s) <- c(n_simu, n_assets)
# Asset values are standard normally distributed
apply(asset_s, MARGIN=2, function(x) c(mean=mean(x), sd=sd(x)))
# Calculate correlations between asset values
cor(asset_s)
# Simulate asset values using for() loop (inefficient way)
# Allocate matrix of assets
asset_s <- matrix(nr=n_simu, nc=n_assets)
# Simulate asset values using for() loop
for (i in 1:n_simu) {  # Perform loop
  asset_s[i, ] <- rho_sqrt*system_atic[i] + rho_sqrtm*rnorm(n_assets)
}  # end for
cor(asset_s)
# benchmark the speed of the two methods
library(microbenchmark)
summary(microbenchmark(
  for_loop={for (i in 1:n_simu) {
    rho_sqrt*system_atic[i] + rho_sqrtm*rnorm(n_assets)}},
  vector_ized={rho_sqrt*system_atic + rho_sqrtm*rnorm(n_simu*n_assets)},
  times=10))[, c(1, 4, 5)]

# Calculate random default probabilities
n_assets <- 5
def_probs <- runif(n_assets, max=0.2)
mean(def_probs)
# Calculate default thresholds
def_thresh <- qnorm(def_probs)
# Calculate number of defaults using vectorized functions (efficient way)
# Calculate vector of number of defaults
rowMeans(t(asset_s) < def_thresh)
def_probs
# Calculate number of defaults using for() loop (inefficient way)
# allocate matrix of de_faults
de_faults <- matrix(nr=n_simu, nc=n_assets)
# Simulate asset values using for() loop
for (i in 1:n_simu) {  # Perform loop
  de_faults[i, ] <- (asset_s[i, ] < def_thresh)
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
asset_s <- rho_sqrt*system_atic + rho_sqrtm*rnorm(n_simu*n_assets)
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
integrate(func_tion, lower=0, upper=Inf, lamb_da=2)
# Cumulative probability over normal distribution
pnorm(-2)
integrate(dnorm, low=2, up=Inf)
str(dnorm)
pnorm(-1)
integrate(dnorm, low=2, up=Inf, mean=1)
# Expected value over normal distribution
integrate(function(x) x*dnorm(x), low=2, up=Inf)

rh_o <- 0.1; l_gd <- 0.4
# Define Vasicek loss distribution function
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

# Define cumulative default probability function
cum_loss <- function(x, def_thresh=(-2), rh_o=0.2, l_gd=0.4)
  pnorm((sqrt(1-rh_o)*qnorm(x/l_gd) - def_thresh)/sqrt(rh_o))
# Define Vasicek loss distribution function
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
densi_ty <- sapply(var_s, loss_distr,
  def_thresh=def_thresh, rh_o=rh_o)
# Draw shaded polygon
polygon(c(at_tach, var_s, de_tach), density=20,
  c(-1, densi_ty, -1), col="red", border=NA)
text(x=0.5*(at_tach+de_tach), y=0, labels="CDO tranche", cex=0.9, lwd=2, pos=3)

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
text(x=l_gd*def_prob-0.001, y=10, labels="expected loss", lwd=2, srt=90, pos=3)

# Add lines for VaR
abline(v=va_r, col="red", lwd=3)
text(x=va_r-0.001, y=10, labels="VaR",
 lwd=2, srt=90, pos=3)
# Add shading for CVaR
var_s <- seq(va_r, var_max, length=100)
densi_ty <- sapply(var_s, loss_distr,
  def_thresh=def_thresh, rh_o=rh_o)
# Draw shaded polygon
polygon(c(va_r, var_s, var_max), density=20,
  c(-1, densi_ty, -1), col="red", border=NA)
text(x=va_r+0.005, y=0, labels="CVaR", lwd=2, pos=3)

# VaR (quantile of the loss distribution)
var_func <- function(x, def_thresh=qnorm(0.1), rh_o=0.1, l_gd=0.4)
  l_gd*pnorm((sqrt(rh_o)*qnorm(x) + def_thresh)/sqrt(1-rh_o))
var_func(x=0.99, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)
# Plot VaR
curve(expr=var_func(x, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd),
type="l", xlim=c(0, 0.999), xlab="confidence level", ylab="VaR", lwd=3,
col="orange", main="VaR versus Confidence Level")
# Add line for expected loss
abline(h=l_gd*def_prob, col="red", lwd=3)
text(x=0.2, y=l_gd*def_prob, labels="expected loss", lwd=2, pos=3)

# Integrate loss_distr() over full range
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
level_s[match(TRUE, level_s[, "level_s"] < 0.05), "VaRs"]
plot(x=1-level_s[, "level_s"],
     y=level_s[, "VaRs"], lwd=2,
     xlab="confidence level", ylab="VaRs",
     t="l", main="VaR Values and Confidence Levels")

# Calculate CVaR values
cvar_s <- sapply(var_s, function(va_r, ...) {
  integrate(function(x, ...) x*loss_distr(x, ...),
      low=va_r, up=l_gd, ...)
}, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)  # end sapply
level_s <- cbind(level_s, as.numeric(t(cvar_s)[, 1]))
colnames(level_s)[3] <- "CVaRs"
# Divide CVaR by confidence level
level_s[, "CVaRs"] <- level_s[, "CVaRs"]/level_s[, "level_s"]
# Calculate 95% confidence level CVaR value
level_s[match(TRUE,
  level_s[, "level_s"] < 0.05), "CVaRs"]
# Plot CVaRs
plot(x=1-level_s[, "level_s"],
     y=level_s[, "CVaRs"],
     t="l", col="red", lwd=2,
     ylim=range(level_s[, c("VaRs", "CVaRs")]),
     xlab="confidence level", ylab="CVaRs",
     main="CVaR Values and Confidence Levels")

# Add VaRs
lines(x=1-level_s[, "level_s"], y=level_s[, "VaRs"], lwd=2)
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
asset_s <- matrix(rnorm(n_simu*n_assets), ncol=n_simu)
asset_s <- t(rho_sqrt*system_atic + t(rho_sqrtm*asset_s))
loss_es <- l_gd*colSums(asset_s < def_thresh)/n_assets

# Calculate VaRs
level_s <- seq(0.93, 0.99, 0.01)
var_s <- quantile(loss_es, probs=level_s)
plot(x=level_s, y=var_s, t="l", lwd=2,
     xlab="confidence level", ylab="VaRs",
     main="Simulated VaR and Confidence Levels")

# Calculate CVaRs
cvar_s <- sapply(var_s, function(va_r) {
  mean(loss_es[loss_es >= va_r])
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
     xlab="confidence level", ylab="CVaRs",
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
boot_data <- sapply(rep(l_gd, n_boot), calc_var,
  def_thresh=def_thresh,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end sapply
boot_data <- t(boot_data)
# Calculate vectors of standard errors of VaR and CVaR from boot_data data
std_error_var <- apply(boot_data[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
std_error_cvar <- apply(boot_data[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
std_error_var[2, ] <- std_error_var[2, ]/std_error_var[1, ]
std_error_cvar[2, ] <- std_error_cvar[2, ]/std_error_cvar[1, ]

# Plot the standard errors of VaRs and CVaRs
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(x=colnames(std_error_cvar), y=std_error_cvar[2, ],
  t="l", col="red", lwd=2,
  ylim=range(c(std_error_var[2, ], std_error_cvar[2, ])),
  xlab="confidence level", ylab="standard error",
  main="Scaled standard errors of CVaR and VaR")
lines(x=colnames(std_error_var), y=std_error_var[2, ], lwd=2)
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=1, col=c("red", "black"))

library(parallel)  # load package parallel
n_cores <- detectCores() - 1  # number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster
# Perform bootstrap of calc_var for Windows
clusterSetRNGStream(clus_ter, 1121)
boot_data <- parLapply(clus_ter, rep(l_gd, n_boot),
  fun=calc_var, def_thresh=def_thresh,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end parLapply
# Bootstrap under Mac-OSX or Linux
boot_data <- mclapply(rep(l_gd, n_boot),
  FUN=calc_var, def_thresh=def_thresh,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end mclapply
boot_data <- rutils::do_call(rbind, boot_data)
stopCluster(clus_ter)  # Stop R processes over cluster
# Calculate vectors of standard errors of VaR and CVaR from boot_data data
std_error_var <- apply(boot_data[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
std_error_cvar <- apply(boot_data[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
std_error_var[2, ] <- std_error_var[2, ]/std_error_var[1, ]
std_error_cvar[2, ] <- std_error_cvar[2, ]/std_error_cvar[1, ]

# Plot the standard errors of VaRs and CVaRs
plot(x=colnames(std_error_cvar),
  y=std_error_cvar[2, ], t="l", col="red", lwd=2,
  ylim=range(c(std_error_var[2, ], std_error_cvar[2, ])),
  xlab="confidence level", ylab="standard error",
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
  # Calculate random default thresholds
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
    mean(loss_es[loss_es >= va_r])
  })  # end sapply
  names(var_s) <- level_s
  names(cvar_s) <- level_s
  c(var_s, cvar_s)
}  # end calc_var

library(parallel)  # load package parallel
n_cores <- detectCores() - 1  # number of cores
clus_ter <- makeCluster(n_cores)  # Initialize compute cluster
# Perform bootstrap of calc_var for Windows
clusterSetRNGStream(clus_ter, 1121)
boot_data <- parLapply(clus_ter, rep(l_gd, n_boot),
  fun=calc_var, def_probs=def_probs,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end parLapply
# Bootstrap under Mac-OSX or Linux
boot_data <- mclapply(rep(l_gd, n_boot),
  FUN=calc_var, def_probs=def_probs,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  n_simu=n_simu, level_s=level_s)  # end mclapply
boot_data <- rutils::do_call(rbind, boot_data)
stopCluster(clus_ter)  # Stop R processes over cluster
# Calculate vectors of standard errors of VaR and CVaR from boot_data data
std_error_var <- apply(boot_data[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
std_error_cvar <- apply(boot_data[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
std_error_var[2, ] <- std_error_var[2, ]/std_error_var[1, ]
std_error_cvar[2, ] <- std_error_cvar[2, ]/std_error_cvar[1, ]

# Plot the standard errors of VaRs and CVaRs
plot(x=colnames(std_error_cvar),
  y=std_error_cvar[2, ], t="l", col="red", lwd=2,
  ylim=range(c(std_error_var[2, ], std_error_cvar[2, ])),
  xlab="confidence level", ylab="standard error",
  main="Standard Errors of CVaR and VaR
  with Uncertain Default Probabilities")
lines(x=colnames(std_error_var), y=std_error_var[2, ], lwd=2)
legend(x="topright", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=1.0, bg="white",
 lwd=6, lty=1, col=c("red", "black"))

NA

App setup code that runs only once at startup.
n_data <- 1e4
std_dev <- 1.0

Define the user interface
inter_face <- shiny::fluidPage(
  # Create numeric input for the number of data points.
  numericInput('n_data', "Number of data points:", value=n_data),
  # Create slider input for the standard deviation parameter.
  sliderInput("std_dev", label="Standard deviation:",
        min=0.1, max=3.0, value=std_dev, step=0.1),
  # Render plot in a panel.
  plotOutput("plo_t", height=300, width=500)
)  # end user interface

Define the server function
ser_ver <- function(input, output) {
  output$plo_t <- shiny::renderPlot({
    # Simulate the data
    da_ta <- rnorm(input$n_data, sd=input$std_dev)
    # Plot the data
    par(mar=c(2, 4, 4, 0), oma=c(0, 0, 0, 0))
    hist(da_ta, xlim=c(-4, 4), main="Histogram of Random Data")
  })  # end renderPlot
}  # end ser_ver

# Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)

Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel("VWAP Moving Average"),
  # Create single row of widgets with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=3, selectInput("sym_bol", label="Symbol",
                          choices=sym_bols, selected=sym_bol)),
    # Input look-back interval
    column(width=3, sliderInput("look_back", label="Lookback interval",
                          min=1, max=150, value=11, step=1))
  ),  # end fluidRow
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dy_graph"), width=12)
)  # end fluidPage interface

Define the server function
ser_ver <- shiny::shinyServer(function(input, output) {
  # Get the close and volume data in a reactive environment
  clos_e <- shiny::reactive({
    # Get the data
    oh_lc <- get(input$sym_bol, data_env)
    clos_e <- log(quantmod::Cl(oh_lc))
    vol_ume <- quantmod::Vo(oh_lc)
    # Return the data
    cbind(clos_e, vol_ume)
  })  # end reactive code

  # Calculate the VWAP indicator in a reactive environment
  v_wap <- shiny::reactive({
    # Get model parameters from input argument
    look_back <- input$look_back
    # Calculate the VWAP indicator
    clos_e <- clos_e()[, 1]
    vol_ume <- clos_e()[, 2]
    v_wap <- HighFreq::roll_sum(se_ries=clos_e*vol_ume, look_back=look_back)
    volume_rolling <- HighFreq::roll_sum(se_ries=vol_ume, look_back=look_back)
    v_wap <- v_wap/volume_rolling
    v_wap[is.na(v_wap)] <- 0
    # Return the plot data
    da_ta <- cbind(clos_e, v_wap)
    colnames(da_ta) <- c(input$sym_bol, "VWAP")
    da_ta
  })  # end reactive code

  # Return the dygraph plot to output argument
  output$dy_graph <- dygraphs::renderDygraph({
    col_names <- colnames(v_wap())
    dygraphs::dygraph(v_wap(), main=paste(col_names[1], "VWAP")) %>%
dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
  })  # end output plot
})  # end server code

Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)

Define the server function
ser_ver <- shiny::shinyServer(function(input, output) {

  # Create an empty list of reactive values.
  value_s <- reactiveValues()

  # Get input parameters from the user interface.
  n_rows <- reactive({
    # Add n_rows to list of reactive values.
    value_s$n_rows <- input$n_rows
    input$n_rows
  })  # end reactive code

  # Broadcast a message to the console when the button is pressed.
  observeEvent(eventExpr=input$but_ton, handlerExpr={
    cat("Input button pressed\n")
  })  # end observeEvent

  # Send the data when the button is pressed.
  da_ta <- eventReactive(eventExpr=input$but_ton, valueExpr={
    # eventReactive() executes on input$but_ton, but not on n_rows() or input$n_rows.
    cat("Sending", n_rows(), "rows of data\n")
    da_ta <- head(mtcars, input$n_rows)
    value_s$mpg <- mean(da_ta$mpg)
    da_ta
  })  # end eventReactive
  #   da_ta

  # Draw table of the data when the button is pressed.
  observeEvent(eventExpr=input$but_ton, handlerExpr={
    da_ta <- da_ta()
    cat("Received", value_s$n_rows, "rows of data\n")
    cat("Average mpg = ", value_s$mpg, "\n")
    cat("Drawing table\n")
    output$tabl_e <- renderTable(da_ta)
  })  # end observeEvent

})  # end server code

Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
