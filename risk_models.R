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
library(rutils)  # Load package rutils
# Calculate VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
# Mean and standard deviation of returns
c(mean(re_turns), sd(re_turns))
# Plot histogram
x11(width=6, height=5)
par(mar=c(1, 1, 1, 1), oma=c(2, 2, 2, 0))
ma_d <- mad(re_turns)
histo_gram <- hist(re_turns, breaks=100,
  main="", xlim=c(-5*ma_d, 5*ma_d),
  xlab="", ylab="", freq=FALSE)
# Draw kernel density of histogram
lines(density(re_turns), col="red", lwd=2)
# Add density of normal distribution
curve(expr=dnorm(x, mean=mean(re_turns), sd=sd(re_turns)),
add=TRUE, type="l", lwd=2, col="blue")
title(main="VTI Return Distribution", line=0)  # Add title
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
# Calculate VTI percentage returns
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
# Objective function is negative log-likelihood
likeli_hood <- function(par, dfree, data) {
  sum(-log(gamma((dfree+1)/2)/(sqrt(pi*dfree)*gamma(dfree/2))) +
    log(par[2]) + (dfree+1)/2*log(1+((data-par[1])/par[2])^2/dfree))
}  # end likeli_hood
# Demonstrate equivalence with log(dt())
likeli_hood(c(1, 0.5), 2, 2:5)
-sum(log(dt(x=(2:5-1)/0.5, df=2)/0.5))
# Simpler objective function
likeli_hood <- function(par, dfree, data) {
  -sum(log(dt(x=(data-par[1])/par[2], df=dfree)/par[2]))
}  # end likeli_hood
# Calculate VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
# Initial parameters
par_init <- c(mean=0, scale=0.01)
# Fit distribution using optim()
optim_fit <- optim(par=par_init,
  fn=likeli_hood, # Log-likelihood function
  data=re_turns,
  dfree=2, # Degrees of freedom
  method="L-BFGS-B", # Quasi-Newton method
  upper=c(1, 0.1), # Upper constraint
  lower=c(-1, 1e-7)) # Lower constraint
# Optimal parameters
lo_cation <- optim_fit$par["mean"]
scal_e <- optim_fit$par["scale"]
# Fit VTI returns using MASS::fitdistr()
optim_fit <- MASS::fitdistr(re_turns, densfun="t", df=2)
summary(optim_fit)
# Fitted parameters
optim_fit$estimate
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]
# Standard errors of parameters
optim_fit$sd
# Log-likelihood value
optim_fit$value
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
ks_test <- ks.test(rnorm(100), pnorm)
ks_test$p.value
# KS test for uniform distribution
ks.test(runif(100), pnorm)
# KS test for two similar normal distributions
ks.test(rnorm(100), rnorm(100, mean=0.1))
# KS test for two different normal distributions
ks.test(rnorm(100), rnorm(100, sd=2.0))
# KS test for VTI returns vs normal distribution
re_turns <- as.numeric(na.omit(rutils::etf_env$re_turns$VTI))
re_turns <- (re_turns - mean(re_turns))/sd(re_turns)
ks.test(re_turns, rnorm)
# Fit t-dist into VTI returns
optim_fit <- MASS::fitdistr(re_turns, densfun="t", df=2)
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]
# KS test for VTI returns vs t-distribution
# define non-standard Student’s t-distribution
tdistr <- function(x, dfree, loc, scale) {
  dt((x-loc)/scale, df=dfree)/scale
}  # end likeli_hood
# tdistr(x=re_turns[1], dfree=2, loc=lo_cation, scale=scal_e)
# Plot shows that tdistr() fits re_turns very well
den_sity <- density(re_turns)
ma_d <- mad(re_turns)
plot(den_sity, xlim=c(-5*ma_d, 5*ma_d),
     xlab="returns", ylab="density",
     col="blue", lwd=2, main="Density of VTI Returns")
curve(expr=tdistr(x, dfree=2, loc=lo_cation, scale=scal_e), col="red", lwd=2, add=TRUE)
# Or
tdistr <- function(x, dfree, loc, scale) {
  gamma((dfree+1)/2)/(sqrt(pi*dfree)*gamma(dfree/2)*scale)*
    (1+((x-loc)/scale)^2/dfree)^(-(dfree+1)/2)
}  # end likeli_hood
# tdistr(x=re_turns[1], dfree=2, loc=lo_cation, scale=scal_e)
# Not the same as the below - gives p-value << 0.01 - rejects NULL
ks.test(re_turns, tdistr, dfree=2, loc=lo_cation, scale=scal_e)
# Not the same as the above - gives p-value > 0.02 most of the time - doesn't reject NULL
# KS test for VTI returns vs t-distribution
da_ta <- lo_cation + scal_e*rt(NROW(re_turns), df=2)
ks.test(re_turns, da_ta)
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
# Calculate VTI returns
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
clos_e <- log(quantmod::Cl(rutils::etf_env$VTI))
draw_downs <- (clos_e - cummax(clos_e))
# Extract the date index from the time series clos_e
date_s <- zoo::index(clos_e)
# Calculate the maximum drawdown date and depth
index_min <- which.min(draw_downs)
date_min <- date_s[index_min]
max_drawdown <- draw_downs[date_min]
# Calculate the drawdown start and end dates
date_start <- max(date_s[(date_s < date_min) & (draw_downs == 0)])
date_end <- min(date_s[(date_s > date_min) & (draw_downs == 0)])
# dygraph plot of VTI drawdowns
da_ta <- cbind(clos_e, draw_downs)
col_names <- c("VTI", "Drawdowns")
colnames(da_ta) <- col_names
dygraphs::dygraph(da_ta, main="VTI Drawdowns") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2],
   valueRange=(1.2*range(draw_downs)+0.1), independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue") %>%
  dySeries(name=col_names[2], axis="y2", col="red") %>%
  dyEvent(date_start, "start drawdown", col="blue") %>%
  dyEvent(date_min, "max drawdown", col="red") %>%
  dyEvent(date_end, "end drawdown", col="green")
# Plot VTI drawdowns using package quantmod
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
x11(width=6, height=5)
quantmod::chart_Series(x=clos_e, name="VTI Drawdowns", theme=plot_theme)
xval <- match(date_start, date_s)
yval <- max(clos_e)
abline(v=xval, col="blue")
text(x=xval, y=0.95*yval, "start drawdown", col="blue", cex=0.9)
xval <- match(date_min, date_s)
abline(v=xval, col="red")
text(x=xval, y=0.9*yval, "max drawdown", col="red", cex=0.9)
xval <- match(date_end, date_s)
abline(v=xval, col="green")
text(x=xval, y=0.85*yval, "end drawdown", col="green", cex=0.9)
library(xtable)
library(PerformanceAnalytics)
clos_e <- log(quantmod::Cl(rutils::etf_env$VTI))
re_turns <- rutils::diff_it(clos_e)
# Calculate table of VTI drawdowns
tabl_e <- PerformanceAnalytics::table.Drawdowns(re_turns, geometric=FALSE)
# Convert dates to strings
tabl_e <- cbind(sapply(tabl_e[, 1:3], as.character), tabl_e[, 4:7])
# Print table of VTI drawdowns
print(xtable(tabl_e), comment=FALSE, size="tiny", include.rownames=FALSE)
library(xtable)
library(PerformanceAnalytics)
clos_e <- log(quantmod::Cl(rutils::etf_env$VTI))
re_turns <- rutils::diff_it(clos_e)
# Calculate table of VTI drawdowns
tabl_e <- PerformanceAnalytics::table.Drawdowns(re_turns, geometric=FALSE)
# Convert dates to strings
tabl_e <- cbind(sapply(tabl_e[, 1:3], as.character), tabl_e[, 4:7])
# Print table of VTI drawdowns
print(xtable(tabl_e), comment=FALSE, size="tiny", include.rownames=FALSE)
# Load "managers" data set
data(managers)
charts.PerformanceSummary(ham_1,
  main="", lwd=2, ylog=TRUE)
# Calculate VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
conf_level <- 0.1
va_r <- quantile(re_turns, conf_level)
c_var <- mean(re_turns[re_turns < va_r])
# Plot histogram of VTI returns
x11(width=6, height=5)
par(mar=c(3, 2, 1, 0), oma=c(0, 0, 0, 0))
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", ylab="frequency", breaks=100,
  xlim=c(-0.05, 0.01), freq=FALSE, main="VTI Returns Histogram")
# Calculate density
densi_ty <- density(re_turns, adjust=1.5)
# Plot density
lines(densi_ty, lwd=3, col="blue")
# Plot line for VaR
abline(v=va_r, col="red", lwd=3)
text(x=va_r, y=25, labels="VaR", lwd=2, pos=2)
# Plot polygon shading for CVaR
text(x=1.5*va_r, y=10, labels="CVaR", lwd=2, pos=2)
var_max <- -0.06
rang_e <- (densi_ty$x < va_r) &  (densi_ty$x > var_max)
polygon(c(var_max, densi_ty$x[rang_e], va_r),
  c(0, densi_ty$y[rang_e], 0), col=rgb(1, 0, 0,0.5), border=NA)
# Calculate VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
conf_level <- 0.05
# Calculate VaR as quantile
va_r <- quantile(re_turns, probs=conf_level)
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
# dygraph plot of VXX versus SVXY
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
#Below is for ETFs
# Sort on Sharpe ratio
risk_ret <- risk_ret[order(risk_ret$Skewness, decreasing=TRUE), ]
# Select high skew and low skew ETFs
cut_off <- (NROW(risk_ret) %/% 2)
high_skew <- risk_ret$Name[1:cut_off]
low_skew <- risk_ret$Name[(cut_off+1):NROW(risk_ret)]
# Calculate returns and log prices
re_turns <- rutils::etf_env$re_turns
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
re_turns[is.na(re_turns)] <- 0
sum(is.na(re_turns))
high_skew <- rowMeans(re_turns[, high_skew])
low_skew <- rowMeans(re_turns[, low_skew])
weal_th <- cbind(high_skew, low_skew)
weal_th <- xts::xts(weal_th, index(re_turns))
weal_th <- cumsum(weal_th)
# dygraph plot of high skew and low skew ETFs
col_names <- colnames(weal_th)
dygraphs::dygraph(weal_th, main="Log Wealth of High and Low Skew ETFs") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", strokeWidth=2, col="green") %>%
  dyLegend(show="always", width=500)
#Below is for S&P500 constituent stocks
# calc_mom() calculates the moments of returns
calc_mom <- function(re_turns, mo_ment=3) {
  re_turns <- na.omit(re_turns)
  sum(((re_turns - mean(re_turns))/sd(re_turns))^mo_ment)/NROW(re_turns)
}  # end calc_mom
# Calculate skew and kurtosis of VTI returns
calc_mom(re_turns, mo_ment=3)
calc_mom(re_turns, mo_ment=4)
# Load the S&P500 constituent stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
dim(re_turns)
sum(is.na(re_turns))
# re_turns <- re_turns["2000/"]
skew_s <- sapply(re_turns, calc_mom, mo_ment=3)
# skew_s <- sapply(re_turns, calc_mom, mo_ment=4)
# skew_s <- sapply(re_turns, sd, na.rm=TRUE)
skew_s <- sort(skew_s)
name_s <- names(skew_s)
n_rows <- NROW(name_s)
# Select high skew and low skew ETFs
cut_off <- (n_rows %/% 2)
low_skew <- name_s[1:cut_off]
high_skew <- name_s[(cut_off+1):n_rows]
# low_skew <- name_s[1:50]
# high_skew <- name_s[(n_rows-51):n_rows]
# Calculate returns and log prices
low_skew <- rowMeans(re_turns[, low_skew], na.rm=TRUE)
low_skew[1] <- 0
high_skew <- rowMeans(re_turns[, high_skew], na.rm=TRUE)
high_skew[1] <- 0
weal_th <- cbind(high_skew, low_skew)
weal_th <- xts::xts(weal_th, index(re_turns))
weal_th <- cumsum(weal_th)
# dygraph plot of high skew and low skew ETFs
col_names <- colnames(weal_th)
dygraphs::dygraph(weal_th, main="Log Wealth of High and Low Skew Stocks") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", strokeWidth=2, col="green") %>%
  dyLegend(show="always", width=500)
library(PerformanceAnalytics)
re_turns <- rutils::etf_env$re_turns[, c("VTI", "IEF")]
re_turns <- na.omit(re_turns)
# Calculate the Sharpe ratio
conf_level <- 0.05
PerformanceAnalytics::SharpeRatio(re_turns, p=(1-conf_level),
  method="historical")
# Calculate the Sortino ratio
PerformanceAnalytics::SortinoRatio(re_turns)
# Calculate the Calmar ratio
PerformanceAnalytics::CalmarRatio(re_turns)
# Calculate the Dowd ratio
PerformanceAnalytics::SharpeRatio(re_turns, FUN="VaR",
  p=(1-conf_level), method="historical")
# Calculate the Dowd ratio from scratch
va_r <- sapply(re_turns, quantile, probs=conf_level)
-sapply(re_turns, mean)/va_r
# Calculate the Conditional Dowd ratio
PerformanceAnalytics::SharpeRatio(re_turns, FUN="ES",
  p=(1-conf_level), method="historical")
# Calculate the Conditional Dowd ratio from scratch
c_var <- sapply(re_turns, function(x) {
  mean(x[x < quantile(x, conf_level)])
})
-sapply(re_turns, mean)/c_var
# Calculate VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
re_turns <- drop(zoo::coredata(re_turns))
n_rows <- NROW(re_turns)
# Calculate compounded VTI returns
hold_p <- 252
cum_rets <- sqrt(hold_p)*sapply(1:n_rows, function(x) {
    mean(re_turns[sample.int(n_rows, size=hold_p, replace=TRUE)])
})  # end sapply
# Calculate mean, standard deviation, skewness, and kurtosis
da_ta <- cbind(re_turns, cum_rets)
colnames(da_ta) <- c("VTI", "Agg")
apply(da_ta, MARGIN=2, function(x) {
  # Standardize the returns
  meanval <- mean(x); stddev <- sd(x); x <- (x - meanval)/stddev
  c(mean=meanval, stddev=stddev, skew=mean(x^3), kurt=mean(x^4))
})  # end sapply
# Calculate the Sharpe and Dowd ratios
conf_level <- 0.05
sapply(colnames(da_ta), function(name) {
  x <- da_ta[, name]; stddev <- sd(x)
  va_r <- unname(quantile(x, probs=conf_level))
  c_var <- mean(x[x < va_r])
  fac_tor <- 1
  if (name == colnames(da_ta)[2]) {fac_tor <- hold_p}
  sqrt(252/fac_tor)*mean(x)/c(Sharpe=stddev, Dowd=-va_r, DowdC=-c_var)
})  # end sapply
# Plot the densities of returns
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
plot(density(re_turns), t="l", lwd=3, col="blue",
     xlab="returns", ylab="density", xlim=c(-0.04, 0.04),
     main="Distribution of Compounded Stock Returns")
lines(density(cum_rets), t="l", col="red", lwd=3)
curve(expr=dnorm(x, mean=mean(cum_rets), sd=sd(cum_rets)), col="green", lwd=3, add=TRUE)
legend("topright", legend=c("VTI Daily", "Compounded", "Normal"),
 inset=-0.1, bg="white", lty=1, lwd=6, col=c("blue", "red", "green"), bty="n")
# Number of flights from each airport
data_table[, .N, by=origin]
# Same, but add names to output
data_table[, .(flights=.N), by=.(airport=origin)]
# Number of AA flights from each airport
data_table[carrier=="AA", .(flights=.N),
     by=.(airport=origin)]
# Number of flights from each airport and airline
data_table[, .(flights=.N),
     by=.(airport=origin, airline=carrier)]
# Average aircraft_delay
data_table[, mean(aircraft_delay)]
# Average aircraft_delay from JFK
data_table[origin=="JFK", mean(aircraft_delay)]
# Average aircraft_delay from each airport
data_table[, .(delay=mean(aircraft_delay)),
     by=.(airport=origin)]
# Average and max delays from each airport and month
data_table[, .(mean_delay=mean(aircraft_delay), max_delay=max(aircraft_delay)),
     by=.(airport=origin, month=month)]
# Average and max delays from each airport and month
data_table[, .(mean_delay=mean(aircraft_delay), max_delay=max(aircraft_delay)),
     keyby=.(airport=origin, month=month)]
# Extract time series of VTI log prices
clos_e <- log(na.omit(rutils::etf_env$price_s$VTI))
# Inspect the R code of the function filter()
filter
# Calculate EWMA weight_s
look_back <- 21
weight_s <- exp(-0.1*1:look_back)
weight_s <- weight_s/sum(weight_s)
# Calculate convolution using filter()
filter_ed <- filter(clos_e, filter=weight_s,
              method="convolution", sides=1)
# filter() returns time series of class "ts"
class(filter_ed)
# Get information about C_cfilter()
getAnywhere(C_cfilter)
# Filter using C_cfilter() over past values (sides=1).
filter_fast <- .Call(stats:::C_cfilter, clos_e, filter=weight_s,
               sides=1, circular=FALSE)
all.equal(as.numeric(filter_ed), filter_fast, check.attributes=FALSE)
# Calculate EWMA prices using roll::roll_sum()
weights_rev <- rev(weight_s)
roll_ed <- roll::roll_sum(clos_e, width=look_back, weights=weights_rev, min_obs=1)
all.equal(filter_fast[-(1:look_back)], as.numeric(roll_ed)[-(1:look_back)])
# Benchmark speed of rolling calculations
library(microbenchmark)
summary(microbenchmark(
  filter=filter(clos_e, filter=weight_s, method="convolution", sides=1),
  filter_fast=.Call(stats:::C_cfilter, clos_e, filter=weight_s, sides=1, circular=FALSE),
  roll=roll::roll_sum(clos_e, width=look_back, weights=weights_rev)
  ), times=10)[, c(1, 4, 5)]
# Simulate AR process using filter()
n_rows <- NROW(clos_e)
# Calculate ARIMA coefficients and innovations
co_eff <- weight_s/4
n_coeff <- NROW(co_eff)
in_nov <- rnorm(n_rows)
ari_ma <- filter(x=in_nov, filter=co_eff, method="recursive")
# Get information about C_rfilter()
getAnywhere(C_rfilter)
# Filter using C_rfilter() compiled C++ function directly
arima_fast <- .Call(stats:::C_rfilter, in_nov, co_eff,
              double(n_coeff + n_rows))
all.equal(as.numeric(ari_ma), arima_fast[-(1:n_coeff)],
    check.attributes=FALSE)
# Filter using C++ code
arima_fastest <- HighFreq::sim_arima(in_nov, rev(co_eff))
all.equal(arima_fast[-(1:n_coeff)], drop(arima_fastest))
# Benchmark speed of the three methods
summary(microbenchmark(
  filter=filter(x=in_nov, filter=co_eff, method="recursive"),
  filter_fast=.Call(stats:::C_rfilter, in_nov, co_eff, double(n_coeff + n_rows)),
  Rcpp=HighFreq::sim_arima(in_nov, rev(co_eff))
  ), times=10)[, c(1, 4, 5)]
# Calculate trailing EWMA prices using roll::roll_sum()
look_back <- 21
weight_s <- exp(-0.1*1:look_back)
weight_s <- weight_s/sum(weight_s)
weights_rev <- rev(weight_s)
filter_ed <- roll::roll_sum(clos_e, width=NROW(weight_s), weights=weights_rev)
# Copy warmup period
filter_ed[1:look_back] <- clos_e[1:look_back]
# Combine prices with smoothed prices
price_s <- cbind(clos_e, filter_ed)
colnames(price_s)[2] <- "VTI Smooth"
# Calculate standard deviations of returns
sapply(rutils::diff_it(price_s), sd)
# Plot dygraph
dygraphs::dygraph(price_s["2009"], main="VTI Prices and Trailing Smoothed Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)
# Calculate centered EWMA prices using roll::roll_sum()
weight_s <- c(weights_rev, weight_s[-1])
weight_s <- weight_s/sum(weight_s)
filter_ed <- roll::roll_sum(clos_e, width=NROW(weight_s), weights=weight_s, online=FALSE)
# Copy warmup period
filter_ed[1:(2*look_back)] <- clos_e[1:(2*look_back)]
# Center the data
filter_ed <- rutils::lag_it(filter_ed, -(look_back-1), pad_zeros=FALSE)
# Combine prices with smoothed prices
price_s <- cbind(clos_e, filter_ed)
colnames(price_s)[2] <- "VTI Smooth"
# Calculate standard deviations of returns
sapply(rutils::diff_it(price_s), sd)
# Plot dygraph
dygraphs::dygraph(price_s["2009"], main="VTI Prices and Centered Smoothed Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)
library(rutils)  # Load package rutils
library(ggplot2)  # Load ggplot2
library(gridExtra)  # Load gridExtra
# Coerce to zoo and merge the time series
filter_ed <- cbind(clos_e, filter_ed)
colnames(filter_ed) <- c("VTI", "VTI filtered")
# Plot ggplot2
autoplot(filter_ed["2008/2010"],
    main="Filtered VTI", facets=NULL) +  # end autoplot
xlab("") + ylab("") +
theme(  # Modify plot theme
    legend.position=c(0.1, 0.5),
    plot.title=element_text(vjust=-2.0),
    plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
    plot.background=element_blank(),
    axis.text.y=element_blank()
    )  # end theme
# end ggplot2
# Open plot window
x11(width=6, height=7)
# Set plot parameters
par(oma=c(1, 1, 0, 1), mar=c(1, 1, 1, 1), mgp=c(0, 0.5, 0),
    cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Set two plot panels
par(mfrow=c(2,1))
# Plot ACF of VTI returns
rutils::plot_acf(re_turns[, 1], lag=10, xlab="")
title(main="ACF of VTI Returns", line=-1)
# Plot ACF of smoothed VTI returns
rutils::plot_acf(re_turns[, 2], lag=10, xlab="")
title(main="ACF of Smoothed VTI Returns", line=-1)
# Get close prices and calculate close-to-close returns
# clos_e <- quantmod::Cl(rutils::etf_env$VTI)
clos_e <- quantmod::Cl(HighFreq::SPY)
colnames(clos_e) <- rutils::get_name(colnames(clos_e))
re_turns <- TTR::ROC(clos_e)
re_turns[1] <- 0
# Calculate the RSI indicator
r_si <- TTR::RSI(clos_e, 2)
# Calculate the long (up) and short (dn) signals
sig_up <- ifelse(r_si < 10, 1, 0)
sig_dn <- ifelse(r_si > 90, -1, 0)
# Lag signals by one period
sig_up <- rutils::lag_it(sig_up, 1)
sig_dn <- rutils::lag_it(sig_dn, 1)
# Replace NA signals with zero position
sig_up[is.na(sig_up)] <- 0
sig_dn[is.na(sig_dn)] <- 0
# Combine up and down signals into one
sig_nals <- sig_up + sig_dn
# Calculate cumulative returns
eq_up <- exp(cumsum(sig_up*re_turns))
eq_dn <- exp(cumsum(-1*sig_dn*re_turns))
eq_all <- exp(cumsum(sig_nals*re_turns))
# Plot daily cumulative returns in panels
end_p <- endpoints(re_turns, on="days")
plot.zoo(cbind(eq_all, eq_up, eq_dn)[end_p], lwd=c(2, 2, 2),
  ylab=c("Total","Long","Short"), col=c("red","green","blue"),
  main=paste("RSI(2) strategy for", colnames(clos_e), "from",
       format(start(re_turns), "%B %Y"), "to",
       format(end(re_turns), "%B %Y")))
# Extract log VTI prices
oh_lc <- rutils::etf_env$VTI
clos_e <- log(quantmod::Cl(oh_lc))
n_rows <- NROW(clos_e)
# Calculate EWMA weights
look_back <- 252
lamb_da <- 0.07
weight_s <- exp(lamb_da*1:look_back)
weight_s <- weight_s/sum(weight_s)
# Calculate EWMA prices
ew_ma <- roll::roll_sum(clos_e, width=look_back, weights=weight_s, min_obs=1)
# Copy over NA values
ew_ma <- zoo::na.locf(ew_ma, fromLast=TRUE)
price_s <- cbind(clos_e, ew_ma)
colnames(price_s) <- c("VTI", "VTI EWMA")
# Dygraphs plot with custom line colors
col_ors <- c("blue", "red")
dygraphs::dygraph(price_s["2009"], main="VTI EWMA Prices") %>%
  dyOptions(colors=col_ors, strokeWidth=2)
# Plot EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
quantmod::chart_Series(price_s["2009"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
legend("bottomright", legend=colnames(price_s),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Calculate log OHLC prices and volumes
oh_lc <- rutils::etf_env$VTI
clos_e <- log(quantmod::Cl(oh_lc))
colnames(clos_e) <- "VTI"
vol_ume <- quantmod::Vo(oh_lc)
colnames(vol_ume) <- "Volume"
n_rows <- NROW(clos_e)
# Calculate the VWAP prices
look_back <- 21
vwap <- roll::roll_sum(clos_e*vol_ume, width=look_back, min_obs=1)
volume_roll <- roll::roll_sum(vol_ume, width=look_back, min_obs=1)
vwap <- vwap/volume_roll
colnames(vwap) <- "VWAP"
price_s <- cbind(clos_e, vwap)
# Dygraphs plot with custom line colors
col_ors <- c("blue", "red")
dygraphs::dygraph(price_s["2009"], main="VTI VWAP Prices") %>%
  dyOptions(colors=col_ors, strokeWidth=2)
# Plot VWAP prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
quantmod::chart_Series(price_s["2009"], theme=plot_theme,
       lwd=2, name="VTI VWAP Prices")
legend("bottomright", legend=colnames(price_s),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Calculate two EWMA prices
look_back <- 21
lamb_da <- 0.1
weight_s <- exp(lamb_da*1:look_back)
weight_s <- weight_s/sum(weight_s)
ewma_fast <- roll::roll_sum(clos_e, width=look_back, weights=weight_s, min_obs=1)
lamb_da <- 0.05
weight_s <- exp(lamb_da*1:look_back)
weight_s <- weight_s/sum(weight_s)
ewma_slow <- roll::roll_sum(clos_e, width=look_back, weights=weight_s, min_obs=1)
# Calculate VTI returns
re_turns <- (ewma_fast - ewma_slow)
price_s <- cbind(clos_e, re_turns)
colnames(price_s) <- c(sym_bol, paste(sym_bol, "Returns"))
# Plot dygraph of VTI Returns
col_names <- colnames(price_s)
dygraphs::dygraph(price_s["2009"], main=paste(sym_bol, "EWMA Returns")) %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
# Calculate fractional weights
del_ta <- 0.1
weight_s <- (del_ta - 0:(look_back-2)) / 1:(look_back-1)
weight_s <- (-1)^(1:(look_back-1))*cumprod(weight_s)
weight_s <- c(1, weight_s)
weight_s <- (weight_s - mean(weight_s))
weight_s <- rev(weight_s)
# Calculate fractional VTI returns
re_turns <- roll::roll_sum(clos_e, width=look_back, weights=weight_s, min_obs=1, online=FALSE)
price_s <- cbind(clos_e, re_turns)
colnames(price_s) <- c(sym_bol, paste(sym_bol, "Returns"))
# Plot dygraph of VTI Returns
col_names <- colnames(price_s)
dygraphs::dygraph(price_s["2009"], main=paste(sym_bol, "Fractional Returns")) %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
# Calculate VTI log returns
clos_e <- log(quantmod::Cl(rutils::etf_env$VTI))
re_turns <- rutils::diff_it(clos_e)
# Perform ADF test for prices
tseries::adf.test(clos_e)
# Perform ADF test for returns
tseries::adf.test(re_turns)
# Calculate fractional VTI returns
delta_s <- 0.1*c(1, 3, 5, 7, 9)
re_turns <- lapply(delta_s, function(del_ta) {
  weight_s <- (del_ta - 0:(look_back-2)) / 1:(look_back-1)
  weight_s <- c(1, (-1)^(1:(look_back-1))*cumprod(weight_s))
  weight_s <- rev(weight_s - mean(weight_s))
  roll::roll_sum(clos_e, width=look_back, weights=weight_s, min_obs=1, online=FALSE)
})  # end lapply
re_turns <- do.call(cbind, re_turns)
re_turns <- cbind(clos_e, re_turns)
colnames(re_turns) <- c("VTI", paste0("frac_", delta_s))
# Calculate ADF test statistics
adf_stats <- sapply(re_turns, function(x)
  suppressWarnings(tseries::adf.test(x)$statistic)
)  # end sapply
names(adf_stats) <- colnames(re_turns)
# Plot dygraph of fractional VTI returns
color_s <- colorRampPalette(c("blue", "red"))(NCOL(re_turns))
col_names <- colnames(re_turns)
dy_graph <- dygraphs::dygraph(re_turns["2019"], main="Fractional Returns") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col=color_s[1])
for (i in 2:NROW(col_names))
  dy_graph <- dy_graph %>%
  dyAxis("y2", label=col_names[i], independentTicks=TRUE) %>%
  dySeries(name=col_names[i], axis="y2", label=col_names[i], strokeWidth=2, col=color_s[i])
dy_graph <- dy_graph %>% dyLegend(width=500)
dy_graph
# Calculate volume z-scores
vol_ume <- quantmod::Vo(rutils::etf_env$VTI)
look_back <- 21
volume_mean <- roll::roll_mean(vol_ume, width=look_back, min_obs=1)
volume_sd <- roll::roll_sd(rutils::diff_it(vol_ume), width=look_back, min_obs=1)
volume_sd[1] <- 0
volume_scores <- ifelse(volume_sd > 0, (vol_ume - volume_mean)/volume_sd, 0)
# Plot histogram of volume z-scores
x11(width=6, height=5)
hist(volume_scores, breaks=1e2)
# Plot dygraph of volume z-scores of VTI prices
price_s <- cbind(clos_e, volume_scores)
colnames(price_s) <- c("VTI", "Z-scores")
col_names <- colnames(price_s)
dygraphs::dygraph(price_s["2009"], main="VTI Volume Z-Scores") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
# Extract VTI log OHLC prices
oh_lc <- log(rutils::etf_env$VTI)
# Calculate volatility z-scores
vol_at <- quantmod::Hi(oh_lc)-quantmod::Lo(oh_lc)
look_back <- 21
volat_mean <- roll::roll_mean(vol_at, width=look_back, min_obs=1)
volat_sd <- roll::roll_sd(rutils::diff_it(vol_at), width=look_back, min_obs=1)
volat_sd[1] <- 0
volat_scores <- ifelse(volat_sd > 0, (vol_at - volat_mean)/volat_sd, 0)
# Plot histogram of volatility z-scores
x11(width=6, height=5)
hist(volat_scores, breaks=1e2)
# Plot scatterplot of volume and volatility z-scores
plot(as.numeric(volat_scores), as.numeric(volume_scores),
     xlab="volatility z-score", ylab="volume z-score")
# Plot dygraph of VTI volatility z-scores
clos_e <- quantmod::Cl(oh_lc)
price_s <- cbind(clos_e, volat_scores)
colnames(price_s) <- c("VTI", "Z-scores")
col_names <- colnames(price_s)
dygraphs::dygraph(price_s["2009"], main="VTI Volatility Z-Scores") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
# Calculate the centered volatility
look_back <- 21
half_back <- look_back %/% 2
re_turns <- rutils::diff_it(clos_e)
vol_at <- roll::roll_sd(re_turns, width=look_back, min_obs=1)
vol_at <- rutils::lag_it(vol_at, lagg=(-half_back))
# Calculate the z-scores of prices
price_scores <- (2*clos_e -
  rutils::lag_it(clos_e, half_back, pad_zeros=FALSE) -
  rutils::lag_it(clos_e, -half_back, pad_zeros=FALSE))
price_scores <- ifelse(vol_at > 0, price_scores/vol_at, 0)
# Plot dygraph of z-scores of VTI prices
price_s <- cbind(clos_e, price_scores)
colnames(price_s) <- c("VTI", "Z-scores")
col_names <- colnames(price_s)
dygraphs::dygraph(price_s["2009"], main="VTI Price Z-Scores") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
# Calculate thresholds for labeling tops and bottoms
threshold_s <- quantile(price_scores, c(0.1, 0.9))
# Calculate the vectors of tops and bottoms
top_s <- (price_scores > threshold_s[2])
colnames(top_s) <- "tops"
bottom_s <- (price_scores < threshold_s[1])
colnames(bottom_s) <- "bottoms"
# Backtest in-sample VTI strategy
position_s <- rep(NA_integer_, NROW(re_turns))
position_s[1] <- 0
position_s[top_s] <- (-1)
position_s[bottom_s] <- 1
position_s <- zoo::na.locf(position_s)
position_s <- rutils::lag_it(position_s)
pnl_s <- cumsum(re_turns*position_s)
# Plot dygraph of in-sample VTI strategy
price_s <- cbind(clos_e, pnl_s)
colnames(price_s) <- c("VTI", "Strategy")
col_names <- colnames(price_s)
dygraphs::dygraph(price_s, main="VTI Strategy Using In-sample Labels") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
# Calculate trailing price z-scores
date_s <- matrix(as.numeric(zoo::index(clos_e)))
look_back <- 21
price_scores <- drop(HighFreq::roll_zscores(res_ponse=clos_e, de_sign=date_s, look_back=look_back))
price_scores[1:look_back] <- 0
# Plot dygraph of z-scores of VTI prices
price_s <- cbind(clos_e, price_scores)
colnames(price_s) <- c("VTI", "Z-scores")
col_names <- colnames(price_s)
dygraphs::dygraph(price_s["2009"], main="VTI Price Z-Scores") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
# Extract time series of VTI log prices
clos_e <- log(na.omit(rutils::etf_env$price_s$VTI))
# Define look-back window
look_back <- 11
# Calculate time series of medians
medi_an <- roll::roll_median(clos_e, width=look_back)
# medi_an <- TTR::runMedian(clos_e, n=look_back)
# Calculate time series of MAD
ma_d <- HighFreq::roll_var(clos_e, look_back=look_back, method="nonparametric")
# ma_d <- TTR::runMAD(clos_e, n=look_back)
# Calculate time series of z-scores
z_scores <- (clos_e - medi_an)/ma_d
z_scores[1:look_back, ] <- 0
tail(z_scores, look_back)
range(z_scores)
x11(width=6, height=5)
# Plot prices and medians
dygraphs::dygraph(cbind(clos_e, medi_an), main="VTI median") %>%
  dyOptions(colors=c("black", "red"))
# Plot histogram of z-scores
histo_gram <- hist(z_scores, col="lightgrey",
  xlab="z-scores", breaks=50, xlim=c(-4, 4),
  ylab="frequency", freq=FALSE, main="Hampel Z-scores histogram")
lines(density(z_scores, adjust=1.5), lwd=3, col="blue")
# Calculate one-sided Hampel z-scores
medi_an <- roll::roll_median(clos_e, width=look_back)
# medi_an <- TTR::runMedian(clos_e, n=look_back)
ma_d <- HighFreq::roll_var(clos_e, look_back=look_back, method="nonparametric")
# ma_d <- TTR::runMAD(clos_e, n=look_back)
z_scores <- (clos_e - medi_an)/ma_d
z_scores[1:look_back, ] <- 0
tail(z_scores, look_back)
range(z_scores)
# Calculate two-sided Hampel z-scores
half_back <- look_back %/% 2
medi_an <- rutils::lag_it(medi_an, lagg=-half_back)
ma_d <- rutils::lag_it(ma_d, lagg=-half_back)
z_scores <- (clos_e - medi_an)/ma_d
z_scores[1:look_back, ] <- 0
tail(z_scores, look_back)
range(z_scores)
# Calculate EWMA VTI variance using compiled C++ function
look_back <- 51
weight_s <- exp(-0.1*1:look_back)
weight_s <- weight_s/sum(weight_s)
vari_ance <- .Call(stats:::C_cfilter, re_turns^2,
  filter=weight_s, sides=1, circular=FALSE)
vari_ance[1:(look_back-1)] <- vari_ance[look_back]
# Plot EWMA volatility
vari_ance <- xts:::xts(sqrt(vari_ance), order.by=index(re_turns))
dygraphs::dygraph(vari_ance, main="VTI EWMA Volatility")
quantmod::chart_Series(x_ts, name="VTI EWMA Volatility")
# Calculate VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
n_rows <- NROW(re_turns)
# Define end points
end_p <- 1:NROW(re_turns)
# Start points are multi-period lag of end_p
look_back <- 11
start_p <- c(rep_len(0, look_back-1), end_p[1:(n_rows-look_back+1)])
# Calculate rolling variance in sapply() loop - takes very long
vari_ance <- sapply(1:n_rows, function(in_dex) {
  ret_s <- re_turns[start_p[in_dex]:end_p[in_dex]]
  sum((ret_s - mean(ret_s))^2)
}) / (look_back-1)  # end sapply
# Use only vectorized functions
cum_rets <- cumsum(re_turns)
cum_rets <- (cum_rets -
  c(rep_len(0, look_back), cum_rets[1:(n_rows-look_back)]))
cum_rets2 <- cumsum(re_turns^2)
cum_rets2 <- (cum_rets2 -
  c(rep_len(0, look_back), cum_rets2[1:(n_rows-look_back)]))
vari_ance2 <- (cum_rets2 - cum_rets^2/look_back)/(look_back-1)
all.equal(vari_ance[-(1:look_back)], as.numeric(vari_ance2)[-(1:look_back)])
# Same, using package rutils
cum_rets <- rutils::roll_sum(re_turns, look_back=look_back, min_obs=1)
cum_rets2 <- rutils::roll_sum(re_turns^2, look_back=look_back, min_obs=1)
vari_ance2 <- (cum_rets2 - cum_rets^2/look_back)/(look_back-1)
# Coerce vari_ance into xts
tail(vari_ance)
class(vari_ance)
vari_ance <- xts(vari_ance, order.by=index(re_turns))
colnames(vari_ance) <- "VTI.variance"
head(vari_ance)
# Calculate rolling VTI variance using package roll
library(roll)  # Load roll
vari_ance <- roll::roll_var(re_turns, width=look_back)
colnames(vari_ance) <- "VTI.variance"
head(vari_ance)
sum(is.na(vari_ance))
vari_ance[1:(look_back-1)] <- 0
# Benchmark calculation of rolling variance
library(microbenchmark)
summary(microbenchmark(
  roll_sapply=sapply(2:n_rows, function(in_dex) {
    ret_s <- re_turns[start_p[in_dex]:end_p[in_dex]]
    sum((ret_s - mean(ret_s))^2)
  }),
  ro_ll=roll::roll_var(re_turns, width=look_back),
  times=10))[, c(1, 4, 5)]
# Calculate EWMA VTI variance using compiled C++ function
look_back <- 51
weight_s <- exp(-0.1*1:look_back)
weight_s <- weight_s/sum(weight_s)
vari_ance <- .Call(stats:::C_cfilter, re_turns^2,
  filter=weight_s, sides=1, circular=FALSE)
vari_ance[1:(look_back-1)] <- vari_ance[look_back]
# Plot EWMA volatility
vari_ance <- xts:::xts(sqrt(vari_ance), order.by=index(re_turns))
dygraphs::dygraph(vari_ance, main="VTI EWMA Volatility") %>%
  dyOptions(colors="blue")
quantmod::chart_Series(x_ts, name="VTI EWMA Volatility")
# Calculate rolling VTI variance using package roll
library(roll)  # Load roll
vari_ance <- roll::roll_var(re_turns,
  weights=rev(weight_s), width=look_back)
colnames(vari_ance) <- "VTI.variance"
class(vari_ance)
head(vari_ance)
sum(is.na(vari_ance))
vari_ance[1:(look_back-1)] <- 0
library(HighFreq)  # Load HighFreq
# Minutely SPY returns (unit per minute) single day
# Minutely SPY volatility (unit per minute)
re_turns <- rutils::diff_it(log(SPY["2012-02-13", 4]))
sd(re_turns)
# SPY returns multiple days (includes overnight jumps)
re_turns <- rutils::diff_it(log(SPY[, 4]))
sd(re_turns)
# Table of time intervals - 60 second is most frequent
in_dex <- rutils::diff_it(.index(SPY))
table(in_dex)
# SPY returns divided by the overnight time intervals (unit per second)
re_turns <- re_turns / in_dex
re_turns[1] <- 0
# Minutely SPY volatility scaled to unit per minute
60*sd(re_turns)
library(HighFreq)  # Load HighFreq
sp_y <- HighFreq::SPY["2009"]
# Calculate daily SPY volatility using package HighFreq
sqrt(6.5*60*HighFreq::calc_var_ohlc(log(sp_y),
  method="yang_zhang"))
# Calculate daily SPY volatility from minutely prices using package TTR
sqrt((6.5*60)*mean(na.omit(
  TTR::volatility(sp_y, N=1, calc="yang.zhang"))^2))
# Calculate rolling SPY variance using package HighFreq
vari_ance <- HighFreq::roll_var_ohlc(log(sp_y), method="yang_zhang",
  look_back=look_back)
# Plot range volatility
vari_ance <- xts:::xts(sqrt(vari_ance), order.by=index(sp_y))
dygraphs::dygraph(vari_ance["2009-02"],
  main="SPY Rolling Range Volatility") %>%
  dyOptions(colors="blue")
# Benchmark the speed of HighFreq vs TTR
library(microbenchmark)
summary(microbenchmark(
  ttr=TTR::volatility(rutils::etf_env$VTI, N=1, calc="yang.zhang"),
  highfreq=HighFreq::calc_var_ohlc(log(rutils::etf_env$VTI), method="yang_zhang"),
  times=2))[, c(1, 4, 5)]
# Calculate VXX log prices
vx_x <- na.omit(rutils::etf_env$price_s$VXX)
date_s <- zoo::index(vx_x)
look_back <- 41
vx_x <- log(vx_x)
# Calculate rolling VTI volatility
clos_e <- get("VTI", rutils::etf_env)[date_s]
clos_e <- log(clos_e)
vol_at <- sqrt(HighFreq::roll_var_ohlc(oh_lc=clos_e, look_back=look_back, scal_e=FALSE))
vol_at[1:look_back] <- vol_at[look_back+1]
# Plot dygraph of VXX and VTI volatility
da_ta <- cbind(vx_x, vol_at)
colnames(da_ta)[2] <- "VTI Volatility"
col_names <- colnames(da_ta)
cap_tion <- "VXX and VTI Volatility"
dygraphs::dygraph(da_ta[, 1:2], main=cap_tion) %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="red")
# Calculate VXX log prices
vx_x <- na.omit(rutils::etf_env$price_s$VXX)
date_s <- zoo::index(vx_x)
look_back <- 41
vx_x <- log(vx_x)
vx_x <- (vx_x - roll::roll_mean(vx_x, width=look_back))
vx_x[1:look_back] <- vx_x[look_back+1]
# Calculate rolling VTI volatility
clos_e <- get("VTI", rutils::etf_env)[date_s]
clos_e <- log(clos_e)
vol_at <- sqrt(HighFreq::roll_var_ohlc(oh_lc=clos_e, look_back=look_back, scal_e=FALSE))
vol_at[1:look_back] <- vol_at[look_back+1]
# Calculate regression coefficients of XLB ~ XLE
be_ta <- drop(cov(vx_x, vol_at)/var(vol_at))
al_pha <- drop(mean(vx_x) - be_ta*mean(vol_at))
# Calculate regression residuals
fit_ted <- (al_pha + be_ta*vol_at)
residual_s <- (vx_x - fit_ted)
# Perform ADF test on residuals
tseries::adf.test(residual_s, k=1)
# Plot dygraph of VXX and VTI volatility
da_ta <- cbind(vx_x, vol_at)
col_names <- colnames(da_ta)
cap_tion <- "VXX and VTI Volatility"
dygraphs::dygraph(da_ta[, 1:2], main=cap_tion) %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="red")
x11(width=6, height=5)
par(mar=c(4, 3, 1, 1), oma=c(0, 0, 0, 0))
# Calculate VTI percentage returns
re_turns <- na.omit(rutils::etf_env$re_turns$VTI)
# Calculate rolling VTI variance using package roll
look_back <- 22
vari_ance <- roll::roll_var(re_turns, width=look_back)
vari_ance[1:(look_back-1)] <- 0
colnames(vari_ance) <- "VTI.variance"
# Number of look_backs that fit over re_turns
n_rows <- NROW(re_turns)
n_agg <- n_rows %/% look_back
# Define end_p with beginning stub
end_p <- c(0, n_rows-look_back*n_agg + (0:n_agg)*look_back)
n_rows <- NROW(end_p)
# Subset vari_ance to end_p
vari_ance <- vari_ance[end_p]
# Plot autocorrelation function
rutils::plot_acf(vari_ance, lag=10, main="ACF of Variance")
# Plot partial autocorrelation
pacf(vari_ance, lag=10, main="PACF of Variance", ylab=NA)
# Define GARCH parameters
al_pha <- 0.3; be_ta <- 0.5;
om_ega <- 1e-4*(1-al_pha-be_ta)
n_rows <- 1000
# Calculate matrix of standard normal innovations
set.seed(1121)  # Reset random numbers
in_nov <- rnorm(n_rows)
re_turns <- numeric(n_rows)
vari_ance <- numeric(n_rows)
vari_ance[1] <- om_ega/(1-al_pha-be_ta)
re_turns[1] <- sqrt(vari_ance[1])*in_nov[1]
# Simulate GARCH model
for (i in 2:n_rows) {
  re_turns[i] <- sqrt(vari_ance[i-1])*in_nov[i]
  vari_ance[i] <- om_ega + al_pha*re_turns[i]^2 +
    be_ta*vari_ance[i-1]
}  # end for
# Simulate the GARCH process using Rcpp
garch_data <- HighFreq::sim_garch(omega=om_ega, alpha=al_pha,
  beta=be_ta, innov=matrix(in_nov))
all.equal(garch_data, cbind(re_turns, vari_ance),
  check.attributes=FALSE)
# Define GARCH parameters
al_pha <- 0.3; be_ta <- 0.5;
om_ega <- 1e-4*(1-al_pha-be_ta)
n_rows <- 1000
# Calculate matrix of standard normal innovations
set.seed(1121)  # Reset random numbers
in_nov <- rnorm(n_rows)
re_turns <- numeric(n_rows)
vari_ance <- numeric(n_rows)
vari_ance[1] <- om_ega/(1-al_pha-be_ta)
re_turns[1] <- sqrt(vari_ance[1])*in_nov[1]
# Simulate GARCH model
for (i in 2:n_rows) {
  re_turns[i] <- sqrt(vari_ance[i-1])*in_nov[i]
  vari_ance[i] <- om_ega + al_pha*re_turns[i]^2 +
    be_ta*vari_ance[i-1]
}  # end for
# Simulate the GARCH process using Rcpp
garch_data <- HighFreq::sim_garch(omega=om_ega, alpha=al_pha,
  beta=be_ta, innov=matrix(in_nov))
all.equal(garch_data, cbind(re_turns, vari_ance),
  check.attributes=FALSE)
# Open plot window on Mac
dev.new(width=6, height=5, noRStudioGD=TRUE)
# Set plot parameters to reduce whitespace around plot
par(mar=c(2, 2, 3, 1), oma=c(0, 0, 0, 0))
# Plot GARCH cumulative returns
plot(cumsum(re_turns), t="l", col="blue", xlab="", ylab="",
  main="GARCH Cumulative Returns")
quartz.save("figure/garch_returns.png", type="png",
  width=6, height=5)
# Plot GARCH volatility
plot(sqrt(vari_ance), t="l", col="blue", xlab="", ylab="",
  main="GARCH Volatility")
quartz.save("figure/garch_volat.png", type="png",
  width=6, height=5)
# Calculate kurtosis of GARCH returns
mean(((re_turns-mean(re_turns))/sd(re_turns))^4)
# Perform Jarque-Bera test of normality
tseries::jarque.bera.test(re_turns)
# Fit t-distribution into GARCH returns
optim_fit <- MASS::fitdistr(re_turns, densfun="t", df=2)
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]
# Plot histogram of GARCH returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=200, xlim=c(-0.03, 0.03),
  ylab="frequency", freq=FALSE, main="GARCH Returns Histogram")
lines(density(re_turns, adjust=1.5), lwd=2, col="blue")
curve(expr=dt((x-lo_cation)/scal_e, df=2)/scal_e,
  type="l", xlab="", ylab="", lwd=2,
  col="red", add=TRUE)
legend("topright", inset=-0, bty="n",
 leg=c("density", "t-distr w/ 2 dof"),
 lwd=6, lty=1, col=c("blue", "red"))
quartz.save("figure/garch_hist.png", type="png", width=6, height=5)
# Specify GARCH model
garch_spec <- fGarch::garchSpec(model=list(ar=c(0, 0), omega=om_ega,
  alpha=al_pha, beta=be_ta))
# Simulate GARCH model
garch_sim <- fGarch::garchSim(spec=garch_spec, n=n_rows)
re_turns <- as.numeric(garch_sim)
# Calculate kurtosis of GARCH returns
moments::moment(re_turns, order=4) /
  moments::moment(re_turns, order=2)^2
# Perform Jarque-Bera test of normality
tseries::jarque.bera.test(re_turns)
# Plot histogram of GARCH returns
histo_gram <- hist(re_turns, col="lightgrey",
  xlab="returns", breaks=200, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE,
  main="GARCH Returns Histogram")
lines(density(re_turns, adjust=1.5), lwd=3, col="blue")
# Fit t-distribution into GARCH returns
optim_fit <- MASS::fitdistr(re_turns,
  densfun="t", df=2, lower=c(-1, 1e-7))
lo_cation <- optim_fit$estimate[1]
scal_e <- optim_fit$estimate[2]
curve(expr=dt((x-lo_cation)/scal_e, df=2)/scal_e,
  type="l", xlab="", ylab="", lwd=3,
  col="red", add=TRUE)
legend("topright", inset=0.05, bty="n",
 leg=c("density", "t-distr w/ 2 dof"),
 lwd=6, lty=1, col=c("blue", "red"))
# Calculate variance of GARCH returns
var(re_turns)
# Calculate expected value of variance
om_ega/(1-al_pha-be_ta)
# Calculate kurtosis of GARCH returns
mean(((re_turns-mean(re_turns))/sd(re_turns))^4)
# Calculate expected value of kurtosis
3 + 6*al_pha^2/(1-2*al_pha^2-(al_pha+be_ta)^2)
# Calculate the distribution of GARCH kurtosis
kurt <- sapply(1:1e4, function(x) {
  garch_data <- HighFreq::sim_garch(omega=om_ega, alpha=al_pha,
    beta=be_ta, innov=matrix(rnorm(n_rows)))
  re_turns <- garch_data[, 1]
  c(var(re_turns), mean(((re_turns-mean(re_turns))/sd(re_turns))^4))
})  # end sapply
kurt <- t(kurt)
apply(kurt, 2, mean)
# Plot the distribution of GARCH kurtosis
dev.new(width=6, height=5, noRStudioGD=TRUE)
par(mar=c(2, 2, 3, 1), oma=c(0, 0, 0, 0))
histo_gram <- hist(kurt[, 2], breaks=500, col="lightgrey",
  xlim=c(2, 8), xlab="returns", ylab="frequency", freq=FALSE,
  main="Distribution of GARCH Kurtosis")
lines(density(kurt[, 2], adjust=1.5), lwd=3, col="blue")
abline(v=(3 + 6*al_pha^2/(1-2*al_pha^2-(al_pha+be_ta)^2)), lwd=3, col="red")
text(x=7.0, y=0.4, "Expected Kurtosis")
quartz.save("figure/garch_kurtosis.png", type="png", width=6, height=5)
# Simulate the GARCH process using Rcpp
garch_data <- HighFreq::sim_garch(omega=om_ega, alpha=al_pha,
  beta=be_ta, innov=matrix(in_nov))
# Extract the returns
re_turns <- garch_data[, 1]
# Estimate the rolling variance from the returns
vari_ance <- numeric(n_rows)
vari_ance[1] <- om_ega/(1-al_pha-be_ta)
for (i in 2:n_rows) {
  vari_ance[i] <- om_ega + al_pha*re_turns[i]^2 +
    be_ta*vari_ance[i-1]
}  # end for
all.equal(garch_data[, 2], vari_ance, check.attributes=FALSE)
library(fGarch)
# Fit returns into GARCH
garch_fit <- fGarch::garchFit(data=re_turns)
# Fitted GARCH parameters
garch_fit@fit$coef
# Actual GARCH parameters
c(mu=mean(re_turns), omega=om_ega,alpha=al_pha, beta=be_ta)
# Plot GARCH fitted volatility
plot(sqrt(garch_fit@fit$series$h), t="l",
  col="blue", xlab="", ylab="",
  main="GARCH Fitted Volatility")
quartz.save("figure/garch_fGarch_fitted.png",
  type="png", width=6, height=5)
# Define likelihood function
likeli_hood <- function(om_ega, al_pha, be_ta) {
  # Estimate the rolling variance from the returns
  vari_ance <- numeric(n_rows)
  vari_ance[1] <- om_ega/(1-al_pha-be_ta)
  for (i in 2:n_rows) {
    vari_ance[i] <- om_ega + al_pha*re_turns[i]^2 + be_ta*vari_ance[i-1]
  }  # end for
  vari_ance <- ifelse(vari_ance > 0, vari_ance, 0.000001)
  # Lag the variance
  vari_ance <- rutils::lag_it(vari_ance, pad_zeros=FALSE)
  # Calculate the likelihood
  -sum(re_turns^2/vari_ance + log(vari_ance))
}  # end likeli_hood
# Calculate the likelihood in R
likeli_hood(om_ega, al_pha, be_ta)
# Calculate the likelihood in Rcpp
HighFreq::lik_garch(omega=om_ega, alpha=al_pha,
  beta=be_ta, returns=matrix(re_turns))
# Benchmark speed of likelihood calculations
library(microbenchmark)
summary(microbenchmark(
  Rcode=likeli_hood(om_ega, al_pha, be_ta),
  Rcpp=HighFreq::lik_garch(omega=om_ega, alpha=al_pha, beta=be_ta, returns=matrix(re_turns))
  ), times=10)[, c(1, 4, 5)]
# Calculate the variance of returns
re_turns <- garch_data[, 1, drop=FALSE]
vari_ance <- var(re_turns)
re_turns <- (re_turns - mean(re_turns))
# Calculate likelihood as function of al_pha and be_ta parameters
likeli_hood <- function(al_pha, be_ta) {
  om_ega <- vari_ance*(1 - al_pha - be_ta)
  -HighFreq::lik_garch(omega=om_ega, alpha=al_pha, beta=be_ta, returns=re_turns)
}  # end likeli_hood
# Calculate matrix of likelihood values
alphas <- seq(from=0.15, to=0.35, len=50)
betas <- seq(from=0.35, to=0.5, len=50)
lik_mat <- sapply(alphas, function(al_pha) sapply(betas,
  function(be_ta) likeli_hood(al_pha, be_ta)))
# Set rgl options and load package rgl
options(rgl.useNULL=TRUE); library(rgl)
# Draw and render 3d surface plot of likelihood function
n_col <- 100
col_or <- rainbow(n_col, start=2/6, end=4/6)
z_col <- cut(lik_mat, n_col)
rgl::persp3d(alphas, betas, lik_mat, col=col_or[z_col],
  xlab="alpha", ylab="beta", zlab="likelihood")
rgl::rglwidget(elementId="plot3drgl", width=700, height=700)
# Perform grid search
coord <- which(lik_mat == min(lik_mat), arr.ind=TRUE)
c(alphas[coord[2]], betas[coord[1]])
lik_mat[coord]
likeli_hood(alphas[coord[2]], betas[coord[1]])
# Optimal and actual parameters
options(scipen=2)  # Use fixed not scientific notation
cbind(actual=c(alpha=al_pha, beta=be_ta, omega=om_ega),
  optimal=c(alphas[coord[2]], betas[coord[1]], vari_ance*(1 - sum(alphas[coord[2]], betas[coord[1]]))))
# Define vectorized likelihood function
likeli_hood <- function(x, re_turns) {
  al_pha <- x[1]; be_ta <- x[2]; om_ega <- x[3]
  -HighFreq::lik_garch(omega=om_ega, alpha=al_pha, beta=be_ta, returns=re_turns)
}  # end likeli_hood
# Initial parameters
par_init <- c(alpha=0.2, beta=0.4, omega=vari_ance/0.2)
# Find max likelihood parameters using steepest descent optimizer
optim_fit <- optim(par=par_init,
  fn=likeli_hood, # Log-likelihood function
  method="L-BFGS-B", # Quasi-Newton method
  re_turns=re_turns,
  upper=c(0.35, 0.55, vari_ance), # Upper constraint
  lower=c(0.15, 0.35, vari_ance/100)) # Lower constraint
# Optimal and actual parameters
cbind(actual=c(alpha=al_pha, beta=be_ta, omega=om_ega),
optimal=c(optim_fit$par["alpha"], optim_fit$par["beta"], optim_fit$par["omega"]))
# Find max likelihood parameters using DEoptim
op_tim <- DEoptim::DEoptim(fn=likeli_hood,
  upper=c(0.35, 0.55, vari_ance), # Upper constraint
  lower=c(0.15, 0.35, vari_ance/100), # Lower constraint
  re_turns=re_turns,
  control=list(trace=FALSE, itermax=1000, parallelType=1))
# Optimal and actual parameters
cbind(actual=c(alpha=al_pha, beta=be_ta, omega=om_ega),
optimal=c(op_tim$optim$bestmem[1], op_tim$optim$bestmem[2], op_tim$optim$bestmem[3]))
# Calculate VTI returns
re_turns <- rutils::diff_it(log(quantmod::Cl(rutils::etf_env$VTI)))
# Find max likelihood parameters using DEoptim
op_tim <- DEoptim::DEoptim(fn=likeli_hood,
  upper=c(0.4, 0.9, vari_ance), # Upper constraint
  lower=c(0.1, 0.5, vari_ance/100), # Lower constraint
  re_turns=re_turns,
  control=list(trace=FALSE, itermax=1000, parallelType=1))
# Optimal parameters
par_am <- unname(op_tim$optim$bestmem)
al_pha <- par_am[1]; be_ta <- par_am[2]; om_ega <- par_am[3]
c(al_pha, be_ta, om_ega)
# Equilibrium GARCH variance
om_ega/(1-al_pha-be_ta)
drop(var(re_turns))
# Estimate the GARCH volatility of VTI returns
n_rows <- NROW(re_turns)
vari_ance <- numeric(n_rows)
vari_ance[1] <- om_ega/(1-al_pha-be_ta)
for (i in 2:n_rows) {
  vari_ance[i] <- om_ega + al_pha*re_turns[i]^2 + be_ta*vari_ance[i-1]
}  # end for
# Estimate the GARCH volatility using Rcpp
garch_data <- HighFreq::sim_garch(omega=om_ega, alpha=al_pha,
  beta=be_ta, innov=re_turns, is_random=FALSE)
all.equal(garch_data[, 2], vari_ance, check.attributes=FALSE)
# Plot dygraph of the estimated GARCH volatility
dygraphs::dygraph(xts::xts(sqrt(vari_ance), index(re_turns)),
  main="Estimated GARCH Volatility of VTI") %>%
  dyOptions(colors="blue")
# Simulate GARCH model
garch_data <- HighFreq::sim_garch(omega=om_ega, alpha=al_pha,
  beta=be_ta, innov=matrix(in_nov))
vari_ance <- garch_data[, 2]
# Calculate the equilibrium variance
var_eq <- om_ega/(1-al_pha-be_ta)
# Calculate the variance forecasts
varcasts <- numeric(10)
varcasts[1] <- var_eq +
  (al_pha + be_ta)*(xts::last(vari_ance) - var_eq)
for (i in 2:10) {
  varcasts[i] <- var_eq + (al_pha + be_ta)*(varcasts[i-1] - var_eq)
}  # end for
# Open plot window on Mac
dev.new(width=6, height=5, noRStudioGD=TRUE)
par(mar=c(2, 2, 3, 1), oma=c(0, 0, 0, 0))
# Plot GARCH variance forecasts
plot(tail(vari_ance, 30), t="l", col="blue", xlab="", ylab="",
  xlim=c(1, 40), ylim=c(0, max(tail(vari_ance, 30))),
  main="GARCH Variance Forecasts")
text(x=15, y=0.5*var_eq, "realized variance")
lines(x=30:40, y=c(xts::last(vari_ance), varcasts), col="red", lwd=3)
text(x=35, y=0.6*var_eq, "variance forecasts")
abline(h=var_eq, lwd=3, col="red")
text(x=10, y=1.1*var_eq, "Equilibrium variance")
quartz.save("figure/garch_forecast.png", type="png",
  width=6, height=5)
library(HighFreq)  # Load HighFreq
# Minutely SPY returns (unit per minute) single day
re_turns <- rutils::diff_it(log(SPY["2012-02-13", 4]))
# Minutely SPY volatility (unit per minute)
sd(re_turns)
# Divide minutely SPY returns by time intervals (unit per second)
re_turns <- re_turns / rutils::diff_it(.index(SPY["2012-02-13"]))
re_turns[1] <- 0
# Minutely SPY volatility scaled to unit per minute
60*sd(re_turns)
# SPY returns multiple days
re_turns <- rutils::diff_it(log(SPY[, 4]))
# Minutely SPY volatility (includes overnight jumps)
sd(re_turns)
# Table of intervals - 60 second is most frequent
in_dex <- rutils::diff_it(.index(SPY))
table(in_dex)
# hist(in_dex)
# SPY returns with overnight scaling (unit per second)
re_turns <- re_turns / in_dex
re_turns[1] <- 0
# Minutely SPY volatility scaled to unit per minute
60*sd(re_turns)
library(HighFreq)  # Load HighFreq
# Minutely OHLC SPY prices aggregated to daily prices
SPY_daily <- rutils::to_period(oh_lc=HighFreq::SPY, period="days")
# Daily SPY volatility from daily returns
sd(rutils::diff_it(log(SPY_daily[, 4])))
# Minutely SPY returns (unit per minute)
re_turns <- rutils::diff_it(log(SPY[, 4]))
# Minutely SPY volatility scaled to daily interval
sqrt(6.5*60)*sd(re_turns)
# Minutely SPY returns with overnight scaling (unit per second)
re_turns <- rutils::diff_it(log(SPY[, 4]))
in_dex <- rutils::diff_it(.index(SPY))
re_turns <- re_turns / in_dex
re_turns[1] <- 0
# Daily SPY volatility from minutely returns
sqrt(6.5*60)*60*sd(re_turns)
# Daily SPY volatility
# Scale by extra time over weekends and holidays
24*60*60*sd(rutils::diff_it(log(SPY_daily[, 4]))[-1] /
    rutils::diff_it(.index(SPY_daily))[-1])
# Calculate SPY returns adjusted for overnight jumps
clos_e <- log(as.numeric(Cl(HighFreq::SPY[, 4])))
re_turns <- rutils::diff_it(clos_e) /
  rutils::diff_it(.index(HighFreq::SPY))
re_turns[1] <- 0
clos_e <- cumsum(re_turns)
n_rows <- NROW(clos_e)
# Calculate volatilities for vector of aggregation intervals
interval_s <- seq.int(from=3, to=35, length.out=9)^2
vol_s <- sapply(interval_s, function(inter_val) {
  num_agg <- n_rows %/% inter_val
  end_p <- c(0, n_rows - num_agg*inter_val + (0:num_agg)*inter_val)
  # end_p <- rutils::calc_endpoints(clos_e, inter_val=inter_val)
  sd(rutils::diff_it(clos_e[end_p]))
})  # end sapply
# Calculate Hurst as regression slope using formula
vol_log <- log(vol_s)
inter_log <- log(interval_s)
hurs_t <- cov(vol_log, inter_log)/var(inter_log)
# Or using function lm()
mod_el <- lm(vol_log ~ inter_log)
coef(mod_el)[2]
# Calculate Hurst from single data point
(last(vol_log) - log(sd(re_turns)))/last(inter_log)
# Plot the volatilities
x11(width=6, height=5)
par(mar=c(4, 4, 2, 1), oma=c(1, 1, 1, 1))
plot(vol_log ~ inter_log, lwd=6, col="red",
     xlab="aggregation intervals (log)", ylab="volatility (log)",
     main="Hurst Exponent for SPY From Volatilities")
abline(mod_el, lwd=3, col="blue")
text(inter_log[2], vol_log[NROW(vol_log)-1],
     paste0("Hurst = ", round(hurs_t, 4)))
# Calculate the rescaled range
inter_val <- 500
n_rows <- NROW(clos_e); num_agg <- n_rows %/% inter_val
end_p <- c(0, n_rows - num_agg*inter_val + (0:num_agg)*inter_val)
# Or
# end_p <- rutils::calc_endpoints(clos_e, inter_val=inter_val)
r_s <- sapply(2:NROW(end_p), function(ep) {
  in_dex <- end_p[ep-1]:end_p[ep]
  diff(range(clos_e[in_dex]))/sd(re_turns[in_dex])
})  # end sapply
mean(r_s)
# Calculate Hurst from single data point
log(mean(r_s))/log(inter_val)
# Calculate rescaled range for vector of aggregation intervals
n_rows <- NROW(clos_e)
r_s <- sapply(interval_s, function(inter_val) {
# Calculate end points
  num_agg <- n_rows %/% inter_val
  end_p <- c(0, n_rows - num_agg*inter_val + (0:num_agg)*inter_val)
# Calculate rescaled ranges
  r_s <- sapply(2:NROW(end_p), function(ep) {
    in_dex <- end_p[ep-1]:end_p[ep]
    diff(range(clos_e[in_dex]))/sd(re_turns[in_dex])
  })  # end sapply
  mean(na.omit(r_s))
})  # end sapply
# Calculate Hurst as regression slope using formula
rs_log <- log(r_s)
inter_log <- log(interval_s)
hurs_t <- cov(rs_log, inter_log)/var(inter_log)
# Or using function lm()
mod_el <- lm(rs_log ~ inter_log)
coef(mod_el)[2]
x11(width=6, height=5)
par(mar=c(4, 4, 2, 1), oma=c(1, 1, 1, 1))
plot(rs_log ~ inter_log, lwd=6, col="red",
     xlab="aggregation intervals (log)",
     ylab="rescaled range (log)",
     main="Rescaled Range Analysis for SPY")
abline(mod_el, lwd=3, col="blue")
text(inter_log[2], rs_log[NROW(rs_log)-1],
     paste0("Hurst = ", round(hurs_t, 4)))
library(HighFreq)  # Load HighFreq
oh_lc <- log(rutils::etf_env$VTI)
# Calculate variance
var_close <- HighFreq::run_variance(oh_lc=oh_lc,
  method="close")
var_yang_zhang <- HighFreq::run_variance(oh_lc=oh_lc)
std_dev <- 24*60*60*sqrt(252*cbind(var_close, var_yang_zhang))
colnames(std_dev) <- c("close std_dev", "Yang-Zhang")
# Plot the time series of volatility
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red")
quantmod::chart_Series(std_dev["2011-07/2011-12"],
  theme=plot_theme, name="Standard Deviations: Close and YZ")
legend("top", legend=colnames(std_dev),
 bg="white", lty=1, lwd=6, inset=0.1, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Plot volatility around 2010 flash crash
quantmod::chart_Series(std_dev["2010-04/2010-06"],
  theme=plot_theme, name="Volatility Around 2010 Flash Crash")
legend("top", legend=colnames(std_dev),
 bg="white", lty=1, lwd=6, inset=0.1, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Plot density of volatility distributions
plot(density(std_dev[, 1]), xlab="", ylab="",
  main="Density of Volatility Distributions",
  xlim=c(-0.05, range(std_dev[, 1])[2]/3), type="l", lwd=2, col="blue")
lines(density(std_dev[, 2]), col='red', lwd=2)
legend("top", legend=c("Close-to-Close", "Yang-Zhang"),
 bg="white", lty=1, lwd=6, inset=0.1, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# ? range volatility estimator has lower standard error ?
c(sd(var_close)/mean(var_close), sd(var_yang_zhang)/mean(var_yang_zhang))
foo <- std_dev[var_close<range(var_close)[2]/3, ]
c(sd(foo[, 1])/mean(foo[, 1]), sd(foo[, 2])/mean(foo[, 2]))
plot(density(foo[, 1]), xlab="", ylab="",
  main="Mixture of Normal Returns",
  xlim=c(-0.05, range(foo[, 1])[2]/2), type="l", lwd=2, col="blue")
lines(density(foo[, 2]), col='red', lwd=2)
oh_lc <- rutils::etf_env$VTI
re_turns <- log((oh_lc[, 2] - oh_lc[, 3]) / (oh_lc[, 2] + oh_lc[, 3]))
foo <- rutils::diff_it(log(oh_lc[, 4]))
plot(as.numeric(foo)^2, as.numeric(re_turns)^2)
bar <- lm(re_turns ~ foo)
summary(bar)
# Perform normality tests
shapiro.test(coredata(re_turns))
tseries::jarque.bera.test(re_turns)
# Fit VTI returns using MASS::fitdistr()
optim_fit <- MASS::fitdistr(re_turns,
            densfun="t", df=2)
optim_fit$estimate; optim_fit$sd
# Calculate moments of standardized returns
sapply(3:4, moments::moment,
  x=(re_turns - mean(re_turns))/sd(re_turns))
# Plot histogram of VTI returns
col_ors <- c("lightgray", "blue", "green", "red")
PerformanceAnalytics::chart.Histogram(re_turns,
  main="", xlim=c(-7, -3), col=col_ors[1:3],
  methods = c("add.density", "add.normal"))
curve(expr=dt((x-optim_fit$estimate[1])/
  optim_fit$estimate[2], df=2)/optim_fit$estimate[2],
type="l", xlab="", ylab="", lwd=2,
col=col_ors[4], add=TRUE)
# Add title and legend
title(main="VTI logarithm of range",
cex.main=1.3, line=-1)
legend("topright", inset=0.05,
  legend=c("density", "normal", "t-distr"),
  lwd=6, lty=1, col=col_ors[2:4], bty="n")
# Calculate VTI range variance partial autocorrelations
pacf(re_turns^2, lag=10, xlab=NA, ylab=NA,
     main="PACF of VTI log range")
quantmod::chart_Series(re_turns^2, name="VTI log of range squared")
# Standard errors of variance estimators using bootstrap
boot_data <- sapply(1:1e2, function(x) {
  # Create random OHLC
  oh_lc <- HighFreq::random_ohlc()
  # Calculate variance estimate
  c(var=var(oh_lc[, 4]),
    yang_zhang=HighFreq::calc_variance(
oh_lc, method="yang_zhang", scal_e=FALSE))
})  # end sapply
# Analyze bootstrapped variance
boot_data <- t(boot_data)
head(boot_data)
colMeans(boot_data)
apply(boot_data, MARGIN=2, sd) /
  colMeans(boot_data)
par(oma=c(1, 1, 1, 1), mar=c(2, 2, 1, 1), mgp=c(0, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
# Close variance estimator partial autocorrelations
pacf(var_close, lag=10, xlab=NA, ylab=NA)
title(main="VTI close variance partial autocorrelations")
# Range variance estimator partial autocorrelations
pacf(var_yang_zhang, lag=10, xlab=NA, ylab=NA)
title(main="VTI YZ variance partial autocorrelations")
# Squared range partial autocorrelations
re_turns <- log(rutils::etf_env$VTI[,2] /
            rutils::etf_env$VTI[,3])
pacf(re_turns^2, lag=10, xlab=NA, ylab=NA)
title(main="VTI squared range partial autocorrelations")
oh_lc <- rutils::etf_env$VTI
# Number of data points
n_rows <- NROW(oh_lc["2018-06/"])
# Define end_p at each point in time
end_p <- 0:n_rows
# Number of data points in look_back interval
look_back <- 22
# start_p are end_p lagged by look_back
start_p <- c(rep_len(0, look_back - 1),
    end_p[1:(NROW(end_p)- look_back + 1)])
head(start_p, 33)
# Number of data points
clos_e <- quantmod::Cl(oh_lc["2018/"])
n_rows <- NROW(clos_e)
# Number of periods between endpoints
n_points <- 21
# Number of n_points that fit over n_rows
n_agg <- n_rows %/% n_points
# If n_rows==n_points*n_agg then whole number
end_p <- (0:n_agg)*n_points
# Stub interval at beginning
end_p <- c(0, n_rows-n_points*n_agg + (0:n_agg)*n_points)
# Else stub interval at end
end_p <- c((0:n_agg)*n_points, n_rows)
# Or use xts::endpoints()
end_p <- xts::endpoints(clos_e, on="months")
# Plot data and endpoints as vertical lines
plot.xts(clos_e, col="blue", lwd=2, xlab="", ylab="",
   main="Prices with Endpoints as Vertical Lines")
addEventLines(xts(rep("endpoint", NROW(end_p)-1), index(clos_e)[end_p]),
        col="red", lwd=2, pos=4)
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- "blue"
quantmod::chart_Series(clos_e, theme=plot_theme,
  name="prices with endpoints as vertical lines")
abline(v=end_p, col="red", lwd=2)
# Number of data points
n_rows <- NROW(rutils::etf_env$VTI["2019/"])
# Number of n_points that fit over n_rows
n_points <- 21
n_agg <- n_rows %/% n_points
# Stub interval at beginning
end_p <- c(0, n_rows-n_points*n_agg + (0:n_agg)*n_points)
# look_back defined as number of data points
look_back <- 252
# start_p are end_p lagged by look_back
start_p <- (end_p - look_back + 1)
start_p <- ifelse(start_p < 0, 0, start_p)
# look_back defined as number of end_p
look_back <- 12
start_p <- c(rep_len(0, look_back-1),
    end_p[1:(NROW(end_p)- look_back + 1)])
# Bind start_p with end_p
cbind(start_p, end_p)
# Number of data points
n_rows <- NROW(rutils::etf_env$VTI["2019/"])
# Number of data points per interval
n_points <- 21
# Number of n_pointss that fit over n_rows
n_agg <- n_rows %/% n_points
# Define end_p with beginning stub
end_p <- c(0, n_rows-n_points*n_agg + (0:n_agg)*n_points)
# Define contiguous start_p
start_p <- c(0, end_p[1:(NROW(end_p)-1)])
# Define exclusive start_p
start_p <- c(0, end_p[1:(NROW(end_p)-1)]+1)
# Extract time series of VTI log prices
clos_e <- log(na.omit(rutils::etf_env$price_s$VTI))
end_p <- 0:NROW(clos_e)  # End points at each point
n_rows <- NROW(end_p)
look_back <- 22  # Number of data points per look-back interval
# start_p are multi-period lag of end_p
start_p <- c(rep_len(0, look_back - 1),
    end_p[1:(n_rows - look_back + 1)])
# Define list of look-back intervals for aggregations over past
look_backs <- lapply(2:n_rows, function(in_dex) {
    start_p[in_dex]:end_p[in_dex]
})  # end lapply
# Define aggregation function
agg_regate <- function(x_ts) c(max=max(x_ts), min=min(x_ts))
# Perform aggregations over look_backs list
agg_s <- sapply(look_backs,
    function(look_back) agg_regate(clos_e[look_back])
)  # end sapply
# Coerce agg_s into matrix and transpose it
if (is.vector(agg_s))
  agg_s <- t(agg_s)
agg_s <- t(agg_s)
# Coerce agg_s into xts series
agg_s <- xts(agg_s, order.by=index(clos_e[end_p]))
library(rutils)  # Load package rutils
# Perform aggregations over look_backs list
agg_s <- lapply(look_backs,
    function(look_back) agg_regate(clos_e[look_back])
)  # end lapply
# rbind list into single xts or matrix
agg_s <- rutils::do_call(rbind, agg_s)
# Convert into xts
agg_s <- xts::xts(agg_s, order.by=index(clos_e))
agg_s <- cbind(agg_s, clos_e)
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green")
x11(width=6, height=5)
quantmod::chart_Series(agg_s, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_s),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# library(rutils)  # Load package rutils
# Define functional for rolling aggregations
roll_agg <- function(x_ts, look_back, FUN, ...) {
# Define end points at every period
  end_p <- 0:NROW(x_ts)
  n_rows <- NROW(end_p)
# Define starting points as lag of end_p
  start_p <- c(rep_len(0, look_back - 1),
    end_p[1:(n_rows- look_back + 1)])
# Perform aggregations over look_backs list
  agg_s <- lapply(2:n_rows, function(in_dex)
    FUN(x_ts[start_p[in_dex]:end_p[in_dex]], ...)
  )  # end lapply
# rbind list into single xts or matrix
  agg_s <- rutils::do_call(rbind, agg_s)
# Coerce agg_s into xts series
  if (!is.xts(agg_s))
    agg_s <- xts(agg_s, order.by=index(x_ts))
  agg_s
}  # end roll_agg
# Define aggregation function
agg_regate <- function(x_ts)
  c(max=max(x_ts), min=min(x_ts))
# Perform aggregations over rolling interval
agg_s <- roll_agg(clos_e, look_back=look_back, FUN=agg_regate)
class(agg_s)
dim(agg_s)
# library(rutils)  # Load package rutils
# Define aggregation function that returns a vector
agg_vector <- function(x_ts)
  c(max=max(x_ts), min=min(x_ts))
# Define aggregation function that returns an xts
agg_xts <- function(x_ts)
  xts(t(c(max=max(x_ts), min=min(x_ts))), order.by=end(x_ts))
# Benchmark the speed of aggregation functions
library(microbenchmark)
summary(microbenchmark(
  agg_vector=roll_agg(clos_e, look_back=look_back, FUN=agg_vector),
  agg_xts=roll_agg(clos_e, look_back=look_back, FUN=agg_xts),
  times=10))[, c(1, 4, 5)]
# library(rutils)  # Load package rutils
# Define aggregation function that returns a single value
agg_regate <- function(x_ts)  max(x_ts)
# Perform aggregations over a rolling interval
agg_s <- xts:::rollapply.xts(clos_e, width=look_back,
              FUN=agg_regate, align="right")
# Perform aggregations over a rolling interval
library(PerformanceAnalytics)  # Load package PerformanceAnalytics
agg_s <- apply.rolling(clos_e, width=look_back, FUN=agg_regate)
# Benchmark the speed of the functionals
library(microbenchmark)
summary(microbenchmark(
  roll_agg=roll_agg(clos_e, look_back=look_back, FUN=max),
  roll_xts=xts:::rollapply.xts(clos_e, width=look_back, FUN=max, align="right"),
  apply_rolling=apply.rolling(clos_e, width=look_back, FUN=max),
  times=10))[, c(1, 4, 5)]
# library(rutils)  # Load package rutils
# Rolling sum using cumsum()
roll_sum <- function(x_ts, look_back) {
  cum_sum <- cumsum(na.omit(x_ts))
  out_put <- cum_sum - rutils::lag_it(x=cum_sum, lagg=look_back)
  out_put[1:look_back, ] <- cum_sum[1:look_back, ]
  colnames(out_put) <- paste0(colnames(x_ts), "_stdev")
  out_put
}  # end roll_sum
agg_s <- roll_sum(clos_e, look_back=look_back)
# Perform rolling aggregations using lapply loop
agg_s <- lapply(2:n_rows, function(in_dex)
    sum(clos_e[start_p[in_dex]:end_p[in_dex]])
)  # end lapply
# rbind list into single xts or matrix
agg_s <- rutils::do_call(rbind, agg_s)
head(agg_s)
tail(agg_s)
# Benchmark the speed of both methods
library(microbenchmark)
summary(microbenchmark(
  roll_sum=roll_sum(clos_e, look_back=look_back),
  s_apply=sapply(look_backs,
    function(look_back) sum(clos_e[look_back])),
  times=10))[, c(1, 4, 5)]
# Extract time series of VTI log prices
clos_e <- log(na.omit(rutils::etf_env$price_s$VTI))
# Calculate EWMA prices using filter()
look_back <- 21
weight_s <- exp(-0.1*1:look_back)
weight_s <- weight_s/sum(weight_s)
filter_ed <- stats::filter(clos_e, filter=weight_s,
                   method="convolution", sides=1)
filter_ed <- as.numeric(filter_ed)
# filter() returns time series of class "ts"
class(filter_ed)
# Filter using compiled C++ function directly
getAnywhere(C_cfilter)
str(stats:::C_cfilter)
filter_fast <- .Call(stats:::C_cfilter, clos_e,
               filter=weight_s, sides=1, circular=FALSE)
all.equal(as.numeric(filter_ed), filter_fast, check.attributes=FALSE)
# Calculate EWMA prices using roll::roll_sum()
weights_rev <- rev(weight_s)
roll_ed <- roll::roll_sum(clos_e, width=look_back, weights=weights_rev, min_obs=1)
all.equal(filter_ed[-(1:look_back)],
    as.numeric(roll_ed)[-(1:look_back)],
    check.attributes=FALSE)
# Benchmark speed of rolling calculations
library(microbenchmark)
summary(microbenchmark(
  filter=filter(clos_e, filter=weight_s, method="convolution", sides=1),
  filter_fast=.Call(stats:::C_cfilter, clos_e, filter=weight_s, sides=1, circular=FALSE),
  cum_sum=cumsum(clos_e),
  roll=roll::roll_sum(clos_e, width=look_back, weights=weights_rev)
  ), times=10)[, c(1, 4, 5)]
# Calculate the rolling maximum and minimum over a vector of data
roll_maxminr <- function(vec_tor, look_back) {
  n_rows <- NROW(vec_tor)
  max_min <- matrix(numeric(2*n_rows), nc=2)
  # Loop over periods
  for (it in 1:n_rows) {
    sub_vec <- vec_tor[max(1, it-look_back+1):it]
    max_min[it, 1] <- max(sub_vec)
    max_min[it, 2] <- min(sub_vec)
  }  # end for
  return(max_min)
}  # end roll_maxminr
max_minr <- roll_maxminr(clos_e, look_back)
max_minr <- xts::xts(max_minr, index(clos_e))
library(TTR)  # Load package TTR
max_min <- cbind(TTR::runMax(x=clos_e, n=look_back),
           TTR::runMin(x=clos_e, n=look_back))
all.equal(max_min[-(1:look_back), ], max_minr[-(1:look_back), ], check.attributes=FALSE)
# Benchmark the speed of TTR::runMax
library(microbenchmark)
summary(microbenchmark(
  pure_r=roll_maxminr(clos_e, look_back),
  ttr=TTR::runMax(clos_e, n=look_back),
  times=10))[, c(1, 4, 5)]
# Benchmark the speed of TTR::runSum
summary(microbenchmark(
  vector_r=cumsum(coredata(clos_e)),
  rutils=rutils::roll_sum(clos_e, look_back=look_back),
  ttr=TTR::runSum(clos_e, n=look_back),
  times=10))[, c(1, 4, 5)]
library(rutils)
# Calculate rolling VTI variance using package roll
library(roll)  # Load roll
re_turns <- na.omit(rutils::etf_env$re_turns[, "VTI"])
look_back <- 22
# Calculate rolling sum using RcppRoll
sum_roll <- roll::roll_sum(re_turns, width=look_back, min_obs=1)
# Calculate rolling sum using rutils
sum_rutils <- rutils::roll_sum(re_turns, look_back=look_back)
all.equal(sum_roll[-(1:look_back), ],
    sum_rutils[-(1:look_back), ], check.attributes=FALSE)
# Benchmark speed of rolling calculations
library(microbenchmark)
summary(microbenchmark(
  cum_sum=cumsum(re_turns),
  roll=roll::roll_sum(re_turns, width=look_back),
  RcppRoll=RcppRoll::roll_sum(re_turns, n=look_back),
  rutils=rutils::roll_sum(re_turns, look_back=look_back),
  times=10))[, c(1, 4, 5)]
library(RcppRoll)  # Load package RcppRoll
# Calculate rolling sum using RcppRoll
sum_roll <- RcppRoll::roll_sum(re_turns, align="right", n=look_back)
# Calculate rolling sum using rutils
sum_rutils <- rutils::roll_sum(re_turns, look_back=look_back)
all.equal(sum_roll, coredata(sum_rutils[-(1:(look_back-1))]),
    check.attributes=FALSE)
# Benchmark speed of rolling calculations
library(microbenchmark)
summary(microbenchmark(
  cum_sum=cumsum(re_turns),
  RcppRoll=RcppRoll::roll_sum(re_turns, n=look_back),
  rutils=rutils::roll_sum(re_turns, look_back=look_back),
  times=10))[, c(1, 4, 5)]
# Calculate EWMA prices using RcppRoll
clos_e <- quantmod::Cl(rutils::etf_env$VTI)
weight_s <- exp(0.1*1:look_back)
prices_ewma <- RcppRoll::roll_mean(clos_e,
align="right", n=look_back, weights=weight_s)
prices_ewma <- cbind(clos_e,
  rbind(coredata(clos_e[1:(look_back-1), ]), prices_ewma))
colnames(prices_ewma) <- c("VTI", "VTI EWMA")
# Plot an interactive dygraph plot
dygraphs::dygraph(prices_ewma)
# Or static plot of EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red")
quantmod::chart_Series(prices_ewma, theme=plot_theme, name="EWMA prices")
legend("top", legend=colnames(prices_ewma),
 bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# library(rutils)  # Load package rutils
library(caTools)  # Load package "caTools"
# Get documentation for package "caTools"
packageDescription("caTools")  # Get short description
help(package="caTools")  # Load help page
data(package="caTools")  # List all datasets in "caTools"
ls("package:caTools")  # List all objects in "caTools"
detach("package:caTools")  # Remove caTools from search path
# Median filter
look_back <- 2
clos_e <- quantmod::Cl(HighFreq::SPY["2012-02-01/2012-04-01"])
med_ian <- runmed(x=clos_e, k=look_back)
# Vector of rolling volatilities
sig_ma <- runsd(x=clos_e, k=look_back,
          endrule="constant", align="center")
# Vector of rolling quantiles
quan_tiles <- runquantile(x=clos_e, k=look_back,
  probs=0.9, endrule="constant", align="center")
# Compile Rcpp functions
Rcpp::sourceCpp(file="/Users/jerzy/Develop/R/Rcpp/roll_maxmin.cpp")
max_minarma <- roll_maxmin(clos_e, look_back)
max_minarma <- xts::xts(max_minr, index(clos_e))
max_min <- cbind(TTR::runMax(x=clos_e, n=look_back),
           TTR::runMin(x=clos_e, n=look_back))
all.equal(max_min[-(1:look_back), ], max_minarma[-(1:look_back), ], check.attributes=FALSE)
# Benchmark the speed of TTR::runMax
library(microbenchmark)
summary(microbenchmark(
  arma=roll_maxmin(clos_e, look_back),
  ttr=TTR::runMax(clos_e, n=look_back),
  times=10))[, c(1, 4, 5)]
# Dygraphs plot with max_min lines
da_ta <- cbind(clos_e, max_minarma)
colnames(da_ta)[2:3] <- c("max", "min")
col_ors <- c("blue", "red", "green")
dygraphs::dygraph(da_ta, main=paste(colnames(clos_e), "max and min lines")) %>%
  dyOptions(colors=col_ors)
# Standard plot with max_min lines
plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
quantmod::chart_Series(da_ta["2008/2009"], theme=plot_theme,
  name=paste(colnames(clos_e), "max and min lines"))
legend(x="topright", title=NULL, legend=colnames(da_ta),
 inset=0.1, cex=0.9, bg="white", bty="n",
 lwd=6, lty=1, col=col_ors)
library(rutils)  # Load package rutils
# Indices of last observations in each hour
end_p <- xts::endpoints(clos_e, on="hours")
head(end_p)
# extract the last observations in each hour
head(clos_e[end_p, ])
# Extract time series of VTI log prices
clos_e <- log(na.omit(rutils::etf_env$price_s$VTI))
# Number of data points
n_rows <- NROW(clos_e)
# Number of data points per interval
look_back <- 22
# Number of look_backs that fit over n_rows
n_agg <- n_rows %/% look_back
# Define end_p with beginning stub
end_p <- c(0, n_rows-look_back*n_agg + (0:n_agg)*look_back)
# Define contiguous start_p
start_p <- c(0, end_p[1:(NROW(end_p)-1)])
# Define list of look-back intervals for aggregations over past
look_backs <- lapply(2:NROW(end_p), function(in_dex) {
    start_p[in_dex]:end_p[in_dex]
})  # end lapply
look_backs[[1]]
look_backs[[2]]
# Perform sapply() loop over look_backs list
agg_s <- sapply(look_backs, function(look_back) {
  x_ts <- clos_e[look_back]
  c(max=max(x_ts), min=min(x_ts))
})  # end sapply
# Coerce agg_s into matrix and transpose it
if (is.vector(agg_s))
  agg_s <- t(agg_s)
agg_s <- t(agg_s)
# Coerce agg_s into xts series
agg_s <- xts(agg_s, order.by=index(clos_e[end_p]))
head(agg_s)
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
quantmod::chart_Series(agg_s, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_s),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# library(rutils)  # Load package rutils
# Perform lapply() loop over look_backs list
agg_s <- lapply(look_backs, function(look_back) {
  x_ts <- clos_e[look_back]
  c(max=max(x_ts), min=min(x_ts))
})  # end lapply
# rbind list into single xts or matrix
agg_s <- rutils::do_call(rbind, agg_s)
# Coerce agg_s into xts series
agg_s <- xts(agg_s, order.by=index(clos_e[end_p]))
head(agg_s)
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
quantmod::chart_Series(agg_s, theme=plot_theme, name="price aggregations")
legend("top", legend=colnames(agg_s),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# library(rutils)  # Load package rutils
# Define functional for rolling aggregations over end_p
roll_agg <- function(x_ts, end_p, FUN, ...) {
  n_rows <- NROW(end_p)
# start_p are single-period lag of end_p
  start_p <- c(1, end_p[1:(n_rows-1)])
# Perform aggregations over look_backs list
  agg_s <- lapply(look_backs,
    function(look_back) FUN(x_ts[look_back], ...))  # end lapply
# rbind list into single xts or matrix
  agg_s <- rutils::do_call(rbind, agg_s)
  if (!is.xts(agg_s))
    agg_s <-  # Coerce agg_s into xts series
    xts(agg_s, order.by=index(x_ts[end_p]))
  agg_s
}  # end roll_agg
# Apply sum() over end_p
agg_s <- roll_agg(clos_e, end_p=end_p, FUN=sum)
agg_s <- period.apply(clos_e, INDEX=end_p, FUN=sum)
# Benchmark the speed of aggregation functions
summary(microbenchmark(
  roll_agg=roll_agg(clos_e, end_p=end_p, FUN=sum),
  period_apply=period.apply(clos_e, INDEX=end_p, FUN=sum),
  times=10))[, c(1, 4, 5)]
agg_s <- period.sum(clos_e, INDEX=end_p)
head(agg_s)
# library(rutils)  # Load package rutils
# Load package HighFreq
library(HighFreq)
# Extract closing minutely prices
clos_e <- quantmod::Cl(rutils::etf_env$VTI["2019"])
# Apply "mean" over daily periods
agg_s <- apply.daily(clos_e, FUN=sum)
head(agg_s)
# Define end_p with beginning stub
n_points <- 5
n_rows <- NROW(clos_e)
n_agg <- n_rows %/% n_points
end_p <- c(0, n_rows-n_points*n_agg + (0:n_agg)*n_points)
# Number of data points in look_back interval
look_back <- 22
# start_p are end_p lagged by look_back
start_p <- (end_p - look_back + 1)
start_p <- ifelse(start_p < 0, 0, start_p)
# Perform lapply() loop over look_backs list
agg_s <- lapply(2:NROW(end_p), function(in_dex) {
x_ts <- clos_e[start_p[in_dex]:end_p[in_dex]]
c(max=max(x_ts), min=min(x_ts))
})  # end lapply
# rbind list into single xts or matrix
agg_s <- rutils::do_call(rbind, agg_s)
# Coerce agg_s into xts series
agg_s <- xts(agg_s, order.by=index(clos_e[end_p]))
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
quantmod::chart_Series(agg_s, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_s),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
agg_s <- cbind(clos_e, agg_s)
tail(agg_s)
agg_s <- na.omit(xts:::na.locf.xts(agg_s))
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green")
quantmod::chart_Series(agg_s, theme=plot_theme, name="price aggregations")
legend("top", legend=colnames(agg_s),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
set.seed(1121)  # Reset random number generator
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # Load package zoo
# Create zoo time series of random returns
date_s <- Sys.Date() + 0:365
zoo_series <- zoo(rnorm(NROW(date_s)), order.by=date_s)
# Create monthly dates
dates_agg <- as.Date(as.yearmon(index(zoo_series)))
# Perform monthly mean aggregation
zoo_agg <- aggregate(zoo_series, by=dates_agg, FUN=mean)
# Merge with original zoo - union of dates
zoo_agg <- cbind(zoo_series, zoo_agg)
# Replace NA's using locf
zoo_agg <- na.locf(zoo_agg, na.rm=FALSE)
# Extract aggregated zoo
zoo_agg <- zoo_agg[index(zoo_series), 2]
# library(rutils)  # Load package rutils
# Plot original and aggregated cumulative returns
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_agg), lwd=2, col="red")
# Add legend
legend("topright", inset=0.05, cex=0.8, bty="n",
 title="Aggregated Prices",
 leg=c("orig prices", "agg prices"),
 lwd=2, bg="white", col=c("black", "red"))
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Perform monthly mean aggregation
zoo_agg <- aggregate(zoo_series, by=dates_agg, FUN=mean)
# Merge with original zoo - union of dates
zoo_agg <- cbind(zoo_series, zoo_agg)
# Replace NA's using linear interpolation
zoo_agg <- na.approx(zoo_agg)
# Extract interpolated zoo
zoo_agg <- zoo_agg[index(zoo_series), 2]
# Plot original and interpolated zoo
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_agg), lwd=2, col="red")
# Add legend
legend("topright", inset=0.05, cex=0.8, title="Interpolated Prices",
 leg=c("orig prices", "interpol prices"), lwd=2, bg="white",
 col=c("black", "red"), bty="n")
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# "mean" aggregation over interval with width=11
zoo_mean <- rollapply(zoo_series, width=11,
                FUN=mean, align="right")
# Merge with original zoo - union of dates
zoo_mean <- cbind(zoo_series, zoo_mean)
# Replace NA's using na.locf
zoo_mean <- na.locf(zoo_mean, na.rm=FALSE, fromLast=TRUE)
# Extract mean zoo
zoo_mean <- zoo_mean[index(zoo_series), 2]
# Plot original and interpolated zoo
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_mean), lwd=2, col="red")
# Add legend
legend("topright", inset=0.05, cex=0.8, title="Mean Prices",
 leg=c("orig prices", "mean prices"), lwd=2, bg="white",
 col=c("black", "red"), bty="n")
