






library(rutils)  # Load package rutils
# Calculate VTI percentage returns
returns <- rutils::etfenv$returns$VTI
returns <- drop(coredata(na.omit(returns)))
nrows <- NROW(returns)
# Mean and standard deviation of returns
c(mean(returns), sd(returns))
# Calculate the smoothing bandwidth as the MAD of returns 10 points apart
returns <- sort(returns)
bwidth <- 10*mad(rutils::diffit(returns, lagg=10))
# Calculate the kernel density
densityv <- sapply(1:nrows, function(i_d) {
  sum(dnorm(returns-returns[i_d], sd=bwidth))
})  # end sapply
madv <- mad(returns)
plot(returns, densityv, xlim=c(-5*madv, 5*madv),
     t="l", col="blue", lwd=3,
     xlab="returns", ylab="density",
     main="Density of VTI Returns")
# Calculate the kernel density using density()
densityv <- density(returns, bw=bwidth)
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
add=TRUE, lwd=2, col="blue")
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("VTI", "Normal"), bty="n",
 lwd=6, bg="white", col=c("red", "blue"))

library(rutils)  # Load package rutils
# Calculate VTI percentage returns
returns <- na.omit(rutils::etfenv$returns$VTI)
# Mean and standard deviation of returns
c(mean(returns), sd(returns))

# Plot histogram
x11(width=6, height=5)
par(mar=c(1, 1, 1, 1), oma=c(2, 2, 2, 0))
madv <- mad(returns)
histp <- hist(returns, breaks=100,
  main="", xlim=c(-5*madv, 5*madv),
  xlab="", ylab="", freq=FALSE)
# Draw kernel density of histogram
lines(density(returns), col="red", lwd=2)
# Add density of normal distribution
curve(expr=dnorm(x, mean=mean(returns), sd=sd(returns)),
add=TRUE, type="l", lwd=2, col="blue")
title(main="VTI Return Distribution", line=0)  # Add title
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("VTI", "Normal"), bty="n",
 lwd=6, bg="white", col=c("red", "blue"))

# Create normal Q-Q plot
qqnorm(returns, ylim=c(-0.1, 0.1), main="VTI Q-Q Plot",
 xlab="Normal Quantiles")
# Fit a line to the normal quantiles
qqline(returns, col="red", lwd=2)
# Perform Shapiro-Wilk test
shapiro.test(returns)

# Boxplot method for formula
boxplot(formula=mpg ~ cyl, data=mtcars,
  main="Mileage by number of cylinders",
  xlab="Cylinders", ylab="Miles per gallon")
# Boxplot method for data frame of EuStockMarkets percentage returns
boxplot(x=diff(log(EuStockMarkets)))

# Calculate VTI percentage returns
returns <- na.omit(rutils::etfenv$returns$VTI)
# Number of observations
nrows <- NROW(returns)
# Mean of VTI returns
mean_rets <- mean(returns)
# Standard deviation of VTI returns
sd_rets <- sd(returns)
# Skewness of VTI returns
nrows/((nrows-1)*(nrows-2))*
  sum(((returns - mean_rets)/sd_rets)^3)
# Kurtosis of VTI returns
nrows*(nrows+1)/((nrows-1)^3)*
  sum(((returns - mean_rets)/sd_rets)^4)
# Random normal returns
returns <- rnorm(nrows, sd=sd_rets)
# Mean and standard deviation of random normal returns
mean_rets <- mean(returns)
sd_rets <- sd(returns)
# Skewness of random normal returns
nrows/((nrows-1)*(nrows-2))*
  sum(((returns - mean_rets)/sd_rets)^3)
# Kurtosis of random normal returns
nrows*(nrows+1)/((nrows-1)^3)*
  sum(((returns - mean_rets)/sd_rets)^4)

# calc_skew() calculates skew of returns
calc_skew <- function(returns) {
  returns <- na.omit(returns)
  sum(((returns - mean(returns))/sd(returns))^3)/NROW(returns)
}  # end calc_skew
# calc_kurt() calculates kurtosis of returns
calc_kurt <- function(returns) {
  returns <- na.omit(returns)
  sum(((returns - mean(returns))/sd(returns))^4)/NROW(returns)
}  # end calc_kurt
# Calculate skew and kurtosis of VTI returns
calc_skew(returns)
calc_kurt(returns)
# calcmom() calculates the moments of returns
calcmom <- function(returns, moment=3) {
  returns <- na.omit(returns)
  sum(((returns - mean(returns))/sd(returns))^moment)/NROW(returns)
}  # end calcmom
# Calculate skew and kurtosis of VTI returns
calcmom(returns, moment=3)
calcmom(returns, moment=4)

set.seed(1121)  # Reset random number generator
# Sample from Standard Normal Distribution
nrows <- 1000
datav <- rnorm(nrows)
# Sample mean
mean(datav)
# Sample standard deviation
sd(datav)
# Standard error of sample mean
sd(datav)/sqrt(nrows)

xvar <- seq(-5, 7, length=100)
yvar <- dnorm(xvar, mean=1.0, sd=2.0)
plot(xvar, yvar, type="l", lty="solid", xlab="", ylab="")
title(main="Normal Density Function", line=0.5)
startd <- 3; fin_ish <- 5  # Set lower and upper bounds
# Plot polygon area
are_a <- ((xvar >= startd) & (xvar <= fin_ish))
polygon(c(startd, xvar[are_a], fin_ish),
  c(-1, yvar[are_a], -1), col="red")

par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
sigmavs <- c(0.5, 1, 1.5, 2)  # Sigma values
# Create plot colors
colors <- c("red", "black", "blue", "green")
# Create legend labels
labelv <- paste("sigma", sigmavs, sep="=")
for (indeks in 1:4) {  # Plot four curves
  curve(expr=dnorm(x, sd=sigmavs[indeks]),
  xlim=c(-4, 4), xlab="", ylab="", lwd=2,
  col=colors[indeks], add=as.logical(indeks-1))
}  # end for
# Add title
title(main="Normal Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, title="Sigmas",
 labelv, cex=0.8, lwd=2, lty=1, bty="n", col=colors)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
degf <- c(3, 6, 9)  # Df values
colors <- c("black", "red", "blue", "green")
labelv <- c("normal", paste("df", degf, sep="="))
# Plot a Normal probability distribution
curve(expr=dnorm, xlim=c(-4, 4), xlab="", ylab="", lwd=2)
for (indeks in 1:3) {  # Plot three t-distributions
  curve(expr=dt(x, df=degf[indeks]), xlab="", ylab="",
lwd=2, col=colors[indeks+1], add=TRUE)
}  # end for

# Add title
title(main="t-distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n",
       title="Degrees\n of freedom", labelv,
       cex=0.8, lwd=6, lty=1, col=colors)

# Mixture of two normal distributions with sd=1 and sd=2
nrows <- 1e5
returns <- c(rnorm(nrows/2), 2*rnorm(nrows/2))
returns <- (returns-mean(returns))/sd(returns)
# Kurtosis of normal
calc_kurt(rnorm(nrows))
# Kurtosis of mixture
calc_kurt(returns)
# Or
nrows*sum(returns^4)/(nrows-1)^2

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot the distributions
plot(density(returns), xlab="", ylab="",
  main="Mixture of Normal Returns",
  xlim=c(-3, 3), type="l", lwd=3, col="red")
curve(expr=dnorm, lwd=2, col="blue", add=TRUE)
curve(expr=dt(x, df=3), lwd=2, col="green", add=TRUE)
# Add legend
legend("topright", inset=0.05, lty=1, lwd=6, bty="n",
  legend=c("Mixture", "Normal", "t-distribution"),
  col=c("red", "blue", "green"))

# Objective function is negative log-likelihood
likefun <- function(par, dfree, data) {
  sum(-log(gamma((dfree+1)/2)/(sqrt(pi*dfree)*gamma(dfree/2))) +
    log(par[2]) + (dfree+1)/2*log(1+((data-par[1])/par[2])^2/dfree))
}  # end likefun
# Demonstrate equivalence with log(dt())
likefun(c(1, 0.5), 2, 2:5)
-sum(log(dt(x=(2:5-1)/0.5, df=2)/0.5))
# Simpler objective function
likefun <- function(par, dfree, data) {
  -sum(log(dt(x=(data-par[1])/par[2], df=dfree)/par[2]))
}  # end likefun

# Calculate VTI percentage returns
returns <- na.omit(rutils::etfenv$returns$VTI)
# Initial parameters
initp <- c(mean=0, scale=0.01)
# Fit distribution using optim()
optim_fit <- optim(par=initp,
  fn=likefun, # Log-likelihood function
  data=returns,
  dfree=2, # Degrees of freedom
  method="L-BFGS-B", # Quasi-Newton method
  upper=c(1, 0.1), # Upper constraint
  lower=c(-1, 1e-7)) # Lower constraint
# Optimal parameters
loc <- optim_fit$par["mean"]
scalev <- optim_fit$par["scale"]
# Fit VTI returns using MASS::fitdistr()
optim_fit <- MASS::fitdistr(returns, densfun="t", df=2)
summary(optim_fit)
# Fitted parameters
optim_fit$estimate
loc <- optim_fit$estimate[1]
scalev <- optim_fit$estimate[2]
# Standard errors of parameters
optim_fit$sd
# Log-likelihood value
optim_fit$value

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot histogram of VTI returns
histp <- hist(returns, col="lightgrey",
  xlab="returns", breaks=100, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE, main="VTI Returns Histogram")
lines(density(returns, adjust=1.5), lwd=3, col="blue")
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(returns),
  sd=sd(returns)), add=TRUE, lwd=3, col="green")
# Plot t-distribution function
curve(expr=dt((x-loc)/scalev, df=2)/scalev,
type="l", lwd=3, col="red", add=TRUE)
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("density", "t-distr", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot histogram of VTI returns
histp <- hist(returns, breaks=100, plot=FALSE)
plot(histp, xlab="returns", ylab="frequency",
     col="lightgrey", freq=FALSE, main="VTI Left Tail Returns Histogram",
     xlim=c(min(returns), -0.02),
     ylim=c(0.0, histp$density[findInterval(-0.02, histp$breaks)]))
lines(density(returns, adjust=1.5), lwd=4, col="blue")
# Plot t-distribution function
curve(expr=dt((x-loc)/scalev, df=2)/scalev, type="l", lwd=4, col="red", add=TRUE)
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(returns), sd=sd(returns)), add=TRUE, lwd=4, col="green")
# Add legend
legend("topleft", inset=0.05, bty="n",
  leg=c("density", "t-distr", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))

# Calculate VTI returns and trading volumes
ohlc <- rutils::etfenv$VTI
closep <- drop(coredata(quantmod::Cl(ohlc)))
returns <- rutils::diffit(log(closep))
volumes <- coredata(quantmod::Vo(ohlc))
# Calculate rolling variance
look_back <- 121
variance <- HighFreq::roll_var_ohlc(log(ohlc), method="close", look_back=look_back, scale=FALSE)
variance[1:look_back, ] <- variance[look_back+1, ]
# Calculate rolling average volume
volume_roll <- HighFreq::roll_vec(volumes, look_back=look_back)/look_back
# dygraph plot of VTI variance and trading volumes
datav <- xts::xts(cbind(variance, volume_roll), zoo::index(ohlc))
colnamev <- c("variance", "volume")
colnames(datav) <- colnamev
dygraphs::dygraph(datav, main="VTI Variance and Trading Volumes") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], strokeWidth=2, axis="y", col="blue") %>%
  dySeries(name=colnamev[2], strokeWidth=2, axis="y2", col="red")

# Scale returns using volume (volume clock)
rets_scaled <- ifelse(volumes > 0, sqrt(volume_roll)*returns/sqrt(volumes), 0)
rets_scaled <- sd(returns)*rets_scaled/sd(rets_scaled)
# rets_scaled <- ifelse(volumes > 1e4, returns/volumes, 0)
# Calculate moments of scaled returns
nrows <- NROW(returns)
sapply(list(returns=returns, rets_scaled=rets_scaled),
  function(rets) {sapply(c(skew=3, kurt=4),
     function(x) sum((rets/sd(rets))^x)/nrows)
})  # end sapply

# x11(width=6, height=5)
dev.new(width=6, height=5, noRStudioGD=TRUE)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot densities of SPY returns
madv <- mad(returns)
# bwidth <- mad(rutils::diffit(returns))
plot(density(returns, bw=madv/10), xlim=c(-5*madv, 5*madv),
     lwd=3, mgp=c(2, 1, 0), col="blue",
     xlab="returns (standardized)", ylab="frequency",
     main="Density of Volume-scaled VTI Returns")
lines(density(rets_scaled, bw=madv/10), lwd=3, col="red")
curve(expr=dnorm(x, mean=mean(returns), sd=sd(returns)),
add=TRUE, lwd=3, col="green")
# Add legend
legend("topright", inset=0.05, bty="n",
  leg=c("unscaled", "scaled", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))
quartz.save("figure/vti_scaled.png", type="png", width=6, height=5)

# Calculate VTI percentage returns
library(rutils)
returns <- na.omit(rutils::etfenv$returns$VTI)
# Reset output digits
ndigits <- options(digits=5)
# Shapiro-Wilk test for normal distribution
shapiro.test(rnorm(nrows))
# Shapiro-Wilk test for VTI returns
shapiro.test(returns)
# Shapiro-Wilk test for uniform distribution
shapiro.test(runif(NROW(returns)))
# Restore output digits
options(digits=ndigits$digits)

library(tseries)  # Load package tseries
# Jarque-Bera test for normal distribution
jarque.bera.test(rnorm(nrows))
# Jarque-Bera test for VTI returns
jarque.bera.test(returns)
# Jarque-Bera test for uniform distribution
jarque.bera.test(runif(NROW(returns)))

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
returns <- as.numeric(na.omit(rutils::etfenv$returns$VTI))
returns <- (returns - mean(returns))/sd(returns)
ks.test(returns, rnorm)
# Fit t-dist into VTI returns
optim_fit <- MASS::fitdistr(returns, densfun="t", df=2)
loc <- optim_fit$estimate[1]
scalev <- optim_fit$estimate[2]
# KS test for VTI returns vs t-distribution
# define non-standard Student’s t-distribution
tdistr <- function(x, dfree, loc, scale) {
  dt((x-loc)/scale, df=dfree)/scale
}  # end likefun
# tdistr(x=returns[1], dfree=2, loc=loc, scale=scalev)
# Plot shows that tdistr() fits returns very well
densityv <- density(returns)
madv <- mad(returns)
plot(densityv, xlim=c(-5*madv, 5*madv),
     xlab="returns", ylab="density",
     col="blue", lwd=2, main="Density of VTI Returns")
curve(expr=tdistr(x, dfree=2, loc=loc, scale=scalev), col="red", lwd=2, add=TRUE)
# Or
tdistr <- function(x, dfree, loc, scale) {
  gamma((dfree+1)/2)/(sqrt(pi*dfree)*gamma(dfree/2)*scale)*
    (1+((x-loc)/scale)^2/dfree)^(-(dfree+1)/2)
}  # end likefun
# tdistr(x=returns[1], dfree=2, loc=loc, scale=scalev)

# Not the same as the below - gives p-value << 0.01 - rejects NULL
ks.test(returns, tdistr, dfree=2, loc=loc, scale=scalev)

# Not the same as the above - gives p-value > 0.02 most of the time - doesn't reject NULL
# KS test for VTI returns vs t-distribution
datav <- loc + scalev*rt(NROW(returns), df=2)
ks.test(returns, datav)

x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Degrees of freedom
degf <- c(2, 5, 8, 11)
# Plot four curves in loop
colors <- c("red", "black", "blue", "green")
for (indeks in 1:4) {
  curve(expr=dchisq(x, df=degf[indeks]),
  xlim=c(0, 20), ylim=c(0, 0.3),
  xlab="", ylab="", col=colors[indeks],
  lwd=2, add=as.logical(indeks-1))
}  # end for

# Add title
title(main="Chi-squared Distributions", line=0.5)
# Add legend
labelv <- paste("df", degf, sep="=")
legend("topright", inset=0.05, bty="n",
       title="Degrees of freedom", labelv,
       cex=0.8, lwd=6, lty=1, col=colors)

# Observed frequencies from random normal data
histp <- hist(rnorm(1e3, mean=0), breaks=100, plot=FALSE)
freq_o <- histp$counts
# Theoretical frequencies
freq_t <- rutils::diffit(pnorm(histp$breaks))
# Perform Chi-squared test for normal data
chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
# Return p-value
chisq_test <- chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
chisq_test$p.value
# Observed frequencies from shifted normal data
histp <- hist(rnorm(1e3, mean=2), breaks=100, plot=FALSE)
freq_o <- histp$counts/sum(histp$counts)
# Theoretical frequencies
freq_t <- rutils::diffit(pnorm(histp$breaks))
# Perform Chi-squared test for shifted normal data
chisq.test(x=freq_o, p=freq_t, rescale.p=TRUE, simulate.p.value=TRUE)
# Calculate histogram of VTI returns
histp <- hist(returns, breaks=100, plot=FALSE)
freq_o <- histp$counts
# Calculate cumulative probabilities and then difference them
freq_t <- pt((histp$breaks-loc)/scalev, df=2)
freq_t <- rutils::diffit(freq_t)
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
returns <- rutils::etfenv$returns[, c("VTI", "DBC", "IEF")]
returns <- na.omit(returns)
# Plot cumulative ETF returns
x11(width=6, height=5)
chart.CumReturns(returns, lwd=2, ylab="",
  legend.loc="topleft", main="ETF Cumulative Returns")

returns <- rutils::etfenv$returns$VTI
returns <- na.omit(returns)
x11(width=6, height=5)
chart.Histogram(returns, xlim=c(-0.04, 0.04),
  colorset = c("lightgray", "red", "blue"), lwd=3,
  main=paste("Distribution of", colnames(returns), "Returns"),
  methods = c("add.density", "add.normal"))
legend("topright", inset=0.05, bty="n",
 leg=c("VTI Density", "Normal"),
 lwd=6, lty=1, col=c("red", "blue"))

returns <- rutils::etfenv$returns[,
  c("VTI", "IEF", "IVW", "VYM", "IWB", "DBC", "VXX")]
x11(width=6, height=5)
chart.Boxplot(names=FALSE, returns)
par(cex.lab=0.8, cex.axis=0.8)
axis(side=2, at=(1:NCOL(returns))/7.5-0.05,labels=colnames(returns))

# Simulate normally distributed data
nrows <- 1000
datav <- rnorm(nrows)
sd(datav)
mad(datav)
median(abs(datav - median(datav)))
median(abs(datav - median(datav)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
boot_data <- sapply(1:10000, function(x) {
  samplev <- datav[sample.int(nrows, replace=TRUE)]
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
boot_data <- parLapply(cluster, 1:10000,
  function(x, datav) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, datav=datav)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:10000, function(x) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, mc.cores=ncores)  # end mclapply
stopCluster(cluster)  # Stop R processes over cluster
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))

# Calculate VTI returns
returns <- rutils::etfenv$returns$VTI
returns <- na.omit(returns)
nrows <- NROW(returns)
sd(returns)
mad(returns)
# Bootstrap of sd and mad estimators
boot_data <- sapply(1:10000, function(x) {
  samplev <- returns[sample.int(nrows, replace=TRUE)]
  c(sd=sd(samplev), mad=mad(samplev))
})  # end sapply
boot_data <- t(boot_data)
# Means and standard errors from bootstrap
100*apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
cluster <- makeCluster(ncores)  # Initialize compute cluster
clusterExport(cluster, c("nrows", "returns"))
boot_data <- parLapply(cluster, 1:10000,
  function(x) {
    samplev <- returns[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  })  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
boot_data <- mclapply(1:10000, function(x) {
    samplev <- returns[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, mc.cores=ncores)  # end mclapply
stopCluster(cluster)  # Stop R processes over cluster
boot_data <- rutils::do_call(rbind, boot_data)
# Means and standard errors from bootstrap
apply(boot_data, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))

library(PerformanceAnalytics)
# Define target rate of return of 50 bps
targetr <- 0.005
# Calculate the full downside returns
returns_sub <- (returns - targetr)
returns_sub <- ifelse(returns_sub < 0, returns_sub, 0)
nrows <- NROW(returns_sub)
# Calculate the downside deviation
all.equal(sqrt(sum(returns_sub^2)/nrows),
  drop(DownsideDeviation(returns, MAR=targetr, method="full")))
# Calculate the subset downside returns
returns_sub <- (returns - targetr)
returns_sub <- returns_sub[returns_sub < 0]
nrows <- NROW(returns_sub)
# Calculate the downside deviation
all.equal(sqrt(sum(returns_sub^2)/nrows),
  DownsideDeviation(returns, MAR=targetr, method="subset"))

# Calculate time series of VTI drawdowns
closep <- log(quantmod::Cl(rutils::etfenv$VTI))
draw_downs <- (closep - cummax(closep))
# Extract the date index from the time series closep
dates <- zoo::index(closep)
# Calculate the maximum drawdown date and depth
index_min <- which.min(draw_downs)
date_min <- dates[index_min]
max_drawdown <- draw_downs[date_min]
# Calculate the drawdown start and end dates
startd <- max(dates[(dates < date_min) & (draw_downs == 0)])
endd <- min(dates[(dates > date_min) & (draw_downs == 0)])
# dygraph plot of VTI drawdowns
datav <- cbind(closep, draw_downs)
colnamev <- c("VTI", "Drawdowns")
colnames(datav) <- colnamev
dygraphs::dygraph(datav, main="VTI Drawdowns") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2],
   valueRange=(1.2*range(draw_downs)+0.1), independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red") %>%
  dyEvent(startd, "start drawdown", col="blue") %>%
  dyEvent(date_min, "max drawdown", col="red") %>%
  dyEvent(endd, "end drawdown", col="green")

# Plot VTI drawdowns using package quantmod
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
x11(width=6, height=5)
quantmod::chart_Series(x=closep, name="VTI Drawdowns", theme=plot_theme)
xval <- match(startd, dates)
yval <- max(closep)
abline(v=xval, col="blue")
text(x=xval, y=0.95*yval, "start drawdown", col="blue", cex=0.9)
xval <- match(date_min, dates)
abline(v=xval, col="red")
text(x=xval, y=0.9*yval, "max drawdown", col="red", cex=0.9)
xval <- match(endd, dates)
abline(v=xval, col="green")
text(x=xval, y=0.85*yval, "end drawdown", col="green", cex=0.9)

library(xtable)
library(PerformanceAnalytics)
closep <- log(quantmod::Cl(rutils::etfenv$VTI))
returns <- rutils::diffit(closep)
# Calculate table of VTI drawdowns
tablev <- PerformanceAnalytics::table.Drawdowns(returns, geometric=FALSE)
# Convert dates to strings
tablev <- cbind(sapply(tablev[, 1:3], as.character), tablev[, 4:7])
# Print table of VTI drawdowns
print(xtable(tablev), comment=FALSE, size="tiny", include.rownames=FALSE)
library(xtable)
library(PerformanceAnalytics)
closep <- log(quantmod::Cl(rutils::etfenv$VTI))
returns <- rutils::diffit(closep)
# Calculate table of VTI drawdowns
tablev <- PerformanceAnalytics::table.Drawdowns(returns, geometric=FALSE)
# Convert dates to strings
tablev <- cbind(sapply(tablev[, 1:3], as.character), tablev[, 4:7])
# Print table of VTI drawdowns
print(xtable(tablev), comment=FALSE, size="tiny", include.rownames=FALSE)

# Load "managers" data set
data(managers)
charts.PerformanceSummary(ham1,
  main="", lwd=2, ylog=TRUE)

# Calculate VTI percentage returns
returns <- na.omit(rutils::etfenv$returns$VTI)
confl <- 0.1
varisk <- quantile(returns, confl)
cvar <- mean(returns[returns < varisk])
# Plot histogram of VTI returns
x11(width=6, height=5)
par(mar=c(3, 2, 1, 0), oma=c(0, 0, 0, 0))
histp <- hist(returns, col="lightgrey",
  xlab="returns", ylab="frequency", breaks=100,
  xlim=c(-0.05, 0.01), freq=FALSE, main="VTI Returns Histogram")
# Calculate density
densv <- density(returns, adjust=1.5)

# Plot density
lines(densv, lwd=3, col="blue")
# Plot line for VaR
abline(v=varisk, col="red", lwd=3)
text(x=varisk, y=25, labels="VaR", lwd=2, pos=2)
# Plot polygon shading for CVaR
text(x=1.5*varisk, y=10, labels="CVaR", lwd=2, pos=2)
var_max <- -0.06
rangev <- (densv$x < varisk) &  (densv$x > var_max)
polygon(c(var_max, densv$x[rangev], varisk),
  c(0, densv$y[rangev], 0), col=rgb(1, 0, 0,0.5), border=NA)

# Calculate VTI percentage returns
returns <- na.omit(rutils::etfenv$returns$VTI)
confl <- 0.05
# Calculate VaR as quantile
varisk <- quantile(returns, probs=confl)
# Or by sorting
sortv <- sort(as.numeric(returns))
indeks <- round(confl*NROW(returns))
varisk <- sortv[indeks]
# PerformanceAnalytics VaR
PerformanceAnalytics::VaR(returns,
  p=(1-confl), method="historical")
all.equal(unname(varisk),
  as.numeric(PerformanceAnalytics::VaR(returns,
  p=(1-confl), method="historical")))

x11(width=6, height=5)
par(mar=c(3, 2, 1, 0), oma=c(0, 0, 0, 0))
# Calculate VaR as quantile
varisk <- quantile(returns, confl)
# Calculate CVaR as expected loss
cvar <- mean(returns[returns < varisk])
# Or by sorting
sortv <- sort(as.numeric(returns))
indeks <- round(confl*NROW(returns))
varisk <- sortv[indeks]
cvar <- mean(sortv[1:indeks])
# PerformanceAnalytics VaR
PerformanceAnalytics::ETL(returns,
  p=(1-confl), method="historical")
all.equal(cvar,
  as.numeric(PerformanceAnalytics::ETL(returns,
  p=(1-confl), method="historical")))

# Calculate the risk-return statistics
risk_ret <-
  PerformanceAnalytics::table.Stats(rutils::etfenv$returns)
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
risk_ret <- rutils::etfenv$riskstats
# Add Sharpe ratio column
risk_ret$Sharpe <- risk_ret$"Arithmetic Mean"/risk_ret$Stdev
# Sort on Sharpe ratio
risk_ret <- risk_ret[order(risk_ret$Sharpe, decreasing=TRUE), ]
# Print data frame
knitr::kable(risk_ret[, c("Sharpe", "Skewness", "Kurtosis")])

# Print data frame
knitr::kable(risk_ret[c("VXX", "SVXY"), c("Sharpe", "Skewness", "Kurtosis")])

# dygraph plot of VXX versus SVXY
prices <- na.omit(rutils::etfenv$prices[, c("VXX", "SVXY")])
prices <- prices["2017/"]
colnamev <- c("VXX", "SVXY")
colnames(prices) <- colnamev
dygraphs::dygraph(prices, main="Prices of VXX and SVXY") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="green") %>%
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
cutoff <- (NROW(risk_ret) %/% 2)
high_skew <- risk_ret$Name[1:cutoff]
low_skew <- risk_ret$Name[(cutoff+1):NROW(risk_ret)]
# Calculate returns and log prices
returns <- rutils::etfenv$returns
returns <- zoo::na.locf(returns, na.rm=FALSE)
returns[is.na(returns)] <- 0
sum(is.na(returns))
high_skew <- rowMeans(returns[, high_skew])
low_skew <- rowMeans(returns[, low_skew])
wealth <- cbind(high_skew, low_skew)
wealth <- xts::xts(wealth, zoo::index(returns))
wealth <- cumsum(wealth)
# dygraph plot of high skew and low skew ETFs
colnamev <- colnames(wealth)
dygraphs::dygraph(wealth, main="Log Wealth of High and Low Skew ETFs") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="green") %>%
  dyLegend(show="always", width=500)

#Below is for S&P500 constituent stocks
# calcmom() calculates the moments of returns
calcmom <- function(returns, moment=3) {
  returns <- na.omit(returns)
  sum(((returns - mean(returns))/sd(returns))^moment)/NROW(returns)
}  # end calcmom
# Calculate skew and kurtosis of VTI returns
calcmom(returns, moment=3)
calcmom(returns, moment=4)
# Load the S&P500 constituent stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
dim(returns)
sum(is.na(returns))
# returns <- returns["2000/"]
skews <- sapply(returns, calcmom, moment=3)
# skews <- sapply(returns, calcmom, moment=4)
# skews <- sapply(returns, sd, na.rm=TRUE)
skews <- sort(skews)
namesv <- names(skews)
nrows <- NROW(namesv)
# Select high skew and low skew ETFs
cutoff <- NROW(risk_ret %/% 2)
low_skew <- namesv[1:cutoff]
high_skew <- namesv[(cutoff+1)/nrows]

# low_skew <- namesv[1:50]
# Calculate returns and log prices
low_skew <- rowMeans(returns[, low_skew], na.rm=TRUE)
low_skew[1] <- 0
high_skew <- rowMeans(returns[, high_skew], na.rm=TRUE)
high_skew[1] <- 0
wealth <- cbind(high_skew, low_skew)
wealth <- xts::xts(wealth, zoo::index(returns))
wealth <- cumsum(wealth)
# dygraph plot of high skew and low skew ETFs
colnamev <- colnames(wealth)
dygraphs::dygraph(wealth, main="Log Wealth of High and Low Skew Stocks") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="green") %>%
  dyLegend(show="always", width=500)


library(PerformanceAnalytics)
returns <- rutils::etfenv$returns[, c("VTI", "IEF")]
returns <- na.omit(returns)
# Calculate the Sharpe ratio
confl <- 0.05
PerformanceAnalytics::SharpeRatio(returns, p=(1-confl),
  method="historical")
# Calculate the Sortino ratio
PerformanceAnalytics::SortinoRatio(returns)
# Calculate the Calmar ratio
PerformanceAnalytics::CalmarRatio(returns)
# Calculate the Dowd ratio
PerformanceAnalytics::SharpeRatio(returns, FUN="VaR",
  p=(1-confl), method="historical")
# Calculate the Dowd ratio from scratch
varisk <- sapply(returns, quantile, probs=confl)
-sapply(returns, mean)/varisk
# Calculate the Conditional Dowd ratio
PerformanceAnalytics::SharpeRatio(returns, FUN="ES",
  p=(1-confl), method="historical")
# Calculate the Conditional Dowd ratio from scratch
cvar <- sapply(returns, function(x) {
  mean(x[x < quantile(x, confl)])
})
-sapply(returns, mean)/cvar

# Calculate VTI percentage returns
returns <- na.omit(rutils::etfenv$returns$VTI)
returns <- drop(zoo::coredata(returns))
nrows <- NROW(returns)
# Calculate compounded VTI returns
holdp <- 252
cumrets <- sqrt(holdp)*sapply(1:nrows, function(x) {
    mean(returns[sample.int(nrows, size=holdp, replace=TRUE)])
})  # end sapply
# Calculate mean, standard deviation, skewness, and kurtosis
datav <- cbind(returns, cumrets)
colnames(datav) <- c("VTI", "Agg")
apply(datav, MARGIN=2, function(x) {
  # Standardize the returns
  meanval <- mean(x); stddev <- sd(x); x <- (x - meanval)/stddev
  c(mean=meanval, stddev=stddev, skew=mean(x^3), kurt=mean(x^4))
})  # end sapply
# Calculate the Sharpe and Dowd ratios
confl <- 0.05
sapply(colnames(datav), function(name) {
  x <- datav[, name]; stddev <- sd(x)
  varisk <- unname(quantile(x, probs=confl))
  cvar <- mean(x[x < varisk])
  ratio <- 1
  if (name == colnames(datav)[2]) {ratio <- holdp}
  sqrt(252/ratio)*mean(x)/c(Sharpe=stddev, Dowd=-varisk, DowdC=-cvar)
})  # end sapply

# Plot the densities of returns
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
plot(density(returns), t="l", lwd=3, col="blue",
     xlab="returns", ylab="density", xlim=c(-0.04, 0.04),
     main="Distribution of Compounded Stock Returns")
lines(density(cumrets), t="l", col="red", lwd=3)
curve(expr=dnorm(x, mean=mean(cumrets), sd=sd(cumrets)), col="green", lwd=3, add=TRUE)
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
closep <- log(na.omit(rutils::etfenv$prices$VTI))
# Inspect the R code of the function filter()
filter
# Calculate EWMA weights
look_back <- 21
weights <- exp(-0.1*1:look_back)
weights <- weights/sum(weights)
# Calculate convolution using filter()
filtered <- filter(closep, filter=weights,
              method="convolution", sides=1)
# filter() returns time series of class "ts"
class(filtered)
# Get information about C_cfilter()
getAnywhere(C_cfilter)
# Filter using C_cfilter() over past values (sides=1).
filter_fast <- .Call(stats:::C_cfilter, closep, filter=weights,
               sides=1, circular=FALSE)
all.equal(as.numeric(filtered), filter_fast, check.attributes=FALSE)
# Calculate EWMA prices using roll::roll_sum()
weights_rev <- rev(weights)
roll_ed <- roll::roll_sum(closep, width=look_back, weights=weights_rev, min_obs=1)
all.equal(filter_fast[-(1:look_back)], as.numeric(roll_ed)[-(1:look_back)])
# Benchmark speed of rolling calculations
library(microbenchmark)
summary(microbenchmark(
  filter=filter(closep, filter=weights, method="convolution", sides=1),
  filter_fast=.Call(stats:::C_cfilter, closep, filter=weights, sides=1, circular=FALSE),
  roll=roll::roll_sum(closep, width=look_back, weights=weights_rev)
  ), times=10)[, c(1, 4, 5)]

# Simulate AR process using filter()
nrows <- NROW(closep)
# Calculate ARIMA coefficients and innovations
coeff <- weights/4
n_coeff <- NROW(coeff)
innov <- rnorm(nrows)
arimav <- filter(x=innov, filter=coeff, method="recursive")
# Get information about C_rfilter()
getAnywhere(C_rfilter)
# Filter using C_rfilter() compiled C++ function directly
arima_fast <- .Call(stats:::C_rfilter, innov, coeff,
              double(n_coeff + nrows))
all.equal(as.numeric(arimav), arima_fast[-(1:n_coeff)],
    check.attributes=FALSE)
# Filter using C++ code
arima_fastest <- HighFreq::sim_arima(innov, rev(coeff))
all.equal(arima_fast[-(1:n_coeff)], drop(arima_fastest))
# Benchmark speed of the three methods
summary(microbenchmark(
  filter=filter(x=innov, filter=coeff, method="recursive"),
  filter_fast=.Call(stats:::C_rfilter, innov, coeff, double(n_coeff + nrows)),
  Rcpp=HighFreq::sim_arima(innov, rev(coeff))
  ), times=10)[, c(1, 4, 5)]

# Calculate trailing EWMA prices using roll::roll_sum()
look_back <- 21
weights <- exp(-0.1*1:look_back)
weights <- weights/sum(weights)
weights_rev <- rev(weights)
filtered <- roll::roll_sum(closep, width=NROW(weights), weights=weights_rev)
# Copy warmup period
filtered[1:look_back] <- closep[1:look_back]
# Combine prices with smoothed prices
prices <- cbind(closep, filtered)
colnames(prices)[2] <- "VTI Smooth"
# Calculate standard deviations of returns
sapply(rutils::diffit(prices), sd)
# Plot dygraph
dygraphs::dygraph(prices["2009"], main="VTI Prices and Trailing Smoothed Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)

# Calculate centered EWMA prices using roll::roll_sum()
weights <- c(weights_rev, weights[-1])
weights <- weights/sum(weights)
filtered <- roll::roll_sum(closep, width=NROW(weights), weights=weights, online=FALSE)
# Copy warmup period
filtered[1:(2*look_back)] <- closep[1:(2*look_back)]
# Center the data
filtered <- rutils::lagit(filtered, -(look_back-1), pad_zeros=FALSE)
# Combine prices with smoothed prices
prices <- cbind(closep, filtered)
colnames(prices)[2] <- "VTI Smooth"
# Calculate standard deviations of returns
sapply(rutils::diffit(prices), sd)
# Plot dygraph
dygraphs::dygraph(prices["2009"], main="VTI Prices and Centered Smoothed Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)

library(rutils)  # Load package rutils
library(ggplot2)  # Load ggplot2
library(gridExtra)  # Load gridExtra
# Coerce to zoo and merge the time series
filtered <- cbind(closep, filtered)
colnames(filtered) <- c("VTI", "VTI filtered")
# Plot ggplot2
autoplot(filtered["2008/2010"],
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
rutils::plot_acf(returns[, 1], lag=10, xlab="")
title(main="ACF of VTI Returns", line=-1)
# Plot ACF of smoothed VTI returns
rutils::plot_acf(returns[, 2], lag=10, xlab="")
title(main="ACF of Smoothed VTI Returns", line=-1)

# Get close prices and calculate close-to-close returns
# closep <- quantmod::Cl(rutils::etfenv$VTI)
closep <- quantmod::Cl(HighFreq::SPY)
colnames(closep) <- rutils::get_name(colnames(closep))
returns <- TTR::ROC(closep)
returns[1] <- 0
# Calculate the RSI indicator
r_si <- TTR::RSI(closep, 2)
# Calculate the long (up) and short (dn) signals
sig_up <- ifelse(r_si < 10, 1, 0)
sig_dn <- ifelse(r_si > 90, -1, 0)
# Lag signals by one period
sig_up <- rutils::lagit(sig_up, 1)
sig_dn <- rutils::lagit(sig_dn, 1)
# Replace NA signals with zero position
sig_up[is.na(sig_up)] <- 0
sig_dn[is.na(sig_dn)] <- 0
# Combine up and down signals into one
sig_nals <- sig_up + sig_dn
# Calculate cumulative returns
eq_up <- exp(cumsum(sig_up*returns))
eq_dn <- exp(cumsum(-1*sig_dn*returns))
eq_all <- exp(cumsum(sig_nals*returns))

# Plot daily cumulative returns in panels
endp <- endpoints(returns, on="days")
plot.zoo(cbind(eq_all, eq_up, eq_dn)[endp], lwd=c(2, 2, 2),
  ylab=c("Total","Long","Short"), col=c("red","green","blue"),
  main=paste("RSI(2) strategy for", colnames(closep), "from",
       format(start(returns), "%B %Y"), "to",
       format(end(returns), "%B %Y")))

# Extract log VTI prices
ohlc <- log(rutils::etfenv$VTI)
closep <- quantmod::Cl(ohlc)
colnames(closep) <- "VTI"
nrows <- NROW(closep)
# Calculate EWMA weights
look_back <- 333
lambda <- 0.9
weights <- lambda^(1:look_back)
weights <- weights/sum(weights)
# Calculate EWMA prices as the convolution
ewmap <- HighFreq::roll_wsum(closep, weights=weights)
prices <- cbind(closep, ewmap)
colnames(prices) <- c("VTI", "VTI EWMA")

# Dygraphs plot with custom line colors
colnamev <- colnames(prices)
dygraphs::dygraph(prices["2009"], main="VTI EWMA Prices") %>%
  dySeries(name=colnamev[1], label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=500)
# Standard plot of  EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
colors <- c("blue", "red")
plot_theme$col$line.col <- colors
quantmod::chart_Series(prices["2009"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
legend("topleft", legend=colnames(prices),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")

# Calculate EWMA prices recursively using C++ code
ewma_rfilter <- .Call(stats:::C_rfilter, closep, lambda, c(as.numeric(closep[1])/(1-lambda), double(NROW(closep))))[-1]
# Or R code
# ewma_rfilter <- filter(closep, filter=lambda, init=as.numeric(closep[1, 1])/(1-lambda), method="recursive")
ewma_rfilter <- (1-lambda)*ewma_rfilter
# Calculate EWMA prices recursively using RcppArmadillo
ewmap <- HighFreq::run_mean(closep, lambda=lambda)
all.equal(drop(ewmap), ewma_rfilter)
# Compare the speed of C++ code with RcppArmadillo
library(microbenchmark)
summary(microbenchmark(
  run_mean=HighFreq::run_mean(closep, lambda=lambda),
  rfilter=.Call(stats:::C_rfilter, closep, lambda, c(as.numeric(closep[1])/(1-lambda), double(NROW(closep)))),
  times=10))[, c(1, 4, 5)]

# Dygraphs plot with custom line colors
prices <- cbind(closep, ewmap)
colnames(prices) <- c("VTI", "VTI EWMA")
colnamev <- colnames(prices)
dygraphs::dygraph(prices["2009"], main="Recursive VTI EWMA Prices") %>%
  dySeries(name=colnamev[1], label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=500)
# Standard plot of  EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
colors <- c("blue", "red")
plot_theme$col$line.col <- colors
quantmod::chart_Series(prices["2009"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
legend("topleft", legend=colnames(prices),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")

# Calculate log OHLC prices and volumes
ohlc <- rutils::etfenv$VTI
closep <- log(quantmod::Cl(ohlc))
colnames(closep) <- "VTI"
volumes <- quantmod::Vo(ohlc)
colnames(volumes) <- "Volume"
nrows <- NROW(closep)
# Calculate the VWAP prices
look_back <- 21
vwap <- roll::roll_sum(closep*volumes, width=look_back, min_obs=1)
volume_roll <- roll::roll_sum(volumes, width=look_back, min_obs=1)
vwap <- vwap/volume_roll
colnames(vwap) <- "VWAP"
prices <- cbind(closep, vwap)

# Dygraphs plot with custom line colors
colors <- c("blue", "red")
dygraphs::dygraph(prices["2009"], main="VTI VWAP Prices") %>%
  dyOptions(colors=colors, strokeWidth=2)
# Plot VWAP prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(prices["2009"], theme=plot_theme,
       lwd=2, name="VTI VWAP Prices")
legend("bottomright", legend=colnames(prices),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")

# Calculate VWAP prices recursively using C++ code
volume_rec <- .Call(stats:::C_rfilter, volumes, lambda, c(as.numeric(volumes[1])/(1-lambda), double(NROW(volumes))))[-1]
price_rec <- .Call(stats:::C_rfilter, volumes*closep, lambda, c(as.numeric(volumes[1]*closep[1])/(1-lambda), double(NROW(closep))))[-1]
vwap_rec <- price_rec/volume_rec
# Calculate VWAP prices recursively using RcppArmadillo
vwap_arma <- HighFreq::run_mean(closep, lambda=lambda, weights=volumes)
all.equal(vwap_rec, drop(vwap_arma))
# Dygraphs plot the VWAP prices
prices <- xts(cbind(vwap, vwap_arma), zoo::index(ohlc))
colnames(prices) <- c("VWAP rolling", "VWAP running")
dygraphs::dygraph(prices["2009"], main="VWAP Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Calculate two EWMA prices
look_back <- 21
lambda <- 0.1
weights <- exp(lambda*1:look_back)
weights <- weights/sum(weights)
ewma_fast <- roll::roll_sum(closep, width=look_back, weights=weights, min_obs=1)
lambda <- 0.05
weights <- exp(lambda*1:look_back)
weights <- weights/sum(weights)
ewma_slow <- roll::roll_sum(closep, width=look_back, weights=weights, min_obs=1)

# Calculate VTI returns
returns <- (ewma_fast - ewma_slow)
prices <- cbind(closep, returns)
colnames(prices) <- c(symbol, paste(symbol, "Returns"))
# Plot dygraph of VTI Returns
colnamev <- colnames(prices)
dygraphs::dygraph(prices["2009"], main=paste(symbol, "EWMA Returns")) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")

# Calculate fractional weights
del_ta <- 0.1
weights <- (del_ta - 0:(look_back-2)) / 1:(look_back-1)
weights <- (-1)^(1:(look_back-1))*cumprod(weights)
weights <- c(1, weights)
weights <- (weights - mean(weights))
weights <- rev(weights)
# Calculate fractional VTI returns
returns <- roll::roll_sum(closep, width=look_back, weights=weights, min_obs=1, online=FALSE)
prices <- cbind(closep, returns)
colnames(prices) <- c(symbol, paste(symbol, "Returns"))
# Plot dygraph of VTI Returns
colnamev <- colnames(prices)
dygraphs::dygraph(prices["2009"], main=paste(symbol, "Fractional Returns")) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")

# Calculate VTI log returns
closep <- log(quantmod::Cl(rutils::etfenv$VTI))
returns <- rutils::diffit(closep)
# Perform ADF test for prices
tseries::adf.test(closep)
# Perform ADF test for returns
tseries::adf.test(returns)

# Calculate fractional VTI returns
delta_s <- 0.1*c(1, 3, 5, 7, 9)
returns <- lapply(delta_s, function(del_ta) {
  weights <- (del_ta - 0:(look_back-2)) / 1:(look_back-1)
  weights <- c(1, (-1)^(1:(look_back-1))*cumprod(weights))
  weights <- rev(weights - mean(weights))
  roll::roll_sum(closep, width=look_back, weights=weights, min_obs=1, online=FALSE)
})  # end lapply
returns <- do.call(cbind, returns)
returns <- cbind(closep, returns)
colnames(returns) <- c("VTI", paste0("frac_", delta_s))
# Calculate ADF test statistics
adfstats <- sapply(returns, function(x)
  suppressWarnings(tseries::adf.test(x)$statistic)
)  # end sapply
names(adfstats) <- colnames(returns)

# Plot dygraph of fractional VTI returns
colorv <- colorRampPalette(c("blue", "red"))(NCOL(returns))
colnamev <- colnames(returns)
dyplot <- dygraphs::dygraph(returns["2019"], main="Fractional Returns") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col=colorv[1])
for (i in 2:NROW(colnamev))
  dyplot <- dyplot %>%
  dyAxis("y2", label=colnamev[i], independentTicks=TRUE) %>%
  dySeries(name=colnamev[i], axis="y2", label=colnamev[i], strokeWidth=2, col=colorv[i])
dyplot <- dyplot %>% dyLegend(width=500)
dyplot

# Calculate volume z-scores
volumes <- quantmod::Vo(rutils::etfenv$VTI)
look_back <- 21
volume_mean <- roll::roll_mean(volumes, width=look_back, min_obs=1)
volume_sd <- roll::roll_sd(rutils::diffit(volumes), width=look_back, min_obs=1)
volume_sd[1] <- 0
volume_scores <- ifelse(volume_sd > 0, (volumes - volume_mean)/volume_sd, 0)
# Plot histogram of volume z-scores
x11(width=6, height=5)
hist(volume_scores, breaks=1e2)

# Plot dygraph of volume z-scores of VTI prices
prices <- cbind(closep, volume_scores)
colnames(prices) <- c("VTI", "Z-scores")
colnamev <- colnames(prices)
dygraphs::dygraph(prices["2009"], main="VTI Volume Z-Scores") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")

# Extract VTI log OHLC prices
ohlc <- log(rutils::etfenv$VTI)
# Calculate volatility z-scores
volat <- quantmod::Hi(ohlc)-quantmod::Lo(ohlc)
look_back <- 21
volat_mean <- roll::roll_mean(volat, width=look_back, min_obs=1)
volat_sd <- roll::roll_sd(rutils::diffit(volat), width=look_back, min_obs=1)
volat_sd[1] <- 0
volat_scores <- ifelse(volat_sd > 0, (volat - volat_mean)/volat_sd, 0)
# Plot histogram of volatility z-scores
x11(width=6, height=5)
hist(volat_scores, breaks=1e2)
# Plot scatterplot of volume and volatility z-scores
plot(as.numeric(volat_scores), as.numeric(volume_scores),
     xlab="volatility z-score", ylab="volume z-score")

# Plot dygraph of VTI volatility z-scores
closep <- quantmod::Cl(ohlc)
prices <- cbind(closep, volat_scores)
colnames(prices) <- c("VTI", "Z-scores")
colnamev <- colnames(prices)
dygraphs::dygraph(prices["2009"], main="VTI Volatility Z-Scores") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")

# Calculate the centered volatility
look_back <- 21
half_back <- look_back %/% 2
returns <- rutils::diffit(closep)
volat <- roll::roll_sd(returns, width=look_back, min_obs=1)
volat <- rutils::lagit(volat, lagg=(-half_back))
# Calculate the z-scores of prices
pricescores <- (2*closep -
  rutils::lagit(closep, half_back, pad_zeros=FALSE) -
  rutils::lagit(closep, -half_back, pad_zeros=FALSE))
pricescores <- ifelse(volat > 0, pricescores/volat, 0)

# Plot dygraph of z-scores of VTI prices
prices <- cbind(closep, pricescores)
colnames(prices) <- c("VTI", "Z-scores")
colnamev <- colnames(prices)
dygraphs::dygraph(prices["2009"], main="VTI Price Z-Scores") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")

# Calculate thresholds for labeling tops and bottoms
threshv <- quantile(pricescores, c(0.1, 0.9))
# Calculate the vectors of tops and bottoms
tops <- (pricescores > threshv[2])
colnames(tops) <- "tops"
bottoms <- (pricescores < threshv[1])
colnames(bottoms) <- "bottoms"
# Backtest in-sample VTI strategy
position_s <- rep(NA_integer_, NROW(returns))
position_s[1] <- 0
position_s[tops] <- (-1)
position_s[bottoms] <- 1
position_s <- zoo::na.locf(position_s)
position_s <- rutils::lagit(position_s)
pnls <- cumsum(returns*position_s)

# Plot dygraph of in-sample VTI strategy
prices <- cbind(closep, pnls)
colnames(prices) <- c("VTI", "Strategy")
colnamev <- colnames(prices)
dygraphs::dygraph(prices, main="VTI Strategy Using In-sample Labels") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")

# Calculate trailing price z-scores
dates <- matrix(as.numeric(zoo::index(closep)))
look_back <- 21
pricescores <- drop(HighFreq::roll_zscores(response=closep, design=dates, look_back=look_back))
pricescores[1:look_back] <- 0

# Plot dygraph of z-scores of VTI prices
prices <- cbind(closep, pricescores)
colnames(prices) <- c("VTI", "Z-scores")
colnamev <- colnames(prices)
dygraphs::dygraph(prices["2009"], main="VTI Price Z-Scores") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")

# Extract time series of VTI log prices
closep <- log(na.omit(rutils::etfenv$prices$VTI))
# Define look-back window
look_back <- 11
# Calculate time series of medians
medi_an <- roll::roll_median(closep, width=look_back)
# medi_an <- TTR::runMedian(closep, n=look_back)
# Calculate time series of MAD
madv <- HighFreq::roll_var(closep, look_back=look_back, method="nonparametric")
# madv <- TTR::runMAD(closep, n=look_back)
# Calculate time series of z-scores
zscores <- (closep - medi_an)/madv
zscores[1:look_back, ] <- 0
tail(zscores, look_back)
range(zscores)

x11(width=6, height=5)
# Plot prices and medians
dygraphs::dygraph(cbind(closep, medi_an), main="VTI median") %>%
  dyOptions(colors=c("black", "red"))
# Plot histogram of z-scores
histp <- hist(zscores, col="lightgrey",
  xlab="z-scores", breaks=50, xlim=c(-4, 4),
  ylab="frequency", freq=FALSE, main="Hampel Z-scores histogram")
lines(density(zscores, adjust=1.5), lwd=3, col="blue")

# Calculate one-sided Hampel z-scores
medi_an <- roll::roll_median(closep, width=look_back)
# medi_an <- TTR::runMedian(closep, n=look_back)
madv <- HighFreq::roll_var(closep, look_back=look_back, method="nonparametric")
# madv <- TTR::runMAD(closep, n=look_back)
zscores <- (closep - medi_an)/madv
zscores[1:look_back, ] <- 0
tail(zscores, look_back)
range(zscores)
# Calculate two-sided Hampel z-scores
half_back <- look_back %/% 2
medi_an <- rutils::lagit(medi_an, lagg=-half_back)
madv <- rutils::lagit(madv, lagg=-half_back)
zscores <- (closep - medi_an)/madv
zscores[1:look_back, ] <- 0
tail(zscores, look_back)
range(zscores)

# Calculate EWMA VTI variance using compiled C++ function
look_back <- 51
weights <- exp(-0.1*1:look_back)
weights <- weights/sum(weights)
variance <- .Call(stats:::C_cfilter, returns^2,
  filter=weights, sides=1, circular=FALSE)
variance[1:(look_back-1)] <- variance[look_back]
# Plot EWMA volatility
variance <- xts:::xts(sqrt(variance), order.by=zoo::index(returns))
dygraphs::dygraph(variance, main="VTI EWMA Volatility")
quantmod::chart_Series(xtes, name="VTI EWMA Volatility")

# Calculate VTI percentage returns
returns <- na.omit(rutils::etfenv$returns$VTI)
nrows <- NROW(returns)
# Define end points
endp <- 1:NROW(returns)
# Start points are multi-period lag of endp
look_back <- 11
startp <- c(rep_len(0, look_back-1), endp[1:(nrows-look_back+1)])
# Calculate rolling variance in sapply() loop - takes long
variance <- sapply(1:nrows, function(indeks) {
  ret_s <- returns[startp[indeks]:endp[indeks]]
  sum((ret_s - mean(ret_s))^2)
}) / (look_back-1)  # end sapply
# Use only vectorized functions
cumrets <- cumsum(returns)
cumrets <- (cumrets -
  c(rep_len(0, look_back), cumrets[1:(nrows-look_back)]))
cumrets2 <- cumsum(returns^2)
cumrets2 <- (cumrets2 -
  c(rep_len(0, look_back), cumrets2[1:(nrows-look_back)]))
variance2 <- (cumrets2 - cumrets^2/look_back)/(look_back-1)
all.equal(variance[-(1:look_back)], as.numeric(variance2)[-(1:look_back)])
# Same, using package rutils
cumrets <- rutils::roll_sum(returns, look_back=look_back, min_obs=1)
cumrets2 <- rutils::roll_sum(returns^2, look_back=look_back, min_obs=1)
variance2 <- (cumrets2 - cumrets^2/look_back)/(look_back-1)
# Coerce variance into xts
tail(variance)
class(variance)
variance <- xts(variance, order.by=zoo::index(returns))
colnames(variance) <- "VTI.variance"
head(variance)

# Calculate rolling VTI variance using package roll
library(roll)  # Load roll
variance <- roll::roll_var(returns, width=look_back)
colnames(variance) <- "VTI.variance"
head(variance)
sum(is.na(variance))
variance[1:(look_back-1)] <- 0
# Benchmark calculation of rolling variance
library(microbenchmark)
summary(microbenchmark(
  roll_sapply=sapply(2:nrows, function(indeks) {
    ret_s <- returns[startp[indeks]:endp[indeks]]
    sum((ret_s - mean(ret_s))^2)
  }),
  ro_ll=roll::roll_var(returns, width=look_back),
  times=10))[, c(1, 4, 5)]

# Calculate EWMA VTI variance using compiled C++ function
look_back <- 51
weights <- exp(-0.1*1:look_back)
weights <- weights/sum(weights)
variance <- .Call(stats:::C_cfilter, returns^2,
  filter=weights, sides=1, circular=FALSE)
variance[1:(look_back-1)] <- variance[look_back]
# Plot EWMA volatility
variance <- xts:::xts(sqrt(variance), order.by=zoo::index(returns))
dygraphs::dygraph(variance, main="VTI EWMA Volatility") %>%
  dyOptions(colors="blue")
quantmod::chart_Series(xtes, name="VTI EWMA Volatility")

# Calculate rolling VTI variance using package roll
library(roll)  # Load roll
variance <- roll::roll_var(returns,
  weights=rev(weights), width=look_back)
colnames(variance) <- "VTI.variance"
class(variance)
head(variance)
sum(is.na(variance))
variance[1:(look_back-1)] <- 0

library(HighFreq)  # Load HighFreq
# Minutely SPY returns (unit per minute) single day
# Minutely SPY volatility (unit per minute)
returns <- rutils::diffit(log(SPY["2012-02-13", 4]))
sd(returns)
# SPY returns multiple days (includes overnight jumps)
returns <- rutils::diffit(log(SPY[, 4]))
sd(returns)
# Table of time intervals - 60 second is most frequent
indeks <- rutils::diffit(.zoo::index(SPY))
table(indeks)
# SPY returns divided by the overnight time intervals (unit per second)
returns <- returns / indeks
returns[1] <- 0
# Minutely SPY volatility scaled to unit per minute
60*sd(returns)

library(HighFreq)  # Load HighFreq
spy <- HighFreq::SPY["2009"]
# Calculate daily SPY volatility using package HighFreq
sqrt(6.5*60*HighFreq::calcvar_ohlc(log(spy),
  method="yang_zhang"))
# Calculate daily SPY volatility from minutely prices using package TTR
sqrt((6.5*60)*mean(na.omit(
  TTR::volatility(spy, N=1, calc="yang.zhang"))^2))
# Calculate rolling SPY variance using package HighFreq
variance <- HighFreq::roll_var_ohlc(log(spy), method="yang_zhang",
  look_back=look_back)
# Plot range volatility
variance <- xts:::xts(sqrt(variance), order.by=zoo::index(spy))
dygraphs::dygraph(variance["2009-02"],
  main="SPY Rolling Range Volatility") %>%
  dyOptions(colors="blue")
# Benchmark the speed of HighFreq vs TTR
library(microbenchmark)
summary(microbenchmark(
  ttr=TTR::volatility(rutils::etfenv$VTI, N=1, calc="yang.zhang"),
  highfreq=HighFreq::calcvar_ohlc(log(rutils::etfenv$VTI), method="yang_zhang"),
  times=2))[, c(1, 4, 5)]

# Calculate VXX log prices
vxx <- na.omit(rutils::etfenv$prices$VXX)
dates <- zoo::index(vxx)
look_back <- 41
vxx <- log(vxx)
# Calculate rolling VTI volatility
closep <- get("VTI", rutils::etfenv)[dates]
closep <- log(closep)
volat <- sqrt(HighFreq::roll_var_ohlc(ohlc=closep, look_back=look_back, scalev=FALSE))
volat[1:look_back] <- volat[look_back+1]

# Plot dygraph of VXX and VTI volatility
datav <- cbind(vxx, volat)
colnames(datav)[2] <- "VTI Volatility"
colnamev <- colnames(datav)
cap_tion <- "VXX and VTI Volatility"
dygraphs::dygraph(datav[, 1:2], main=cap_tion) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red")

# Calculate VXX log prices
vxx <- na.omit(rutils::etfenv$prices$VXX)
dates <- zoo::index(vxx)
look_back <- 41
vxx <- log(vxx)
vxx <- (vxx - roll::roll_mean(vxx, width=look_back))
vxx[1:look_back] <- vxx[look_back+1]
# Calculate rolling VTI volatility
closep <- get("VTI", rutils::etfenv)[dates]
closep <- log(closep)
volat <- sqrt(HighFreq::roll_var_ohlc(ohlc=closep, look_back=look_back, scalev=FALSE))
volat[1:look_back] <- volat[look_back+1]
# Calculate regression coefficients of XLB ~ XLE
betav <- drop(cov(vxx, volat)/var(volat))
alpha <- drop(mean(vxx) - betav*mean(volat))
# Calculate regression residuals
fittedv <- (alpha + betav*volat)
residuals <- (vxx - fittedv)
# Perform ADF test on residuals
tseries::adf.test(residuals, k=1)

# Plot dygraph of VXX and VTI volatility
datav <- cbind(vxx, volat)
colnamev <- colnames(datav)
cap_tion <- "VXX and VTI Volatility"
dygraphs::dygraph(datav[, 1:2], main=cap_tion) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red")

x11(width=6, height=5)
par(mar=c(4, 3, 1, 1), oma=c(0, 0, 0, 0))
# Calculate VTI percentage returns
returns <- na.omit(rutils::etfenv$returns$VTI)
# Calculate rolling VTI variance using package roll
look_back <- 22
variance <- roll::roll_var(returns, width=look_back)
variance[1:(look_back-1)] <- 0
colnames(variance) <- "VTI.variance"
# Number of look_backs that fit over returns
nrows <- NROW(returns)
nagg <- nrows %/% look_back
# Define endp with beginning stub
endp <- c(0, nrows-look_back*nagg + (0:nagg)*look_back)
nrows <- NROW(endp)
# Subset variance to endp
variance <- variance[endp]
# Plot autocorrelation function
rutils::plot_acf(variance, lag=10, main="ACF of Variance")
# Plot partial autocorrelation
pacf(variance, lag=10, main="PACF of Variance", ylab=NA)

# Define GARCH parameters
alpha <- 0.3; betav <- 0.5;
om_ega <- 1e-4*(1-alpha-betav)
nrows <- 1000
# Calculate matrix of standard normal innovations
set.seed(1121)  # Reset random numbers
innov <- rnorm(nrows)
returns <- numeric(nrows)
variance <- numeric(nrows)
variance[1] <- om_ega/(1-alpha-betav)
returns[1] <- sqrt(variance[1])*innov[1]
# Simulate GARCH model
for (i in 2:nrows) {
  returns[i] <- sqrt(variance[i-1])*innov[i]
  variance[i] <- om_ega + alpha*returns[i]^2 +
    betav*variance[i-1]
}  # end for
# Simulate the GARCH process using Rcpp
garch_data <- HighFreq::sim_garch(omega=om_ega, alpha=alpha,
  beta=betav, innov=matrix(innov))
all.equal(garch_data, cbind(returns, variance),
  check.attributes=FALSE)

# Define GARCH parameters
alpha <- 0.3; betav <- 0.5;
om_ega <- 1e-4*(1-alpha-betav)
nrows <- 1000
# Calculate matrix of standard normal innovations
set.seed(1121)  # Reset random numbers
innov <- rnorm(nrows)
returns <- numeric(nrows)
variance <- numeric(nrows)
variance[1] <- om_ega/(1-alpha-betav)
returns[1] <- sqrt(variance[1])*innov[1]
# Simulate GARCH model
for (i in 2:nrows) {
  returns[i] <- sqrt(variance[i-1])*innov[i]
  variance[i] <- om_ega + alpha*returns[i]^2 +
    betav*variance[i-1]
}  # end for
# Simulate the GARCH process using Rcpp
garch_data <- HighFreq::sim_garch(omega=om_ega, alpha=alpha,
  beta=betav, innov=matrix(innov))
all.equal(garch_data, cbind(returns, variance),
  check.attributes=FALSE)

# Open plot window on Mac
dev.new(width=6, height=5, noRStudioGD=TRUE)
# Set plot parameters to reduce whitespace around plot
par(mar=c(2, 2, 3, 1), oma=c(0, 0, 0, 0))
# Plot GARCH cumulative returns
plot(cumsum(returns), t="l", col="blue", xlab="", ylab="",
  main="GARCH Cumulative Returns")
quartz.save("figure/garch_returns.png", type="png",
  width=6, height=5)
# Plot GARCH volatility
plot(sqrt(variance), t="l", col="blue", xlab="", ylab="",
  main="GARCH Volatility")
quartz.save("figure/garch_volat.png", type="png",
  width=6, height=5)

# Calculate kurtosis of GARCH returns
mean(((returns-mean(returns))/sd(returns))^4)
# Perform Jarque-Bera test of normality
tseries::jarque.bera.test(returns)
# Fit t-distribution into GARCH returns
optim_fit <- MASS::fitdistr(returns, densfun="t", df=2)
loc <- optim_fit$estimate[1]
scalev <- optim_fit$estimate[2]

# Plot histogram of GARCH returns
histp <- hist(returns, col="lightgrey",
  xlab="returns", breaks=200, xlim=c(-0.03, 0.03),
  ylab="frequency", freq=FALSE, main="GARCH Returns Histogram")
lines(density(returns, adjust=1.5), lwd=2, col="blue")
curve(expr=dt((x-loc)/scalev, df=2)/scalev,
  type="l", xlab="", ylab="", lwd=2,
  col="red", add=TRUE)
legend("topright", inset=-0, bty="n",
 leg=c("density", "t-distr w/ 2 dof"),
 lwd=6, lty=1, col=c("blue", "red"))
quartz.save("figure/garch_hist.png", type="png", width=6, height=5)

# Specify GARCH model
garch_spec <- fGarch::garchSpec(model=list(ar=c(0, 0), omega=om_ega,
  alpha=alpha, beta=betav))
# Simulate GARCH model
garch_sim <- fGarch::garchSim(spec=garch_spec, n=nrows)
returns <- as.numeric(garch_sim)
# Calculate kurtosis of GARCH returns
moments::moment(returns, order=4) /
  moments::moment(returns, order=2)^2
# Perform Jarque-Bera test of normality
tseries::jarque.bera.test(returns)
# Plot histogram of GARCH returns
histp <- hist(returns, col="lightgrey",
  xlab="returns", breaks=200, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE,
  main="GARCH Returns Histogram")
lines(density(returns, adjust=1.5), lwd=3, col="blue")

# Fit t-distribution into GARCH returns
optim_fit <- MASS::fitdistr(returns,
  densfun="t", df=2, lower=c(-1, 1e-7))
loc <- optim_fit$estimate[1]
scalev <- optim_fit$estimate[2]
curve(expr=dt((x-loc)/scalev, df=2)/scalev,
  type="l", xlab="", ylab="", lwd=3,
  col="red", add=TRUE)
legend("topright", inset=0.05, bty="n",
 leg=c("density", "t-distr w/ 2 dof"),
 lwd=6, lty=1, col=c("blue", "red"))

# Calculate variance of GARCH returns
var(returns)
# Calculate expected value of variance
om_ega/(1-alpha-betav)
# Calculate kurtosis of GARCH returns
mean(((returns-mean(returns))/sd(returns))^4)
# Calculate expected value of kurtosis
3 + 6*alpha^2/(1-2*alpha^2-(alpha+betav)^2)

# Calculate the distribution of GARCH kurtosis
kurt <- sapply(1:1e4, function(x) {
  garch_data <- HighFreq::sim_garch(omega=om_ega, alpha=alpha,
    beta=betav, innov=matrix(rnorm(nrows)))
  returns <- garch_data[, 1]
  c(var(returns), mean(((returns-mean(returns))/sd(returns))^4))
})  # end sapply
kurt <- t(kurt)
apply(kurt, 2, mean)
# Plot the distribution of GARCH kurtosis
dev.new(width=6, height=5, noRStudioGD=TRUE)
par(mar=c(2, 2, 3, 1), oma=c(0, 0, 0, 0))
histp <- hist(kurt[, 2], breaks=500, col="lightgrey",
  xlim=c(2, 8), xlab="returns", ylab="frequency", freq=FALSE,
  main="Distribution of GARCH Kurtosis")
lines(density(kurt[, 2], adjust=1.5), lwd=3, col="blue")
abline(v=(3 + 6*alpha^2/(1-2*alpha^2-(alpha+betav)^2)), lwd=3, col="red")
text(x=7.0, y=0.4, "Expected Kurtosis")
quartz.save("figure/garch_kurtosis.png", type="png", width=6, height=5)

# Simulate the GARCH process using Rcpp
garch_data <- HighFreq::sim_garch(omega=om_ega, alpha=alpha,
  beta=betav, innov=matrix(innov))
# Extract the returns
returns <- garch_data[, 1]
# Estimate the rolling variance from the returns
variance <- numeric(nrows)
variance[1] <- om_ega/(1-alpha-betav)
for (i in 2:nrows) {
  variance[i] <- om_ega + alpha*returns[i]^2 +
    betav*variance[i-1]
}  # end for
all.equal(garch_data[, 2], variance, check.attributes=FALSE)

library(fGarch)
# Fit returns into GARCH
garch_fit <- fGarch::garchFit(data=returns)
# Fitted GARCH parameters
garch_fit@fit$coef
# Actual GARCH parameters
c(mu=mean(returns), omega=om_ega,alpha=alpha, beta=betav)
# Plot GARCH fitted volatility
plot(sqrt(garch_fit@fit$series$h), t="l",
  col="blue", xlab="", ylab="",
  main="GARCH Fitted Volatility")
quartz.save("figure/garch_fGarch_fitted.png",
  type="png", width=6, height=5)

# Define likelihood function
likefun <- function(om_ega, alpha, betav) {
  # Estimate the rolling variance from the returns
  variance <- numeric(nrows)
  variance[1] <- om_ega/(1-alpha-betav)
  for (i in 2:nrows) {
    variance[i] <- om_ega + alpha*returns[i]^2 + betav*variance[i-1]
  }  # end for
  variance <- ifelse(variance > 0, variance, 0.000001)
  # Lag the variance
  variance <- rutils::lagit(variance, pad_zeros=FALSE)
  # Calculate the likelihood
  -sum(returns^2/variance + log(variance))
}  # end likefun
# Calculate the likelihood in R
likefun(om_ega, alpha, betav)
# Calculate the likelihood in Rcpp
HighFreq::lik_garch(omega=om_ega, alpha=alpha,
  beta=betav, returns=matrix(returns))
# Benchmark speed of likelihood calculations
library(microbenchmark)
summary(microbenchmark(
  Rcode=likefun(om_ega, alpha, betav),
  Rcpp=HighFreq::lik_garch(omega=om_ega, alpha=alpha, beta=betav, returns=matrix(returns))
  ), times=10)[, c(1, 4, 5)]

# Calculate the variance of returns
returns <- garch_data[, 1, drop=FALSE]
variance <- var(returns)
returns <- (returns - mean(returns))
# Calculate likelihood as function of alpha and betav parameters
likefun <- function(alpha, betav) {
  om_ega <- variance*(1 - alpha - betav)
  -HighFreq::lik_garch(omega=om_ega, alpha=alpha, beta=betav, returns=returns)
}  # end likefun
# Calculate matrix of likelihood values
alphas <- seq(from=0.15, to=0.35, len=50)
betas <- seq(from=0.35, to=0.5, len=50)
lik_mat <- sapply(alphas, function(alpha) sapply(betas,
  function(betav) likefun(alpha, betav)))

# Set rgl options and load package rgl
options(rgl.useNULL=TRUE); library(rgl)
# Draw and render 3d surface plot of likelihood function
n_col <- 100
color <- rainbow(n_col, start=2/6, end=4/6)
z_col <- cut(lik_mat, n_col)
rgl::persp3d(alphas, betas, lik_mat, col=color[z_col],
  xlab="alpha", ylab="beta", zlab="likelihood")
rgl::rglwidget(elementId="plot3drgl", width=700, height=700)
# Perform grid search
coord <- which(lik_mat == min(lik_mat), arr.ind=TRUE)
c(alphas[coord[2]], betas[coord[1]])
lik_mat[coord]
likefun(alphas[coord[2]], betas[coord[1]])
# Optimal and actual parameters
options(scipen=2)  # Use fixed not scientific notation
cbind(actual=c(alpha=alpha, beta=betav, omega=om_ega),
  optimal=c(alphas[coord[2]], betas[coord[1]], variance*(1 - sum(alphas[coord[2]], betas[coord[1]]))))

# Define vectorized likelihood function
likefun <- function(x, returns) {
  alpha <- x[1]; betav <- x[2]; om_ega <- x[3]
  -HighFreq::lik_garch(omega=om_ega, alpha=alpha, beta=betav, returns=returns)
}  # end likefun
# Initial parameters
initp <- c(alpha=0.2, beta=0.4, omega=variance/0.2)
# Find max likelihood parameters using steepest descent optimizer
optim_fit <- optim(par=initp,
  fn=likefun, # Log-likelihood function
  method="L-BFGS-B", # Quasi-Newton method
  returns=returns,
  upper=c(0.35, 0.55, variance), # Upper constraint
  lower=c(0.15, 0.35, variance/100)) # Lower constraint
# Optimal and actual parameters
cbind(actual=c(alpha=alpha, beta=betav, omega=om_ega),
optimal=c(optim_fit$par["alpha"], optim_fit$par["beta"], optim_fit$par["omega"]))
# Find max likelihood parameters using DEoptim
optiml <- DEoptim::DEoptim(fn=likefun,
  upper=c(0.35, 0.55, variance), # Upper constraint
  lower=c(0.15, 0.35, variance/100), # Lower constraint
  returns=returns,
  control=list(trace=FALSE, itermax=1000, parallelType=1))
# Optimal and actual parameters
cbind(actual=c(alpha=alpha, beta=betav, omega=om_ega),
optimal=c(optiml$optim$bestmem[1], optiml$optim$bestmem[2], optiml$optim$bestmem[3]))

# Calculate VTI returns
returns <- rutils::diffit(log(quantmod::Cl(rutils::etfenv$VTI)))
# Find max likelihood parameters using DEoptim
optiml <- DEoptim::DEoptim(fn=likefun,
  upper=c(0.4, 0.9, variance), # Upper constraint
  lower=c(0.1, 0.5, variance/100), # Lower constraint
  returns=returns,
  control=list(trace=FALSE, itermax=1000, parallelType=1))
# Optimal parameters
par_am <- unname(optiml$optim$bestmem)
alpha <- par_am[1]; betav <- par_am[2]; om_ega <- par_am[3]
c(alpha, betav, om_ega)
# Equilibrium GARCH variance
om_ega/(1-alpha-betav)
drop(var(returns))

# Estimate the GARCH volatility of VTI returns
nrows <- NROW(returns)
variance <- numeric(nrows)
variance[1] <- om_ega/(1-alpha-betav)
for (i in 2:nrows) {
  variance[i] <- om_ega + alpha*returns[i]^2 + betav*variance[i-1]
}  # end for
# Estimate the GARCH volatility using Rcpp
garch_data <- HighFreq::sim_garch(omega=om_ega, alpha=alpha,
  beta=betav, innov=returns, is_random=FALSE)
all.equal(garch_data[, 2], variance, check.attributes=FALSE)
# Plot dygraph of the estimated GARCH volatility
dygraphs::dygraph(xts::xts(sqrt(variance), zoo::index(returns)),
  main="Estimated GARCH Volatility of VTI") %>%
  dyOptions(colors="blue")

# Simulate GARCH model
garch_data <- HighFreq::sim_garch(omega=om_ega, alpha=alpha,
  beta=betav, innov=matrix(innov))
variance <- garch_data[, 2]
# Calculate the equilibrium variance
var_eq <- om_ega/(1-alpha-betav)
# Calculate the variance forecasts
varcasts <- numeric(10)
varcasts[1] <- var_eq +
  (alpha + betav)*(xts::last(variance) - var_eq)
for (i in 2:10) {
  varcasts[i] <- var_eq + (alpha + betav)*(varcasts[i-1] - var_eq)
}  # end for

# Open plot window on Mac
dev.new(width=6, height=5, noRStudioGD=TRUE)
par(mar=c(2, 2, 3, 1), oma=c(0, 0, 0, 0))
# Plot GARCH variance forecasts
plot(tail(variance, 30), t="l", col="blue", xlab="", ylab="",
  xlim=c(1, 40), ylim=c(0, max(tail(variance, 30))),
  main="GARCH Variance Forecasts")
text(x=15, y=0.5*var_eq, "realized variance")
lines(x=30:40, y=c(xts::last(variance), varcasts), col="red", lwd=3)
text(x=35, y=0.6*var_eq, "variance forecasts")
abline(h=var_eq, lwd=3, col="red")
text(x=10, y=1.1*var_eq, "Equilibrium variance")
quartz.save("figure/garch_forecast.png", type="png",
  width=6, height=5)

library(HighFreq)  # Load HighFreq
# Minutely SPY returns (unit per minute) single day
returns <- rutils::diffit(log(SPY["2012-02-13", 4]))
# Minutely SPY volatility (unit per minute)
sd(returns)
# Divide minutely SPY returns by time intervals (unit per second)
returns <- returns / rutils::diffit(.zoo::index(SPY["2012-02-13"]))
returns[1] <- 0
# Minutely SPY volatility scaled to unit per minute
60*sd(returns)
# SPY returns multiple days
returns <- rutils::diffit(log(SPY[, 4]))
# Minutely SPY volatility (includes overnight jumps)
sd(returns)
# Table of intervals - 60 second is most frequent
indeks <- rutils::diffit(.zoo::index(SPY))
table(indeks)
# hist(indeks)
# SPY returns with overnight scaling (unit per second)
returns <- returns / indeks
returns[1] <- 0
# Minutely SPY volatility scaled to unit per minute
60*sd(returns)

library(HighFreq)  # Load HighFreq
# Minutely OHLC SPY prices aggregated to daily prices
SPY_daily <- rutils::to_period(ohlc=HighFreq::SPY, period="days")
# Daily SPY volatility from daily returns
sd(rutils::diffit(log(SPY_daily[, 4])))
# Minutely SPY returns (unit per minute)
returns <- rutils::diffit(log(SPY[, 4]))
# Minutely SPY volatility scaled to daily interval
sqrt(6.5*60)*sd(returns)
# Minutely SPY returns with overnight scaling (unit per second)
returns <- rutils::diffit(log(SPY[, 4]))
indeks <- rutils::diffit(.zoo::index(SPY))
returns <- returns / indeks
returns[1] <- 0
# Daily SPY volatility from minutely returns
sqrt(6.5*60)*60*sd(returns)
# Daily SPY volatility
# Scale by extra time over weekends and holidays
24*60*60*sd(rutils::diffit(log(SPY_daily[, 4]))[-1] /
    rutils::diffit(.zoo::index(SPY_daily))[-1])

# Calculate SPY returns adjusted for overnight jumps
closep <- log(as.numeric(Cl(HighFreq::SPY[, 4])))
returns <- rutils::diffit(closep) /
  rutils::diffit(.zoo::index(HighFreq::SPY))
returns[1] <- 0
closep <- cumsum(returns)
nrows <- NROW(closep)
# Calculate volatilities for vector of aggregation intervals
interval_s <- seq.int(from=3, to=35, length.out=9)^2
vol_s <- sapply(interval_s, function(interval) {
  num_agg <- nrows %/% interval
  endp <- c(0, nrows - num_agg*interval + (0:num_agg)*interval)
  # endp <- rutils::calc_endpoints(closep, interval=interval)
  sd(rutils::diffit(closep[endp]))
})  # end sapply
# Calculate Hurst as regression slope using formula
vol_log <- log(vol_s)
inter_log <- log(interval_s)
hurs_t <- cov(vol_log, inter_log)/var(inter_log)
# Or using function lm()
model <- lm(vol_log ~ inter_log)
coef(model)[2]

# Calculate Hurst from single data point
(last(vol_log) - log(sd(returns)))/last(inter_log)
# Plot the volatilities
x11(width=6, height=5)
par(mar=c(4, 4, 2, 1), oma=c(1, 1, 1, 1))
plot(vol_log ~ inter_log, lwd=6, col="red",
     xlab="aggregation intervals (log)", ylab="volatility (log)",
     main="Hurst Exponent for SPY From Volatilities")
abline(model, lwd=3, col="blue")
text(inter_log[2], vol_log[NROW(vol_log)-1],
     paste0("Hurst = ", round(hurs_t, 4)))

# Calculate the rescaled range
interval <- 500
nrows <- NROW(closep); num_agg <- nrows %/% interval
endp <- c(0, nrows - num_agg*interval + (0:num_agg)*interval)
# Or
# endp <- rutils::calc_endpoints(closep, interval=interval)
r_s <- sapply(2:NROW(endp), function(ep) {
  indeks <- endp[ep-1]:endp[ep]
  diff(range(closep[indeks]))/sd(returns[indeks])
})  # end sapply
mean(r_s)
# Calculate Hurst from single data point
log(mean(r_s))/log(interval)

# Calculate rescaled range for vector of aggregation intervals
nrows <- NROW(closep)
r_s <- sapply(interval_s, function(interval) {
# Calculate end points
  num_agg <- nrows %/% interval
  endp <- c(0, nrows - num_agg*interval + (0:num_agg)*interval)
# Calculate rescaled ranges
  r_s <- sapply(2:NROW(endp), function(ep) {
    indeks <- endp[ep-1]:endp[ep]
    diff(range(closep[indeks]))/sd(returns[indeks])
  })  # end sapply
  mean(na.omit(r_s))
})  # end sapply
# Calculate Hurst as regression slope using formula
rs_log <- log(r_s)
inter_log <- log(interval_s)
hurs_t <- cov(rs_log, inter_log)/var(inter_log)
# Or using function lm()
model <- lm(rs_log ~ inter_log)
coef(model)[2]

x11(width=6, height=5)
par(mar=c(4, 4, 2, 1), oma=c(1, 1, 1, 1))
plot(rs_log ~ inter_log, lwd=6, col="red",
     xlab="aggregation intervals (log)",
     ylab="rescaled range (log)",
     main="Rescaled Range Analysis for SPY")
abline(model, lwd=3, col="blue")
text(inter_log[2], rs_log[NROW(rs_log)-1],
     paste0("Hurst = ", round(hurs_t, 4)))

library(HighFreq)  # Load HighFreq
ohlc <- log(rutils::etfenv$VTI)
# Calculate variance
var_close <- HighFreq::run_variance(ohlc=ohlc,
  method="close")
var_yang_zhang <- HighFreq::run_variance(ohlc=ohlc)
stdev <- 24*60*60*sqrt(252*cbind(var_close, var_yang_zhang))
colnames(stdev) <- c("close stdev", "Yang-Zhang")
# Plot the time series of volatility
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red")
quantmod::chart_Series(stdev["2011-07/2011-12"],
  theme=plot_theme, name="Standard Deviations: Close and YZ")
legend("top", legend=colnames(stdev),
 bg="white", lty=1, lwd=6, inset=0.1, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Plot volatility around 2010 flash crash
quantmod::chart_Series(stdev["2010-04/2010-06"],
  theme=plot_theme, name="Volatility Around 2010 Flash Crash")
legend("top", legend=colnames(stdev),
 bg="white", lty=1, lwd=6, inset=0.1, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Plot density of volatility distributions
plot(density(stdev[, 1]), xlab="", ylab="",
  main="Density of Volatility Distributions",
  xlim=c(-0.05, range(stdev[, 1])[2]/3), type="l", lwd=2, col="blue")
lines(density(stdev[, 2]), col='red', lwd=2)
legend("top", legend=c("Close-to-Close", "Yang-Zhang"),
 bg="white", lty=1, lwd=6, inset=0.1, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# ? range volatility estimator has lower standard error ?
c(sd(var_close)/mean(var_close), sd(var_yang_zhang)/mean(var_yang_zhang))
foo <- stdev[var_close<range(var_close)[2]/3, ]
c(sd(foo[, 1])/mean(foo[, 1]), sd(foo[, 2])/mean(foo[, 2]))
plot(density(foo[, 1]), xlab="", ylab="",
  main="Mixture of Normal Returns",
  xlim=c(-0.05, range(foo[, 1])[2]/2), type="l", lwd=2, col="blue")
lines(density(foo[, 2]), col='red', lwd=2)

ohlc <- rutils::etfenv$VTI
returns <- log((ohlc[, 2] - ohlc[, 3]) / (ohlc[, 2] + ohlc[, 3]))
foo <- rutils::diffit(log(ohlc[, 4]))
plot(as.numeric(foo)^2, as.numeric(returns)^2)
bar <- lm(returns ~ foo)
summary(bar)


# Perform normality tests
shapiro.test(coredata(returns))
tseries::jarque.bera.test(returns)
# Fit VTI returns using MASS::fitdistr()
optim_fit <- MASS::fitdistr(returns,
            densfun="t", df=2)
optim_fit$estimate; optim_fit$sd
# Calculate moments of standardized returns
sapply(3:4, moments::moment,
  x=(returns - mean(returns))/sd(returns))

# Plot histogram of VTI returns
colors <- c("lightgray", "blue", "green", "red")
PerformanceAnalytics::chart.Histogram(returns,
  main="", xlim=c(-7, -3), col=colors[1:3],
  methods = c("add.density", "add.normal"))
curve(expr=dt((x-optim_fit$estimate[1])/
  optim_fit$estimate[2], df=2)/optim_fit$estimate[2],
type="l", xlab="", ylab="", lwd=2,
col=colors[4], add=TRUE)
# Add title and legend
title(main="VTI logarithm of range",
cex.main=1.3, line=-1)
legend("topright", inset=0.05,
  legend=c("density", "normal", "t-distr"),
  lwd=6, lty=1, col=colors[2:4], bty="n")

# Calculate VTI range variance partial autocorrelations
pacf(returns^2, lag=10, xlab=NA, ylab=NA,
     main="PACF of VTI log range")
quantmod::chart_Series(returns^2, name="VTI log of range squared")

# Standard errors of variance estimators using bootstrap
boot_data <- sapply(1:1e2, function(x) {
  # Create random OHLC
  ohlc <- HighFreq::random_ohlc()
  # Calculate variance estimate
  c(var=var(ohlc[, 4]),
    yang_zhang=HighFreq::calcvariance(
ohlc, method="yang_zhang", scalev=FALSE))
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
returns <- log(rutils::etfenv$VTI[,2] /
            rutils::etfenv$VTI[,3])
pacf(returns^2, lag=10, xlab=NA, ylab=NA)
title(main="VTI squared range partial autocorrelations")

ohlc <- rutils::etfenv$VTI
# Number of data points
nrows <- NROW(ohlc["2018-06/"])
# Define endp at each point in time
endp <- 0:nrows
# Number of data points in look_back interval
look_back <- 22
# startp are endp lagged by look_back
startp <- c(rep_len(0, look_back - 1),
    endp[1:(NROW(endp)- look_back + 1)])
head(startp, 33)

# Number of data points
closep <- quantmod::Cl(ohlc["2018/"])
nrows <- NROW(closep)
# Number of periods between endpoints
npoints <- 21
# Number of npoints that fit over nrows
nagg <- nrows %/% npoints
# If(nrows==npoints*nagg then whole number
endp <- (0:nagg)*npoints
# Stub interval at beginning
endp <- c(0, nrows-npoints*nagg + (0:nagg)*npoints)
# Else stub interval at end
endp <- c((0:nagg)*npoints, nrows)
# Or use xts::endpoints()
endp <- xts::endpoints(closep, on="months")

# Plot data and endpoints as vertical lines
plot.xts(closep, col="blue", lwd=2, xlab="", ylab="",
   main="Prices with Endpoints as Vertical Lines")
addEventLines(xts(rep("endpoint", NROW(endp)-1), zoo::index(closep)[endp]),
        col="red", lwd=2, pos=4)
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- "blue"
quantmod::chart_Series(closep, theme=plot_theme,
  name="prices with endpoints as vertical lines")
abline(v=endp, col="red", lwd=2)

# Number of data points
nrows <- NROW(rutils::etfenv$VTI["2019/"])
# Number of npoints that fit over nrows
npoints <- 21
nagg <- nrows %/% npoints
# Stub interval at beginning
endp <- c(0, nrows-npoints*nagg + (0:nagg)*npoints)

# look_back defined as number of data points
look_back <- 252
# startp are endp lagged by look_back
startp <- (endp - look_back + 1)
startp <- ifelse(startp < 0, 0, startp)
# look_back defined as number of endp
look_back <- 12
startp <- c(rep_len(0, look_back-1),
    endp[1:(NROW(endp)- look_back + 1)])
# Bind startp with endp
cbind(startp, endp)

# Number of data points
nrows <- NROW(rutils::etfenv$VTI["2019/"])
# Number of data points per interval
npoints <- 21
# Number of npointss that fit over nrows
nagg <- nrows %/% npoints
# Define endp with beginning stub
endp <- c(0, nrows-npoints*nagg + (0:nagg)*npoints)
# Define contiguous startp
startp <- c(0, endp[1:(NROW(endp)-1)])
# Define exclusive startp
startp <- c(0, endp[1:(NROW(endp)-1)]+1)

# Extract time series of VTI log prices
closep <- log(na.omit(rutils::etfenv$prices$VTI))
endp <- 0:NROW(closep)  # End points at each point
nrows <- NROW(endp)
look_back <- 22  # Number of data points per look-back interval
# startp are multi-period lag of endp
startp <- c(rep_len(0, look_back - 1),
    endp[1:(nrows - look_back + 1)])
# Define list of look-back intervals for aggregations over past
look_backs <- lapply(2:nrows, function(indeks) {
    startp[indeks]:endp[indeks]
})  # end lapply
# Define aggregation function
aggfun <- function(xtes) c(max=max(xtes), min=min(xtes))
# Perform aggregations over look_backs list
agg_s <- sapply(look_backs,
    function(look_back) aggfun(closep[look_back])
)  # end sapply
# Coerce agg_s into matrix and transpose it
if (is.vector(agg_s))
  agg_s <- t(agg_s)
agg_s <- t(agg_s)
# Coerce agg_s into xts series
agg_s <- xts(agg_s, order.by=zoo::index(closep[endp]))

library(rutils)  # Load package rutils
# Perform aggregations over look_backs list
agg_s <- lapply(look_backs,
    function(look_back) aggfun(closep[look_back])
)  # end lapply
# rbind list into single xts or matrix
agg_s <- rutils::do_call(rbind, agg_s)
# Convert into xts
agg_s <- xts::xts(agg_s, order.by=zoo::index(closep))
agg_s <- cbind(agg_s, closep)
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
roll_agg <- function(xtes, look_back, FUN, ...) {
# Define end points at every period
  endp <- 0:NROW(xtes)
  nrows <- NROW(endp)
# Define starting points as lag of endp
  startp <- c(rep_len(0, look_back - 1),
    endp[1:(nrows- look_back + 1)])
# Perform aggregations over look_backs list
  agg_s <- lapply(2:nrows, function(indeks)
    FUN(xtes[startp[indeks]:endp[indeks]], ...)
  )  # end lapply
# rbind list into single xts or matrix
  agg_s <- rutils::do_call(rbind, agg_s)
# Coerce agg_s into xts series
  if (!is.xts(agg_s))
    agg_s <- xts(agg_s, order.by=zoo::index(xtes))
  agg_s
}  # end roll_agg
# Define aggregation function
aggfun <- function(xtes)
  c(max=max(xtes), min=min(xtes))
# Perform aggregations over rolling interval
agg_s <- roll_agg(closep, look_back=look_back, FUN=aggfun)
class(agg_s)
dim(agg_s)

# library(rutils)  # Load package rutils
# Define aggregation function that returns a vector
agg_vector <- function(xtes)
  c(max=max(xtes), min=min(xtes))
# Define aggregation function that returns an xts
agg_xts <- function(xtes)
  xts(t(c(max=max(xtes), min=min(xtes))), order.by=end(xtes))
# Benchmark the speed of aggregation functions
library(microbenchmark)
summary(microbenchmark(
  agg_vector=roll_agg(closep, look_back=look_back, FUN=agg_vector),
  agg_xts=roll_agg(closep, look_back=look_back, FUN=agg_xts),
  times=10))[, c(1, 4, 5)]

# library(rutils)  # Load package rutils
# Define aggregation function that returns a single value
aggfun <- function(xtes)  max(xtes)
# Perform aggregations over a rolling interval
agg_s <- xts:::rollapply.xts(closep, width=look_back,
              FUN=aggfun, align="right")
# Perform aggregations over a rolling interval
library(PerformanceAnalytics)  # Load package PerformanceAnalytics
agg_s <- apply.rolling(closep, width=look_back, FUN=aggfun)
# Benchmark the speed of the functionals
library(microbenchmark)
summary(microbenchmark(
  roll_agg=roll_agg(closep, look_back=look_back, FUN=max),
  roll_xts=xts:::rollapply.xts(closep, width=look_back, FUN=max, align="right"),
  apply_rolling=apply.rolling(closep, width=look_back, FUN=max),
  times=10))[, c(1, 4, 5)]

# library(rutils)  # Load package rutils
# Rolling sum using cumsum()
roll_sum <- function(xtes, look_back) {
  cumsumv <- cumsum(na.omit(xtes))
  output <- cumsumv - rutils::lagit(x=cumsumv, lagg=look_back)
  output[1:look_back, ] <- cumsumv[1:look_back, ]
  colnames(output) <- paste0(colnames(xtes), "_stdev")
  output
}  # end roll_sum
agg_s <- roll_sum(closep, look_back=look_back)
# Perform rolling aggregations using lapply loop
agg_s <- lapply(2:nrows, function(indeks)
    sum(closep[startp[indeks]:endp[indeks]])
)  # end lapply
# rbind list into single xts or matrix
agg_s <- rutils::do_call(rbind, agg_s)
head(agg_s)
tail(agg_s)
# Benchmark the speed of both methods
library(microbenchmark)
summary(microbenchmark(
  roll_sum=roll_sum(closep, look_back=look_back),
  s_apply=sapply(look_backs,
    function(look_back) sum(closep[look_back])),
  times=10))[, c(1, 4, 5)]

# Extract time series of VTI log prices
closep <- log(na.omit(rutils::etfenv$prices$VTI))
# Calculate EWMA prices using filter()
look_back <- 21
weights <- exp(-0.1*1:look_back)
weights <- weights/sum(weights)
filtered <- stats::filter(closep, filter=weights,
                   method="convolution", sides=1)
filtered <- as.numeric(filtered)
# filter() returns time series of class "ts"
class(filtered)
# Filter using compiled C++ function directly
getAnywhere(C_cfilter)
str(stats:::C_cfilter)
filter_fast <- .Call(stats:::C_cfilter, closep,
               filter=weights, sides=1, circular=FALSE)
all.equal(as.numeric(filtered), filter_fast, check.attributes=FALSE)
# Calculate EWMA prices using roll::roll_sum()
weights_rev <- rev(weights)
roll_ed <- roll::roll_sum(closep, width=look_back, weights=weights_rev, min_obs=1)
all.equal(filtered[-(1:look_back)],
    as.numeric(roll_ed)[-(1:look_back)],
    check.attributes=FALSE)
# Benchmark speed of rolling calculations
library(microbenchmark)
summary(microbenchmark(
  filter=filter(closep, filter=weights, method="convolution", sides=1),
  filter_fast=.Call(stats:::C_cfilter, closep, filter=weights, sides=1, circular=FALSE),
  cumsumv=cumsum(closep),
  roll=roll::roll_sum(closep, width=look_back, weights=weights_rev)
  ), times=10)[, c(1, 4, 5)]

# Calculate the rolling maximum and minimum over a vector of data
roll_maxminr <- function(vectorv, look_back) {
  nrows <- NROW(vectorv)
  max_min <- matrix(numeric(2:nrows), nc=2)
  # Loop over periods
  for (it in 1:nrows) {
    sub_vec <- vectorv[max(1, it-look_back+1):it]
    max_min[it, 1] <- max(sub_vec)
    max_min[it, 2] <- min(sub_vec)
  }  # end for
  return(max_min)
}  # end roll_maxminr
max_minr <- roll_maxminr(closep, look_back)
max_minr <- xts::xts(max_minr, zoo::index(closep))
library(TTR)  # Load package TTR
max_min <- cbind(TTR::runMax(x=closep, n=look_back),
           TTR::runMin(x=closep, n=look_back))
all.equal(max_min[-(1:look_back), ], max_minr[-(1:look_back), ], check.attributes=FALSE)
# Benchmark the speed of TTR::runMax
library(microbenchmark)
summary(microbenchmark(
  pure_r=roll_maxminr(closep, look_back),
  ttr=TTR::runMax(closep, n=look_back),
  times=10))[, c(1, 4, 5)]
# Benchmark the speed of TTR::runSum
summary(microbenchmark(
  vector_r=cumsum(coredata(closep)),
  rutils=rutils::roll_sum(closep, look_back=look_back),
  ttr=TTR::runSum(closep, n=look_back),
  times=10))[, c(1, 4, 5)]

library(rutils)
# Calculate rolling VTI variance using package roll
library(roll)  # Load roll
returns <- na.omit(rutils::etfenv$returns[, "VTI"])
look_back <- 22
# Calculate rolling sum using RcppRoll
sum_roll <- roll::roll_sum(returns, width=look_back, min_obs=1)
# Calculate rolling sum using rutils
sum_rutils <- rutils::roll_sum(returns, look_back=look_back)
all.equal(sum_roll[-(1:look_back), ],
    sum_rutils[-(1:look_back), ], check.attributes=FALSE)
# Benchmark speed of rolling calculations
library(microbenchmark)
summary(microbenchmark(
  cumsumv=cumsum(returns),
  roll=roll::roll_sum(returns, width=look_back),
  RcppRoll=RcppRoll::roll_sum(returns, n=look_back),
  rutils=rutils::roll_sum(returns, look_back=look_back),
  times=10))[, c(1, 4, 5)]

library(RcppRoll)  # Load package RcppRoll
# Calculate rolling sum using RcppRoll
sum_roll <- RcppRoll::roll_sum(returns, align="right", n=look_back)
# Calculate rolling sum using rutils
sum_rutils <- rutils::roll_sum(returns, look_back=look_back)
all.equal(sum_roll, coredata(sum_rutils[-(1:(look_back-1))]),
    check.attributes=FALSE)
# Benchmark speed of rolling calculations
library(microbenchmark)
summary(microbenchmark(
  cumsumv=cumsum(returns),
  RcppRoll=RcppRoll::roll_sum(returns, n=look_back),
  rutils=rutils::roll_sum(returns, look_back=look_back),
  times=10))[, c(1, 4, 5)]
# Calculate EWMA prices using RcppRoll
closep <- quantmod::Cl(rutils::etfenv$VTI)
weights <- exp(0.1*1:look_back)
prices_ewma <- RcppRoll::roll_mean(closep,
align="right", n=look_back, weights=weights)
prices_ewma <- cbind(closep,
  rbind(coredata(closep[1:(look_back-1), ]), prices_ewma))
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
closep <- quantmod::Cl(HighFreq::SPY["2012-02-01/2012-04-01"])
med_ian <- runmed(x=closep, k=look_back)
# Vector of rolling volatilities
sigmav <- runsd(x=closep, k=look_back,
          endrule="constant", align="center")
# Vector of rolling quantiles
quantilevs <- runquantile(x=closep, k=look_back,
  probs=0.9, endrule="constant", align="center")

# Compile Rcpp functions
Rcpp::sourceCpp(file="/Users/jerzy/Develop/R/Rcpp/roll_maxmin.cpp")
max_minarma <- roll_maxmin(closep, look_back)
max_minarma <- xts::xts(max_minr, zoo::index(closep))
max_min <- cbind(TTR::runMax(x=closep, n=look_back),
           TTR::runMin(x=closep, n=look_back))
all.equal(max_min[-(1:look_back), ], max_minarma[-(1:look_back), ], check.attributes=FALSE)
# Benchmark the speed of TTR::runMax
library(microbenchmark)
summary(microbenchmark(
  arma=roll_maxmin(closep, look_back),
  ttr=TTR::runMax(closep, n=look_back),
  times=10))[, c(1, 4, 5)]
# Dygraphs plot with max_min lines
datav <- cbind(closep, max_minarma)
colnames(datav)[2:3] <- c("max", "min")
colors <- c("blue", "red", "green")
dygraphs::dygraph(datav, main=paste(colnames(closep), "max and min lines")) %>%
  dyOptions(colors=colors)
# Standard plot with max_min lines
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(datav["2008/2009"], theme=plot_theme,
  name=paste(colnames(closep), "max and min lines"))
legend(x="topright", title=NULL, legend=colnames(datav),
 inset=0.1, cex=0.9, bg="white", bty="n",
 lwd=6, lty=1, col=colors)

library(rutils)  # Load package rutils
# Indices of last observations in each hour
endp <- xts::endpoints(closep, on="hours")
head(endp)
# extract the last observations in each hour
head(closep[endp, ])

# Extract time series of VTI log prices
closep <- log(na.omit(rutils::etfenv$prices$VTI))
# Number of data points
nrows <- NROW(closep)
# Number of data points per interval
look_back <- 22
# Number of look_backs that fit over nrows
nagg <- nrows %/% look_back
# Define endp with beginning stub
endp <- c(0, nrows-look_back*nagg + (0:nagg)*look_back)
# Define contiguous startp
startp <- c(0, endp[1:(NROW(endp)-1)])
# Define list of look-back intervals for aggregations over past
look_backs <- lapply(2:NROW(endp), function(indeks) {
    startp[indeks]:endp[indeks]
})  # end lapply
look_backs[[1]]
look_backs[[2]]
# Perform sapply() loop over look_backs list
agg_s <- sapply(look_backs, function(look_back) {
  xtes <- closep[look_back]
  c(max=max(xtes), min=min(xtes))
})  # end sapply
# Coerce agg_s into matrix and transpose it
if (is.vector(agg_s))
  agg_s <- t(agg_s)
agg_s <- t(agg_s)
# Coerce agg_s into xts series
agg_s <- xts(agg_s, order.by=zoo::index(closep[endp]))
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
  xtes <- closep[look_back]
  c(max=max(xtes), min=min(xtes))
})  # end lapply
# rbind list into single xts or matrix
agg_s <- rutils::do_call(rbind, agg_s)
# Coerce agg_s into xts series
agg_s <- xts(agg_s, order.by=zoo::index(closep[endp]))
head(agg_s)
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
quantmod::chart_Series(agg_s, theme=plot_theme, name="price aggregations")
legend("top", legend=colnames(agg_s),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# library(rutils)  # Load package rutils
# Define functional for rolling aggregations over endp
roll_agg <- function(xtes, endp, FUN, ...) {
  nrows <- NROW(endp)
# startp are single-period lag of endp
  startp <- c(1, endp[1:(nrows-1)])
# Perform aggregations over look_backs list
  agg_s <- lapply(look_backs,
    function(look_back) FUN(xtes[look_back], ...))  # end lapply
# rbind list into single xts or matrix
  agg_s <- rutils::do_call(rbind, agg_s)
  if (!is.xts(agg_s))
    agg_s <-  # Coerce agg_s into xts series
    xts(agg_s, order.by=zoo::index(xtes[endp]))
  agg_s
}  # end roll_agg
# Apply sum() over endp
agg_s <- roll_agg(closep, endp=endp, FUN=sum)
agg_s <- period.apply(closep, INDEX=endp, FUN=sum)
# Benchmark the speed of aggregation functions
summary(microbenchmark(
  roll_agg=roll_agg(closep, endp=endp, FUN=sum),
  period_apply=period.apply(closep, INDEX=endp, FUN=sum),
  times=10))[, c(1, 4, 5)]
agg_s <- period.sum(closep, INDEX=endp)
head(agg_s)

# library(rutils)  # Load package rutils
# Load package HighFreq
library(HighFreq)
# Extract closing minutely prices
closep <- quantmod::Cl(rutils::etfenv$VTI["2019"])
# Apply "mean" over daily periods
agg_s <- apply.daily(closep, FUN=sum)
head(agg_s)

# Define endp with beginning stub
npoints <- 5
nrows <- NROW(closep)
nagg <- nrows %/% npoints
endp <- c(0, nrows-npoints*nagg + (0:nagg)*npoints)
# Number of data points in look_back interval
look_back <- 22
# startp are endp lagged by look_back
startp <- (endp - look_back + 1)
startp <- ifelse(startp < 0, 0, startp)
# Perform lapply() loop over look_backs list
agg_s <- lapply(2:NROW(endp), function(indeks) {
xtes <- closep[startp[indeks]:endp[indeks]]
c(max=max(xtes), min=min(xtes))
})  # end lapply
# rbind list into single xts or matrix
agg_s <- rutils::do_call(rbind, agg_s)
# Coerce agg_s into xts series
agg_s <- xts(agg_s, order.by=zoo::index(closep[endp]))
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
quantmod::chart_Series(agg_s, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(agg_s),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

agg_s <- cbind(closep, agg_s)
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
dates <- Sys.Date() + 0:365
zoo_series <- zoo(rnorm(NROW(dates)), order.by=dates)
# Create monthly dates
dates_agg <- as.Date(as.yearmon(zoo::index(zoo_series)))
# Perform monthly mean aggregation
zoo_agg <- aggregate(zoo_series, by=dates_agg, FUN=mean)
# Merge with original zoo - union of dates
zoo_agg <- cbind(zoo_series, zoo_agg)
# Replace NA's using locf
zoo_agg <- na.locf(zoo_agg, na.rm=FALSE)
# Extract aggregated zoo
zoo_agg <- zoo_agg[zoo::index(zoo_series), 2]

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
zoo_agg <- zoo_agg[zoo::index(zoo_series), 2]
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
zoo_mean <- zoo_mean[zoo::index(zoo_series), 2]
# Plot original and interpolated zoo
plot(cumsum(zoo_series), xlab="", ylab="")
lines(cumsum(zoo_mean), lwd=2, col="red")
# Add legend
legend("topright", inset=0.05, cex=0.8, title="Mean Prices",
 leg=c("orig prices", "mean prices"), lwd=2, bg="white",
 col=c("black", "red"), bty="n")
