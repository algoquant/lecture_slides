library(rutils)  # Load package rutils
# Calculate VTI percentage returns
retp <- rutils::etfenv$returns$VTI
retp <- drop(coredata(na.omit(retp)))
nrows <- NROW(retp)
# Mean and standard deviation of returns
c(mean(retp), sd(retp))
# Calculate the smoothing bandwidth as the MAD of returns 10 points apart
retp <- sort(retp)
bwidth <- 10*mad(rutils::diffit(retp, lagg=10))
# Calculate the kernel density using a loop
dens1 <- sapply(1:nrows, function(it) {
  sum(dnorm(retp-retp[it], sd=bwidth))
})/nrows  # end sapply
# Plot the kernel density
madv <- mad(retp)
plot(retp, dens1, xlim=c(-5*madv, 5*madv),
     t="l", col="blue", lwd=3,
     xlab="returns", ylab="density",
     main="Density of VTI Returns")
# Calculate the kernel density using density()
densv <- density(retp, bw=bwidth)
NROW(densv$y)
plot(densv, xlim=c(-5*madv, 5*madv),
     xlab="returns", ylab="density",
     col="blue", lwd=3, main="Density of VTI Returns")
# Interpolate the densv vector into returns
densv <- approx(densv$x, densv$y, xout=retp)
all.equal(densv$x, retp)
# Plot the two density estimates
plot(retp, dens1, xlim=c(-5*madv, 5*madv),
     xlab="returns", ylab="density",
     t="l", col="blue", lwd=1,
     main="Density of VTI Returns")
lines(retp, densv$y, col="red")
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("density", "densfun"), bty="n", y.intersp=0.4,
 lwd=6, bg="white", col=c("blue", "red"))
# Plot histogram
histp <- hist(retp, breaks=100, freq=FALSE,
  xlim=c(-5*madv, 5*madv), xlab="", ylab="",
  main="VTI Return Distribution")
# Draw kernel density of histogram
lines(densv, col="red", lwd=2)
# Add density of normal distribution
curve(expr=dnorm(x, mean=mean(retp), sd=sd(retp)),
add=TRUE, lwd=2, col="blue")
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("VTI", "Normal"), bty="n", y.intersp=0.4,
 lwd=6, bg="white", col=c("red", "blue"))
library(rutils)  # Load package rutils
# Calculate VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
# Mean and standard deviation of returns
c(mean(retp), sd(retp))
# Plot histogram
x11(width=6, height=5)
par(mar=c(1, 1, 1, 1), oma=c(2, 2, 2, 0))
madv <- mad(retp)
histp <- hist(retp, breaks=100,
  main="", xlim=c(-5*madv, 5*madv),
  xlab="", ylab="", freq=FALSE)
# Draw kernel density of histogram
lines(density(retp), col="red", lwd=2)
# Add density of normal distribution
curve(expr=dnorm(x, mean=mean(retp), sd=sd(retp)),
add=TRUE, type="l", lwd=2, col="blue")
title(main="VTI Return Distribution", line=0)  # Add title
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("VTI", "Normal"), bty="n", y.intersp=0.4,
 lwd=6, bg="white", col=c("red", "blue"))
# Create normal Q-Q plot
qqnorm(retp, ylim=c(-0.1, 0.1), main="VTI Q-Q Plot",
 xlab="Normal Quantiles")
# Fit a line to the normal quantiles
qqline(retp, col="red", lwd=2)
# Perform Shapiro-Wilk test
shapiro.test(retp)
# Boxplot method for formula
boxplot(formula=mpg ~ cyl, data=mtcars,
  main="Mileage by number of cylinders",
  xlab="Cylinders", ylab="Miles per gallon")
# Boxplot method for data frame of EuStockMarkets percentage returns
boxplot(x=diff(log(EuStockMarkets)))
# Calculate VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
# Number of observations
nrows <- NROW(retp)
# Mean of VTI returns
retm <- mean(retp)
# Standard deviation of VTI returns
stdev <- sd(retp)
# Skewness of VTI returns
nrows/((nrows-1)*(nrows-2))*sum(((retp - retm)/stdev)^3)
# Kurtosis of VTI returns
nrows*(nrows+1)/((nrows-1)^3)*sum(((retp - retm)/stdev)^4)
# Random normal returns
retp <- rnorm(nrows, sd=stdev)
# Mean and standard deviation of random normal returns
retm <- mean(retp)
stdev <- sd(retp)
# Skewness of random normal returns
nrows/((nrows-1)*(nrows-2))*sum(((retp - retm)/stdev)^3)
# Kurtosis of random normal returns
nrows*(nrows+1)/((nrows-1)^3)*sum(((retp - retm)/stdev)^4)
# calc_skew() calculates skew of returns
calc_skew <- function(retp) {
  retp <- na.omit(retp)
  sum(((retp - mean(retp))/sd(retp))^3)/NROW(retp)
}  # end calc_skew
# calc_kurt() calculates kurtosis of returns
calc_kurt <- function(retp) {
  retp <- na.omit(retp)
  sum(((retp - mean(retp))/sd(retp))^4)/NROW(retp)
}  # end calc_kurt
# Calculate skew and kurtosis of VTI returns
calc_skew(retp)
calc_kurt(retp)
# calc_mom() calculates the moments of returns
calc_mom <- function(retp, moment=3) {
  retp <- na.omit(retp)
  sum(((retp - mean(retp))/sd(retp))^moment)/NROW(retp)
}  # end calc_mom
# Calculate skew and kurtosis of VTI returns
calc_mom(retp, moment=3)
calc_mom(retp, moment=4)
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
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
startp <- 3; endd <- 5  # Set lower and upper bounds
# Set polygon base
subv <- ((xvar >= startp) & (xvar <= endd))
polygon(c(startp, xvar[subv], endd),  # Draw polygon
  c(-1, yvar[subv], -1), col="red")
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
sigmavs <- c(0.5, 1, 1.5, 2)  # Sigma values
# Create plot colors
colorv <- c("red", "black", "blue", "green")
# Create legend labels
labelv <- paste("sigma", sigmavs, sep="=")
for (it in 1:4) {  # Plot four curves
  curve(expr=dnorm(x, sd=sigmavs[it]),
  xlim=c(-4, 4), xlab="", ylab="", lwd=2,
  col=colorv[it], add=as.logical(it-1))
}  # end for
# Add title
title(main="Normal Distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, title="Sigmas", y.intersp=0.4,
 labelv, cex=0.8, lwd=2, lty=1, bty="n", col=colorv)
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
degf <- c(3, 6, 9)  # Df values
colorv <- c("black", "red", "blue", "green")
labelv <- c("normal", paste("df", degf, sep="="))
# Plot a Normal probability distribution
curve(expr=dnorm, xlim=c(-4, 4), xlab="", ylab="", lwd=2)
for (it in 1:3) {  # Plot three t-distributions
  curve(expr=dt(x, df=degf[it]), xlab="", ylab="",
lwd=2, col=colorv[it+1], add=TRUE)
}  # end for
# Add title
title(main="t-distributions", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n", y.intersp=0.4,
       title="Degrees\n of freedom", labelv,
       cex=0.8, lwd=6, lty=1, col=colorv)
# Mixture of two normal distributions with sd=1 and sd=2
nrows <- 1e5
retp <- c(rnorm(nrows/2), 2*rnorm(nrows/2))
retp <- (retp-mean(retp))/sd(retp)
# Kurtosis of normal
calc_kurt(rnorm(nrows))
# Kurtosis of mixture
calc_kurt(retp)
# Or
nrows*sum(retp^4)/(nrows-1)^2
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Plot the distributions
plot(density(retp), xlab="", ylab="",
  main="Mixture of Normal Returns",
  xlim=c(-3, 3), type="l", lwd=3, col="red")
curve(expr=dnorm, lwd=2, col="blue", add=TRUE)
curve(expr=dt(x, df=3), lwd=2, col="green", add=TRUE)
# Add legend
legend("topright", inset=0.05, lty=1, lwd=6, bty="n",
  legend=c("Mixture", "Normal", "t-distribution"), y.intersp=0.4,
  col=c("red", "blue", "green"))
dev.new(width=6, height=5, noRStudioGD=TRUE)
# x11(width=6, height=5)
# Define density of non-standard t-distribution
tdistr <- function(x, dfree, locv=0, scalev=1) {
  dt((x-locv)/scalev, df=dfree)/scalev
}  # end tdistr
# Or
tdistr <- function(x, dfree, locv=0, scalev=1) {
  gamma((dfree+1)/2)/(sqrt(pi*dfree)*gamma(dfree/2)*scalev)*
    (1+((x-locv)/scalev)^2/dfree)^(-(dfree+1)/2)
}  # end tdistr
# Calculate vector of scale values
scalev <- c(0.5, 1.0, 2.0)
colorv <- c("blue", "black", "red")
labelv <- paste("scale", format(scalev, digits=2), sep="=")
# Plot three t-distributions
for (it in 1:3) {
  curve(expr=tdistr(x, dfree=3, scalev=scalev[it]), xlim=c(-3, 3),
xlab="", ylab="", lwd=2, col=colorv[it], add=(it>1))
}  # end for
# Add title
title(main="t-distributions with Different Scale Parameters", line=0.5)
# Add legend
legend("topright", inset=0.05, bty="n", title="Scale Parameters", labelv,
       cex=0.8, lwd=6, lty=1, col=colorv, y.intersp=0.4)
# Calculate VTI percentage returns
library(rutils)
retp <- as.numeric(na.omit(rutils::etfenv$returns$VTI))[1:499]
# Reduce number of output digits
ndigits <- options(digits=5)
# Shapiro-Wilk test for normal distribution
nrows <- NROW(retp)
shapiro.test(rnorm(nrows))
# Shapiro-Wilk test for VTI returns
shapiro.test(retp)
# Shapiro-Wilk test for uniform distribution
shapiro.test(runif(nrows))
# Restore output digits
options(digits=ndigits$digits)
library(tseries)  # Load package tseries
# Jarque-Bera test for normal distribution
jarque.bera.test(rnorm(nrows))
# Jarque-Bera test for VTI returns
jarque.bera.test(retp)
# Jarque-Bera test for uniform distribution
jarque.bera.test(runif(NROW(retp)))
# KS test for normal distribution
kstest <- ks.test(rnorm(100), pnorm)
kstest$p.value
# KS test for uniform distribution
ks.test(runif(100), pnorm)
# KS test for two shifted normal distributions
ks.test(rnorm(100), rnorm(100, mean=0.1))
ks.test(rnorm(100), rnorm(100, mean=1.0))
# KS test for two different normal distributions
ks.test(rnorm(100), rnorm(100, sd=2.0))
# KS test for VTI returns vs normal distribution
retp <- as.numeric(na.omit(rutils::etfenv$returns$VTI))
retp <- (retp - mean(retp))/sd(retp)
ks.test(retp, pnorm)
x11(width=6, height=5)
par(mar=c(2, 2, 2, 1), oma=c(1, 1, 1, 1))
# Degrees of freedom
degf <- c(2, 5, 8, 11)
# Plot four curves in loop
colorv <- c("red", "black", "blue", "green")
for (it in 1:4) {
  curve(expr=dchisq(x, df=degf[it]),
  xlim=c(0, 20), ylim=c(0, 0.3),
  xlab="", ylab="", col=colorv[it],
  lwd=2, add=as.logical(it-1))
}  # end for
# Add title
title(main="Chi-squared Distributions", line=0.5)
# Add legend
labelv <- paste("df", degf, sep="=")
legend("topright", inset=0.05, bty="n", y.intersp=0.4,
       title="Degrees of freedom", labelv,
       cex=0.8, lwd=6, lty=1, col=colorv)
# Observed frequencies from random normal data
histp <- hist(rnorm(1e3, mean=0), breaks=100, plot=FALSE)
countsn <- histp$counts
# Theoretical frequencies
countst <- rutils::diffit(pnorm(histp$breaks))
# Perform Chi-squared test for normal data
chisq.test(x=countsn, p=countst, rescale.p=TRUE, simulate.p.value=TRUE)
# Return p-value
chisqtest <- chisq.test(x=countsn, p=countst, rescale.p=TRUE, simulate.p.value=TRUE)
chisqtest$p.value
# Observed frequencies from shifted normal data
histp <- hist(rnorm(1e3, mean=2), breaks=100, plot=FALSE)
countsn <- histp$counts/sum(histp$counts)
# Theoretical frequencies
countst <- rutils::diffit(pnorm(histp$breaks))
# Perform Chi-squared test for shifted normal data
chisq.test(x=countsn, p=countst, rescale.p=TRUE, simulate.p.value=TRUE)
# Calculate histogram of VTI returns
histp <- hist(retp, breaks=100, plot=FALSE)
countsn <- histp$counts
# Calculate cumulative probabilities and then difference them
countst <- pt((histp$breaks-locv)/scalev, df=2)
countst <- rutils::diffit(countst)
# Perform Chi-squared test for VTI returns
chisq.test(x=countsn, p=countst, rescale.p=TRUE, simulate.p.value=TRUE)
# Objective function from function dt()
likefun <- function(par, dfree, datav) {
  -sum(log(dt(x=(datav-par[1])/par[2], df=dfree)/par[2]))
}  # end likefun
# Demonstrate equivalence with log(dt())
likefun(c(1, 0.5), 2, 2:5)
-sum(log(dt(x=(2:5-1)/0.5, df=2)/0.5))
# Objective function is negative log-likelihood
likefun <- function(par, dfree, datav) {
  sum(-log(gamma((dfree+1)/2)/(sqrt(pi*dfree)*gamma(dfree/2))) +
    log(par[2]) + (dfree+1)/2*log(1+((datav-par[1])/par[2])^2/dfree))
}  # end likefun
# Calculate VTI percentage returns
retp <- as.numeric(na.omit(rutils::etfenv$returns$VTI))
# Fit VTI returns using MASS::fitdistr()
fitobj <- MASS::fitdistr(retp, densfun="t", df=3)
summary(fitobj)
# Fitted parameters
fitobj$estimate
locv <- fitobj$estimate[1]
scalev <- fitobj$estimate[2]
locv; scalev
# Standard errors of parameters
fitobj$sd
# Log-likelihood value
fitobj$value
# Fit distribution using optim()
initp <- c(mean=0, scale=0.01)  # Initial parameters
fitobj <- optim(par=initp,
  fn=likefun, # Log-likelihood function
  datav=retp,
  dfree=3, # Degrees of freedom
  method="L-BFGS-B", # Quasi-Newton method
  upper=c(1, 0.1), # Upper constraint
  lower=c(-1, 1e-7)) # Lower constraint
# Optimal parameters
locv <- fitobj$par["mean"]
scalev <- fitobj$par["scale"]
locv; scalev
dev.new(width=6, height=5, noRStudioGD=TRUE)
# x11(width=6, height=5)
# Plot histogram of VTI returns
madv <- mad(retp)
histp <- hist(retp, col="lightgrey",
  xlab="returns", breaks=100, xlim=c(-5*madv, 5*madv),
  ylab="frequency", freq=FALSE, main="Histogram of VTI Returns")
lines(density(retp, adjust=1.5), lwd=3, col="blue")
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(retp),
  sd=sd(retp)), add=TRUE, lwd=3, col="green")
# Define non-standard t-distribution
tdistr <- function(x, dfree, locv=0, scalev=1) {
  dt((x-locv)/scalev, df=dfree)/scalev
}  # end tdistr
# Plot t-distribution function
curve(expr=tdistr(x, dfree=3, locv=locv, scalev=scalev), col="red", lwd=3, add=TRUE)
# Add legend
legend("topright", inset=0.05, bty="n", y.intersp=0.4,
  leg=c("density", "t-distr", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))
# Calculate sample from non-standard t-distribution with df=3
datat <- locv + scalev*rt(NROW(retp), df=3)
# KS test for VTI returns vs t-distribution data
ks.test(retp, datat)
# Q-Q plot of VTI Returns vs non-standard t-distribution
qqplot(datat, retp, xlab="t-Dist Quantiles", ylab="VTI Quantiles",
       main="Q-Q plot of VTI Returns vs Student's t-distribution")
# Calculate quartiles of the distributions
probs <- c(0.25, 0.75)
qrets <- quantile(retp, probs)
qtdata <- quantile(datat, probs)
# Calculate slope and plot line connecting quartiles
slope <- diff(qrets)/diff(qtdata)
intercept <- qrets[1]-slope*qtdata[1]
abline(intercept, slope, lwd=2, col="red")
# Plot log density of VTI returns
plot(density(retp, adjust=4), xlab="VTI Returns", ylab="Density",
     main="Fat Left Tail of VTI Returns (density in log scale)",
     type="l", lwd=3, col="blue", xlim=c(min(retp), -0.02), log="y")
# Plot t-distribution function
curve(expr=dt((x-locv)/scalev, df=3)/scalev, lwd=3, col="red", add=TRUE, log="y")
# Plot the Normal probability distribution
curve(expr=dnorm(x, mean=mean(retp), sd=sd(retp)), lwd=3, col="green", add=TRUE, log="y")
# Add legend
legend("topleft", inset=0.01, bty="n", y.intersp=c(0.25, 0.25, 0.25),
  legend=c("density", "t-distr", "normal"), y.intersp=0.4,
  lwd=6, lty=1, col=c("blue", "red", "green"))
# Calculate VTI returns and trading volumes
ohlc <- rutils::etfenv$VTI
closep <- drop(coredata(quantmod::Cl(ohlc)))
retp <- rutils::diffit(log(closep))
volumv <- coredata(quantmod::Vo(ohlc))
# Calculate trailing variance
lookb <- 121
varv <- HighFreq::roll_var_ohlc(log(ohlc), method="close", lookb=lookb, scale=FALSE)
varv[1:lookb, ] <- varv[lookb+1, ]
# Calculate trailing average volume
volumr <- HighFreq::roll_sum(volumv, lookb=lookb)/lookb
# dygraph plot of VTI variance and trading volumes
datav <- xts::xts(cbind(varv, volumr), zoo::index(ohlc))
colv <- c("variance", "volume")
colnames(datav) <- colv
dygraphs::dygraph(datav, main="VTI Variance and Trading Volumes") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], strokeWidth=2, axis="y", col="blue") %>%
  dySeries(name=colv[2], strokeWidth=2, axis="y2", col="red") %>%
  dyLegend(show="always", width=500)
# Scale the returns using volume clock to trading time
retsc <- ifelse(volumv > 0, sqrt(volumr)*retp/sqrt(volumv), 0)
retsc <- sd(retp)*retsc/sd(retsc)
# retsc <- ifelse(volumv > 1e4, retp/volumv, 0)
# Calculate moments of scaled returns
nrows <- NROW(retp)
sapply(list(retp=retp, retsc=retsc),
  function(rets) {sapply(c(skew=3, kurt=4),
     function(x) sum((rets/sd(rets))^x)/nrows)
})  # end sapply
# x11(width=6, height=5)
dev.new(width=6, height=5, noRStudioGD=TRUE)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
# Plot densities of SPY returns
madv <- mad(retp)
# bwidth <- mad(rutils::diffit(retp))
plot(density(retp, bw=madv/10), xlim=c(-5*madv, 5*madv),
     lwd=3, mgp=c(2, 1, 0), col="blue",
     xlab="returns (standardized)", ylab="frequency",
     main="Density of Volume-scaled VTI Returns")
lines(density(retsc, bw=madv/10), lwd=3, col="red")
curve(expr=dnorm(x, mean=mean(retp), sd=sd(retp)),
add=TRUE, lwd=3, col="green")
# Add legend
legend("topright", inset=0.05, bty="n", y.intersp=0.4,
  leg=c("unscaled", "scaled", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))
quartz.save("figure/vti_scaled.png", type="png", width=6, height=5)
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
perfstats <- unclass(data(
    package="PerformanceAnalytics"))$results[, -(1:2)]
apply(perfstats, 1, paste, collapse=" - ")
# Load "managers" data set
data(managers)
class(managers)
dim(managers)
head(managers, 3)
# Load package "PerformanceAnalytics"
library(PerformanceAnalytics)
# Calculate ETF returns
retp <- rutils::etfenv$returns[, c("VTI", "DBC", "IEF")]
retp <- na.omit(retp)
# Plot cumulative ETF returns
x11(width=6, height=5)
chart.CumReturns(retp, lwd=2, ylab="",
  legend.loc="topleft", main="ETF Cumulative Returns")
retp <- na.omit(rutils::etfenv$returns$VTI)
chart.Histogram(retp, xlim=c(-0.04, 0.04),
  colorset = c("lightgray", "red", "blue"), lwd=3,
  main=paste("Distribution of", colnames(retp), "Returns"),
  methods = c("add.density", "add.normal"))
legend("topright", inset=0.05, bty="n", y.intersp=0.4,
 leg=c("VTI Density", "Normal"),
 lwd=6, lty=1, col=c("red", "blue"))
retp <- rutils::etfenv$returns[,
  c("VTI", "IEF", "IVW", "VYM", "IWB", "DBC", "VXX")]
x11(width=6, height=5)
chart.Boxplot(names=FALSE, retp)
par(cex.lab=0.8, cex.axis=0.8)
axis(side=2, at=(1:NCOL(retp))/7.5-0.05,labels=colnames(retp))
# Simulate normally distributed data
nrows <- 1000
datav <- rnorm(nrows)
sd(datav)
mad(datav)
median(abs(datav - median(datav)))
median(abs(datav - median(datav)))/qnorm(0.75)
# Bootstrap of sd and mad estimators
bootd <- sapply(1:10000, function(x) {
  samplev <- datav[sample.int(nrows, replace=TRUE)]
  c(sd=sd(samplev), mad=mad(samplev))
})  # end sapply
bootd <- t(bootd)
# Analyze bootstrapped variance
head(bootd)
sum(is.na(bootd))
# Means and standard errors from bootstrap
apply(bootd, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
compclust <- makeCluster(ncores)  # Initialize compute cluster
bootd <- parLapply(compclust, 1:10000,
  function(x, datav) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, datav=datav)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:10000, function(x) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, mc.cores=ncores)  # end mclapply
stopCluster(compclust)  # Stop R processes over cluster
bootd <- rutils::do_call(rbind, bootd)
# Means and standard errors from bootstrap
apply(bootd, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
# Calculate VTI returns
retp <- na.omit(rutils::etfenv$returns$VTI)
nrows <- NROW(retp)
sd(retp)
mad(retp)
# Bootstrap of sd and mad estimators
bootd <- sapply(1:10000, function(x) {
  samplev <- retp[sample.int(nrows, replace=TRUE)]
  c(sd=sd(samplev), mad=mad(samplev))
})  # end sapply
bootd <- t(bootd)
# Means and standard errors from bootstrap
100*apply(bootd, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
# Parallel bootstrap under Windows
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
compclust <- makeCluster(ncores)  # Initialize compute cluster
clusterExport(compclust, c("nrows", "returns"))
bootd <- parLapply(compclust, 1:10000,
  function(x) {
    samplev <- retp[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  })  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:10000, function(x) {
    samplev <- retp[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, mc.cores=ncores)  # end mclapply
stopCluster(compclust)  # Stop R processes over cluster
bootd <- rutils::do_call(rbind, bootd)
# Means and standard errors from bootstrap
apply(bootd, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))
library(PerformanceAnalytics)
# Define target rate of return of 50 bps
targetr <- 0.005
# Calculate the full downside returns
retsub <- (retp - targetr)
retsub <- ifelse(retsub < 0, retsub, 0)
nrows <- NROW(retsub)
# Calculate the downside deviation
all.equal(sqrt(sum(retsub^2)/nrows),
  drop(DownsideDeviation(retp, MAR=targetr, method="full")))
# Calculate the subset downside returns
retsub <- (retp - targetr)
retsub <- retsub[retsub < 0]
nrows <- NROW(retsub)
# Calculate the downside deviation
all.equal(sqrt(sum(retsub^2)/nrows),
  drop(DownsideDeviation(retp, MAR=targetr, method="subset")))
# Calculate time series of VTI drawdowns
closep <- log(quantmod::Cl(rutils::etfenv$VTI))
drawdns <- (closep - cummax(closep))
# Extract the date index from the time series closep
datev <- zoo::index(closep)
# Calculate the drawdown trough date
indexm <- which.min(drawdns)
datem <- datev[indexm]
# Calculate the drawdown start and end dates
startd <- max(datev[(datev < datem) & (drawdns == 0)])
startd <- datev[which(startd==datev)+1] # Shift ahead by one day
endd <- min(datev[(datev > datem) & (drawdns == 0)])
# Calculate the drawdown depth
maxdd <- drawdns[datem]
# dygraph plot of VTI drawdowns
datav <- cbind(closep, drawdns)
colv <- c("VTI", "Drawdowns")
colnames(datav) <- colv
dygraphs::dygraph(datav, main="VTI Drawdowns") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2],
   valueRange=(1.2*range(drawdns)+0.1), independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue") %>%
  dySeries(name=colv[2], axis="y2", col="red") %>%
  dyEvent(startd, "start drawdown", col="blue") %>%
  dyEvent(datem, "max drawdown", col="red") %>%
  dyEvent(endd, "end drawdown", col="green")
# Plot VTI drawdowns using package quantmod
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
x11(width=6, height=5)
quantmod::chart_Series(x=closep, name="VTI Drawdowns", theme=plot_theme)
xval <- match(startd, datev)
yval <- max(closep)
abline(v=xval, col="blue")
text(x=xval, y=0.95*yval, "start drawdown", col="blue", cex=0.9)
xval <- match(datem, datev)
abline(v=xval, col="red")
text(x=xval, y=0.9*yval, "max drawdown", col="red", cex=0.9)
xval <- match(endd, datev)
abline(v=xval, col="green")
text(x=xval, y=0.85*yval, "end drawdown", col="green", cex=0.9)
library(xtable)
library(PerformanceAnalytics)
closep <- log(quantmod::Cl(rutils::etfenv$VTI))
retp <- rutils::diffit(closep)
# Calculate table of VTI drawdowns
tablev <- PerformanceAnalytics::table.Drawdowns(retp, geometric=FALSE)
# Convert dates to strings
tablev <- cbind(sapply(tablev[, 1:3], as.character), tablev[, 4:7])
# Print table of VTI drawdowns
print(xtable(tablev), comment=FALSE, size="tiny", include.rownames=FALSE)
library(xtable)
library(PerformanceAnalytics)
closep <- log(quantmod::Cl(rutils::etfenv$VTI))
retp <- rutils::diffit(closep)
# Calculate table of VTI drawdowns
tablev <- PerformanceAnalytics::table.Drawdowns(retp, geometric=FALSE)
# Convert dates to strings
tablev <- cbind(sapply(tablev[, 1:3], as.character), tablev[, 4:7])
# Print table of VTI drawdowns
print(xtable(tablev), comment=FALSE, size="tiny", include.rownames=FALSE)
# Load "managers" data set
data(managers)
charts.PerformanceSummary(ham1,
  main="", lwd=2, ylog=TRUE)
# Calculate VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
confl <- 0.1
varisk <- quantile(retp, confl)
cvar <- mean(retp[retp <= varisk])
# Plot histogram of VTI returns
x11(width=6, height=5)
par(mar=c(3, 2, 1, 0), oma=c(0, 0, 0, 0))
histp <- hist(retp, col="lightgrey",
  xlab="returns", ylab="frequency", breaks=100,
  xlim=c(-0.05, 0.01), freq=FALSE, main="VTI Returns Histogram")
# Calculate density
densv <- density(retp, adjust=1.5)
# Plot density
lines(densv, lwd=3, col="blue")
# Plot line for VaR
abline(v=varisk, col="red", lwd=3)
text(x=varisk, y=25, labels="VaR", lwd=2, pos=2)
# Plot polygon shading for CVaR
text(x=1.5*varisk, y=10, labels="CVaR", lwd=2, pos=2)
varmax <- -0.06
rangev <- (densv$x < varisk) &  (densv$x > varmax)
polygon(c(varmax, densv$x[rangev], varisk),
  c(0, densv$y[rangev], 0), col=rgb(1, 0, 0,0.5), border=NA)
# Calculate VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
nrows <- NROW(retp)
confl <- 0.05
# Calculate VaR approximately by sorting
sortv <- sort(as.numeric(retp))
cutoff <- round(confl*nrows)
varisk <- sortv[cutoff]
# Calculate VaR as quantile
varisk <- quantile(retp, probs=confl)
# PerformanceAnalytics VaR
PerformanceAnalytics::VaR(retp, p=(1-confl), method="historical")
all.equal(unname(varisk),
  as.numeric(PerformanceAnalytics::VaR(retp,
  p=(1-confl), method="historical")))
# Calculate VaR as quantile
varisk <- quantile(retp, confl)
# Calculate CVaR as expected loss
cvar <- mean(retp[retp <= varisk])
# PerformanceAnalytics VaR
PerformanceAnalytics::ETL(retp, p=(1-confl), method="historical")
all.equal(unname(cvar),
  as.numeric(PerformanceAnalytics::ETL(retp,
    p=(1-confl), method="historical")))
# Calculate the risk-return statistics
riskstats <-
  PerformanceAnalytics::table.Stats(rutils::etfenv$returns)
class(riskstats)
# Transpose the data frame
riskstats <- as.data.frame(t(riskstats))
# Add Name column
riskstats$Name <- rownames(riskstats)
# Add Sharpe ratio column
riskstats$"Arithmetic Mean" <-
  sapply(rutils::etfenv$returns, mean, na.rm=TRUE)
riskstats$Sharpe <-
  sqrt(252)*riskstats$"Arithmetic Mean"/riskstats$Stdev
# Sort on Sharpe ratio
riskstats <- riskstats[order(riskstats$Sharpe, decreasing=TRUE), ]
# Copy from rutils to save time
riskstats <- rutils::etfenv$riskstats
# Add Sharpe ratio column
# riskstats$Sharpe <- riskstats$"Arithmetic Mean"/riskstats$Stdev
# Sort on Sharpe ratio
riskstats <- riskstats[order(riskstats$Sharpe, decreasing=TRUE), ]
# Print data frame
knitr::kable(riskstats[, c("Sharpe", "Skewness", "Kurtosis")])
# Print data frame
knitr::kable(riskstats[c("VXX", "SVXY"), c("Sharpe", "Skewness", "Kurtosis")])
# dygraph plot of VXX versus SVXY
pricev <- na.omit(rutils::etfenv$prices[, c("VXX", "SVXY")])
pricev <- pricev["2017/"]
colv <- c("VXX", "SVXY")
colnames(pricev) <- colv
dygraphs::dygraph(pricev, main="Prices of VXX and SVXY") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="green") %>%
  dyLegend(show="always", width=300) %>% dyLegend(show="always", width=300) %>%
  dyLegend(show="always", width=300)
# Remove VIX volatility ETF data
riskstats <- riskstats[-match(c("VXX", "SVXY"), riskstats$Name), ]
# Plot scatterplot of Sharpe vs Skewness
plot(Sharpe ~ Skewness, data=riskstats,
     ylim=1.1*range(riskstats$Sharpe),
     main="Sharpe vs Skewness")
# Add labels
text(x=riskstats$Skewness, y=riskstats$Sharpe,
    labels=riskstats$Name, pos=3, cex=0.8)
# Plot scatterplot of Kurtosis vs Skewness
x11(width=6, height=5)
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
plot(Kurtosis ~ Skewness, data=riskstats,
     ylim=c(1, max(riskstats$Kurtosis)),
     main="Kurtosis vs Skewness")
# Add labels
text(x=riskstats$Skewness, y=riskstats$Kurtosis,
    labels=riskstats$Name, pos=1, cex=0.5)
#Below is for ETFs
# Sort on Sharpe ratio
riskstats <- riskstats[order(riskstats$Skewness, decreasing=TRUE), ]
# Select high skew and low skew ETFs
cutoff <- (NROW(riskstats) %/% 2)
high_skew <- riskstats$Name[1:cutoff]
low_skew <- riskstats$Name[(cutoff+1):NROW(riskstats)]
# Calculate returns and log prices
retp <- rutils::etfenv$returns
retp <- zoo::na.locf(retp, na.rm=FALSE)
retp[is.na(retp)] <- 0
sum(is.na(retp))
high_skew <- rowMeans(retp[, high_skew])
low_skew <- rowMeans(retp[, low_skew])
wealthv <- cbind(high_skew, low_skew)
wealthv <- xts::xts(wealthv, zoo::index(retp))
wealthv <- cumsum(wealthv)
# dygraph plot of high skew and low skew ETFs
colv <- colnames(wealthv)
dygraphs::dygraph(wealthv, main="Log Wealth of Low and High Skew ETFs") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="green") %>%
  dyLegend(show="always", width=300)
#Below is for S&P500 constituent stocks
# calc_mom() calculates the moments of returns
calc_mom <- function(retp, moment=3) {
  retp <- na.omit(retp)
  sum(((retp - mean(retp))/sd(retp))^moment)/NROW(retp)
}  # end calc_mom
# Calculate skew and kurtosis of VTI returns
calc_mom(retp, moment=3)
calc_mom(retp, moment=4)
# Load the S&P500 constituent stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
dim(retp)
sum(is.na(retp))
# retp <- retp["2000/"]
skews <- sapply(retp, calc_mom, moment=3)
# skews <- sapply(retp, calc_mom, moment=4)
# skews <- sapply(retp, sd, na.rm=TRUE)
skews <- sort(skews)
namev <- names(skews)
nrows <- NROW(namev)
# Select high skew and low skew ETFs
cutoff <- NROW(riskstats %/% 2)
low_skew <- namev[1:cutoff]
high_skew <- namev[(cutoff+1):nrows]
# low_skew <- namev[1:50]
# Calculate returns and log prices
low_skew <- rowMeans(retp[, low_skew], na.rm=TRUE)
low_skew[1] <- 0
high_skew <- rowMeans(retp[, high_skew], na.rm=TRUE)
high_skew[1] <- 0
wealthv <- cbind(high_skew, low_skew)
wealthv <- xts::xts(wealthv, zoo::index(retp))
wealthv <- cumsum(wealthv)
# dygraph plot of high skew and low skew ETFs
colv <- colnames(wealthv)
dygraphs::dygraph(wealthv, main="Log Wealth of Low and High Skew Stocks") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="green") %>%
  dyLegend(show="always", width=300)
library(PerformanceAnalytics)
retp <- rutils::etfenv$returns[, c("VTI", "IEF")]
retp <- na.omit(retp)
# Calculate the Sharpe ratio
confl <- 0.05
PerformanceAnalytics::SharpeRatio(retp, p=(1-confl),
  method="historical")
# Calculate the Sortino ratio
PerformanceAnalytics::SortinoRatio(retp)
# Calculate the Calmar ratio
PerformanceAnalytics::CalmarRatio(retp)
# Calculate the Dowd ratio
PerformanceAnalytics::SharpeRatio(retp, FUN="VaR",
  p=(1-confl), method="historical")
# Calculate the Dowd ratio from scratch
varisk <- sapply(retp, quantile, probs=confl)
-sapply(retp, mean)/varisk
# Calculate the Conditional Dowd ratio
PerformanceAnalytics::SharpeRatio(retp, FUN="ES",
  p=(1-confl), method="historical")
# Calculate the Conditional Dowd ratio from scratch
cvar <- sapply(retp, function(x) {
  mean(x[x < quantile(x, confl)])
})
-sapply(retp, mean)/cvar
# Calculate VTI daily log returns
pricev <- log(drop(coredata(na.omit(rutils::etfenv$prices$VTI))))
retp <- rutils::diffit(pricev)
nrows <- NROW(retp)
# Calculate VTI monthly log returns
holdp <- 22 # Holding period in days
pricem <- pricev[rutils::calc_endpoints(pricev, holdp)]
retm <- rutils::diffit(pricem)
retm <- retm[-1] # Drop the first zero return
# Calculate the mean, standard deviation, skewness, and kurtosis
datav <- list(retp, retm)
names(datav) <- c("Daily", "Monthly")
do.call(cbind, lapply(datav, function(x) {
  # Standardize the returns
  meanv <- mean(x); stdev <- sd(x); x <- (x - meanv)/stdev
  c(mean=meanv, stdev=stdev, skew=mean(x^3), kurt=mean(x^4))
}))  # end lapply
# Calculate the Sharpe and Dowd ratios
do.call(cbind, lapply(datav, function(x) {
  meanv <- mean(x); stdev <- sd(x)
  varisk <- unname(quantile(x, probs=0.02))
  cvar <- mean(x[x < varisk])
  # Annualize the ratios
  sqrt(252*NROW(x)/nrows)*mean(x)/c(Sharpe=stdev, Dowd=-varisk, DowdC=-cvar)
}))  # end lapply
# Plot the density of monthly returns
plot(density(retm), t="l", lwd=3, col="blue",
     xlab="returns", ylab="density", xlim=c(-4*mad(retm), 4*mad(retm)),
     main="Distribution of Monthly VTI Returns")
curve(expr=dnorm(x, mean=mean(retm), sd=sd(retm)), col="green", lwd=3, add=TRUE)
legend("topright", legend=c("Monthly", "Normal"), y.intersp=0.4, cex=1.1,
 inset=0.0, bg="white", lty=1, lwd=6, col=c("blue", "green"), bty="n")
# Number of flights from each airport
dtable[, .N, by=origin]
# Same, but add names to output
dtable[, .(flights=.N), by=.(airport=origin)]
# Number of AA flights from each airport
dtable[carrier=="AA", .(flights=.N),
     by=.(airport=origin)]
# Number of flights from each airport and airline
dtable[, .(flights=.N),
     by=.(airport=origin, airline=carrier)]
# Average aircraft_delay
dtable[, mean(aircraft_delay)]
# Average aircraft_delay from JFK
dtable[origin=="JFK", mean(aircraft_delay)]
# Average aircraft_delay from each airport
dtable[, .(delay=mean(aircraft_delay)),
     by=.(airport=origin)]
# Average and max delays from each airport and month
dtable[, .(mean_delay=mean(aircraft_delay), max_delay=max(aircraft_delay)),
     by=.(airport=origin, month=month)]
# Average and max delays from each airport and month
dtable[, .(mean_delay=mean(aircraft_delay), max_delay=max(aircraft_delay)),
     keyby=.(airport=origin, month=month)]
# Extract log VTI prices
ohlc <- log(rutils::etfenv$VTI)
closep <- quantmod::Cl(ohlc)
colnames(closep) <- "VTI"
nrows <- NROW(closep)
# Inspect the R code of the function filter()
filter
# Calculate EMA weights
lookb <- 21
weightv <- exp(-0.1*1:lookb)
weightv <- weightv/sum(weightv)
# Calculate convolution using filter()
pricef <- filter(closep, filter=weightv, method="convolution", sides=1)
# filter() returns time series of class "ts"
class(pricef)
# Get information about C_cfilter()
getAnywhere(C_cfilter)
# Filter using C_cfilter() over past values (sides=1).
priceff <- .Call(stats:::C_cfilter, closep, filter=weightv,
               sides=1, circular=FALSE)
all.equal(as.numeric(pricef), priceff, check.attributes=FALSE)
# Calculate EMA prices using HighFreq::roll_conv()
pricecpp <- HighFreq::roll_conv(closep, weightv=weightv)
all.equal(priceff[-(1:lookb)], as.numeric(pricecpp)[-(1:lookb)])
# Benchmark speed of trailing calculations
library(microbenchmark)
summary(microbenchmark(
  filter=filter(closep, filter=weightv, method="convolution", sides=1),
  priceff=.Call(stats:::C_cfilter, closep, filter=weightv, sides=1, circular=FALSE),
  rcpp=HighFreq::roll_conv(closep, weightv=weightv)
  ), times=10)[, c(1, 4, 5)]
# Simulate AR process using filter()
nrows <- NROW(closep)
# Calculate AR coefficients and innovations
coeff <- matrix(weightv)/4
ncoeff <- NROW(coeff)
innov <- matrix(rnorm(nrows))
arimav <- filter(x=innov, filter=coeff, method="recursive")
# Get information about C_rfilter()
getAnywhere(C_rfilter)
# Filter using C_rfilter() compiled C++ function directly
arimafast <- .Call(stats:::C_rfilter, innov, coeff,
              double(ncoeff + nrows))
all.equal(as.numeric(arimav), arimafast[-(1:ncoeff)],
    check.attributes=FALSE)
# Filter using C++ code
arimacpp <- HighFreq::sim_ar(coeff, innov)
all.equal(arimafast[-(1:ncoeff)], drop(arimacpp))
# Benchmark speed of the three methods
summary(microbenchmark(
  filter=filter(x=innov, filter=coeff, method="recursive"),
  priceff=.Call(stats:::C_rfilter, innov, coeff, double(ncoeff + nrows)),
  Rcpp=HighFreq::sim_ar(coeff, innov)
  ), times=10)[, c(1, 4, 5)]
# Extract log VTI prices
closep <- log(na.omit(rutils::etfenv$prices$VTI))
retp <- rutils::diffit(closep)
nrows <- NROW(closep)
# Calculate EMA prices using HighFreq::run_mean()
pricema <- HighFreq::run_mean(closep, lambda=0.9)
# Combine prices with EMA prices
pricev <- cbind(closep, pricema)
colnames(pricev)[2] <- "VTI EMA"
# Calculate standard deviations of returns
sapply(rutils::diffit(pricev), sd)
# Plot dygraph
dygraphs::dygraph(pricev["2009"], main="VTI Prices and EMA Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
library(rutils)  # Load package rutils
library(ggplot2)  # Load ggplot2
library(gridExtra)  # Load gridExtra
# Coerce to zoo and merge the time series
pricef <- cbind(closep, pricef)
colnames(pricef) <- c("VTI", "VTI filtered")
# Plot ggplot2
autoplot(pricef["2008/2010"],
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
# Calculate VTI log returns
retf <- rutils::diffit(closef)
# Open plot window
x11(width=6, height=7)
# Set plot parameters
par(oma=c(1, 1, 0, 1), mar=c(1, 1, 1, 1), mgp=c(0, 0.5, 0),
    cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Set two plot panels
par(mfrow=c(2,1))
# Plot ACF of VTI returns
rutils::plot_acf(retf[, 1], lag=10, xlab="")
title(main="ACF of VTI Returns", line=-1)
# Plot ACF of smoothed VTI returns
rutils::plot_acf(retf[, 2], lag=10, xlab="")
title(main="ACF of Smoothed VTI Returns", line=-1)
# Calculate the EMA gains and losses
lambdaf <- 0.9
gainm <- HighFreq::run_mean(ifelse(retp > 0, retp, 0), lambdaf)
lossm <- HighFreq::run_mean(ifelse(retp < 0, -retp, 0), lambdaf)
# Calculate the RSI indicator
rsii <- 100 * gainm/(gainm + lossm)
# Plot dygraph of the RSI indicator
datav <- cbind(closep, rsii)
colnames(datav)[2] <- "RSI"
colv <- colnames(datav)
dygraphs::dygraph(datav["2020"], main="RSI Indicator for VTI") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Extract log VTI prices
ohlc <- rutils::etfenv$VTI
datev <- zoo::index(ohlc)
closep <- log(quantmod::Cl(ohlc))
colnames(closep) <- "VTI"
nrows <- NROW(closep)
# Calculate EMA weights
lookb <- 111
lambdaf <- 0.9
weightv <- lambdaf^(1:lookb)
weightv <- weightv/sum(weightv)
# Calculate EMA prices as the convolution
pricema <- HighFreq::roll_sumw(closep, weightv=weightv)
pricev <- cbind(closep, pricema)
colnames(pricev) <- c("VTI", "VTI EMA")
# Dygraphs plot with custom line colors
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main="VTI EMA Prices") %>%
  dySeries(name=colv[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colv[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Standard plot of  EMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
colorv <- c("blue", "red")
plot_theme$col$line.col <- colors
quantmod::chart_Series(pricev["2008/2009"], theme=plot_theme,
       lwd=2, name="VTI EMA Prices")
legend("topleft", legend=colnames(pricev), y.intersp=0.4,
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Calculate EMA prices recursively using C++ code
emar <- .Call(stats:::C_rfilter, closep, lambdaf, c(as.numeric(closep[1])/(1-lambdaf), double(NROW(closep))))[-1]
# Or R code
# emar <- filter(closep, filter=lambdaf, init=as.numeric(closep[1, 1])/(1-lambdaf), method="recursive")
emar <- (1-lambdaf)*emar
# Calculate EMA prices recursively using RcppArmadillo C++
pricema <- HighFreq::run_mean(closep, lambda=lambdaf)
all.equal(drop(pricema), emar)
# Compare the speed of C++ code with RcppArmadillo C++
library(microbenchmark)
summary(microbenchmark(
  filtercpp=HighFreq::run_mean(closep, lambda=lambdaf),
  rfilter=.Call(stats:::C_rfilter, closep, lambdaf, c(as.numeric(closep[1])/(1-lambdaf), double(NROW(closep)))),
  times=10))[, c(1, 4, 5)]
# Dygraphs plot with custom line colors
pricev <- cbind(closep, pricema)
colnames(pricev) <- c("VTI", "VTI EMA")
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main="Recursive VTI EMA Prices") %>%
  dySeries(name=colv[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colv[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Standard plot of  EMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
colorv <- c("blue", "red")
plot_theme$col$line.col <- colors
quantmod::chart_Series(pricev["2008/2009"], theme=plot_theme,
       lwd=2, name="VTI EMA Prices")
legend("topleft", legend=colnames(pricev),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Calculate log OHLC prices and volumes
volumv <- quantmod::Vo(ohlc)
colnames(volumv) <- "Volume"
nrows <- NROW(closep)
# Calculate the VWAP prices
lookb <- 21
vwap <- HighFreq::roll_sum(closep, lookb=lookb, weightv=volumv)
colnames(vwap) <- "VWAP"
pricev <- cbind(closep, vwap)
# Dygraphs plot with custom line colors
colorv <- c("blue", "red")
dygraphs::dygraph(pricev["2008/2009"], main="VTI VWAP Prices") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Plot VWAP prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(pricev["2008/2009"], theme=plot_theme,
       lwd=2, name="VTI VWAP Prices")
legend("bottomright", legend=colnames(pricev),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Calculate VWAP prices recursively using C++ code
lambdaf <- 0.9
volumer <- .Call(stats:::C_rfilter, volumv, lambdaf, c(as.numeric(volumv[1])/(1-lambdaf), double(NROW(volumv))))[-1]
pricer <- .Call(stats:::C_rfilter, volumv*closep, lambdaf, c(as.numeric(volumv[1]*closep[1])/(1-lambdaf), double(NROW(closep))))[-1]
vwapr <- pricer/volumer
# Calculate VWAP prices recursively using RcppArmadillo C++
vwapc <- HighFreq::run_mean(closep, lambda=lambdaf, weightv=volumv)
all.equal(vwapr, drop(vwapc))
# Dygraphs plot the VWAP prices
pricev <- xts(cbind(vwap, vwapr), zoo::index(ohlc))
colnames(pricev) <- c("VWAP trailing", "VWAP recursive")
dygraphs::dygraph(pricev["2008/2009"], main="VWAP Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate fast and slow EMA prices
lambdaf <- 0.8 # Fast EMA
emaf <- HighFreq::run_mean(closep, lambda=lambdaf)
lambdas <- 0.9 # Slow EMAs
emas <- HighFreq::run_mean(closep, lambda=lambdas)
# Calculate VTI prices
emad <- (emaf - emas)
pricev <- cbind(closep, emad)
symboln <- "VTI"
colnames(pricev) <- c(symboln, paste(symboln, "Returns"))
# Plot dygraph of VTI Returns
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main=paste(symboln, "EMA Returns")) %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Calculate fast and slow EMA prices
lambdaf <- 1 - 2/(1+12) # 12-day fast EMA
lambdas <- 1 - 2/(1+26) # 26-day slow EMA
emaf <- HighFreq::run_mean(closep, lambda=lambdaf)
emas <- HighFreq::run_mean(closep, lambda=lambdas)
# Plot dygraph of the fast and slow EMA prices
pricev <- cbind(log(ohlc[, 1:4]), emaf, emas)["2020-01/2020-05"]
colnames(pricev)[5:6] <- c("EMA fast", "EMA slow")
colv <- colnames(pricev)
dygraphs::dygraph(pricev, main="MACD EMA Prices") %>%
  dygraphs::dyCandlestick() %>%
  dySeries(name=colv[5], strokeWidth=2, col="red") %>%
  dySeries(name=colv[6], strokeWidth=2, col="purple") %>%
  dyLegend(show="always", width=300)
# Calculate the MACD series
macds <- emaf - emas
# Calculate the signal series
lambdasig <- 1 - 2/(1+9) # 9-day fast EMA
macdsig <- HighFreq::run_mean(macds, lambda=lambdasig)
# Calculate the divergence series
macddiv <- macds - macdsig
# Plot dygraph of the signal and divergence series
pricev <- cbind(macds, macdsig, macddiv)
pricev <- xts::xts(pricev, datev)
colnames(pricev) <- c("MACD", "Sig", "Div")
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2020-01/2020-05"], main="MACD Signal and Divergence") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="red") %>%
  dySeries(name=colv[2], axis="y", strokeWidth=2, col="purple") %>%
  dyBarSeries(name=colv[3], axis="y2", col="blue") %>%
  dyLegend(show="always", width=300)
# Calculate the fractional weights
lookb <- 21
deltav <- 0.1
weightv <- (deltav - 0:(lookb-2)) / 1:(lookb-1)
weightv <- (-1)^(1:(lookb-1))*cumprod(weightv)
weightv <- c(1, weightv)
weightv <- (weightv - mean(weightv))
# Calculate the fractional VTI returns
retf <- HighFreq::roll_conv(closep, weightv=weightv)
pricev <- cbind(closep, retf)
symboln <- "VTI"
colnames(pricev) <- c(symboln, paste(symboln, "Returns"))
# Plot dygraph of VTI Returns
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2008-08/2009-08"], main=paste(symboln, "Fractional Returns")) %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Perform ADF test for prices
tseries::adf.test(closep)
# Perform ADF test for returns
tseries::adf.test(retp)
# Calculate fractional VTI returns
deltav <- 0.1*c(1, 3, 5, 7, 9)
retfrac <- lapply(deltav, function(deltav) {
  weightv <- (deltav - 0:(lookb-2)) / 1:(lookb-1)
  weightv <- c(1, (-1)^(1:(lookb-1))*cumprod(weightv))
  weightv <- (weightv - mean(weightv))
  HighFreq::roll_conv(closep, weightv=weightv)
})  # end lapply
retfrac <- do.call(cbind, retfrac)
retfrac <- cbind(closep, retfrac)
colnames(retfrac) <- c("VTI", paste0("frac_", deltav))
# Calculate ADF test statistics
adfstats <- sapply(retfrac, function(x)
  suppressWarnings(tseries::adf.test(x)$statistic)
)  # end sapply
names(adfstats) <- colnames(retfrac)
# Plot dygraph of fractional VTI returns
colorv <- colorRampPalette(c("blue", "red"))(NCOL(retfrac))
colv <- colnames(retfrac)
dyplot <- dygraphs::dygraph(retfrac["2008-08/2009-08"], main="Fractional Returns") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col=colorv[1])
for (i in 2:NROW(colv))
  dyplot <- dyplot %>%
  dyAxis("y2", label=colv[i], independentTicks=TRUE) %>%
  dySeries(name=colv[i], axis="y2", strokeWidth=2, col=colorv[i])
dyplot <- dyplot %>% dyLegend(width=300)
dyplot
# Calculate volume z-scores
volumv <- quantmod::Vo(ohlc)
lookb <- 21
volumean <- HighFreq::roll_mean(volumv, lookb=lookb)
volumsd <- sqrt(HighFreq::roll_var(rutils::diffit(volumv), lookb=lookb))
volumsd[1] <- 0
volumz <- ifelse(volumsd > 0, (volumv - volumean)/volumsd, 0)
# Plot histogram of volume z-scores
hist(volumz, breaks=1e2)
# Plot dygraph of volume z-scores of VTI prices
pricev <- cbind(closep, volumz)
colnames(pricev) <- c("VTI", "Z-Scores")
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main="VTI Volume Z-Scores") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Calculate volatility (true range) z-scores
volv <- log(quantmod::Hi(ohlc) - quantmod::Lo(ohlc))
lookb <- 21
volatm <- HighFreq::roll_mean(volv, lookb=lookb)
volv <- (volv - volatm)
volatsd <- sqrt(HighFreq::roll_var(rutils::diffit(volv), lookb=lookb))
volatsd[1] <- 0
volatz <- ifelse(volatsd > 0, volv/volatsd, 0)
# Plot histogram of the volatility z-scores
hist(volatz, breaks=1e2)
# Plot scatterplot of volume and volatility z-scores
plot(as.numeric(volatz), as.numeric(volumz),
     xlab="volatility z-score", ylab="volume z-score")
regmod <- lm(volatz ~ volumz)
abline(regmod, col="red", lwd=3)
# Plot dygraph of VTI volatility z-scores
pricev <- cbind(closep, volatz)
colnames(pricev) <- c("VTI", "Z-Scores")
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main="VTI Volatility Z-Scores") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Calculate the recursive trailing VTI volatility
lambdaf <- 0.8 # Fast lambda
lambdas <- 0.81 # Slow lambda
volatf <- sqrt(HighFreq::run_var(retp, lambda=lambdaf)[, 2])
volats <- sqrt(HighFreq::run_var(retp, lambda=lambdas)[, 2])
# Calculate the recursive trailing z-scores of VTI volatility
volatd <- volatf - volats
volatsd <- sqrt(HighFreq::run_var(rutils::diffit(volatd), lambda=lambdaf)[, 2])
volatsd[1] <- 0
volatz <- ifelse(volatsd > 0, volatd/volatsd, 0)
# Plot histogram of the volatility z-scores
hist(volatz, breaks=1e2)
# Plot scatterplot of volume and volatility z-scores
plot(as.numeric(volatz), as.numeric(volumz),
     xlab="volatility z-score", ylab="volume z-score")
regmod <- lm(volatz ~ volumz)
abline(regmod, col="red", lwd=3)
# Plot dygraph of VTI volatility z-scores
pricev <- cbind(closep, volatz)
colnames(pricev) <- c("VTI", "Z-Scores")
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main="VTI Online Volatility Z-Scores") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Calculate the centered volatility
lookb <- 21
halfb <- lookb %/% 2
volv <- HighFreq::roll_var(closep, lookb=lookb)
volv <- sqrt(volv)
volv <- rutils::lagit(volv, lagg=(-halfb))
# Calculate the z-scores of prices
pricez <- (closep -
  0.5*(rutils::lagit(closep, halfb, pad_zeros=FALSE) +
  rutils::lagit(closep, -halfb, pad_zeros=FALSE)))
pricez <- ifelse(volv > 0, pricez/volv, 0)
# Plot dygraph of z-scores of VTI prices
pricev <- cbind(closep, pricez)
colnames(pricev) <- c("VTI", "Z-Scores")
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2009"], main="VTI Centered Price Z-Scores") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Calculate the thresholds for labeling tops and bottoms
confl <- c(0.2, 0.8)
threshv <- quantile(pricez, confl)
# Calculate the vectors of tops and bottoms
topl <- zoo::coredata(pricez > threshv[2])
bottoml <- zoo::coredata(pricez < threshv[1])
# Simulate in-sample VTI strategy
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv[topl] <- (-1)
posv[bottoml] <- 1
posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv)
pnls <- retp*posv
# Plot dygraph of in-sample VTI strategy
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
  main="Price Tops and Bottoms Strategy In-sample") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="Strategy", independentTicks=TRUE) %>%
  dySeries(name="VTI", axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name="Strategy", axis="y2", strokeWidth=2, col="red")
# Calculate the trailing VTI volatility
volv <- HighFreq::roll_var(closep, lookb=lookb)
volv <- sqrt(volv)
# Calculate the trailing z-scores of VTI prices
pricez <- (closep - rutils::lagit(closep, lookb, pad_zeros=FALSE))
pricez <- ifelse(volv > 0, pricez/volv, 0)
# Plot dygraph of the trailing z-scores of VTI prices
pricev <- cbind(closep, pricez)
colnames(pricev) <- c("VTI", "Z-Score")
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2009"],
  main="VTI Trailing Price Z-Scores") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(axis="y", label=colv[1], strokeWidth=2, col="blue") %>%
  dySeries(axis="y2", label=colv[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Calculate the EMA returns and volatilities
lambdaf <- 0.9
volv <- HighFreq::run_var(closep, lambda=lambdaf)
# Calculate the recursive trailing z-scores of VTI prices
pricer <- (closep - volv[, 1])
pricer <- ifelse(volv > 0, pricer/volv, 0)
volv <- sqrt(volv[, 2])
# Plot dygraph of the trailing z-scores of VTI prices
pricev <- xts::xts(cbind(pricez, pricer), datev)
colnames(pricev) <- c("Z-Scores", "Recursive")
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2009"], main="VTI Online Trailing Price Z-Scores") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate trailing price regression z-scores
datev <- matrix(zoo::index(closep))
lookb <- 21
# Create a default list of regression parameters
controll <- HighFreq::param_reg()
regs <- HighFreq::roll_reg(respv=closep, predm=datev,
   lookb=lookb, controll=controll)
regs[1:lookb, ] <- 0
# Plot dygraph of z-scores of VTI prices
datav <- cbind(closep, regs[, NCOL(regs)])
colnames(datav) <- c("VTI", "Z-Scores")
colv <- colnames(datav)
dygraphs::dygraph(datav["2009"], main="VTI Regression Z-Scores") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Calculate recursive trailing price regression versus time
lambdaf <- 0.9
# Create a list of regression parameters
controll <- HighFreq::param_reg(residscale="standardize")
regs <- HighFreq::run_reg(closep, matrix(datev), lambda=lambdaf, controll=controll)
colnames(regs) <- c("alpha", "beta", "zscores")
tail(regs)
# Plot dygraph of regression betas
datav <- cbind(closep, 252*regs[, "beta"])
colnames(datav) <- c("VTI", "Slope")
colv <- colnames(datav)
dygraphs::dygraph(datav["2009"], main="VTI Online Regression Slope") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(axis="y", label=colv[1], strokeWidth=2, col="blue") %>%
  dySeries(axis="y2", label=colv[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Plot dygraph of z-scores of VTI prices
datav <- cbind(closep, regs[, "zscores"])
colnames(datav) <- c("VTI", "Z-Scores")
colv <- colnames(datav)
dygraphs::dygraph(datav["2009"], main="VTI Online Regression Z-Scores") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
# Extract time series of VTI log prices
closep <- log(na.omit(rutils::etfenv$prices$VTI))
# Define look-back window
lookb <- 11
# Calculate time series of trailing medians
medianv <- HighFreq::roll_mean(closep, lookb=lookb, method="nonparametric")
# Calculate time series of MAD
madv <- HighFreq::roll_var(closep, lookb=lookb, method="nonparametric")
# madv <- TTR::runMAD(closep, n=lookb)
# Calculate time series of z-scores
zscores <- (closep - medianv)/madv
zscores[1:lookb, ] <- 0
tail(zscores, lookb)
range(zscores)
x11(width=6, height=5)
# Plot the prices and medians
dygraphs::dygraph(cbind(closep, medianv), main="VTI median") %>%
  dyOptions(colors=c("black", "red")) %>%
  dyLegend(show="always", width=300)
# Plot histogram of z-scores
histp <- hist(zscores, col="lightgrey",
  xlab="z-scores", breaks=50, xlim=c(-4, 4),
  ylab="frequency", freq=FALSE, main="Hampel Z-Scores histogram")
lines(density(zscores, adjust=1.5), lwd=3, col="blue")
# Calculate one-sided Hampel z-scores
medianv <- HighFreq::roll_mean(closep, lookb=lookb, method="nonparametric")
madv <- HighFreq::roll_var(closep, lookb=lookb, method="nonparametric")
zscores <- (closep - medianv)/madv
zscores[1:lookb, ] <- 0
tail(zscores, lookb)
range(zscores)
# Calculate two-sided Hampel z-scores
halfb <- lookb %/% 2
medianv <- rutils::lagit(medianv, lagg=(-halfb))
madv <- rutils::lagit(madv, lagg=(-halfb))
zscores <- (closep - medianv)/madv
zscores[1:lookb, ] <- 0
tail(zscores, lookb)
range(zscores)
# Calculate VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
nrows <- NROW(retp)
# Define end points
endd <- 1:NROW(retp)
# Start points are multi-period lag of endd
lookb <- 11
startp <- c(rep_len(0, lookb-1), endd[1:(nrows-lookb+1)])
# Calculate trailing variance in sapply() loop - takes long
varv <- sapply(1:nrows, function(it) {
  retp <- retp[startp[it]:endd[it]]
  sum((retp - mean(retp))^2)/lookb
})  # end sapply
# Use only vectorized functions
retc <- cumsum(retp)
retc <- (retc - c(rep_len(0, lookb), retc[1:(nrows-lookb)]))
retc2 <- cumsum(retp^2)
retc2 <- (retc2 - c(rep_len(0, lookb), retc2[1:(nrows-lookb)]))
var2 <- (retc2 - retc^2/lookb)/lookb
all.equal(varv[-(1:lookb)], as.numeric(var2)[-(1:lookb)])
# Or using package rutils
retc <- rutils::roll_sum(retp, lookb=lookb)
retc2 <- rutils::roll_sum(retp^2, lookb=lookb)
var2 <- (retc2 - retc^2/lookb)/lookb
# Coerce variance into xts
tail(varv)
class(varv)
varv <- xts(varv, order.by=zoo::index(retp))
colnames(varv) <- "VTI.variance"
head(varv)
# Calculate trailing VTI variance using package HighFreq
varv <- roll::roll_var(retp, width=lookb)
colnames(varv) <- "Variance"
head(varv)
sum(is.na(varv))
varv[1:(lookb-1)] <- 0
# Benchmark calculation of trailing variance
library(microbenchmark)
summary(microbenchmark(
  sapply=sapply(1:nrows, function(it) {
    var(retp[startp[it]:endd[it]])
  }),
  roll=roll::roll_var(retp, width=lookb),
  times=10))[, c(1, 4, 5)]
# Calculate EMA VTI variance using compiled C++ function
lookb <- 51
weightv <- exp(-0.1*1:lookb)
weightv <- weightv/sum(weightv)
varv <- .Call(stats:::C_cfilter, retp^2, filter=weightv, sides=1, circular=FALSE)
varv[1:(lookb-1)] <- varv[lookb]
# Plot EMA volatility
varv <- xts:::xts(sqrt(varv), order.by=zoo::index(retp))
dygraphs::dygraph(varv, main="VTI EMA Volatility") %>%
  dyOptions(colors="blue") %>% dyLegend(show="always", width=300)
quantmod::chart_Series(xtsv, name="VTI EMA Volatility")
# Calculate trailing VTI variance using package roll
library(roll)  # Load roll
varv <- roll::roll_var(retp, weights=rev(weightv), width=lookb)
colnames(varv) <- "VTI.variance"
class(varv)
head(varv)
sum(is.na(varv))
varv[1:(lookb-1)] <- 0
# Calculate realized variance recursively
lambdaf <- 0.9
volv <- HighFreq::run_var(retp, lambda=lambdaf)
volv <- sqrt(volv[, 2])
# Plot EMA volatility
volv <- xts:::xts(volv, order.by=datev)
dygraphs::dygraph(volv, main="VTI Realized Volatility") %>%
  dyOptions(colors="blue") %>% dyLegend(show="always", width=300)
library(HighFreq)  # Load HighFreq
# Minutely SPY returns (unit per minute) single day
# Minutely SPY volatility (unit per minute)
retspy <- rutils::diffit(log(SPY["2012-02-13", 4]))
sd(retspy)
# SPY returns multiple days (includes overnight jumps)
retspy <- rutils::diffit(log(SPY[, 4]))
sd(retspy)
# Table of time intervals - 60 second is most frequent
indeks <- rutils::diffit(xts::.index(SPY))
table(indeks)
# SPY returns divided by the overnight time intervals (unit per second)
retspy <- retspy/indeks
retspy[1] <- 0
# Minutely SPY volatility scaled to unit per minute
60*sd(retspy)
library(HighFreq)  # Load HighFreq
spy <- HighFreq::SPY["2008/2009"]
# Calculate daily SPY volatility using package HighFreq
sqrt(6.5*60*HighFreq::calcvar_ohlc(log(spy),
  method="yang_zhang"))
# Calculate daily SPY volatility from minutely prices using package TTR
sqrt((6.5*60)*mean(na.omit(
  TTR::volatility(spy, N=1, calc="yang.zhang"))^2))
# Calculate trailing SPY variance using package HighFreq
varv <- HighFreq::roll_var_ohlc(log(spy), method="yang_zhang",
  lookb=lookb)
# Plot range volatility
varv <- xts:::xts(sqrt(varv), order.by=zoo::index(spy))
dygraphs::dygraph(varv["2009-02"], main="SPY Trailing Range Volatility") %>%
  dyOptions(colors="blue") %>% dyLegend(show="always", width=300)
# Benchmark the speed of HighFreq vs TTR
library(microbenchmark)
summary(microbenchmark(
  ttr=TTR::volatility(rutils::etfenv$VTI, N=1, calc="yang.zhang"),
  highfreq=HighFreq::calcvar_ohlc(log(rutils::etfenv$VTI), method="yang_zhang"),
  times=2))[, c(1, 4, 5)]
# Calculate VXX log prices
vxx <- na.omit(rutils::etfenv$prices$VXX)
datev <- zoo::index(vxx)
lookb <- 41
vxx <- log(vxx)
# Calculate trailing VTI volatility
closep <- get("VTI", rutils::etfenv)[datev]
closep <- log(closep)
volv <- sqrt(HighFreq::roll_var_ohlc(ohlc=closep, lookb=lookb, scalev=FALSE))
volv[1:lookb] <- volv[lookb+1]
# Plot dygraph of VXX and VTI volatility
datav <- cbind(vxx, volv)
colnames(datav)[2] <- "VTI Volatility"
colv <- colnames(datav)
captiont <- "VXX and VTI Volatility"
dygraphs::dygraph(datav[, 1:2], main=captiont) %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=1, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=1, col="red") %>%
  dyLegend(show="always", width=300)
# Calculate VXX log prices
vxx <- na.omit(rutils::etfenv$prices$VXX)
datev <- zoo::index(vxx)
lookb <- 41
vxx <- log(vxx)
vxx <- (vxx - HighFreq::roll_mean(vxx, lookb=lookb))
vxx[1:lookb] <- vxx[lookb+1]
# Calculate trailing VTI volatility
closep <- get("VTI", rutils::etfenv)[datev]
closep <- log(closep)
volv <- sqrt(HighFreq::roll_var_ohlc(ohlc=closep, lookb=lookb, scalev=FALSE))
volv[1:lookb] <- volv[lookb+1]
# Calculate regression coefficients of XLB ~ XLE
betac <- drop(cov(vxx, volv)/var(volv))
alphac <- drop(mean(vxx) - betac*mean(volv))
# Calculate regression residuals
fitv <- (alphac + betac*volv)
residuals <- (vxx - fitv)
# Perform ADF test on residuals
tseries::adf.test(residuals, k=1)
# Plot dygraph of VXX and VTI volatility
datav <- cbind(vxx, volv)
colv <- colnames(datav)
captiont <- "VXX and VTI Volatility"
dygraphs::dygraph(datav[, 1:2], main=captiont) %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=1, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=1, col="red") %>%
  dyLegend(show="always", width=300)
# Calculate VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
# Calculate VTI rolling variance
lookb <- 21
varv <- HighFreq::roll_var(retp, lookb=lookb)
colnames(varv) <- "Variance"
# Number of lookbv that fit over returns
nrows <- NROW(retp)
nagg <- nrows %/% lookb
# Define end points with beginning stub
endd <- c(0, nrows-lookb*nagg + (0:nagg)*lookb)
nrows <- NROW(endd)
# Subset variance to end points
varv <- varv[endd]
# Plot autocorrelation function
rutils::plot_acf(varv, lag=10, main="ACF of Variance")
# Plot partial autocorrelation
pacf(varv, lag=10, main="PACF of Variance", ylab=NA)
# Define GARCH parameters
alphac <- 0.3; betac <- 0.5;
omega <- 1e-4*(1 - alphac - betac)
nrows <- 1000
# Calculate matrix of standard normal innovations
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
innov <- rnorm(nrows)
retp <- numeric(nrows)
varv <- numeric(nrows)
varv[1] <- omega/(1 - alphac - betac)
retp[1] <- sqrt(varv[1])*innov[1]
# Simulate GARCH model
for (i in 2:nrows) {
  retp[i] <- sqrt(varv[i-1])*innov[i]
  varv[i] <- omega + alphac*retp[i]^2 + betac*varv[i-1]
}  # end for
# Simulate the GARCH process using Rcpp
garchsim <- HighFreq::sim_garch(omega=omega, alpha=alphac,
  beta=betac, innov=matrix(innov))
all.equal(garchsim, cbind(retp, varv), check.attributes=FALSE)
# Define GARCH parameters
alphac <- 0.3; betac <- 0.5;
omega <- 1e-4*(1 - alphac - betac)
nrows <- 1000
# Calculate matrix of standard normal innovations
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
innov <- rnorm(nrows)
retp <- numeric(nrows)
varv <- numeric(nrows)
varv[1] <- omega/(1 - alphac - betac)
retp[1] <- sqrt(varv[1])*innov[1]
# Simulate GARCH model
for (i in 2:nrows) {
  retp[i] <- sqrt(varv[i-1])*innov[i]
  varv[i] <- omega + alphac*retp[i]^2 + betac*varv[i-1]
}  # end for
# Simulate the GARCH process using Rcpp
garchsim <- HighFreq::sim_garch(omega=omega, alpha=alphac,
  beta=betac, innov=matrix(innov))
all.equal(garchsim, cbind(retp, varv), check.attributes=FALSE)
# Open plot window on Mac
dev.new(width=6, height=5, noRStudioGD=TRUE)
# Set plot parameters to reduce whitespace around plot
par(mar=c(2, 2, 3, 1), oma=c(0, 0, 0, 0))
# Plot GARCH cumulative returns
plot(cumsum(retp), t="l", col="blue", xlab="", ylab="",
  main="GARCH Cumulative Returns")
quartz.save("figure/garch_returns.png", type="png",
  width=6, height=5)
# Plot GARCH volatility
plot(sqrt(varv), t="l", col="blue", xlab="", ylab="",
  main="GARCH Volatility")
quartz.save("figure/garch_volat.png", type="png",
  width=6, height=5)
# Calculate kurtosis of GARCH returns
mean(((retp-mean(retp))/sd(retp))^4)
# Perform Jarque-Bera test of normality
tseries::jarque.bera.test(retp)
# Fit t-distribution into GARCH returns
fitobj <- MASS::fitdistr(retp, densfun="t", df=2)
locv <- fitobj$estimate[1]
scalev <- fitobj$estimate[2]
# Plot histogram of GARCH returns
histp <- hist(retp, col="lightgrey",
  xlab="returns", breaks=200, xlim=c(-0.03, 0.03),
  ylab="frequency", freq=FALSE, main="GARCH Returns Histogram")
lines(density(retp, adjust=1.5), lwd=2, col="blue")
curve(expr=dt((x-locv)/scalev, df=2)/scalev,
  type="l", xlab="", ylab="", lwd=2,
  col="red", add=TRUE)
legend("topright", inset=-0, bty="n", y.intersp=0.4,
 leg=c("density", "t-distr w/ 2 dof"),
 lwd=6, lty=1, col=c("blue", "red"))
quartz.save("figure/garch_hist.png", type="png", width=6, height=5)
# Specify GARCH model
garch_spec <- fGarch::garchSpec(model=list(ar=c(0, 0), omega=omega,
  alpha=alphac, beta=betac))
# Simulate GARCH model
garch_sim <- fGarch::garchSim(spec=garch_spec, n=nrows)
retp <- as.numeric(garch_sim)
# Calculate kurtosis of GARCH returns
moments::moment(retp, order=4) /
  moments::moment(retp, order=2)^2
# Perform Jarque-Bera test of normality
tseries::jarque.bera.test(retp)
# Plot histogram of GARCH returns
histp <- hist(retp, col="lightgrey",
  xlab="returns", breaks=200, xlim=c(-0.05, 0.05),
  ylab="frequency", freq=FALSE,
  main="GARCH Returns Histogram")
lines(density(retp, adjust=1.5), lwd=3, col="blue")
# Fit t-distribution into GARCH returns
fitobj <- MASS::fitdistr(retp, densfun="t", df=2, lower=c(-1, 1e-7))
locv <- fitobj$estimate[1]
scalev <- fitobj$estimate[2]
curve(expr=dt((x-locv)/scalev, df=2)/scalev,
  type="l", xlab="", ylab="", lwd=3,
  col="red", add=TRUE)
legend("topright", inset=0.05, bty="n", y.intersp=0.4,
 leg=c("density", "t-distr w/ 2 dof"),
 lwd=6, lty=1, col=c("blue", "red"))
# Calculate variance of GARCH returns
var(retp)
# Calculate expected value of variance
omega/(1 - alphac - betac)
# Calculate kurtosis of GARCH returns
mean(((retp-mean(retp))/sd(retp))^4)
# Calculate expected value of kurtosis
3 + 6*alpha^2/(1-2*alpha^2-(alphac+betac)^2)
# Calculate the distribution of GARCH kurtosis
kurt <- sapply(1:1e4, function(x) {
  garchsim <- HighFreq::sim_garch(omega=omega, alpha=alphac,
    beta=betac, innov=matrix(rnorm(nrows)))
  retp <- garchsim[, 1]
  c(var(retp), mean(((retp-mean(retp))/sd(retp))^4))
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
abline(v=(3 + 6*alpha^2/(1-2*alpha^2-(alphac+betac)^2)), lwd=3, col="red")
text(x=7.0, y=0.4, "Expected Kurtosis")
quartz.save("figure/garch_kurtosis.png", type="png", width=6, height=5)
# Simulate the GARCH process using Rcpp
garchsim <- HighFreq::sim_garch(omega=omega, alpha=alphac,
  beta=betac, innov=matrix(innov))
# Extract the returns
retp <- garchsim[, 1]
# Estimate the trailing variance from the returns
varv <- numeric(nrows)
varv[1] <- omega/(1 - alphac - betac)
for (i in 2:nrows) {
  varv[i] <- omega + alphac*retp[i]^2 +
    betac*varv[i-1]
}  # end for
all.equal(garchsim[, 2], varv, check.attributes=FALSE)
library(fGarch)
# Fit returns into GARCH
garchfit <- fGarch::garchFit(data=retp)
# Fitted GARCH parameters
garchfit@fit$coef
# Actual GARCH parameters
c(mu=mean(retp), omega=omega,alpha=alphac, beta=betac)
# Plot GARCH fitted volatility
plot(sqrt(garchfit@fit$series$h), t="l",
  col="blue", xlab="", ylab="",
  main="GARCH Fitted Volatility")
quartz.save("figure/garch_fGarch_fitted.png",
  type="png", width=6, height=5)
# Define likelihood function
likefun <- function(omega, alphac, betac) {
  # Estimate the trailing variance from the returns
  varv <- numeric(nrows)
  varv[1] <- omega/(1 - alphac - betac)
  for (i in 2:nrows) {
    varv[i] <- omega + alphac*retp[i]^2 + betac*varv[i-1]
  }  # end for
  varv <- ifelse(varv > 0, varv, 0.000001)
  # Lag the variance
  varv <- rutils::lagit(varv, pad_zeros=FALSE)
  # Calculate the likelihood
  -sum(retp^2/varv + log(varv))
}  # end likefun
# Calculate the likelihood in R
likefun(omega, alphac, betac)
# Calculate the likelihood in Rcpp
HighFreq::lik_garch(omega=omega, alpha=alphac,
  beta=betac, returns=matrix(retp))
# Benchmark speed of likelihood calculations
library(microbenchmark)
summary(microbenchmark(
  Rcode=likefun(omega, alphac, betac),
  Rcpp=HighFreq::lik_garch(omega=omega, alpha=alphac, beta=betac, returns=matrix(retp))
  ), times=10)[, c(1, 4, 5)]
# Calculate the variance of returns
retp <- garchsim[, 1, drop=FALSE]
varv <- var(retp)
retp <- (retp - mean(retp))
# Calculate likelihood as function of alpha and betac parameters
likefun <- function(alphac, betac) {
  omega <- variance*(1 - alpha - betac)
  -HighFreq::lik_garch(omega=omega, alpha=alphac, beta=betac, returns=retp)
}  # end likefun
# Calculate matrix of likelihood values
alphas <- seq(from=0.15, to=0.35, len=50)
betac <- seq(from=0.35, to=0.5, len=50)
likmat <- sapply(alphacs, function(alphac) sapply(betac,
  function(betac) likefun(alphac, betac)))
# Set rgl options and load package rgl
options(rgl.useNULL=TRUE); library(rgl)
# Draw and render 3d surface plot of likelihood function
ncols <- 100
color <- rainbow(ncols, start=2/6, end=4/6)
zcols <- cut(likmat, ncols)
rgl::persp3d(alphacs, betac, likmat, col=color[zcols],
  xlab="alpha", ylab="beta", zlab="likelihood")
rgl::rglwidget(elementId="plot3drgl", width=700, height=700)
# Perform grid search
coord <- which(likmat == min(likmat), arr.ind=TRUE)
c(alphacs[coord[2]], betac[coord[1]])
likmat[coord]
likefun(alphacs[coord[2]], betac[coord[1]])
# Optimal and actual parameters
options(scipen=2)  # Use fixed not scientific notation
cbind(actual=c(alphac=alphac, beta=betac, omega=omega),
  optimal=c(alphacs[coord[2]], betac[coord[1]], variance*(1 - sum(alphacs[coord[2]], betac[coord[1]]))))
# Define vectorized likelihood function
likefun <- function(x, retp) {
  alphac <- x[1]; betac <- x[2]; omega <- x[3]
  -HighFreq::lik_garch(omega=omega, alpha=alphac, beta=betac, returns=retp)
}  # end likefun
# Initial parameters
initp <- c(alphac=0.2, beta=0.4, omega=varv/0.2)
# Find max likelihood parameters using steepest descent optimizer
fitobj <- optim(par=initp,
  fn=likefun, # Log-likelihood function
  method="L-BFGS-B", # Quasi-Newton method
  returns=retp,
  upper=c(0.35, 0.55, varv), # Upper constraint
  lower=c(0.15, 0.35, varv/100)) # Lower constraint
# Optimal and actual parameters
cbind(actual=c(alphac=alphac, beta=betac, omega=omega),
optimal=c(fitobj$par["alpha"], fitobj$par["beta"], fitobj$par["omega"]))
# Find max likelihood parameters using DEoptim
optiml <- DEoptim::DEoptim(fn=likefun,
  upper=c(0.35, 0.55, varv), # Upper constraint
  lower=c(0.15, 0.35, varv/100), # Lower constraint
  returns=retp,
  control=list(trace=FALSE, itermax=1000, parallelType=1))
# Optimal and actual parameters
cbind(actual=c(alphac=alphac, beta=betac, omega=omega),
optimal=c(optiml$optim$bestmem[1], optiml$optim$bestmem[2], optiml$optim$bestmem[3]))
# Calculate VTI returns
retp <- na.omit(rutils::etfenv$returns$VTI)
# Find max likelihood parameters using DEoptim
optiml <- DEoptim::DEoptim(fn=likefun,
  upper=c(0.4, 0.9, varv), # Upper constraint
  lower=c(0.1, 0.5, varv/100), # Lower constraint
  returns=retp,
  control=list(trace=FALSE, itermax=1000, parallelType=1))
# Optimal parameters
paramv <- unname(optiml$optim$bestmem)
alphac <- paramv[1]; betac <- paramv[2]; omega <- paramv[3]
c(alphac, betac, omega)
# Equilibrium GARCH variance
omega/(1 - alphac - betac)
drop(var(retp))
# Estimate the GARCH volatility of VTI returns
nrows <- NROW(retp)
varv <- numeric(nrows)
varv[1] <- omega/(1 - alphac - betac)
for (i in 2:nrows) {
  varv[i] <- omega + alphac*retp[i]^2 + betac*varv[i-1]
}  # end for
# Estimate the GARCH volatility using Rcpp
garchsim <- HighFreq::sim_garch(omega=omega, alpha=alphac,
  beta=betac, innov=retp, is_random=FALSE)
all.equal(garchsim[, 2], varv, check.attributes=FALSE)
# Plot dygraph of the estimated GARCH volatility
dygraphs::dygraph(xts::xts(sqrt(varv), zoo::index(retp)),
  main="Estimated GARCH Volatility of VTI") %>%
  dyOptions(colors="blue") %>% dyLegend(show="always", width=300)
# Simulate GARCH model
garchsim <- HighFreq::sim_garch(omega=omega, alpha=alphac,
  beta=betac, innov=matrix(innov))
varv <- garchsim[, 2]
# Calculate the equilibrium variance
vareq <- omega/(1 - alphac - betac)
# Calculate the variance forecasts
varf <- numeric(10)
varf[1] <- vareq + (alphac + betac)*(xts::last(varv) - vareq)
for (i in 2:10) {
  varf[i] <- vareq + (alphac + betac)*(varf[i-1] - vareq)
}  # end for
# Open plot window on Mac
dev.new(width=6, height=5, noRStudioGD=TRUE)
par(mar=c(2, 2, 3, 1), oma=c(0, 0, 0, 0))
# Plot GARCH variance forecasts
plot(tail(varv, 30), t="l", col="blue", xlab="", ylab="",
  xlim=c(1, 40), ylim=c(0, max(tail(varv, 30))),
  main="GARCH Variance Forecasts")
text(x=15, y=0.5*vareq, "realized variance")
lines(x=30:40, y=c(xts::last(varv), varf), col="red", lwd=3)
text(x=35, y=0.6*vareq, "variance forecasts")
abline(h=vareq, lwd=3, col="red")
text(x=10, y=1.1*vareq, "Equilibrium variance")
quartz.save("figure/garch_forecast.png", type="png", width=6, height=5)
library(HighFreq)  # Load HighFreq
# Minutely SPY returns (unit per minute) single day
retspy <- rutils::diffit(log(SPY["2012-02-13", 4]))
# Minutely SPY volatility (unit per minute)
sd(retspy)
# Divide minutely SPY returns by time intervals (unit per second)
retspy <- retspy/rutils::diffit(xts::.index(SPY["2012-02-13"]))
retspy[1] <- 0
# Minutely SPY volatility scaled to unit per minute
60*sd(retspy)
# SPY returns multiple days
retspy <- rutils::diffit(log(SPY[, 4]))
# Minutely SPY volatility (includes overnight jumps)
sd(retspy)
# Table of intervals - 60 second is most frequent
indeks <- rutils::diffit(xts::.index(SPY))
table(indeks)
# hist(indeks)
# SPY returns with overnight scaling (unit per second)
retspy <- retspy/indeks
retspy[1] <- 0
# Minutely SPY volatility scaled to unit per minute
60*sd(retspy)
library(HighFreq)  # Load HighFreq
ohlc <- log(rutils::etfenv$VTI)
# Calculate variance
varcl <- HighFreq::run_variance(ohlc=ohlc,
  method="close")
var_yang_zhang <- HighFreq::run_variance(ohlc=ohlc)
stdev <- 24*60*60*sqrt(252*cbind(varcl, var_yang_zhang))
colnames(stdev) <- c("close stdev", "Yang-Zhang")
# Plot the time series of volatility
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red")
quantmod::chart_Series(stdev["2011-07/2011-12"],
  theme=plot_theme, name="Standard Deviations: Close and YZ")
legend("top", legend=colnames(stdev), y.intersp=0.4,
 bg="white", lty=1, lwd=6, inset=0.1, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Plot volatility around 2010 flash crash
quantmod::chart_Series(stdev["2010-04/2010-06"],
  theme=plot_theme, name="Volatility Around 2010 Flash Crash")
legend("top", legend=colnames(stdev), y.intersp=0.4,
 bg="white", lty=1, lwd=6, inset=0.1, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Plot density of volatility distributions
plot(density(stdev[, 1]), xlab="", ylab="",
  main="Density of Volatility Distributions",
  xlim=c(-0.05, range(stdev[, 1])[2]/3), type="l", lwd=2, col="blue")
lines(density(stdev[, 2]), col='red', lwd=2)
legend("top", legend=c("Close-to-Close", "Yang-Zhang"),
 bg="white", lty=1, lwd=6, inset=0.1, cex=0.8, y.intersp=0.4,
 col=plot_theme$col$line.col, bty="n")
# ? range volatility estimator has lower standard error ?
c(sd(varcl)/mean(varcl), sd(var_yang_zhang)/mean(var_yang_zhang))
foo <- stdev[varcl < range(varcl)[2]/3, ]
c(sd(foo[, 1])/mean(foo[, 1]), sd(foo[, 2])/mean(foo[, 2]))
plot(density(foo[, 1]), xlab="", ylab="",
  main="Mixture of Normal Returns",
  xlim=c(-0.05, range(foo[, 1])[2]/2), type="l", lwd=2, col="blue")
lines(density(foo[, 2]), col='red', lwd=2)
ohlc <- rutils::etfenv$VTI
retp <- log((ohlc[, 2] - ohlc[, 3]) / (ohlc[, 2] + ohlc[, 3]))
foo <- rutils::diffit(log(ohlc[, 4]))
plot(as.numeric(foo)^2, as.numeric(retp)^2)
bar <- lm(retp ~ foo)
summary(bar)
# Perform normality tests
shapiro.test(coredata(retp))
tseries::jarque.bera.test(retp)
# Fit VTI returns using MASS::fitdistr()
fitobj <- MASS::fitdistr(retp, densfun="t", df=2)
fitobj$estimate; fitobj$sd
# Calculate moments of standardized returns
sapply(3:4, moments::moment, x=(retp - mean(retp))/sd(retp))
# Plot histogram of VTI returns
colorv <- c("lightgray", "blue", "green", "red")
PerformanceAnalytics::chart.Histogram(retp,
  main="", xlim=c(-7, -3), col=colorv[1:3],
  methods = c("add.density", "add.normal"))
curve(expr=dt((x-fitobj$estimate[1])/
  fitobj$estimate[2], df=2)/fitobj$estimate[2],
type="l", xlab="", ylab="", lwd=2,
col=colorv[4], add=TRUE)
# Add title and legend
title(main="VTI logarithm of range",
cex.main=1.3, line=-1)
legend("topright", inset=0.05, y.intersp=0.4,
  legend=c("density", "normal", "t-distr"),
  lwd=6, lty=1, col=colorv[2:4], bty="n")
# Calculate VTI range variance partial autocorrelations
pacf(retp^2, lag=10, xlab=NA, ylab=NA,
     main="PACF of VTI log range")
quantmod::chart_Series(retp^2, name="VTI log of range squared")
# Standard errors of variance estimators using bootstrap
bootd <- sapply(1:1e2, function(x) {
  # Create random OHLC
  ohlc <- HighFreq::random_ohlc()
  # Calculate variance estimate
  c(var=var(ohlc[, 4]),
    yang_zhang=HighFreq::calcvariance(
ohlc, method="yang_zhang", scalev=FALSE))
})  # end sapply
# Analyze bootstrapped variance
bootd <- t(bootd)
head(bootd)
colMeans(bootd)
apply(bootd, MARGIN=2, sd) /
  colMeans(bootd)
par(oma=c(1, 1, 1, 1), mar=c(2, 2, 1, 1), mgp=c(0, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
# Close variance estimator partial autocorrelations
pacf(varcl, lag=10, xlab=NA, ylab=NA)
title(main="VTI close variance partial autocorrelations")
# Range variance estimator partial autocorrelations
pacf(var_yang_zhang, lag=10, xlab=NA, ylab=NA)
title(main="VTI YZ variance partial autocorrelations")
# Squared range partial autocorrelations
retp <- log(rutils::etfenv$VTI[,2] /
            rutils::etfenv$VTI[,3])
pacf(retp^2, lag=10, xlab=NA, ylab=NA)
title(main="VTI squared range partial autocorrelations")
library(xtable)
gamblev <- data.frame(win=c("p", "a", "1 + a"), lose=c("q = 1 - p", "-b", "1 - b"))
rownames(gamblev) <- c("probability", "payout", "terminal wealth")
# print(xtable(gamblev), comment=FALSE, size="tiny")
print(xtable(gamblev), comment=FALSE)
# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Define logarithmic utility
utilfun <- function(frac, p=0.3, a=20, b=1) {
  p*log(1+frac*a) + (1-p)*log(1-frac*b)
}  # end utilfun
# Plot utility
curve(expr=utilfun, xlim=c(0, 1),
ylim=c(-0.5, 0.4), xlab="betting fraction",
ylab="utility", main="", lwd=2)
title(main="Logarithmic Utility", line=0.5)
# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Define and plot Kelly fraction
kelly_frac <- function(a, p=0.5, b=1) {
  p/b - (1-p)/a
}  # end kelly_frac
curve(expr=kelly_frac, xlim=c(0, 5),
ylim=c(-2, 1), xlab="betting odds",
ylab="kelly fraction", main="", lwd=2)
abline(h=0.5, lwd=2, col="red")
text(x=1.5, y=0.5, pos=3, cex=0.8, labels="max Kelly fraction=0.5")
title(main="Kelly fraction", line=-0.8)
# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Plot several Kelly curves
curve(expr=kelly_frac(x, b=1), xlim=c(0, 5),
ylim=c(-1, 1.5), xlab="betting odds",
ylab="kelly fraction", main="", lwd=2)
abline(h=0.5, lwd=2, col="red")
text(x=1.5, y=0.5, pos=3, cex=0.8, labels="b=1.0; max fraction=0.5")
curve(expr=kelly_frac(x, b=0.5), add=TRUE, main="", lwd=2)
abline(h=1.0, lwd=2, col="red")
text(x=1.5, y=1.0, pos=3, cex=0.8, labels="b=0.5; max fraction=1.0")
title(main="Kelly fraction", line=-0.8)
# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Wealth of multiperiod binary betting
wealthv <- function(f, a=0.8, b=0.1, n=1e3, i=150) {
  (1+f*a)^i * (1-f*b)^(n-i)
}  # end wealth
curve(expr=wealthv, xlim=c(0, 1),
xlab="betting fraction",
ylab="wealth", main="", lwd=2)
title(main="Wealth of Multiperiod Betting", line=0.1)
# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# Simulate asset prices
calc_pricev <- function(x) cumprod(1 + rnorm(1e3, sd=0.01))
price_paths <- sapply(1:3, calc_pricev)
plot(price_paths[, 1], type="l", lwd=3,
     main="Simulated Asset Prices",
     ylim=range(price_paths),
     lty="solid", xlab="time", ylab="price")
lines(price_paths[, 2], col="blue", lwd=3)
lines(price_paths[, 3], col="orange", lwd=3)
abline(h=0.5, col="red", lwd=3)
text(x=200, y=0.5, pos=3, labels="liquidation threshold")
# Calculate the VTI returns
retp <- rutils::etfenv$returns$VTI
retp <- na.omit(retp)
c(mean=mean(retp), stdev=sd(retp))
range(retp)
# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Define vectorized logarithmic utility function
utilfun <- function(kellyfrac, retp) {
  sapply(kellyfrac, function(x) sum(log(1 + x*retp)))
}  # end utilfun
utilfun(1, retp)
utilfun(c(1, 4), retp)
# Plot the logarithmic utility
curve(expr=utilfun(x, retp=retp),
xlim=c(0.1, 5), xlab="leverage", ylab="utility",
main="Utility of Asset Returns", lwd=2)
# Approximate Kelly leverage
mean(retp)/var(retp)
PerformanceAnalytics::KellyRatio(R=retp, method="full")
# Kelly leverage
unlist(optimize(
  f=function(x) -utilfun(x, retp),
  interval=c(1, 4)))
# Calculate the VTI returns
retp <- rutils::etfenv$returns$VTI
retp <- na.omit(retp)
# Calculate the wealth paths
kelly_ratio <- drop(mean(retp)/var(retp))
kelly_wealthv <- cumprod(1 + kelly_ratio*retp)
hyper_kelly <- cumprod(1 + (kelly_ratio+2)*retp)
sub_kelly <- cumprod(1 + (kelly_ratio-2)*retp)
kelly_paths <- cbind(kelly_wealth, hyper_kelly, sub_kelly)
colnames(kelly_paths) <- c("kelly", "hyper-kelly", "sub-kelly")
# Plot wealth paths
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "orange", "blue")
quantmod::chart_Series(kelly_paths, theme=plot_theme, name="Wealth Paths")
legend("topleft", legend=colnames(kelly_paths),
 inset=0.1, bg="white", lty=1, lwd=6, y.intersp=0.5,
 col=plot_theme$col$line.col, bty="n")
# bidask equal to 1 bp for liquid ETFs
bidask <- 0.001
# Calculate the wealth paths
kelly_ratio <- drop(mean(retp)/var(retp))
wealthv <- cumprod(1 + kelly_ratio*retp)
wealth_trans <- cumprod(1 + kelly_ratio*retp -
  0.5*bidask*kelly_ratio*(kelly_ratio-1)*abs(retp))
# Calculate the compounded wealth from returns
wealthv <- cbind(wealthv, wealth_trans)
colnames(wealthv) <- c("Kelly", "Including bid-ask")
# Plot compounded wealth
dygraphs::dygraph(wealthv, main="Kelly Strategy With Transaction Costs") %>%
  dyOptions(colors=c("green", "blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Plot several Kelly curves
curve(expr=kelly_frac(x, b=1), xlim=c(0, 5),
ylim=c(-1, 1.5), xlab="betting odds",
ylab="kelly fraction", main="", lwd=2)
abline(h=0.5, lwd=2, col="red")
text(x=1.5, y=0.5, pos=3, cex=0.8, labels="b=1.0; max fraction=0.5")
curve(expr=kelly_frac(x, b=0.5), add=TRUE, main="", lwd=2)
abline(h=1.0, lwd=2, col="red")
text(x=1.5, y=1.0, pos=3, cex=0.8, labels="b=0.5; max fraction=1.0")
title(main="Kelly fraction", line=-0.8)
# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Plot logarithmic utility function
curve(expr=log, lwd=3, col="blue", xlim=c(0.5, 5),
xlab="wealth", ylab="utility",
main="Logarithmic Utility")
# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Define CRRA utility
cr_ra <- function(w, ra) {
  (w^(1-ra) - 1)/(1-ra)
}  # end cr_ra
# Plot utility functions
curve(expr=cr_ra(x, ra=0.7), xlim=c(0.5, 5), lwd=3,
xlab="wealth", ylab="utility", main="", col="blue")
curve(expr=log, add=TRUE, lwd=3)
curve(expr=cr_ra(x, ra=1.3), add=TRUE, lwd=3, col="red")
# Add title and legend
title(main="CRRA Utility", line=0.5)
legend(x="topleft", legend=c("risk seeking", "logarithmic", "risk averse"),
 title="Risk Aversion", inset=0.05, cex=0.8, bg="white", y.intersp=0.5,
 lwd=6, lty=1, bty="n", col=c("blue", "black", "red"))
# Open x11 for plotting
x11(width=5, height=4)
# Set plot parameters to reduce whitespace around plot
par(mar=c(4, 4, 2, 1), oma=c(0, 0, 0, 0))
# Define CRRA utility
cr_ra <- function(w, ra) {
  (w^(1-ra) - 1)/(1-ra)
}  # end cr_ra
# Plot utility functions
curve(expr=cr_ra(x, ra=0.7), xlim=c(0.5, 5), lwd=3,
xlab="wealth", ylab="utility", main="", col="blue")
curve(expr=log, add=TRUE, lwd=3)
curve(expr=cr_ra(x, ra=1.3), add=TRUE, lwd=3, col="red")
# Add title and legend
title(main="CRRA Utility", line=0.5)
legend(x="topleft", legend=c("risk seeking", "logarithmic", "risk averse"),
 title="Risk Aversion", inset=0.05, cex=0.8, bg="white", y.intersp=0.5,
 lwd=6, lty=1, bty="n", col=c("blue", "black", "red"))
# Calculate the VTI returns
retp <- rutils::etfenv$returns$VTI
retp <- na.omit(retp)
# Calculate the wealth paths
kelly_ratio <- drop(mean(retp)/var(retp))
kelly_wealthv <- cumprod(1 + kelly_ratio*retp)
hyper_kelly <- cumprod(1 + (kelly_ratio+2)*retp)
sub_kelly <- cumprod(1 + (kelly_ratio-2)*retp)
kelly_paths <- cbind(kelly_wealth, hyper_kelly, sub_kelly)
colnames(kelly_paths) <- c("kelly", "hyper-kelly", "sub-kelly")
# Plot wealth paths
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "orange", "blue")
quantmod::chart_Series(kelly_paths, theme=plot_theme,
       name="Wealth Paths")
legend("topleft", legend=colnames(kelly_paths),
 inset=0.1, bg="white", lty=1, lwd=6, y.intersp=0.5,
 col=plot_theme$col$line.col, bty="n")
# Calculate the VTI returns
retp <- rutils::etfenv$returns$VTI
retp <- na.omit(retp)
# Calculate the higher moments of VTI returns
c(mean=sum(retp),
  variance=sum(retp^2),
  mom3=sum(retp^3),
  mom4=sum(retp^4))/NROW(retp)
# Calculate the higher moments of minutely SPY returns
spy <- HighFreq::SPY[, 4]
spy <- na.omit(spy)
spy <- rutils::diffit(log(spy))
c(mean=sum(spy),
  variance=sum(spy^2),
  mom3=sum(spy^3),
  mom4=sum(spy^4))/NROW(spy)
retp <- na.omit(rutils::etfenv$returns[, c("VTI", "IEF")])
# Logarithmic utility of stock and bond portfolio
utilfun <- function(stocku, bondu) {
  -sum(log(1 + stocku*retp$VTI + bondu*retp$IEF))
}  # end utilfun
# Create matrix of utility values
stocku <- seq(from=3, to=7, by=0.2)
bondu <- seq(from=12, to=20, by=0.2)
utilm <- sapply(bondu, function(y) sapply(stocku,
  function(x) utilfun(x, y)))
# Set rgl options and load package rgl
options(rgl.useNULL=TRUE)
library(rgl)
# Draw 3d surface plot of utility
rgl::persp3d(stocku, bondu, utilm, col="green",
  xlab="stocks", ylab="bonds", zlab="utility")
# Render the surface plot
rgl::rglwidget(elementId="plot3drgl")
# Save the surface plot to png file
rgl::rgl.snapshot("utility_surface.png")
# Approximate Kelly weights
weightv <- sapply(retp, function(x) mean(x)/var(x))
# Kelly weight for stocks
unlist(optimize(f=function(x) utilfun(x, bondu=0), interval=c(1, 4)))
# Kelly weight for bonds
unlist(optimize(f=function(x) utilfun(x, stocku=0), interval=c(1, 14)))
# Vectorized utility of stock and bond portfolio
utility_vec <- function(weightv) {
  utilfun(weightv[1], weightv[2])
}  # end utility_vec
# Optimize with respect to vector argument
optiml <- optim(fn=utility_vec, par=c(3, 10),
          method="L-BFGS-B",
          upper=c(8, 20), lower=c(2, 5))
# Exact Kelly weights
optiml$par
# Approximate Kelly weights
retsport <- (retp %*% weightv)
drop(mean(retsport)/var(retsport))*weightv
# Exact Kelly weights
optiml$par
# Quarter-Kelly sub-optimal weights
weightv <- optiml$par/4
# Plot Kelly optimal portfolio
retp <- cbind(retp, weightv[1]*retp$VTI + weightv[2]*retp$IEF)
colnames(retp)[3] <- "Kelly_sub_optimal"
# Calculate the compounded wealth from returns
wealthv <- cumprod(1 + retp)
# Plot compounded wealth
dygraphs::dygraph(wealthv, main="Stock and Bond Portfolio") %>%
  dyOptions(colors=c("green", "blue", "green")) %>%
  dySeries("Kelly_sub_optimal", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)
retp <- na.omit(rutils::etfenv$returns[, c("VTI", "IEF")])
# Calculate the rolling returns and variance
lookb <- 200
var_rolling <- HighFreq::roll_var(retp, lookb)
weightv <- HighFreq::roll_sum(retp, lookb)/lookb
weightv <- weightv/var_rolling
weightv[1, ] <- 1/NCOL(weightv)
weightv <- zoo::na.locf(weightv)
sum(is.na(weightv))
range(weightv)
# Plot the weights
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
plot(density(retp$IEF), t="l", lwd=3, col="red",
     xlab="weights", ylab="density",
     ylim=c(0, max(density(retp$VTI)$y)),
     main="Kelly Weight Distributions")
lines(density(retp$VTI), t="l", col="blue", lwd=3)
legend("topright", legend=c("VTI", "IEF"),
 inset=0.1, bg="white", lty=1, lwd=6, y.intersp=0.5,
 col=c("blue", "red"), bty="n")
# Scale and lag the Kelly weights
weightv <- lapply(weightv, function(x) 10*x/sum(abs(range(x))))
weightv <- do.call(cbind, weightv)
weightv <- rutils::lagit(weightv)
# Calculate the compounded Kelly wealth and VTI
wealthv <- cbind(cumprod(1 + weightv$VTI*retp$VTI), cumprod(1 + retp$VTI))
colnames(wealthv) <- c("Kelly Strategy", "VTI")
dygraphs::dygraph(wealthv, main="VTI Strategy Using Rolling Kelly Weight") %>%
  dyAxis("y", label="Kelly Strategy", independentTicks=TRUE) %>%
  dyAxis("y2", label="VTI", independentTicks=TRUE) %>%
  dySeries(name="Kelly Strategy", axis="y", strokeWidth=1, col="red") %>%
  dySeries(name="VTI", axis="y2", strokeWidth=1, col="blue")
# bidask equal to 1 bp for liquid ETFs
bidask <- 0.001
# Calculate the compounded Kelly wealth and margin
wealthv <- cumprod(1 + weightv$VTI*retp$VTI)
marginv <- (retp$VTI - 1)*wealthv + 1
# Calculate the transaction costs
costs <- bidask*drop(rutils::diffit(marginv))/2
wealth_diff <- drop(rutils::diffit(wealthv))
costs_rel <- ifelse(wealth_diff>0, costs/wealth_diff, 0)
range(costs_rel)
hist(costs_rel, breaks=10000, xlim=c(-0.02, 0.02))
# Scale and lag the transaction costs
costs <- rutils::lagit(abs(costs)/wealthv)
# ReCalculate the compounded Kelly wealth
wealth_trans <- cumprod(1 + retp$VTI*retp$VTI - costs)
# Plot compounded wealth
wealthv <- cbind(wealthv, wealth_trans)
colnames(wealthv) <- c("Kelly", "Including bid-ask")
dygraphs::dygraph(wealthv, main="Kelly Strategy With Transaction Costs") %>%
  dyOptions(colors=c("green", "blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the compounded wealth from returns
wealthv <- cumprod(1 + rowSums(weightv*retp))
wealthv <- xts::xts(wealthv, zoo::index(retp))
quantmod::chart_Series(wealthv, name="Rolling Kelly Strategy For VTI and IEF")
# Calculate the compounded Kelly wealth and VTI
wealthv <- cbind(wealthv, cumprod(1 + 0.6*retp$IEF + 0.4*retp$VTI))
colnames(wealthv) <- c("Kelly Strategy", "VTI plus IEF")
dygraphs::dygraph(wealthv, main="Rolling Kelly Strategy For VTI and IEF") %>%
  dyAxis("y", label="Kelly Strategy", independentTicks=TRUE) %>%
  dyAxis("y2", label="VTI plus IEF", independentTicks=TRUE) %>%
  dySeries(name="Kelly Strategy", axis="y", strokeWidth=1, col="red") %>%
  dySeries(name="VTI plus IEF", axis="y2", strokeWidth=1, col="blue")
# Create a design matrix of IEF and VTI returns
desm <- na.omit(rutils::etfenv$returns[, c("IEF", "VTI")])
retvti <- desm$VTI
# Add returns with perfect timing skill
desm <- cbind(desm, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(desm)[3:4] <- c("merton", "treynor")
# Perform Merton-Henriksson test regression
regmod <- lm(IEF ~ VTI + merton, data=desm); summary(regmod)
# Perform Treynor-Mazuy test regression
regmod <- lm(IEF ~ VTI + treynor, data=desm); summary(regmod)
# Plot residual scatterplot
resids <- (desm$IEF - regmod$coeff["VTI"]*retvti)
plot.default(x=retvti, y=resids, xlab="VTI", ylab="IEF")
title(main="Treynor-Mazuy Market Timing Test\n for IEF vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retvti, y=fitv, pch=16, col="red")
text(x=0.0, y=0.8*max(resids), paste("Treynor test t-value =", tvalue))
library(xtable)
gamblev <- data.frame(win=c("p", "a"), lose=c("q = 1 - p", "-b"))
rownames(gamblev) <- c("probability", "payout")
# print(xtable(gamblev), comment=FALSE, size="tiny")
print(xtable(gamblev), comment=FALSE)
