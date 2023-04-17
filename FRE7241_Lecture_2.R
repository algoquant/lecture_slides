# Load data frame of S&P500 constituents from CSV file
sp500 <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_constituents.csv")
# Inspect data frame of S&P500 constituents
dim(sp500)
colnames(sp500)
# Extract tickers from the column Ticker
symbolv <- sp500$Ticker
# Get duplicate tickers
tablev <- table(symbolv)
duplicates <- tablev[tablev>1]
duplicates <- names(duplicates)
# Get duplicate records (rows) of sp500
sp500[symbolv %in% duplicates, ]
# Get unique tickers
symbolv <- unique(symbolv)
# Find index of ticker "BRK.B"
which(symbolv=="BRK.B")
# Rename "BRK.B" to "BRK-B" and "BF.B" to "BF-B"
symbolv[which(symbolv=="BRK.B")] <- "BRK-B"
symbolv[which(symbolv=="BF.B")] <- "BF-B"

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
# Calculate the kernel density
densityv <- sapply(1:nrows, function(it) {
  sum(dnorm(retp-retp[it], sd=bwidth))
})  # end sapply
madv <- mad(retp)
plot(retp, densityv, xlim=c(-5*madv, 5*madv),
     t="l", col="blue", lwd=3,
     xlab="returns", ylab="density",
     main="Density of VTI Returns")
# Calculate the kernel density using density()
densityv <- density(retp, bw=bwidth)
NROW(densityv$y)
x11(width=6, height=5)
plot(densityv, xlim=c(-5*madv, 5*madv),
     xlab="returns", ylab="density",
     col="blue", lwd=3, main="Density of VTI Returns")
# Interpolate the densityv vector into returns
densityv <- approx(densityv$x, densityv$y, xout=retp)
all.equal(densityv$x, retp)
plot(densityv, xlim=c(-5*madv, 5*madv),
     xlab="returns", ylab="density",
     t="l", col="blue", lwd=3,
     main="Density of VTI Returns")

# Plot histogram
histp <- hist(retp, breaks=100, freq=FALSE,
  xlim=c(-5*madv, 5*madv), xlab="", ylab="",
  main="VTI Return Distribution")
# Draw kernel density of histogram
lines(densityv, col="red", lwd=2)
# Add density of normal distribution
curve(expr=dnorm(x, mean=mean(retp), sd=sd(retp)),
add=TRUE, lwd=2, col="blue")
# Add legend
legend("topright", inset=0.05, cex=0.8, title=NULL,
 leg=c("VTI", "Normal"), bty="n",
 lwd=6, bg="white", col=c("red", "blue"))

# Create normal Q-Q plot
qqnorm(retp, ylim=c(-0.1, 0.1), main="VTI Q-Q Plot",
 xlab="Normal Quantiles")
# Fit a line to the normal quantiles
qqline(retp, col="red", lwd=2)
# Perform Shapiro-Wilk test
shapiro.test(retp[1:499])

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
sdrets <- sd(retp)
# Skewness of VTI returns
nrows/((nrows-1)*(nrows-2))*sum(((retp - retm)/sdrets)^3)
# Kurtosis of VTI returns
nrows*(nrows+1)/((nrows-1)^3)*sum(((retp - retm)/sdrets)^4)
# Random normal returns
retp <- rnorm(nrows, sd=sdrets)
# Mean and standard deviation of random normal returns
retm <- mean(retp)
sdrets <- sd(retp)
# Skewness of random normal returns
nrows/((nrows-1)*(nrows-2))*sum(((retp - retm)/sdrets)^3)
# Kurtosis of random normal returns
nrows*(nrows+1)/((nrows-1)^3)*sum(((retp - retm)/sdrets)^4)

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
# calcmom() calculates the moments of returns
calcmom <- function(retp, moment=3) {
  retp <- na.omit(retp)
  sum(((retp - mean(retp))/sd(retp))^moment)/NROW(retp)
}  # end calcmom
# Calculate skew and kurtosis of VTI returns
calcmom(retp, moment=3)
calcmom(retp, moment=4)

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
legend("topright", inset=0.05, title="Sigmas",
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
legend("topright", inset=0.05, bty="n",
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
  legend=c("Mixture", "Normal", "t-distribution"),
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
       cex=0.8, lwd=6, lty=1, col=colorv)

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
ks_test <- ks.test(rnorm(100), pnorm)
ks_test$p.value
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
legend("topright", inset=0.05, bty="n",
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
chisq_test <- chisq.test(x=countsn, p=countst, rescale.p=TRUE, simulate.p.value=TRUE)
chisq_test$p.value
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
likefun <- function(par, dfree, data) {
  -sum(log(dt(x=(data-par[1])/par[2], df=dfree)/par[2]))
}  # end likefun
# Demonstrate equivalence with log(dt())
likefun(c(1, 0.5), 2, 2:5)
-sum(log(dt(x=(2:5-1)/0.5, df=2)/0.5))
# Objective function is negative log-likelihood
likefun <- function(par, dfree, data) {
  sum(-log(gamma((dfree+1)/2)/(sqrt(pi*dfree)*gamma(dfree/2))) +
    log(par[2]) + (dfree+1)/2*log(1+((data-par[1])/par[2])^2/dfree))
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
  data=retp,
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
legend("topright", inset=0.05, bty="n",
  leg=c("density", "t-distr", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))

# Calculate sample from non-standard t-distribution with df=3
tdata <- scalev*rt(NROW(retp), df=3) + locv
# Q-Q plot of VTI Returns vs non-standard t-distribution
qqplot(tdata, retp, xlab="t-Dist Quantiles", ylab="VTI Quantiles",
       main="Q-Q plot of VTI Returns vs Student's t-distribution")
# Calculate quartiles of the distributions
probs <- c(0.25, 0.75)
qrets <- quantile(retp, probs)
qtdata <- quantile(tdata, probs)
# Calculate slope and plot line connecting quartiles
slope <- diff(qrets)/diff(qtdata)
intercept <- qrets[1]-slope*qtdata[1]
abline(intercept, slope, lwd=2, col="red")

# KS test for VTI returns vs t-distribution data
ks.test(retp, tdata)
# Define cumulative distribution of non-standard t-distribution
ptdistr <- function(x, dfree, locv=0, scalev=1) {
  pt((x-locv)/scalev, df=dfree)
}  # end ptdistr
# KS test for VTI returns vs cumulative t-distribution
ks.test(sample(retp, replace=TRUE), ptdistr, dfree=3, locv=locv, scalev=scalev)

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
  leg=c("density", "t-distr", "normal"),
  lwd=6, lty=1, col=c("blue", "red", "green"))

# Calculate VTI returns and trading volumes
ohlc <- rutils::etfenv$VTI
closep <- drop(coredata(quantmod::Cl(ohlc)))
retp <- rutils::diffit(log(closep))
volumv <- coredata(quantmod::Vo(ohlc))
# Calculate trailing variance
look_back <- 121
varv <- HighFreq::roll_var_ohlc(log(ohlc), method="close", look_back=look_back, scale=FALSE)
varv[1:look_back, ] <- varv[look_back+1, ]
# Calculate trailing average volume
volumr <- HighFreq::roll_var(volumv, look_back=look_back)/look_back
# dygraph plot of VTI variance and trading volumes
datav <- xts::xts(cbind(varv, volumr), zoo::index(ohlc))
colnamev <- c("variance", "volume")
colnames(datav) <- colnamev
dygraphs::dygraph(datav, main="VTI Variance and Trading Volumes") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], strokeWidth=2, axis="y", col="blue") %>%
  dySeries(name=colnamev[2], strokeWidth=2, axis="y2", col="red") %>%
  dyLegend(show="always", width=500)

# Scale returns using volume (volume clock)
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
legend("topright", inset=0.05, bty="n",
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
legend("topright", inset=0.05, bty="n",
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
cluster <- makeCluster(ncores)  # Initialize compute cluster
bootd <- parLapply(cluster, 1:10000,
  function(x, datav) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, datav=datav)  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:10000, function(x) {
    samplev <- datav[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, mc.cores=ncores)  # end mclapply
stopCluster(cluster)  # Stop R processes over cluster
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
cluster <- makeCluster(ncores)  # Initialize compute cluster
clusterExport(cluster, c("nrows", "returns"))
bootd <- parLapply(cluster, 1:10000,
  function(x) {
    samplev <- retp[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  })  # end parLapply
# Parallel bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:10000, function(x) {
    samplev <- retp[sample.int(nrows, replace=TRUE)]
    c(sd=sd(samplev), mad=mad(samplev))
  }, mc.cores=ncores)  # end mclapply
stopCluster(cluster)  # Stop R processes over cluster
bootd <- rutils::do_call(rbind, bootd)
# Means and standard errors from bootstrap
apply(bootd, MARGIN=2, function(x)
  c(mean=mean(x), stderror=sd(x)))

library(PerformanceAnalytics)
# Define target rate of return of 50 bps
targetr <- 0.005
# Calculate the full downside returns
returns_sub <- (retp - targetr)
returns_sub <- ifelse(returns_sub < 0, returns_sub, 0)
nrows <- NROW(returns_sub)
# Calculate the downside deviation
all.equal(sqrt(sum(returns_sub^2)/nrows),
  drop(DownsideDeviation(retp, MAR=targetr, method="full")))
# Calculate the subset downside returns
returns_sub <- (retp - targetr)
returns_sub <- returns_sub[returns_sub < 0]
nrows <- NROW(returns_sub)
# Calculate the downside deviation
all.equal(sqrt(sum(returns_sub^2)/nrows),
  drop(DownsideDeviation(retp, MAR=targetr, method="subset")))

# Calculate time series of VTI drawdowns
closep <- log(quantmod::Cl(rutils::etfenv$VTI))
drawdns <- (closep - cummax(closep))
# Extract the date index from the time series closep
datev <- zoo::index(closep)
# Calculate the maximum drawdown date and depth
indexmin <- which.min(drawdns)
datemin <- datev[indexmin]
maxdd <- drawdns[datemin]
# Calculate the drawdown start and end dates
startd <- max(datev[(datev < datemin) & (drawdns == 0)])
endd <- min(datev[(datev > datemin) & (drawdns == 0)])
# dygraph plot of VTI drawdowns
datav <- cbind(closep, drawdns)
colnamev <- c("VTI", "Drawdowns")
colnames(datav) <- colnamev
dygraphs::dygraph(datav, main="VTI Drawdowns") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2],
   valueRange=(1.2*range(drawdns)+0.1), independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red") %>%
  dyEvent(startd, "start drawdown", col="blue") %>%
  dyEvent(datemin, "max drawdown", col="red") %>%
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
xval <- match(datemin, datev)
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
riskstats$Sharpe <- riskstats$"Arithmetic Mean"/riskstats$Stdev
# Sort on Sharpe ratio
riskstats <- riskstats[order(riskstats$Sharpe, decreasing=TRUE), ]

# Copy from rutils to save time
riskstats <- rutils::etfenv$riskstats
# Add Sharpe ratio column
riskstats$Sharpe <- riskstats$"Arithmetic Mean"/riskstats$Stdev
# Sort on Sharpe ratio
riskstats <- riskstats[order(riskstats$Sharpe, decreasing=TRUE), ]
# Print data frame
knitr::kable(riskstats[, c("Sharpe", "Skewness", "Kurtosis")])

# Print data frame
knitr::kable(riskstats[c("VXX", "SVXY"), c("Sharpe", "Skewness", "Kurtosis")])

# dygraph plot of VXX versus SVXY
pricev <- na.omit(rutils::etfenv$pricev[, c("VXX", "SVXY")])
pricev <- pricev["2017/"]
colnamev <- c("VXX", "SVXY")
colnames(pricev) <- colnamev
dygraphs::dygraph(pricev, main="Prices of VXX and SVXY") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="green") %>%
  dyLegend(show="always", width=500) %>% dyLegend(show="always", width=500) %>%
  dyLegend(show="always", width=500)

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
    labels=riskstats$Name, pos=1, cex=0.8)

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

# Calculate VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
retp <- drop(zoo::coredata(retp))
nrows <- NROW(retp)
# Calculate compounded VTI returns
holdp <- 252
retc <- sqrt(holdp)*sapply(1:nrows, function(x) {
    mean(retp[sample.int(nrows, size=holdp, replace=TRUE)])
})  # end sapply
# Calculate mean, standard deviation, skewness, and kurtosis
datav <- cbind(retp, retc)
colnames(datav) <- c("VTI", "Agg")
apply(datav, MARGIN=2, function(x) {
  # Standardize the returns
  meanval <- mean(x); stdev <- sd(x); x <- (x - meanval)/stdev
  c(mean=meanval, stdev=stdev, skew=mean(x^3), kurt=mean(x^4))
})  # end sapply
# Calculate the Sharpe and Dowd ratios
confl <- 0.05
sapply(colnames(datav), function(name) {
  x <- datav[, name]; stdev <- sd(x)
  varisk <- unname(quantile(x, probs=confl))
  cvar <- mean(x[x < varisk])
  ratio <- 1
  if (name == colnames(datav)[2]) {ratio <- holdp}
  sqrt(252/ratio)*mean(x)/c(Sharpe=stdev, Dowd=-varisk, DowdC=-cvar)
})  # end sapply

# Plot the densities of returns
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
plot(density(retp), t="l", lwd=3, col="blue",
     xlab="returns", ylab="density", xlim=c(-0.04, 0.04),
     main="Distribution of Compounded Stock Returns")
lines(density(retc), t="l", col="red", lwd=3)
curve(expr=dnorm(x, mean=mean(retc), sd=sd(retc)), col="green", lwd=3, add=TRUE)
legend("topright", legend=c("VTI Daily", "Compounded", "Normal"),
 inset=-0.1, bg="white", lty=1, lwd=6, col=c("blue", "red", "green"), bty="n")

# Test if IEF can time VTI
retp <- na.omit(rutils::etfenv$returns[, c("IEF", "VTI")])
retvti <- retp$VTI
desv <- cbind(retp, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(desv)[3:4] <- c("merton", "treynor")
# Merton-Henriksson test
regmod <- lm(IEF ~ VTI + merton, data=desv); summary(regmod)

# Treynor-Mazuy test
regmod <- lm(IEF ~ VTI + treynor, data=desv); summary(regmod)
# Plot residual scatterplot
x11(width=6, height=5)
resids <- (desv$IEF - regmod$coeff["VTI"]*retvti)
plot.default(x=retvti, y=resids, xlab="VTI", ylab="IEF")
title(main="Treynor-Mazuy Market Timing Test\n for IEF vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fittedv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retvti, y=fittedv, pch=16, col="red")
text(x=0.0, y=0.8*max(resids), paste("Treynor test t-value =", tvalue))

library(rutils)
# Extract the ETF prices from rutils::etfenv$prices
pricev <- rutils::etfenv$prices
pricev <- zoo::na.locf(pricev, na.rm=FALSE)
pricev <- zoo::na.locf(pricev, fromLast=TRUE)
datev <- zoo::index(pricev)
# Calculate the simple dollar returns
retd <- rutils::diffit(pricev)
# Or
# retd <- lapply(pricev, rutils::diffit)
# retd <- rutils::do_call(cbind, retd)
# Calculate the percentage returns
retp <- retd/rutils::lagit(pricev, lagg=1, pad_zeros=FALSE)
# Calculate the log returns
retl <- rutils::diffit(log(pricev))

# Set the initial dollar returns
retd[1, ] <- pricev[1, ]
# Calculate the prices from dollar returns
pricen <- cumsum(retd)
all.equal(pricen, pricev)
# Compound the percentage returns
pricen <- cumprod(1+retp)
# Set the initial prices
pricesi <- as.numeric(pricev[1, ])
pricen <- lapply(1:NCOL(pricen), function (i) pricesi[i]*pricen[, i])
pricen <- rutils::do_call(cbind, pricen)
# pricen <- t(t(pricen)*pricesi)
all.equal(pricen, pricev, check.attributes=FALSE)

# Plot log VTI prices
endd <- rutils::calc_endpoints(rutils::etfenv$VTI, interval="weeks")
dygraphs::dygraph(log(quantmod::Cl(rutils::etfenv$VTI)[endd]),
  main="Logarithm of VTI Prices") %>%
  dyOptions(colors="blue", strokeWidth=2) %>%
  dyLegend(show="always", width=200)

# Calculate the percentage VTI returns
pricev <- rutils::etfenv$prices$VTI
pricev <- na.omit(pricev)
retp <- rutils::diffit(pricev)/rutils::lagit(pricev, lagg=1, pad_zeros=FALSE)

# Funding rate per day
frate <- 0.01/252
# Margin account
margin <- cumsum(retp)
# Cumulative funding costs
fcosts <- cumsum(frate*margin)
# Add funding costs to margin account
margin <- (margin + fcosts)
# dygraph plot of margin and funding costs
datav <- cbind(margin, fcosts)
colnamev <- c("Margin", "Cumulative Funding")
colnames(datav) <- colnamev
endd <- rutils::calc_endpoints(datav, interval="weeks")
dygraphs::dygraph(datav[endd], main="VTI Margin Funding Costs") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=200)

# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Cumulative transaction costs
costs <- bid_offer*cumsum(abs(retp))/2
# Subtract transaction costs from margin account
margin <- cumsum(retp)
margin <- (margin - costs)
# dygraph plot of margin and transaction costs
datav <- cbind(margin, costs)
colnamev <- c("Margin", "Cumulative Transaction Costs")
colnames(datav) <- colnamev
dygraphs::dygraph(datav[endd], main="VTI Transaction Costs") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=200)

# Calculate the VTI and IEF dollar returns
pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- na.omit(pricev)
retd <- rutils::diffit(pricev)
datev <- zoo::index(pricev)
# Calculate the VTI and IEF percentage returns
retp <- retd/rutils::lagit(pricev, lagg=1, pad_zeros=FALSE)
# Wealth of fixed shares equal to $0.5 each (without rebalancing)
weightv <- c(0.5, 0.5)  # dollar weights
wealth_tsa <- drop(cumprod(1+retp) %*% weightv)
# Or using the dollar returns
pricesi <- as.numeric(pricev[1, ])
retd[1, ] <- pricev[1, ]
wealth_tsa2 <- cumsum(retd %*% (weightv/pricesi))
all.equal(wealth_tsa, drop(wealth_tsa2))

# Wealth of fixed dollars (with rebalancing)
wealth_tda <- cumsum(retp %*% weightv)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(log(wealth_tsa), wealth_tda)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Fixed shares", "Fixed dollars")
sqrt(252)*sapply(rutils::diffit(wealthv),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the log wealth
colnamev <- colnames(wealthv)
endd <- rutils::calc_endpoints(retp, interval="weeks")
dygraphs::dygraph(wealthv[endd], main="Wealth of Weighted Portfolios") %>%
  dySeries(name=colnamev[1], col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=200)

# Margin account for fixed dollars (with rebalancing)
margin <- cumsum(retp %*% weightv)
# Cumulative transaction costs
costs <- bid_offer*cumsum(abs(retp) %*% weightv)/2
# Subtract transaction costs from margin account
margin <- (margin - costs)
# dygraph plot of margin and transaction costs
datav <- cbind(margin, costs)
datav <- xts::xts(datav, datev)
colnamev <- c("Margin", "Cumulative Transaction Costs")
colnames(datav) <- colnamev
dygraphs::dygraph(datav[endd], main="Fixed Dollar Portfolio Transaction Costs") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=200)

# Wealth of fixed shares (without rebalancing)
wealth_tsa <- cumsum(retd %*% (weightv/pricesi))
# Or compound the percentage returns
wealth_tsa <- cumprod(1+retp) %*% weightv
# Wealth of proportional allocations (with rebalancing)
wealth_pda <- cumprod(1 + retp %*% weightv)
wealthv <- cbind(wealth_tsa, wealth_pda)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Fixed shares", "Prop dollars")
wealthv <- log(wealthv)
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the log wealth
dygraphs::dygraph(wealthv[endd],
  main="Wealth of Proportional Dollar Allocations") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=200)

# Returns in excess of weighted returns
retw <- retp %*% weightv
retx <- lapply(retp, function(x) (retw - x))
retx <- do.call(cbind, retx)
sum(retx %*% weightv)
# Calculate the weighted sum of absolute excess returns
retx <- abs(retx) %*% weightv
# Total dollar amount of stocks that need to be traded
retx <- retx*rutils::lagit(wealth_pda)
# Cumulative transaction costs
costs <- bid_offer*cumsum(retx)/2
# Subtract transaction costs from wealth
wealth_pda <- (wealth_pda - costs)

# dygraph plot of wealth and transaction costs
wealthv <- cbind(wealth_pda, costs)
wealthv <- xts::xts(wealthv, datev)
colnamev <- c("Wealth", "Cumulative Transaction Costs")
colnames(wealthv) <- colnamev
dygraphs::dygraph(wealthv[endd],
  main="Transaction Costs With Proportional Allocations") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=200)

# Wealth of fixed shares (without rebalancing)
wealth_tsa <- drop(apply(retp, 2, function(x) cumprod(1+x)) %*% weightv)-1
# Wealth of proportional dollar allocations (with rebalancing)
wealth_pda <- cumprod(1 + retp %*% weightv) - 1
# Wealth of proportional target allocation (with rebalancing)
retp <- zoo::coredata(retp)
threshv <- 0.05
wealthv <- matrix(nrow=NROW(retp), ncol=2)
colnames(wealthv) <- colnames(retp)
wealthv[1, ] <- weightv
for (it in 2:NROW(retp)) {
  # Accrue wealth without rebalancing
  wealthv[it, ] <- wealthv[it-1, ]*(1 + retp[it, ])
  # Rebalance if wealth allocations differ from weights
  if (sum(abs(wealthv[it, ] - sum(wealthv[it, ])*weightv))/sum(wealthv[it, ]) > threshv) {
    # cat("Rebalance at:", it, "\n")
    wealthv[it, ] <- sum(wealthv[it, ])*weightv
  } # end if
} # end for
wealthv <- rowSums(wealthv) - 1
wealthv <- cbind(wealth_pda, wealthv)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Proportional Allocations", "Proportional Target")
dygraphs::dygraph(wealthv, main="Wealth of Proportional Target Allocations") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=200)

# Calculate the stock and bond returns
retp <- na.omit(rutils::etfenv$returns[, c("VTI", "IEF")])
weightv <- c(0.4, 0.6)
retp <- cbind(retp, retp %*% weightv)
colnames(retp)[3] <- "Combined"
# Calculate the correlations
cor(retp)
# Calculate the Sharpe ratios
sqrt(252)*sapply(retp, function(x) mean(x)/sd(x))
# Calculate the standard deviation, skewness, and kurtosis
sapply(retp, function(x) {
  # Calculate the standard deviation
  stdev <- sd(x)
  # Standardize the returns
  x <- (x - mean(x))/stdev
  c(stdev=stdev, skew=mean(x^3), kurt=mean(x^4))
})  # end sapply

# Wealth of proportional allocations
wealthv <- cumprod(1 + retp)
# Calculate the a vector of monthly end points
endd <- rutils::calc_endpoints(retp, interval="weeks")
# Plot cumulative log wealth
dygraphs::dygraph(log(wealthv[endd]),
  main="Stocks and Bonds With Proportional Allocations") %>%
  dyOptions(colors=c("blue", "green", "blue", "red")) %>%
  dySeries("Combined", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=200)

# Calculate the Sharpe ratios
sqrt(252)*sapply(retp, function(x) mean(x)/sd(x))
# Calculate the Sharpe ratios for vector of weights
weightv <- seq(0.05, 0.95, 0.05)
sharpev <- sqrt(252)*sapply(weightv, function(weight) {
  weightv <- c(weight, 1-weight)
  retp <- (retp[, 1:2] %*% weightv)
  mean(retp)/sd(retp)
})  # end sapply
# Calculate the optimal VTI weight
weightm <- weightv[which.max(sharpev)]
# Calculate the optimal weight using optimization
calc_sharpe <- function(weight) {
  weightv <- c(weight, 1-weight)
  retp <- (retp[, 1:2] %*% weightv)
  -mean(retp)/sd(retp)
}  # end calc_sharpe
optv <- optimize(calc_sharpe, interval=c(0, 1))
weightm <- optv$minimum

# Plot Sharpe ratios
plot(x=weightv, y=sharpev,
     main="Sharpe Ratio as Function of VTI Weight",
     xlab="VTI weight", ylab="Sharpe Ratio",
     t="l", lwd=3, col="blue")
abline(v=weightm, lty="dashed", lwd=1, col="blue")
text(x=weightm, y=0.7*max(sharpev), pos=4, cex=1.2,
     labels=paste("optimal VTI weight =", round(weightm, 2)))

# Coerce the returns from xts time series to matrix
retp <- zoo::coredata(retp[, 1:2])
nrows <- NROW(retp)
# Bootstrap the returns and Calculate the a list of random returns
nboot <- 1e4
library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
# Perform parallel bootstrap under Windows
cluster <- makeCluster(ncores)  # Initialize compute cluster under Windows
clusterSetRNGStream(cluster, 1121)  # Reset random number generator in all cores
clusterExport(cluster, c("retp", "nrows"))
bootd <- parLapply(cluster, 1:nboot, function(x) {
  retp[sample.int(nrows, replace=TRUE), ]
})  # end parLapply
# Perform parallel bootstrap under Mac-OSX or Linux
set.seed(1121)
bootd <- mclapply(1:nboot, function(x) {
  retp[sample.int(nrows, replace=TRUE), ]
}, mc.cores=ncores)  # end mclapply
is.list(bootd); NROW(bootd); dim(bootd[[1]])

# Calculate the distribution of terminal wealths under Windows
wealthv <- parLapply(cluster, bootd, function(retp) {
  apply(retp, 2, function(x) prod(1+x))
})  # end parLapply
# Calculate the distribution of terminal wealths under Mac-OSX or Linux
wealthv <- mclapply(bootd, function(retp) {
  apply(retp, 2, function(x) prod(1+x))
}, mc.cores=ncores)  # end mclapply
wealthv <- do.call(rbind, wealthv)
class(wealthv); dim(wealthv); tail(wealthv)
# Calculate the means and standard deviations of the terminal wealths
apply(wealthv, 2, mean)
apply(wealthv, 2, sd)
# Extract the terminal wealths of VTI and IEF
wealthvti <- wealthv[, "VTI"]
wealthief <- wealthv[, "IEF"]

# Plot the densities of the terminal wealths of VTI and IEF
meanvti <- mean(wealthvti); meanief <- mean(wealthief)
densvti <- density(wealthvti); densief <- density(wealthief)
plot(densvti, col="blue", lwd=3, xlab="wealth",
     xlim=c(0, 2*max(densief$x)), ylim=c(0, max(densief$y)),
     main="Terminal Wealth Distributions of VTI and IEF")
lines(densief, col="green", lwd=3)
abline(v=meanvti, col="blue", lwd=2, lty="dashed")
text(x=meanvti, y=0.5, labels="VTI mean", pos=4, cex=0.8)
abline(v=meanief, col="green", lwd=2, lty="dashed")
text(x=meanief, y=0.5, labels="IEF mean", pos=4, cex=0.8)
legend(x="topright", legend=c("VTI", "IEF"),
 inset=0.1, cex=1.0, bg="white", bty="n", y.intersp=0.5,
 lwd=6, lty=1, col=c("blue", "green"))

# Calculate the distributions of stock wealth
holdv <- nrows*seq(0.1, 1.0, 0.1)
wealthm <- mclapply(bootd, function(retp) {
  sapply(holdv, function(holdp) {
    prod(1 + retp[1:holdp, "VTI"])
  })  # end sapply
}, mc.cores=ncores)  # end mclapply
wealthm <- do.call(rbind, wealthm)
dim(wealthm)
# Define the risk-adjusted wealth measure
riskretfun <- function(wealthv) {
  riskv <- 0.01
  if (min(wealthv) < 1)
    riskv <- mean((1-wealthv)[wealthv<1])
  mean(wealthv)/riskv
}  # end riskretfun
# Calculate the stock wealth risk-return ratios
riskrets <- apply(wealthm, 2, riskretfun)
# Plot the stock wealth risk-return ratios
plot(x=holdv, y=riskrets,
     main="Stock Risk-Return Ratio as Function of Holding Period",
     xlab="Holding Period", ylab="Ratio",
     t="l", lwd=3, col="blue")

# Plot the stock wealth for long and short holding periods
wealth1 <- wealthm[, 9]
wealth2 <- wealthm[, 1]
mean1 <- mean(wealth1); mean2 <- mean(wealth2)
dens1 <- density(wealth1); dens2 <- density(wealth2)
plot(dens1, col="blue", lwd=3, xlab="wealth",
     xlim=c(0, 2*max(dens2$x)), ylim=c(0, max(dens2$y)),
     main="Wealth Distributions for Long and Short Holding Periods")
lines(dens2, col="green", lwd=3)
abline(v=mean1, col="blue", lwd=2, lty="dashed")
text(x=mean1, y=0.5, labels="Long", pos=4, cex=0.8)
abline(v=mean2, col="green", lwd=2, lty="dashed")
text(x=mean2, y=0.5, labels="Short", pos=4, cex=0.8)
legend(x="top", legend=c("Long", "Short"),
 inset=0.1, cex=1.0, bg="white", bty="n", y.intersp=0.5,
 lwd=6, lty=1, col=c("blue", "green"))

# Calculate the distributions of portfolio wealth
weightv <- seq(0.05, 0.95, 0.05)
wealthm <- mclapply(bootd, function(retp) {
  sapply(weightv, function(weight) {
    prod(1 + retp %*% c(weight, 1-weight))
  })  # end sapply
}, mc.cores=ncores)  # end mclapply
wealthm <- do.call(rbind, wealthm)
dim(wealthm)
# Calculate the portfolio risk-return ratios
riskrets <- apply(wealthm, 2, riskretfun)
# Calculate the optimal VTI weight
weightm <- weightv[which.max(riskrets)]

# Plot the portfolio risk-return ratios
plot(x=weightv, y=riskrets,
     main="Portfolio Risk-Return Ratio as Function of VTI Weight",
     xlab="VTI weight", ylab="Ratio",
     t="l", lwd=3, col="blue")
abline(v=weightm, lty="dashed", lwd=1, col="blue")
text(x=weightm, y=0.7*max(riskrets), pos=4, cex=1.2,
     labels=paste("optimal VTI weight =", round(weightm, 2)))

# Extract the ETF returns
symbolv <- c("VTI", "IEF", "DBC")
retp <- na.omit(rutils::etfenv$returns[, symbolv])
# Calculate the all-weather portfolio wealth
weightsaw <- c(0.30, 0.55, 0.15)
retp <- cbind(retp, retp %*% weightsaw)
colnames(retp)[4] <- "All Weather"
# Calculate the Sharpe ratios
sqrt(252)*sapply(retp, function(x) mean(x)/sd(x))

# Calculate the cumulative wealth from returns
wealthv <- cumprod(1+retp)
# Calculate the a vector of monthly end points
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
# dygraph all-weather wealth
dygraphs::dygraph(wealthv[endd], main="All-Weather Portfolio") %>%
  dyOptions(colors=c("blue", "green", "orange", "red")) %>%
  dySeries("All Weather", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=200)
# Plot all-weather wealth
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green", "red")
quantmod::chart_Series(wealthv, theme=plot_theme, lwd=c(2, 2, 2, 4),
       name="All-Weather Portfolio")
legend("topleft", legend=colnames(wealthv),
  inset=0.1, bg="white", lty=1, lwd=6, y.intersp=0.5,
  col=plot_theme$col$line.col, bty="n")

# Calculate the VTI returns
retp <- na.omit(rutils::etfenv$returns$VTI["2008/2009"])
datev <- zoo::index(retp)
nrows <- NROW(retp)
retp <- drop(zoo::coredata(retp))
# Bond floor
bfloor <- 60
# CPPI multiplier
coeff <- 2
# Portfolio market values
portfv <- numeric(nrows)
# Initial principal
portfv[1] <- 100
# Stock allocation
stockv <- numeric(nrows)
stockv[1] <- min(coeff*(portfv[1] - bfloor), portfv[1])
# Bond allocation
bondv <- numeric(nrows)
bondv[1] <- (portfv[1] - stockv[1])

# Simulate CPPI strategy
for (t in 2:nrows) {
  portfv[t] <- portfv[t-1] + stockv[t-1]*retp[t]
  stockv[t] <- min(coeff*(portfv[t] - bfloor), portfv[t])
  bondv[t] <- (portfv[t] - stockv[t])
}  # end for
# dygraph plot of CPPI strategy
pricev <- 100*cumprod(1+retp)
datav <- xts::xts(cbind(stockv, bondv, portfv, pricev), datev)
colnames(datav) <- c("stocks", "bonds", "CPPI", "VTI")
endd <- rutils::calc_endpoints(datav, interval="weeks")
dygraphs::dygraph(datav[endd], main="CPPI strategy") %>%
  dyOptions(colors=c("red", "green", "blue", "orange"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the dollar and percentage returns for VTI and IEF.
pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- na.omit(pricev)
retd <- rutils::diffit(pricev)
retp <- retd/rutils::lagit(pricev, lagg=1, pad_zeros=FALSE)
# Calculate the standardized simple dollar returns
retstd <- lapply(retd, function(x) x/sd(x))
retstd <- do.call(cbind, retstd)
sapply(retstd, sd)
# Wealth of fixed number of shares (without rebalancing)
weightv <- c(0.5, 0.5)
pricesi <- as.numeric(pricev[1, ])
wealth_tsa <- cumsum(retd %*% (weightv/pricesi))
# Calculate the standardized percentage returns
retstdp <- lapply(retp, function(x) x/sd(x))
retstdp <- do.call(cbind, retstdp)
sapply(retstdp, sd)
# Wealth of target dollar allocation of shares (with rebalancing)
wealth_tda <- cumsum(retstdp %*% weightv)
# Plot log wealth
wealthv <- cbind(wealth_tda, log(wealth_tsa))
# wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("With rebalancing", "Without rebalancing")
dygraphs::dygraph(wealthv, main="Wealth of Equal Dollar Amount of Shares") %>%
  dyOptions(colors=c("green", "blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=200)

# Calculate the dollar and percentage returns for VTI and IEF.
pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- na.omit(pricev)
retd <- rutils::diffit(pricev)
retp <- retd/rutils::lagit(pricev, lagg=1, pad_zeros=FALSE)
# Calculate the wealth of proportional allocations.
weightv <- c(0.5, 0.5)
retw <- retp %*% weightv
wealth_pda <- cumprod(1 + retw)
# Calculate the rolling percentage volatility.
look_back <- 21
volat <- HighFreq::roll_var(retp, look_back=look_back)
iszero <- (rowSums(volat) == 0)
volat[iszero, ] <- 1
# Calculate the risk parity portfolio allocations.
alloc <- lapply(1:NCOL(pricev),
  function(x) weightv[x]/volat[, x])
alloc <- do.call(cbind, alloc)
# Scale allocations to 1 dollar total.
alloc <- alloc/rowSums(alloc)
# Lag the allocations
alloc <- rutils::lagit(alloc)
# Calculate the wealth of risk parity.
retw <- rowSums(retp*alloc)
wealth_risk_parity <- cumprod(1 + retw)

# Calculate the log wealths.
wealthv <- log(cbind(wealth_pda, wealth_risk_parity))
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Fixed Ratio", "Risk Parity")
# Calculate the Sharpe ratios.
sqrt(252)*sapply(rutils::diffit(wealthv), function (x) mean(x)/sd(x))
# Plot a dygraph of the log wealths.
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(wealthv[endd],
  main="Log Wealth of Risk Parity vs Proportional Allocations") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=200)

# Test risk parity market timing of VTI using Treynor-Mazuy test
retrp <- rutils::diffit(wealthv)
retvti <- retp$VTI
desv <- cbind(retrp, retvti, retvti^2)
desv <- na.omit(desv)
colnames(desv)[1:2] <- c("fixed", "risk_parity")
colnames(desv)[4] <- "treynor"
regmod <- lm(risk_parity ~ VTI + treynor, data=desv)
summary(regmod)
# Plot residual scatterplot
resids <- regmod$residuals
plot.default(x=retvti, y=resids, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for Risk Parity vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fittedv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retvti, y=fittedv, pch=16, col="red")
text(x=0.0, y=0.8*max(resids), paste("Treynor test t-value =", tvalue))

# Test for fixed ratio market timing of VTI using Treynor-Mazuy test
regmod <- lm(fixed ~ VTI + treynor, data=desv)
summary(regmod)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fittedv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
points.default(x=retvti, y=fittedv, pch=16, col="blue")
text(x=0.05, y=0.6*max(resids), paste("Fixed Ratio t-value =", round(coefreg["treynor", "t value"], 2)))

# Calculate the positions
retp <- na.omit(rutils::etfenv$returns$VTI)
posv <- rep(NA_integer_, NROW(retp))
datev <- zoo::index(retp)
datev <- format(datev, "%m-%d")
posv[datev == "05-01"] <- 0
posv[datev == "05-03"] <- 0
posv[datev == "11-01"] <- 1
posv[datev == "11-03"] <- 1
# Carry forward and backward non-NA posv
posv <- zoo::na.locf(posv, na.rm=FALSE)
posv <- zoo::na.locf(posv, fromLast=TRUE)
# Calculate the strategy returns
sell_inmay <- posv*retp
wealthv <- cbind(retp, sell_inmay)
colnames(wealthv) <- c("VTI", "sell_in_may")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot wealth of Sell in May strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Sell in May Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=200)
# OR: Open x11 for plotting
x11(width=6, height=5)
par(mar=c(4, 4, 3, 1), oma=c(0, 0, 0, 0))
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue", "red")
quantmod::chart_Series(wealthv, theme=plot_theme, name="Sell in May Strategy")
legend("topleft", legend=colnames(wealthv),
  inset=0.1, bg="white", lty=1, lwd=6, y.intersp=0.5,
  col=plot_theme$col$line.col, bty="n")

# Test if Sell in May strategy can time VTI
desv <- cbind(wealth$sell_in_may, 0.5*(retp+abs(retp)), retp^2)
colnames(desv) <- c("VTI", "merton", "treynor")
# Perform Merton-Henriksson test
regmod <- lm(sell_inmay ~ VTI + merton, data=desv)
summary(regmod)
# Perform Treynor-Mazuy test
regmod <- lm(sell_inmay ~ VTI + treynor, data=desv)
summary(regmod)
# Plot Treynor-Mazuy residual scatterplot
resids <- (sell_inmay - regmod$coeff["VTI"]*retp)
plot.default(x=retp, y=resids, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for Sell in May vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fittedv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retp
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retp, y=fittedv, pch=16, col="red")
text(x=0.0, y=0.8*max(resids), paste("Treynor test t-value =", tvalue))

# Calculate the log of OHLC VTI prices
ohlc <- log(rutils::etfenv$VTI)
openp <- quantmod::Op(ohlc)
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
closep <- quantmod::Cl(ohlc)
# Calculate the close-to-close log returns, the intraday
# open-to-close returns and the overnight close-to-open returns.
close_close <- rutils::diffit(closep)
colnames(close_close) <- "close_close"
open_close <- (closep - openp)
colnames(open_close) <- "open_close"
close_open <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE))
colnames(close_open) <- "close_open"

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(close_close, close_open, open_close)
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot log wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Wealth of Close-to-Close, Close-to-Open, and Open-to-Close Strategies") %>%
  dySeries(name="close_close", label="Close-to-Close (static)", strokeWidth=2, col="blue") %>%
  dySeries(name="close_open", label="Close-to-Open (overnight)", strokeWidth=2, col="red") %>%
  dySeries(name="open_close", label="Open-to-Close (daytime)", strokeWidth=2, col="green") %>%
  dyLegend(width=600)

# Calculate the VTI returns
retp <- na.omit(rutils::etfenv$returns$VTI)
datev <- zoo::index(retp)
# Calculate the first business day of every month
dayv <- as.numeric(format(datev, "%d"))
indeks <- which(rutils::diffit(dayv) < 0)
datev[head(indeks)]
# Calculate the Turn of the Month dates
indeks <- lapply((-1):2, function(x) indeks + x)
indeks <- do.call(c, indeks)
sum(indeks > NROW(datev))
indeks <- sort(indeks)
datev[head(indeks, 11)]
# Calculate the Turn of the Month pnls
pnls <- numeric(NROW(retp))
pnls[indeks] <- retp[indeks, ]

# Combine data
wealthv <- cbind(retp, pnls)
colnamev <- c("VTI", "Strategy")
colnames(wealthv) <- colnamev
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot VTI Turn of the Month strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Turn of the Month Strategy") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="red")

# Calculate the VTI returns
retp <- na.omit(rutils::etfenv$returns$VTI)
datev <- zoo::index(retp)
retp <- drop(zoo::coredata(retp))
nrows <- NROW(retp)
# Simulate stop-loss strategy
stopl <- 0.05
maxp <- 0.0
retc <- 0.0
pnls <- retp
for (i in 1:(nrows-1)) {
# Calculate the drawdown
  retc <- retc + retp[i]
  maxp <- max(maxp, retc)
  dd <- (retc - maxp)
# Check for stop-loss
  if (dd < -stopl*maxp)
    pnls[i+1] <- 0
}  # end for
# Same but without using explicit loops
cumsumv <- cumsum(retp)
cummaxv <- cummax(cumsumv)
dd <- (cumsumv - cummaxv)
pnls2 <- retp
isdd <- rutils::lagit(dd < -stopl*cummaxv)
pnls2 <- ifelse(isdd, 0, pnls2)
all.equal(pnls, pnls2)

# Combine data
wealthv <- xts::xts(cbind(retp, pnls), datev)
colnamev <- c("VTI", "Strategy")
colnames(wealthv) <- colnamev
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot VTI stop-loss strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="VTI Stop-loss Strategy") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="red")

# Simulate multiple stop-loss strategies
cumsumv <- cumsum(retp)
cummaxv <- cummax(cumsumv)
dd <- (cumsumv - cummaxv)
stopv <- 0.01*(1:30)
cum_pnls <- sapply(stopv, function(stopl) {
  pnls <- retp
  isdd <- rutils::lagit(dd < -stopl*cummaxv)
  pnls <- ifelse(isdd, 0, pnls)
  sum(pnls)
})  # end sapply

# Plot cumulative pnls for stop-loss strategies
plot(x=stopv, y=cum_pnls,
     main="Cumulative PnLs for Stop-loss Strategies",
     xlab="stop-loss level", ylab="cumulative pnl",
     t="l", lwd=3, col="blue")

NA

App setup code that runs only once at startup.
ndata <- 1e4
stdev <- 1.0

Define the user interface
uiface <- shiny::fluidPage(
  # Create numeric input for the number of data points.
  numericInput("ndata", "Number of data points:", value=ndata),
  # Create slider input for the standard deviation parameter.
  sliderInput("stdev", label="Standard deviation:",
        min=0.1, max=3.0, value=stdev, step=0.1),
  # Render plot in a panel.
  plotOutput("plotobj", height=300, width=500)
)  # end user interface

Define the server function
servfun <- function(input, output) {
  output$plotobj <- shiny::renderPlot({
    # Simulate the data
    datav <- rnorm(input$ndata, sd=input$stdev)
    # Plot the data
    par(mar=c(2, 4, 4, 0), oma=c(0, 0, 0, 0))
    hist(datav, xlim=c(-4, 4), main="Histogram of Random Data")
  })  # end renderPlot
}  # end servfun

# Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)

Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel("VWAP Moving Average"),
  # Create single row of widgets with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=3, selectInput("symbol", label="Symbol",
                          choices=symbolv, selected=symbol)),
    # Input look-back interval
    column(width=3, sliderInput("look_back", label="Lookback interval",
                          min=1, max=150, value=11, step=1))
  ),  # end fluidRow
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface

Define the server function
servfun <- shiny::shinyServer(function(input, output) {
  # Get the close and volume data in a reactive environment
  closep <- shiny::reactive({
    # Get the data
    ohlc <- get(input$symbol, data_env)
    closep <- log(quantmod::Cl(ohlc))
    volum <- quantmod::Vo(ohlc)
    # Return the data
    cbind(closep, volum)
  })  # end reactive code

  # Calculate the VWAP indicator in a reactive environment
  vwapv <- shiny::reactive({
    # Get model parameters from input argument
    look_back <- input$look_back
    # Calculate the VWAP indicator
    closep <- closep()[, 1]
    volum <- closep()[, 2]
    vwapv <- HighFreq::roll_sum(tseries=closep*volum, look_back=look_back)
    volumroll <- HighFreq::roll_sum(tseries=volum, look_back=look_back)
    vwapv <- vwapv/volumroll
    vwapv[is.na(vwapv)] <- 0
    # Return the plot data
    datav <- cbind(closep, vwapv)
    colnames(datav) <- c(input$symbol, "VWAP")
    datav
  })  # end reactive code

  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    colnamev <- colnames(vwapv())
    dygraphs::dygraph(vwapv(), main=paste(colnamev[1], "VWAP")) %>%
dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")
  })  # end output plot
})  # end server code

Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)

Define the server function
servfun <- shiny::shinyServer(function(input, output) {

  # Create an empty list of reactive values.
  value_s <- reactiveValues()

  # Get input parameters from the user interface.
  nrows <- reactive({
    # Add nrows to list of reactive values.
    value_s*nrows <- input$nrows
    input$nrows
  })  # end reactive code

  # Broadcast a message to the console when the button is pressed.
  observeEvent(eventExpr=input$button, handlerExpr={
    cat("Input button pressed\n")
  })  # end observeEvent

  # Send the data when the button is pressed.
  datav <- eventReactive(eventExpr=input$button, valueExpr={
    # eventReactive() executes on input$button, but not on nrows() or input$nrows.
    cat("Sending", nrows(), "rows of data\n")
    datav <- head(mtcars, input$nrows)
    value_s$mpg <- mean(datav$mpg)
    datav
  })  # end eventReactive
  #   datav

  # Draw table of the data when the button is pressed.
  observeEvent(eventExpr=input$button, handlerExpr={
    datav <- datav()
    cat("Received", value_s*nrows, "rows of data\n")
    cat("Average mpg = ", value_s$mpg, "\n")
    cat("Drawing table\n")
    output$tablev <- renderTable(datav)
  })  # end observeEvent

})  # end server code

Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)
