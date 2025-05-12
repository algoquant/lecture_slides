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
# KS test for VTI returns vs t-distribution data
ks.test(retp, datat)
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
x11(width=6, height=5)
par(mar=c(4, 3, 1, 1), oma=c(0, 0, 0, 0))
# Calculate VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
# Calculate trailing VTI variance using package roll
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
# Simulate a Brownian motion path
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
pathv <- cumsum(rnorm(nrows))
plot(pathv, type="l", xlab="time", ylab="path",
     main="Brownian Motion")
# Plot the density of Brownian Motion
curve(expr=dnorm(x), xlim=c(-4, 4), ylim=c(0, 0.9),
  xlab="B_T", ylab="density", lwd=2, col="blue")
# Plot the density of the maximum of Brownian Motion
curve(expr=2*dnorm(x), xlim=c(0, 4), xlab="", ylab="",
  lwd=2, col="red", add=TRUE)
lines(x=c(0, 0), y=c(0, sqrt(2/pi)), lwd=2, col="red")
lines(x=c(-4, 0), y=c(0, 0), lwd=2, col="red")
title(main="Probability Density of
The Maximum Value of Brownian Motion", line=0.5)
legend("topright", inset=0.0, bty="n", y.intersp=0.4,
 title=NULL, c("Brownian", "Max"), lwd=6,
 col=c("blue", "red"))
# Series element
fun1 <- function(n, r) { 2*sin((n-0.5)*pi)/((n-0.5)*pi) *
  (1-exp(-((n-0.5)^2)*pi^2/2/r^2)) }
# fun2 <- function(x) { sum(sapply(1:10, function(n) fun1(n, x))) }
# fun2 <- function(x) { fun1(1, x) + fun1(2, x) + fun1(3, x) + fun1(4, x) + fun1(5, x) + fun1(6, x) }
# Sum of fun1
fun2 <- function(x) {
  valf <- 0
  for (n in 1:20) { valf <- valf + fun1(n, x) }
  return(valf)
  } # end fun2
# Theoretical average value of the range
fun2(2)
# Average value of the range from integration (not quite close)
integrate(fun2, lower=0.01, upper=4)
# Plot the density of Brownian Motion
curve(expr=dnorm(x), xlim=c(-4, 4), ylim=c(0, 1.0),
  xlab="B_T", ylab="density", lwd=2, col="blue")
# Plot the density of the range of Brownian Motion
curve(expr=fun2(x), xlim=c(0, 4), xlab="", ylab="",
  lwd=2, col="red", add=TRUE)
lines(x=c(0, 0), y=c(0, fun2(0.01)), lwd=2, col="red")
lines(x=c(-4, 0), y=c(0, 0), lwd=2, col="red")
title(main="Probability Density of
The Range of Brownian Motion", line=0.5)
legend("topright", inset=0.0, bty="n", y.intersp=0.7,
 title=NULL, c("Brownian", "Range"), lwd=6,
 col=c("blue", "red"))
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
# Sample from Standard Normal Distribution
nrows <- 1000
datav <- rnorm(nrows)
# Sample mean - MC estimate
mean(datav)
# Sample standard deviation - MC estimate
sd(datav)
# Monte Carlo estimate of cumulative probability
pnorm(1)
sum(datav < 1)/nrows
# Monte Carlo estimate of quantile
confl <- 0.98
qnorm(confl)  # Exact value
cutoff <- confl*nrows
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
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
barl <- 20  # Barrier level
nrows <- 1000  # Number of simulation steps
pathv <- numeric(nrows)  # Allocate path vector
pathv[1] <- rnorm(1)  # Initialize path
it <- 2  # Initialize simulation index
while ((it <= nrows) && (pathv[it - 1] < barl)) {
# Simulate next step
  pathv[it] <- pathv[it - 1] + rnorm(1)
  it <- it + 1  # Advance index
}  # end while
# Fill remaining path after it crosses barl
if (it <= nrows)
  pathv[it:nrows] <- pathv[it - 1]
# Plot the Brownian motion
x11(width=6, height=4)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
plot(pathv, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=barl, lwd=3, col="red")
title(main="Brownian Motion Crossing a Barrier Level", line=0.5)
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
barl <- 20  # Barrier level
nrows <- 1000  # Number of simulation steps
# Simulate path of Brownian motion
pathv <- cumsum(rnorm(nrows))
# Find index when path crosses barl
crossp <- which(pathv > barl)
# Fill remaining path after it crosses barl
if (NROW(crossp)>0) {
  pathv[(crossp[1]+1):nrows] <- pathv[crossp[1]]
}  # end if
# Plot the Brownian motion
x11(width=6, height=4)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
plot(pathv, type="l", col="black",
     lty="solid", lwd=2, xlab="", ylab="")
abline(h=barl, lwd=3, col="red")
title(main="Brownian Motion Crossing a Barrier Level", line=0.5)
# Define the daily volatility and growth rate
sigmav <- 0.01; drift <- 0.0; nrows <- 1000
# Simulate geometric Brownian motion
retp <- sigmav*rnorm(nrows) + drift - sigmav^2/2
pricev <- exp(cumsum(retp))
plot(pricev, type="l", xlab="time", ylab="prices",
     main="Geometric Brownian Motion")
# Simulate geometric Brownian motion
sigmav <- 0.01/sqrt(48)
drift <- 0.0
nrows <- 1e4
datev <- seq(from=as.POSIXct(paste(Sys.Date()-250, "09:30:00")),
  length.out=nrows, by="30 min")
pricev <- exp(cumsum(sigmav*rnorm(nrows) + drift - sigmav^2/2))
pricev <- xts(pricev, order.by=datev)
pricev <- cbind(pricev,
  volume=sample(x=10*(2:18), size=nrows, replace=TRUE))
# Aggregate to daily OHLC data
ohlc <- xts::to.daily(pricev)
quantmod::chart_Series(ohlc, name="random prices")
# dygraphs candlestick plot using pipes syntax
library(dygraphs)
dygraphs::dygraph(ohlc[, 1:4]) %>% dyCandlestick()
# dygraphs candlestick plot without using pipes syntax
dygraphs::dyCandlestick(dygraphs::dygraph(ohlc[, 1:4]))
# Standard deviations of log-normal distribution
sigmavs <- c(0.5, 1, 1.5)
# Create plot colors
colorv <- c("black", "red", "blue")
# Plot all curves
for (indeks in 1:NROW(sigmavs)) {
  curve(expr=dlnorm(x, sdlog=sigmavs[indeks]),
  type="l", lwd=2, xlim=c(0, 3),
  xlab="", ylab="", col=colorv[indeks],
  add=as.logical(indeks-1))
}  # end for
# Add title and legend
title(main="Log-normal Distributions", line=0.5)
legend("topright", inset=0.05, title="Sigmas",
 paste("sigma", sigmavs, sep="="),
 cex=0.8, lwd=2, lty=rep(1, NROW(sigmavs)),
 col=colorv)
x11(width=6, height=4)
par(mar=c(4, 4, 3, 1))
# Return volatility of VTI etf
sigmav <- sd(rutils::diffit(log(rutils::etfenv$VTI[, 4])))
sigma2 <- sigmav^2
nrows <- NROW(rutils::etfenv$VTI)
# Standard deviation of log-normal prices
sqrt(nrows)*sigmav
# Skewness of log-normal prices
calcskew <- function(t) {
  expv <- exp(t*sigma2)
  (expv + 2)*sqrt(expv - 1)
}  # end calcskew
curve(expr=calcskew, xlim=c(1, nrows), lwd=3,
xlab="Number of days", ylab="Skewness", col="blue",
main="Skewness of Log-normal Prices
as a Function of Time")
# Probability that random log-normal price will be lower than the mean price
curve(expr=pnorm(sigmav*sqrt(x)/2),
xlim=c(1, nrows), lwd=3,
xlab="Number of days", ylab="Probability", col="blue",
main="Probability That Random Log-normal Price
Will be Lower Than the Mean Price")
# Define the daily volatility and growth rate
sigmav <- 0.01; drift <- 0.0; nrows <- 5000
npaths <- 10
# Simulate multiple paths of geometric Brownian motion
pricev <- rnorm(npaths*nrows, sd=sigmav) + drift - sigmav^2/2
pricev <- matrix(pricev, nc=npaths)
pricev <- exp(matrixStats::colCumsums(pricev))
# Create xts time series
pricev <- xts(pricev, order.by=seq.Date(Sys.Date()-nrows+1, Sys.Date(), by=1))
# Sort the columns according to largest terminal values
pricev <- pricev[, order(pricev[nrows, ])]
# Plot xts time series
colorv <- colorRampPalette(c("red", "blue"))(NCOL(pricev))
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(pricev, main="Multiple paths of geometric Brownian motion",
   xlab=NA, ylab=NA, plot.type="single", col=colorv)
# Define the daily volatility and growth rate
sigmav <- 0.01; drift <- 0.0; nrows <- 10000
npaths <- 100
# Simulate multiple paths of geometric Brownian motion
pricev <- rnorm(npaths*nrows, sd=sigmav) + drift - sigmav^2/2
pricev <- matrix(pricev, nc=npaths)
pricev <- exp(matrixStats::colCumsums(pricev))
# Calculate fraction of paths below the expected value
fractv <- rowSums(pricev < 1.0) / npaths
# Create xts time series of percentage of paths below the expected value
fractv <- xts(fractv, order.by=seq.Date(Sys.Date()-NROW(fractv)+1, Sys.Date(), by=1))
# Plot xts time series of percentage of paths below the expected value
par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
plot.zoo(fractv, main="Percentage of GBM paths below mean",
   xlab=NA, ylab=NA, col="blue")
# Load S&P500 stock prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
ls(sp500env)
# Extract the closing prices
pricev <- eapply(sp500env, quantmod::Cl)
# Flatten the prices into a single xts series
pricev <- rutils::do_call(cbind, pricev)
# Carry forward and backward non-NA prices
pricev <- zoo::na.locf(pricev, na.rm=FALSE)
pricev <- zoo::na.locf(pricev, fromLast=TRUE)
sum(is.na(pricev))
# Drop ".Close" from column names
colnames(pricev)
colnames(pricev) <- rutils::get_name(colnames(pricev))
# Or
# colnames(pricev) <- do.call(rbind,
#   strsplit(colnames(pricev), split="[.]"))[, 1]
# Select prices after the year 2000
pricev <- pricev["2000/", ]
# Scale the columns so that prices start at 1
pricev <- lapply(pricev, function(x) x/as.numeric(x[1]))
pricev <- rutils::do_call(cbind, pricev)
# Sort the columns according to the final prices
nrows <- NROW(pricev)
ordern <- order(pricev[nrows, ])
pricev <- pricev[, ordern]
# Select 20 symbols
symbolv <- colnames(pricev)
symbolv <- symbolv[round(seq.int(from=1, to=NROW(symbolv), length.out=20))]
# Plot xts time series of prices
colorv <- colorRampPalette(c("red", "blue"))(NROW(symbolv))
endd <- rutils::calc_endpoints(pricev, interval="weeks")
plot.zoo(pricev[endd, symbolv], main="20 S&P500 Stock Prices (scaled)",
   xlab=NA, ylab=NA, plot.type="single", col=colorv)
legend(x="topleft", inset=0.02, cex=0.5, bty="n", y.intersp=0.5,
 legend=rev(symbolv), col=rev(colorv), lwd=6, lty=1)
# Calculate the final stock prices
pricef <- drop(zoo::coredata(pricev[nrows, ]))
# Calculate the mean and median stock prices
max(pricef); min(pricef)
which.max(pricef)
which.min(pricef)
mean(pricef)
median(pricef)
# Calculate the percentage of stock prices below the mean
sum(pricef < mean(pricef))/NROW(pricef)
# Plot a histogram of final stock prices
hist(pricef, breaks=1e3, xlim=c(0, 300),
     xlab="Stock price", ylab="Count",
     main="Histogram of Final Stock Prices")
# Plot a histogram of final stock prices
abline(v=median(pricef), lwd=3, col="blue")
text(x=median(pricef), y=150, lab="median", pos=4)
abline(v=mean(pricef), lwd=3, col="red")
text(x=mean(pricef), y=100, lab="mean", pos=4)
# Calculate average of valid stock prices
validp <- (pricev != 1)  # Valid stocks
nstocks <- rowSums(validp)
nstocks[1] <- NCOL(pricev)
indeks <- rowSums(pricev*validp)/nstocks
# Calculate fraction of stock prices below the average price
fractv <- rowSums((pricev < indeks) & validp)/nstocks
# Create xts time series of average stock prices
indeks <- xts(indeks, order.by=zoo::index(pricev))
dev.new(width=6, height=4, noRStudioGD=TRUE)
# x11(width=6, height=4)
# Plot xts time series of average stock prices
plot.zoo(indeks, main="Average S&P500 Stock Prices (normalized from 1990)",
   xlab=NA, ylab=NA, col="blue")
# Create xts time series of percentage of stock prices below the average price
fractv <- xts(fractv, order.by=zoo::index(pricev))
# Plot percentage of stock prices below the average price
plot.zoo(fractv[-(1:2),],
   main="Percentage of S&P500 Stock Prices
   Below the Average Price",
   xlab=NA, ylab=NA, col="blue")
# Simulate AR processes
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
datev <- Sys.Date() + 0:728  # Two year daily series
# AR time series of returns
arimav <- xts(x=arima.sim(n=NROW(datev), model=list(ar=0.2)),
          order.by=datev)
arimav <- cbind(arimav, cumsum(arimav))
colnames(arimav) <- c("AR returns", "AR prices")
library(ggplot2)  # Load ggplot2
library(gridExtra)  # Load gridExtra
autoplot(object=arimav, # ggplot AR process
 facets="Series ~ .",
 main="Autoregressive process (phi=0.2)") +
  facet_grid("Series ~ .", scales="free_y") +
  xlab("") + ylab("") +
theme(legend.position=c(0.1, 0.5),
  plot.background=element_blank(),
  axis.text.y=element_blank())
coeff <- c(-0.9, 0.01, 0.9)  # AR coefficients
# Create three AR time series
arimav <- sapply(coeff, function(phi) {
  set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
  arima.sim(n=NROW(datev), model=list(ar=phi))
})  # end sapply
colnames(arimav) <- paste("autocorr", coeff)
plot.zoo(arimav, main="AR(1) prices", xlab=NA)
# Or plot using ggplot
arimav <- xts(x=arimav, order.by=datev)
library(ggplot)
autoplot(arimav, main="AR(1) prices",
   facets=Series ~ .) +
    facet_grid(Series ~ ., scales="free_y") +
xlab("") +
theme(
  legend.position=c(0.1, 0.5),
  plot.title=element_text(vjust=-2.0),
  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
  plot.background=element_blank(),
  axis.text.y=element_blank())
# Define AR(3) coefficients and innovations
coeff <- c(0.1, 0.39, 0.5)
nrows <- 1e2
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection"); innov <- rnorm(nrows)
# Simulate AR process using recursive loop in R
arimav <- numeric(nrows)
arimav[1] <- innov[1]
arimav[2] <- coeff[1]*arimav[1] + innov[2]
arimav[3] <- coeff[1]*arimav[2] + coeff[2]*arimav[1] + innov[3]
for (it in 4:NROW(arimav)) {
  arimav[it] <- arimav[(it-1):(it-3)] %*% coeff + innov[it]
}  # end for
# Simulate AR process using filter()
arimaf <- filter(x=innov, filter=coeff, method="recursive")
class(arimaf)
all.equal(arimav, as.numeric(arimaf))
# Fast simulation of AR process using C_rfilter()
arimacpp <- .Call(stats:::C_rfilter, innov, coeff,
     double(NROW(coeff) + NROW(innov)))[-(1:3)]
all.equal(arimav, arimacpp)
# Fastest simulation of AR process using HighFreq::sim_ar()
arimav <- HighFreq::sim_ar(coeff=matrix(coeff), innov=matrix(innov))
arimav <- drop(arimav)
all.equal(arimav, arimacpp)
# Benchmark the speed of the three methods of simulating AR process
library(microbenchmark)
summary(microbenchmark(
  Rloop={for (it in 4:NROW(arimav)) {
    arimav[it] <- arimav[(it-1):(it-3)] %*% coeff + innov[it]
  }},
  filter=filter(x=innov, filter=coeff, method="recursive"),
  cpp=HighFreq::sim_ar(coeff=matrix(coeff), innov=matrix(innov))
  ), times=10)[, c(1, 4, 5)]
# Calculate modulus of roots of characteristic equation
rootv <- Mod(polyroot(c(1, -coeff)))
# Calculate warmup period
warmup <- NROW(coeff) + ceiling(6/log(min(rootv)))
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nrows <- 1e4
innov <- rnorm(nrows + warmup)
# Simulate AR process using arima.sim()
arimav <- arima.sim(n=nrows,
  model=list(ar=coeff),
  start.innov=innov[1:warmup],
  innov=innov[(warmup+1):NROW(innov)])
# Simulate AR process using filter()
arimaf <- filter(x=innov, filter=coeff, method="recursive")
all.equal(arimaf[-(1:warmup)], as.numeric(arimav))
# Benchmark the speed of the three methods of simulating AR process
library(microbenchmark)
summary(microbenchmark(
  filter=filter(x=innov, filter=coeff, method="recursive"),
  arima_sim=arima.sim(n=nrows,
                  model=list(ar=coeff),
                  start.innov=innov[1:warmup],
                  innov=innov[(warmup+1):NROW(innov)]),
  arima_loop={for (it in 4:NROW(arimav)) {
  arimav[it] <- arimav[(it-1):(it-3)] %*% coeff + innov[it]}}
  ), times=10)[, c(1, 4, 5)]
x11(width=6, height=4)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0))
# Simulate AR(1) process
arimav <- arima.sim(n=1e3, model=list(ar=0.8))
# ACF of AR(1) process
acfl <- rutils::plot_acf(arimav, lag=10, xlab="", ylab="",
  main="Autocorrelations of AR(1) process")
acfl$acf[1:5]
# PACF of AR(1) process
pacfl <- pacf(arimav, lag=10, xlab="", ylab="", main="")
title("Partial autocorrelations of AR(1) process", line=1)
pacfl <- as.numeric(pacfl$acf)
pacfl[1:5]
library(rutils)  # Load rutils
library(ggplot2)  # Load ggplot2
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
randw <- cumsum(zoo(matrix(rnorm(3*100), ncol=3),
order.by=(Sys.Date()+0:99)))
colnames(randw) <- paste("randw", 1:3, sep="_")
plot.zoo(randw, main="Random walks",
     xlab="", ylab="", plot.type="single",
     col=c("black", "red", "blue"))
# Add legend
legend(x="topleft", legend=colnames(randw),
 col=c("black", "red", "blue"), lty=1)
# Simulate arima with large AR coefficient
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
nrows <- 1e4
arimav <- arima.sim(n=nrows, model=list(ar=0.99))
tseries::adf.test(arimav)
# Integrated series has unit root
tseries::adf.test(cumsum(arimav))
# Simulate arima with negative AR coefficient
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
arimav <- arima.sim(n=nrows, model=list(ar=-0.99))
tseries::adf.test(arimav)
# Integrated series has unit root
tseries::adf.test(cumsum(arimav))
# Simulate random walks using apply() loops
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
randws <- matrix(rnorm(1000*100), ncol=1000)
randws <- apply(randws, 2, cumsum)
varv <- apply(randws, 1, var)
# Simulate random walks using vectorized functions
# Initialize the random number generator
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
randws <- matrixStats::colCumsums(matrix(rnorm(1000*100), ncol=1000))
varv <- matrixStats::rowVars(randws)
par(mar=c(5, 3, 2, 2), oma=c(0, 0, 0, 0))
plot(varv, xlab="time steps", ylab="",
     t="l", col="blue", lwd=2,
     main="Variance of Random Walk")
# Define Brownian Motion parameters
nrows <- 1000; sigmav <- 0.01
# Simulate 5 paths of Brownian motion
pricev <- matrix(rnorm(5*nrows, sd=sigmav), nc=5)
pricev <- matrixStats::colCumsums(pricev)
# Plot 5 paths of Brownian motion
matplot(y=pricev, main="Brownian Motion Paths",
  xlab="time", ylab="path",
  type="l", lty="solid", lwd=1, col="blue")
# Save plot to png file on Mac
quartz.save("figure/brown_paths.png", type="png", width=6, height=4)
# Define Ornstein-Uhlenbeck parameters
prici <- 0.0; priceq <- 1.0;
sigmav <- 0.02; thetav <- 0.01; nrows <- 1000
# Initialize the data
innov <- rnorm(nrows)
retp <- numeric(nrows)
pricev <- numeric(nrows)
retp[1] <- sigmav*innov[1]
pricev[1] <- prici
# Simulate Ornstein-Uhlenbeck process in R
for (i in 2:nrows) {
  retp[i] <- thetav*(priceq - pricev[i-1]) + sigmav*innov[i]
  pricev[i] <- pricev[i-1] + retp[i]
}  # end for
# Simulate Ornstein-Uhlenbeck process in Rcpp
pricecpp <- HighFreq::sim_ou(prici=prici, priceq=priceq,
  theta=thetav, innov=matrix(sigmav*innov))
all.equal(pricev, drop(pricecpp))
# Compare the speed of R code with Rcpp
library(microbenchmark)
summary(microbenchmark(
  Rcode={for (i in 2:nrows) {
    retp[i] <- thetav*(priceq - pricev[i-1]) + sigmav*innov[i]
    pricev[i] <- pricev[i-1] + retp[i]}},
  Rcpp=HighFreq::sim_ou(prici=prici, priceq=priceq,
    theta=thetav, innov=matrix(sigmav*innov)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
plot(pricev, type="l", xlab="time", ylab="prices",
     main="Ornstein-Uhlenbeck Process")
legend("topright", title=paste(c(paste0("sigmav = ", sigmav),
     paste0("priceq = ", ),
     paste0("thetav = ", thetav)),
   collapse="\n"),
 legend="", cex=0.8, inset=0.1, bg="white", bty="n")
abline(h=, col='red', lwd=2)
retp <- rutils::diffit(pricev)
pricelag <- rutils::lagit(pricev)
formulav <- retp ~ pricelag
regmod <- lm(formulav)
summary(regmod)
# Plot regression
plot(formulav, main="OU Returns Versus Lagged Prices")
abline(regmod, lwd=2, col="red")
# Calculate volatility parameter
c(volatility=sigmav, estimate=sd(retp))
# Extract OU parameters from regression
coeff <- summary(regmod)$coefficients
# Calculate regression alpha and beta directly
betac <- cov(retp, pricelag)/var(pricelag)
alphac <- (mean(retp) - betac*mean(pricelag))
cbind(direct=c(alpha=alphac, beta=betac), lm=coeff[, 1])
all.equal(c(alpha=alphac, beta=betac), coeff[, 1],
    check.attributes=FALSE)
# Calculate regression standard errors directly
betac <- c(alpha=alphac, beta=betac)
fitv <- (alphac + betac*pricelag)
resids <- (retp - fitv)
prices2 <- sum((pricelag - mean(pricelag))^2)
betasd <- sqrt(sum(resids^2)/prices2/(nrows-2))
alphasd <- sqrt(sum(resids^2)/(nrows-2)*(1:nrows + mean(pricelag)^2/prices2))
cbind(direct=c(alphasd=alphasd, betasd=betasd), lm=coeff[, 2])
all.equal(c(alphasd=alphasd, betasd=betasd), coeff[, 2],
    check.attributes=FALSE)
# Compare mean reversion parameter theta
c(theta=(-thetav), round(coeff[2, ], 3))
# Compare equilibrium price mu
c(priceq=priceq, estimate=-coeff[1, 1]/coeff[2, 1])
# Compare actual and estimated parameters
coeff <- cbind(c(thetav*priceq, -thetav), coeff[, 1:2])
rownames(coeff) <- c("drift", "theta")
colnames(coeff)[1] <- "actual"
round(coeff, 4)
# Simulate Schwartz process
retp <- numeric(nrows)
pricev <- numeric(nrows)
pricev[1] <- exp(sigmav*innov[1])
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random numbers
for (i in 2:nrows) {
  retp[i] <- thetav*(priceq - pricev[i-1]) + sigmav*innov[i]
  pricev[i] <- pricev[i-1]*exp(retp[i])
}  # end for
plot(pricev, type="l", xlab="time", ylab="prices",
     main="Schwartz Process")
legend("topright",
 title=paste(c(paste0("sigmav = ", sigmav),
     paste0("priceq = ", priceq),
     paste0("thetav = ", thetav)),
   collapse="\n"),
 legend="", cex=0.8, inset=0.12, bg="white", bty="n")
abline(h=priceq, col='red', lwd=2)
# Define Dickey-Fuller parameters
prici <- 0.0;  priceq <- 1.0
thetav <- 0.01;  nrows <- 1000
coeff <- c(0.1, 0.39, 0.5)
# Initialize the data
innov <- rnorm(nrows, sd=0.01)
retp <- numeric(nrows)
pricev <- numeric(nrows)
# Simulate Dickey-Fuller process using recursive loop in R
retp[1] <- innov[1]
pricev[1] <- prici
retp[2] <- thetav*(priceq - pricev[1]) + coeff[1]*retp[1] +
  innov[2]
pricev[2] <- pricev[1] + retp[2]
retp[3] <- thetav*(priceq - pricev[2]) + coeff[1]*retp[2] +
  coeff[2]*retp[1] + innov[3]
pricev[3] <- pricev[2] + retp[3]
for (it in 4:nrows) {
  retp[it] <- thetav*(priceq - pricev[it-1]) +
    retp[(it-1):(it-3)] %*% coeff + innov[it]
  pricev[it] <- pricev[it-1] + retp[it]
}  # end for
# Simulate Dickey-Fuller process in Rcpp
pricecpp <- HighFreq::sim_df(prici=prici, priceq=priceq,
   theta=thetav, coeff=matrix(coeff), innov=matrix(innov))
# Compare prices
all.equal(pricev, drop(pricecpp))
# Compare the speed of R code with Rcpp
library(microbenchmark)
summary(microbenchmark(
  Rcode={for (it in 4:nrows) {
  retp[it] <- thetav*(priceq - pricev[it-1]) + retp[(it-1):(it-3)] %*% coeff + innov[it]
  pricev[it] <- pricev[it-1] + retp[it]
  }},
  Rcpp=HighFreq::sim_df(prici=prici, priceq=priceq, theta=thetav, coeff=matrix(coeff), innov=matrix(innov)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Simulate AR(1) process with coefficient=1, with unit root
innov <- matrix(rnorm(1e4, sd=0.01))
arimav <- HighFreq::sim_ar(coeff=matrix(1), innov=innov)
plot(arimav, t="l", main="Brownian Motion")
# Perform ADF test with lag = 1
tseries::adf.test(arimav, k=1)
# Perform standard Dickey-Fuller test
tseries::adf.test(arimav, k=0)
# Simulate AR(1) with coefficient close to 1, without unit root
arimav <- HighFreq::sim_ar(coeff=matrix(0.99), innov=innov)
plot(arimav, t="l", main="AR(1) coefficient = 0.99")
tseries::adf.test(arimav, k=1)
# Simulate Ornstein-Uhlenbeck OU process with mean reversion
prici <- 0.0; priceq <- 0.0; thetav <- 0.1
pricev <- HighFreq::sim_ou(prici=prici, priceq=priceq,
  theta=thetav, innov=innov)
plot(pricev, t="l", main=paste("OU coefficient =", thetav))
tseries::adf.test(pricev, k=1)
# Simulate Ornstein-Uhlenbeck OU process with zero reversion
thetav <- 0.0
pricev <- HighFreq::sim_ou(prici=prici, priceq=priceq,
  theta=thetav, innov=innov)
plot(pricev, t="l", main=paste("OU coefficient =", thetav))
tseries::adf.test(pricev, k=1)
# Simulate AR(1) process with different coefficients
coeffv <- seq(0.99, 0.999, 0.001)
retp <- as.numeric(na.omit(rutils::etfenv$returns$VTI))
adft <- sapply(coeffv, function(coeff) {
  arimav <- filter(x=retp, filter=coeff, method="recursive")
  adft <- suppressWarnings(tseries::adf.test(arimav))
  c(adfstat=unname(adft$statistic), pval=adft$p.value)
})  # end sapply
dev.new(width=6, height=4, noRStudioGD=TRUE)
# x11(width=6, height=4)
plot(x=coeffv, y=adft["pval", ], main="ADF p-val Versus AR Coefficient",
     xlab="AR coefficient", ylab="ADF pval", t="l", col="blue", lwd=2)
plot(x=coeffv, y=adft["adfstat", ], main="ADF Stat Versus AR Coefficient",
     xlab="AR coefficient", ylab="ADF stat", t="l", col="blue", lwd=2)
# Specify AR process parameters
nrows <- 1e3
coeff <- matrix(c(0.1, 0.39, 0.5)); ncoeff <- NROW(coeff)
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection"); innov <- matrix(rnorm(nrows))
# Simulate AR process using HighFreq::sim_ar()
arimav <- HighFreq::sim_ar(coeff=coeff, innov=innov)
# Fit AR model using ar.ols()
arfit <- ar.ols(arimav, order.max=ncoeff, aic=FALSE)
class(arfit)
is.list(arfit)
drop(arfit$ar); drop(coeff)
# Define predictor matrix without intercept column
predm <- sapply(1:ncoeff, rutils::lagit, input=arimav)
# Fit AR model using regression
predinv <- MASS::ginv(predm)
coeff <- drop(predinv %*% arimav)
all.equal(drop(arfit$ar), coeff, check.attributes=FALSE)
# Calculate the model residuals
fitv <- drop(predm %*% coeff)
resids <- drop(arimav - fitv)
# Variance of residuals
residsd <- sum(resids^2)/(nrows-NROW(coeff))
# Inverse of predictor matrix squared
pred2 <- MASS::ginv(crossprod(predm))
# Calculate covariance matrix of AR coefficients
covmat <- residsd*pred2
coefsd <- sqrt(diag(covmat))
# Calculate t-values of AR coefficients
coefft <- drop(coeff)/coefsd
# Plot the t-values of the AR coefficients
barplot(coefft, xlab="lag", ylab="t-value",
  main="Coefficient t-values of AR Forecasting Model")
# Fit AR(5) model into AR(3) process
predm <- sapply(1:5, rutils::lagit, input=arimav)
predinv <- MASS::ginv(predm)
coeff <- drop(predinv %*% arimav)
# Calculate t-values of AR(5) coefficients
resids <- drop(arimav - drop(predm %*% coeff))
residsd <- sum(resids^2)/(nrows-NROW(coeff))
covmat <- residsd*MASS::ginv(crossprod(predm))
coefsd <- sqrt(diag(covmat))
coefft <- drop(coeff)/coefsd
# Fit AR(5) model using arima()
arfit <- arima(arimav, order=c(5, 0, 0), include.mean=FALSE)
arfit$coef
# Fit AR(5) model using auto.arima()
library(forecast)  # Load forecast
arfit <- forecast::auto.arima(arimav, max.p=5, max.q=0, max.d=0)
# Fit AR(5) model into VTI returns
retp <- drop(zoo::coredata(na.omit(rutils::etfenv$returns$VTI)))
predm <- sapply(1:5, rutils::lagit, input=retp)
predinv <- MASS::ginv(predm)
coeff <- drop(predinv %*% retp)
# Calculate t-values of AR(5) coefficients
resids <- drop(retp - drop(predm %*% coeff))
residsd <- sum(resids^2)/(nrows-NROW(coeff))
covmat <- residsd*MASS::ginv(crossprod(predm))
coefsd <- sqrt(diag(covmat))
coefft <- drop(coeff)/coefsd
# Compute autocorrelation coefficients
acfl <- rutils::plot_acf(arimav, lag=10, plot=FALSE)
acfl <- drop(acfl$acf)
nrows <- NROW(acfl)
acf1 <- c(1, acfl[-nrows])
# Define Yule-Walker matrix
ywmat <- sapply(1:nrows, function(lagg) {
  if (lagg < nrows)
    c(acf1[lagg:1], acf1[2:(nrows-lagg+1)])
  else
    acf1[lagg:1]
})  # end sapply
# Generalized inverse of Yule-Walker matrix
ywmatinv <- MASS::ginv(ywmat)
# Solve Yule-Walker equations
ywcoeff <- drop(ywmatinv %*% acfl)
round(ywcoeff, 5)
coeff
# Calculate PACF from acf using Durbin-Levinson algorithm
acfl <- rutils::plot_acf(arimav, lag=10, plotobj=FALSE)
acfl <- drop(acfl$acf)
nrows <- NROW(acfl)
pacfl <- numeric(2)
pacfl[1] <- acfl[1]
pacfl[2] <- (acfl[2] - acfl[1]^2)/(1 - acfl[1]^2)
# Calculate PACF recursively in a loop using Durbin-Levinson algorithm
pacfll <- matrix(numeric(nrows*nrows), nc=nrows)
pacfll[1, 1] <- acfl[1]
for (it in 2:nrows) {
  pacfll[it, it] <- (acfl[it] - pacfll[it-1, 1:(it-1)] %*% acfl[(it-1):1])/(1 - pacfll[it-1, 1:(it-1)] %*% acfl[1:(it-1)])
  for (it2 in 1:(it-1)) {
    pacfll[it, it2] <- pacfll[it-1, it2] - pacfll[it, it] %*% pacfll[it-1, it-it2]
  }  # end for
}  # end for
pacfll <- diag(pacfll)
# Compare with the PACF without loop
all.equal(pacfl, pacfll[1:2])
# Calculate PACF using pacf()
pacfl <- pacf(arimav, lag=10, plot=FALSE)
pacfl <- drop(pacfl$acf)
all.equal(pacfl, pacfll)
# Simulate AR process using HighFreq::sim_ar()
nrows <- 1e2
coeff <- matrix(c(0.1, 0.39, 0.5)); ncoeff <- NROW(coeff)
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection"); innov <- matrix(rnorm(nrows))
arimav <- HighFreq::sim_ar(coeff=coeff, innov=innov)
# Forecast AR process using loop in R
fcast <- numeric(nrows+1)
fcast[2] <- coeff[1]*arimav[1]
fcast[3] <- coeff[1]*arimav[2] + coeff[2]*arimav[1]
for (it in 3:nrows) {
  fcast[it+1] <- arimav[it:(it-2)] %*% coeff
}  # end for
# Plot with legend
plot(arimav, main="Forecasting Using AR(3) Model",
  xlab="", ylab="", type="l")
lines(fcast[-(nrows+1)], col="red", lwd=2)
legend(x="topright", legend=c("series", "forecasts"),
 col=c("black", "red"), lty=1, lwd=6,
 cex=0.9, bg="white", bty="n")
# Forecast using filter()
convf <- filter(x=arimav, sides=1, filter=coeff, method="convolution")
convf <- as.numeric(convf)
# Compare excluding warmup period
all.equal(fcast[-(1:ncoeff)], convf[-(1:(ncoeff-1))],
check.attributes=FALSE)
# Filter using C_cfilter() compiled C++ function directly
convf <- .Call(stats:::C_cfilter, arimav, filter=coeff,
               sides=1, circular=FALSE)
# Compare excluding warmup period
all.equal(fcast[-(1:ncoeff)], convf[-(1:(ncoeff-1))],
check.attributes=FALSE)
# Filter using HighFreq::roll_conv() Rcpp function
convf <- HighFreq::roll_conv(arimav, coeff)
# Compare excluding warmup period
all.equal(fcast[-(1:ncoeff)], convf[-(1:(ncoeff-1))],
check.attributes=FALSE)
# Define predictor matrix for forecasting
predm <- sapply(0:(ncoeff-1), function(lagg) {
  rutils::lagit(arimav, lagg=lagg)
})  # end sapply
# Forecast using predictor matrix
convf <- c(0, drop(predm %*% coeff))
# Compare with loop in R
all.equal(fcast, convf, check.attributes=FALSE)
# Fit ARIMA model using arima()
arfit <- arima(arimav, order=c(3,0,0), include.mean=FALSE)
arfit$coef
coeff
# One-step-ahead forecast using predict.Arima()
predm <- predict(arfit, n.ahead=1)
# Or directly call predict.Arima()
# predm <- predict.Arima(arfit, n.ahead=1)
# Inspect the prediction object
class(predm)
names(predm)
class(predm$pred)
unlist(predm)
# One-step-ahead forecast using matrix algebra
fcast1 <- drop(arimav[nrows:(nrows-2)] %*% arfit$coef)
# Compare one-step-ahead forecasts
all.equal(predm$pred[[1]], fcast1)
# Get information about predict.Arima()
?stats:::predict.Arima
# Calculate the volatilities
sd(arimav); sd(fcast)
# Calculate the in-sample forecasting residuals
resids <- (arimav - fcast[-NROW(fcast)])
# Compare residuals with innovations
all.equal(innov, resids, check.attributes=FALSE)
plot(resids, t="l", lwd=3, xlab="", ylab="",
     main="ARIMA Forecast Errors")
# Define AR process parameters
nrows <- 1e3
coeff <- matrix(c(0.5, 0.0, 0.0)); ncoeff <- NROW(coeff)
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection"); innov <- matrix(rnorm(nrows, sd=0.01))
# Simulate AR process using HighFreq::sim_ar()
arimav <- HighFreq::sim_ar(coeff=coeff, innov=innov)
# Define order of the AR(n) forecasting model
ordern <- 5
# Define predictor matrix for forecasting
predm <- sapply(1:ordern, rutils::lagit, input=arimav)
colnames(predm) <- paste0("pred", 1:NCOL(predm))
# Specify length of look-back interval
lookb <- 100
# Invert the predictor matrix
rangev <- (nrows-lookb):(nrows-1)
predinv <- MASS::ginv(predm[rangev, ])
# Calculate fitted coefficients
coeff <- drop(predinv %*% arimav[rangev])
# Calculate forecast
drop(predm[nrows, ] %*% coeff)
# Actual value
arimav[nrows]
# Calculate a vector of daily VTI log returns
retp <- zoo::coredata(na.omit(rutils::etfenv$returns$VTI))
datev <- zoo::index(retp)
retp <- as.numeric(retp)
nrows <- NROW(retp)
# Define response equal to the returns
respv <- retp
# Define predictor matrix for forecasting
maxorder <- 5
predm <- sapply(1:maxorder, rutils::lagit, input=retp)
predm <- cbind(rep(1, nrows), predm)
# Perform rolling forecasting
lookb <- 100
fcast <- sapply((lookb+1):nrows, function(endd) {
  # Define rolling look-back range
  startp <- max(1, endd-lookb)
  # Or expanding look-back range
  # startp <- 1
  rangev <- startp:(endd-1)
  # Invert the predictor matrix
  predinv <- MASS::ginv(predm[rangev, ])
  # Calculate fitted coefficients
  coeff <- drop(predinv %*% respv[rangev])
  # Calculate forecast
  drop(predm[endd, ] %*% coeff)
})  # end sapply
# Add warmup period
fcast <- c(rep(0, lookb), fcast)
# Calculate the correlation between forecasts and returns
cor(fcasts, retp)
# Calculate the forecasting errors
errorf <- (fcasts - retp)
# Mean squared error
mean(errorf^2)
# Plot the forecasts
datav <- cbind(retp, fcasts)["2020-01/2020-06"]
colnames(datav) <- c("returns", "forecasts")
dygraphs::dygraph(datav,
  main="VTI Returns And Forecasts") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define backtesting function
sim_fcasts <- function(respv, nagg=5, ordern=5,
                 lookb=100, rollp=TRUE) {
  nrows <- NROW(respv)
  # Define predictor as a rolling sum
  predm <- rutils::roll_sum(respv, lookb=nagg)
  # Define predictor matrix for forecasting
  predm <- sapply(1+nagg*(0:ordern), rutils::lagit, input=predm)
  predm <- cbind(rep(1, nrows), predm)
  # Perform rolling forecasting
  fcast <- sapply((lookb+1):nrows, function(endd) {
    # Define rolling look-back range
    if (rollp)
startp <- max(1, endd-lookb)
    else
    # Or expanding look-back range
startp <- 1
    rangev <- startp:(endd-1)
    # Invert the predictor matrix
    predinv <- MASS::ginv(predm[rangev, ])
    # Calculate fitted coefficients
    coeff <- drop(predinv %*% respv[rangev])
    # Calculate forecast
    drop(predm[endd, ] %*% coeff)
  })  # end sapply
  # Add warmup period
  fcast <- c(rep(0, lookb), fcast)
  # Aggregate the forecasts
  rutils::roll_sum(fcast, lookb=nagg)
}  # end sim_fcasts
# Simulate the rolling autoregressive forecasts
fcast <- sim_fcasts(respv=retp, ordern=5, lookb=100)
c(mse=mean((retp - fcast)^2), cor=cor(retp, fcast))
lookbv <- seq(20, 200, 20)
fcast <- sapply(lookbv, sim_fcasts, respv=retp,
               nagg=5, ordern=5)
colnames(fcast) <- lookbv
msev <- apply(fcast, 2, function(x) mean((retp - x)^2))
# Plot forecasting series with legend
plot(x=lookbv, y=msev,
  xlab="look-back", ylab="MSE", type="l", lwd=2,
  main="MSE of AR(5) Forecasting Model")
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
