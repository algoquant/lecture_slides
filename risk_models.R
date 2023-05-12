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
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retvti, y=fitv, pch=16, col="red")
text(x=0.0, y=0.8*max(resids), paste("Treynor test t-value =", tvalue))
library(xtable)
gamblev <- data.frame(win=c("p", "a"), lose=c("q = 1 - p", "-b"))
rownames(gamblev) <- c("probability", "payout")
# print(xtable(gamblev), comment=FALSE, size="tiny")
print(xtable(gamblev), comment=FALSE)
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
set.seed(1121)  # Reset random number generator
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
# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate the wealth paths
kelly_ratio <- drop(mean(retp)/var(retp))
wealthv <- cumprod(1 + kelly_ratio*retp)
wealth_trans <- cumprod(1 + kelly_ratio*retp -
  0.5*bid_offer*kelly_ratio*(kelly_ratio-1)*abs(retp))
# Calculate the compounded wealth from returns
wealthv <- cbind(wealthv, wealth_trans)
colnames(wealthv) <- c("Kelly", "Including bid-offer")
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
utility_vec <- function(weights) {
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
look_back <- 200
var_rolling <- HighFreq::roll_var(retp, look_back)
weightv <- HighFreq::roll_sum(retp, look_back)/look_back
weightv <- weightv/var_rolling
weightv[1, ] <- 1/NCOL(weights)
weightv <- zoo::na.locf(weights)
sum(is.na(weights))
range(weights)
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
weightv <- rutils::lagit(weights)
# Calculate the compounded Kelly wealth and VTI
wealthv <- cbind(cumprod(1 + weightv$VTI*retp$VTI), cumprod(1 + retp$VTI))
colnames(wealthv) <- c("Kelly Strategy", "VTI")
dygraphs::dygraph(wealthv, main="VTI Strategy Using Rolling Kelly Weight") %>%
  dyAxis("y", label="Kelly Strategy", independentTicks=TRUE) %>%
  dyAxis("y2", label="VTI", independentTicks=TRUE) %>%
  dySeries(name="Kelly Strategy", axis="y", label="Kelly Strategy", strokeWidth=1, col="red") %>%
  dySeries(name="VTI", axis="y2", label="VTI", strokeWidth=1, col="blue")
# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate the compounded Kelly wealth and margin
wealthv <- cumprod(1 + weightv$VTI*retp$VTI)
marginv <- (retp$VTI - 1)*wealthv + 1
# Calculate the transaction costs
costs <- bid_offer*drop(rutils::diffit(marginv))/2
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
colnames(wealthv) <- c("Kelly", "Including bid-offer")
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
  dySeries(name="Kelly Strategy", axis="y", label="Kelly Strategy", strokeWidth=1, col="red") %>%
  dySeries(name="VTI plus IEF", axis="y2", label="VTI plus IEF", strokeWidth=1, col="blue")
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
colnamev <- colnames(wealthv)
dygraphs::dygraph(wealthv, main="Log Wealth of High and Low Skew ETFs") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="green") %>%
  dyLegend(show="always", width=500)
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
namesv <- names(skews)
nrows <- NROW(namesv)
# Select high skew and low skew ETFs
cutoff <- NROW(riskstats %/% 2)
low_skew <- namesv[1:cutoff]
high_skew <- namesv[(cutoff+1):nrows]
# low_skew <- namesv[1:50]
# Calculate returns and log prices
low_skew <- rowMeans(retp[, low_skew], na.rm=TRUE)
low_skew[1] <- 0
high_skew <- rowMeans(retp[, high_skew], na.rm=TRUE)
high_skew[1] <- 0
wealthv <- cbind(high_skew, low_skew)
wealthv <- xts::xts(wealthv, zoo::index(retp))
wealthv <- cumsum(wealthv)
# dygraph plot of high skew and low skew ETFs
colnamev <- colnames(wealthv)
dygraphs::dygraph(wealthv, main="Log Wealth of High and Low Skew Stocks") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="green") %>%
  dyLegend(show="always", width=500)
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
# Calculate VTI daily percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
nrows <- NROW(retp)
# Bootstrap aggregated annual VTI returns
holdp <- 252
reta <- sqrt(holdp)*sapply(1:nrows, function(x) {
    mean(retp[sample.int(nrows, size=holdp, replace=TRUE)])
})  # end sapply
# Calculate mean, standard deviation, skewness, and kurtosis
datav <- cbind(retp, reta)
colnames(datav) <- c("VTI", "Agg")
sapply(datav, function(x) {
  # Standardize the returns
  meanv <- mean(x); stdev <- sd(x); x <- (x - meanv)/stdev
  c(mean=meanv, stdev=stdev, skew=mean(x^3), kurt=mean(x^4))
})  # end sapply
# Calculate the Sharpe and Dowd ratios
confl <- 0.02
ratiom <- sapply(datav, function(x) {
  stdev <- sd(x)
  varisk <- unname(quantile(x, probs=confl))
  cvar <- mean(x[x < varisk])
  mean(x)/c(Sharpe=stdev, Dowd=-varisk, DowdC=-cvar)
})  # end sapply
# Annualize the daily risk
ratiom[, 1] <- sqrt(252)*ratiom[, 1]
ratiom
# Plot the densities of returns
plot(density(retp), t="l", lwd=3, col="blue",
     xlab="returns", ylab="density", xlim=c(-0.04, 0.04),
     main="Distribution of Aggregated Stock Returns")
lines(density(reta), t="l", col="red", lwd=3)
curve(expr=dnorm(x, mean=mean(reta), sd=sd(reta)), col="green", lwd=3, add=TRUE)
legend("topright", legend=c("VTI Daily", "Aggregated", "Normal"), y.intersp=0.4,
 inset=-0.1, bg="white", lty=1, lwd=6, col=c("blue", "red", "green"), bty="n")
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
# Calculate EWMA weights
look_back <- 21
weightv <- exp(-0.1*1:look_back)
weightv <- weightv/sum(weights)
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
# Calculate EWMA prices using HighFreq::roll_conv()
pricecpp <- HighFreq::roll_conv(closep, weightv=weightv)
all.equal(priceff[-(1:look_back)], as.numeric(pricecpp)[-(1:look_back)])
# Benchmark speed of trailing calculations
library(microbenchmark)
summary(microbenchmark(
  filter=filter(closep, filter=weightv, method="convolution", sides=1),
  priceff=.Call(stats:::C_cfilter, closep, filter=weightv, sides=1, circular=FALSE),
  rcpp=HighFreq::roll_conv(closep, weightv=weightv)
  ), times=10)[, c(1, 4, 5)]
# Simulate AR process using filter()
nrows <- NROW(closep)
# Calculate ARIMA coefficients and innovations
coeff <- matrix(weights)/4
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
# Calculate trailing EWMA prices using HighFreq::roll_conv()
look_back <- 21
weightv <- exp(-0.1*1:look_back)
weightv <- weightv/sum(weights)
pricef <- HighFreq::roll_conv(closep, weightv=weightv)
# Combine prices with smoothed prices
pricev <- cbind(closep, pricef)
colnames(pricev)[2] <- "VTI Smooth"
# Calculate standard deviations of returns
sapply(rutils::diffit(pricev), sd)
# Plot dygraph
dygraphs::dygraph(pricev["2009"], main="VTI Prices and Trailing Smoothed Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Center the smoothed prices
pricef <- rutils::lagit(pricef, -(look_back %/% 2), pad_zeros=FALSE)
# Combine prices with smoothed prices
pricev <- cbind(closep, pricef)
colnames(pricev)[2] <- "VTI Smooth"
# Plot dygraph
dygraphs::dygraph(pricev["2009"], main="VTI Prices and Centered Smoothed Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
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
retp <- rutils::diffit(closef)
# Open plot window
x11(width=6, height=7)
# Set plot parameters
par(oma=c(1, 1, 0, 1), mar=c(1, 1, 1, 1), mgp=c(0, 0.5, 0),
    cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Set two plot panels
par(mfrow=c(2,1))
# Plot ACF of VTI returns
rutils::plot_acf(retp[, 1], lag=10, xlab="")
title(main="ACF of VTI Returns", line=-1)
# Plot ACF of smoothed VTI returns
rutils::plot_acf(retp[, 2], lag=10, xlab="")
title(main="ACF of Smoothed VTI Returns", line=-1)
# Get close prices and calculate close-to-close returns
# closep <- quantmod::Cl(rutils::etfenv$VTI)
closep <- quantmod::Cl(HighFreq::SPY)
colnames(closep) <- rutils::get_name(colnames(closep))
retspy <- TTR::ROC(closep)
retspy[1] <- 0
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
eq_up <- exp(cumsum(sig_up*retspy))
eq_dn <- exp(cumsum(-1*sig_dn*retspy))
eq_all <- exp(cumsum(sig_nals*retspy))
# Plot daily cumulative returns in panels
endd <- endpoints(retspy, on="days")
plot.zoo(cbind(eq_all, eq_up, eq_dn)[endd], lwd=c(2, 2, 2),
  ylab=c("Total","Long","Short"), col=c("red","green","blue"),
  main=paste("RSI(2) strategy for", colnames(closep), "from",
       format(start(retspy), "%B %Y"), "to",
       format(end(retspy), "%B %Y")))
# Extract log VTI prices
ohlc <- rutils::etfenv$VTI
datev <- zoo::index(ohlc)
closep <- log(quantmod::Cl(ohlc))
colnames(closep) <- "VTI"
nrows <- NROW(closep)
# Calculate EWMA weights
look_back <- 111
lambda <- 0.9
weightv <- lambda^(1:look_back)
weightv <- weightv/sum(weights)
# Calculate EWMA prices as the convolution
ewmacpp <- HighFreq::roll_sumw(closep, weightv=weightv)
pricev <- cbind(closep, ewmacpp)
colnames(pricev) <- c("VTI", "VTI EWMA")
# Dygraphs plot with custom line colors
colnamev <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main="VTI EWMA Prices") %>%
  dySeries(name=colnamev[1], label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=500)
# Standard plot of  EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
colorv <- c("blue", "red")
plot_theme$col$line.col <- colors
quantmod::chart_Series(pricev["2008/2009"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
legend("topleft", legend=colnames(pricev), y.intersp=0.4,
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Calculate EWMA prices recursively using C++ code
ewmar <- .Call(stats:::C_rfilter, closep, lambda, c(as.numeric(closep[1])/(1-lambda), double(NROW(closep))))[-1]
# Or R code
# ewmar <- filter(closep, filter=lambda, init=as.numeric(closep[1, 1])/(1-lambda), method="recursive")
ewmar <- (1-lambda)*ewmar
# Calculate EWMA prices recursively using RcppArmadillo C++
ewmacpp <- HighFreq::run_mean(closep, lambda=lambda)
all.equal(drop(ewmacpp), ewmar)
# Compare the speed of C++ code with RcppArmadillo C++
library(microbenchmark)
summary(microbenchmark(
  filtercpp=HighFreq::run_mean(closep, lambda=lambda),
  rfilter=.Call(stats:::C_rfilter, closep, lambda, c(as.numeric(closep[1])/(1-lambda), double(NROW(closep)))),
  times=10))[, c(1, 4, 5)]
# Dygraphs plot with custom line colors
pricev <- cbind(closep, ewmacpp)
colnames(pricev) <- c("VTI", "VTI EWMA")
colnamev <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main="Recursive VTI EWMA Prices") %>%
  dySeries(name=colnamev[1], label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=500)
# Standard plot of  EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
colorv <- c("blue", "red")
plot_theme$col$line.col <- colors
quantmod::chart_Series(pricev["2008/2009"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
legend("topleft", legend=colnames(pricev),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")
# Calculate log OHLC prices and volumes
volumv <- quantmod::Vo(ohlc)
colnames(volumv) <- "Volume"
nrows <- NROW(closep)
# Calculate the VWAP prices
look_back <- 21
vwap <- HighFreq::roll_sum(closep, look_back=look_back, weightv=volumv)
colnames(vwap) <- "VWAP"
pricev <- cbind(closep, vwap)
# Dygraphs plot with custom line colors
colorv <- c("blue", "red")
dygraphs::dygraph(pricev["2008/2009"], main="VTI VWAP Prices") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
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
lambda <- 0.9
volumer <- .Call(stats:::C_rfilter, volumv, lambda, c(as.numeric(volumv[1])/(1-lambda), double(NROW(volumv))))[-1]
pricer <- .Call(stats:::C_rfilter, volumv*closep, lambda, c(as.numeric(volumv[1]*closep[1])/(1-lambda), double(NROW(closep))))[-1]
vwapr <- pricer/volumer
# Calculate VWAP prices recursively using RcppArmadillo C++
vwapcpp <- HighFreq::run_mean(closep, lambda=lambda, weightv=volumv)
all.equal(vwapr, drop(vwapcpp))
# Dygraphs plot the VWAP prices
pricev <- xts(cbind(vwap, vwapr), zoo::index(ohlc))
colnames(pricev) <- c("VWAP trailing", "VWAP recursive")
dygraphs::dygraph(pricev["2008/2009"], main="VWAP Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate two EWMA prices
look_back <- 21
lambda <- 0.1
weightv <- exp(-lambda*1:look_back)
weightv <- weightv/sum(weights)
ewmaf <- HighFreq::roll_conv(closep, weightv=weightv)
lambda <- 0.05
weightv <- exp(-lambda*1:look_back)
weightv <- weightv/sum(weights)
ewmas <- HighFreq::roll_conv(closep, weightv=weightv)
# Calculate VTI prices
ewmadiff <- (ewmaf - ewmas)
pricev <- cbind(closep, ewmadiff)
symbol <- "VTI"
colnames(pricev) <- c(symbol, paste(symbol, "Returns"))
# Plot dygraph of VTI Returns
colnamev <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main=paste(symbol, "EWMA Returns")) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=500)
# Calculate the fractional weights
deltav <- 0.1
weightv <- (deltav - 0:(look_back-2)) / 1:(look_back-1)
weightv <- (-1)^(1:(look_back-1))*cumprod(weights)
weightv <- c(1, weightv)
weightv <- (weightv - mean(weights))
# Calculate the fractional VTI returns
retf <- HighFreq::roll_conv(closep, weightv=weightv)
pricev <- cbind(closep, retf)
symbol <- "VTI"
colnames(pricev) <- c(symbol, paste(symbol, "Returns"))
# Plot dygraph of VTI Returns
colnamev <- colnames(pricev)
dygraphs::dygraph(pricev["2008-08/2009-08"], main=paste(symbol, "Fractional Returns")) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=500)
# Perform ADF test for prices
tseries::adf.test(closep)
# Perform ADF test for returns
tseries::adf.test(retp)
# Calculate fractional VTI returns
deltav <- 0.1*c(1, 3, 5, 7, 9)
retfrac <- lapply(deltav, function(deltav) {
  weightv <- (deltav - 0:(look_back-2)) / 1:(look_back-1)
  weightv <- c(1, (-1)^(1:(look_back-1))*cumprod(weights))
  weightv <- (weightv - mean(weights))
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
colnamev <- colnames(retfrac)
dyplot <- dygraphs::dygraph(retfrac["2008-08/2009-08"], main="Fractional Returns") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col=colorv[1])
for (i in 2:NROW(colnamev))
  dyplot <- dyplot %>%
  dyAxis("y2", label=colnamev[i], independentTicks=TRUE) %>%
  dySeries(name=colnamev[i], axis="y2", label=colnamev[i], strokeWidth=2, col=colorv[i])
dyplot <- dyplot %>% dyLegend(width=500)
dyplot
# Calculate volume z-scores
volumv <- quantmod::Vo(ohlc)
look_back <- 21
volumean <- HighFreq::roll_mean(volumv, look_back=look_back)
volumsd <- sqrt(HighFreq::roll_var(rutils::diffit(volumv), look_back=look_back))
volumsd[1] <- 0
volumz <- ifelse(volumsd > 0, (volumv - volumean)/volumsd, 0)
# Plot histogram of volume z-scores
hist(volumz, breaks=1e2)
# Plot dygraph of volume z-scores of VTI prices
pricev <- cbind(closep, volumz)
colnames(pricev) <- c("VTI", "Z-Scores")
colnamev <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main="VTI Volume Z-Scores") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=500)
# Calculate volatility (true range) z-scores
volat <- log(quantmod::Hi(ohlc) - quantmod::Lo(ohlc))
look_back <- 21
volatm <- HighFreq::roll_mean(volat, look_back=look_back)
volat <- (volat - volatm)
volatsd <- sqrt(HighFreq::roll_var(rutils::diffit(volat), look_back=look_back))
volatsd[1] <- 0
volatz <- ifelse(volatsd > 0, volat/volatsd, 0)
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
colnamev <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main="VTI Volatility Z-Scores") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=500)
# Calculate the recursive trailing VTI volatility
lambdaf <- 0.8
lambdas <- 0.81
volatf <- sqrt(HighFreq::run_var(retp, lambda=lambdaf))
volats <- sqrt(HighFreq::run_var(retp, lambda=lambdas))
# Calculate the recursive trailing z-scores of VTI volatility
volatd <- volatf - volats
volatsd <- sqrt(HighFreq::run_var(rutils::diffit(volatd), lambda=lambdaf))
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
colnamev <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main="VTI Online Volatility Z-Scores") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=500)
# Calculate the centered volatility
look_back <- 21
half_back <- look_back %/% 2
volat <- HighFreq::roll_var(closep, look_back=look_back)
volat <- sqrt(volat)
volat <- rutils::lagit(volat, lagg=(-half_back))
# Calculate the z-scores of prices
pricez <- (closep -
  0.5*(rutils::lagit(closep, half_back, pad_zeros=FALSE) +
  rutils::lagit(closep, -half_back, pad_zeros=FALSE)))
pricez <- ifelse(volat > 0, pricez/volat, 0)
# Plot dygraph of z-scores of VTI prices
pricev <- cbind(closep, pricez)
colnames(pricev) <- c("VTI", "Z-Scores")
colnamev <- colnames(pricev)
dygraphs::dygraph(pricev["2009"], main="VTI Centered Price Z-Scores") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=500)
# Calculate the thresholds for labeling tops and bottoms
confl <- c(0.2, 0.8)
threshv <- quantile(pricez, confl)
# Calculate the vectors of tops and bottoms
tops <- zoo::coredata(pricez > threshv[2])
bottoms <- zoo::coredata(pricez < threshv[1])
# Simulate in-sample VTI strategy
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv[tops] <- (-1)
posv[bottoms] <- 1
posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv)
pnls <- retp*posv
# Plot dygraph of in-sample VTI strategy
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Price Tops and Bottoms Strategy In-sample") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="Strategy", independentTicks=TRUE) %>%
  dySeries(name="VTI", axis="y", label="VTI", strokeWidth=2, col="blue") %>%
  dySeries(name="Strategy", axis="y2", label="Strategy", strokeWidth=2, col="red")
# Calculate the trailing VTI volatility
volat <- HighFreq::roll_var(closep, look_back=look_back)
volat <- sqrt(volat)
# Calculate the trailing z-scores of VTI prices
pricez <- (closep - rutils::lagit(closep, look_back, pad_zeros=FALSE))
pricez <- ifelse(volat > 0, pricez/volat, 0)
# Plot dygraph of the trailing z-scores of VTI prices
pricev <- cbind(closep, pricez)
colnames(pricev) <- c("VTI", "Z-Scores")
colnamev <- colnames(pricev)
dygraphs::dygraph(pricev["2009"], main="VTI Trailing Price Z-Scores") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=500)
# Calculate the recursive trailing VTI volatility
lambda <- 0.9
volat <- HighFreq::run_var(closep, lambda=lambda)
volat <- sqrt(volat)
# Calculate the recursive trailing z-scores of VTI prices
pricer <- (closep - HighFreq::run_mean(closep, lambda=lambda))
pricer <- ifelse(volat > 0, pricer/volat, 0)
# Plot dygraph of the trailing z-scores of VTI prices
pricev <- xts::xts(cbind(pricez, pricer), datev)
colnames(pricev) <- c("Z-Scores", "Recursive")
colnamev <- colnames(pricev)
dygraphs::dygraph(pricev["2009"], main="VTI Online Trailing Price Z-Scores") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Calculate trailing price regression z-scores
datev <- matrix(zoo::index(closep))
look_back <- 21
# Create a default list of regression parameters
controlv <- HighFreq::param_reg()
regz <- HighFreq::roll_reg(respv=closep, predm=datev,
   look_back=look_back, controlv=controlv)
regz[1:look_back, ] <- 0
# Plot dygraph of z-scores of VTI prices
datav <- cbind(closep, regz[, NCOL(regz)])
colnames(datav) <- c("VTI", "Z-Scores")
colnamev <- colnames(datav)
dygraphs::dygraph(datav["2009"], main="VTI Regression Z-Scores") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=500)
# Calculate recursive trailing price regression versus time
lambda <- 0.9
regz <- HighFreq::run_reg(closep, datev, lambda=lambda,
  method="scale")
colnames(regz) <- c("betas", "alphas", "zscores")
tail(regz)
# Plot dygraph of regression betas
datav <- cbind(closep, regz[, "betas"])
colnames(datav) <- c("VTI", "Slope")
colnamev <- colnames(datav)
dygraphs::dygraph(datav["2009"], main="VTI Online Regression Slope") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=500)
# Plot dygraph of z-scores of VTI prices
datav <- cbind(closep, regz[, "zscores"])
colnames(datav) <- c("VTI", "Z-Scores")
colnamev <- colnames(datav)
dygraphs::dygraph(datav["2009"], main="VTI Online Regression Z-Scores") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=500)
# Extract time series of VTI log prices
closep <- log(na.omit(rutils::etfenv$prices$VTI))
# Define look-back window
look_back <- 11
# Calculate time series of trailing medians
medianv <- roll::roll_median(closep, width=look_back)
# medianv <- TTR::runMedian(closep, n=look_back)
# Calculate time series of MAD
madv <- HighFreq::roll_var(closep, look_back=look_back, method="nonparametric")
# madv <- TTR::runMAD(closep, n=look_back)
# Calculate time series of z-scores
zscores <- (closep - medianv)/madv
zscores[1:look_back, ] <- 0
tail(zscores, look_back)
range(zscores)
x11(width=6, height=5)
# Plot the prices and medians
dygraphs::dygraph(cbind(closep, medianv), main="VTI median") %>%
  dyOptions(colors=c("black", "red")) %>%
  dyLegend(show="always", width=500)
# Plot histogram of z-scores
histp <- hist(zscores, col="lightgrey",
  xlab="z-scores", breaks=50, xlim=c(-4, 4),
  ylab="frequency", freq=FALSE, main="Hampel Z-Scores histogram")
lines(density(zscores, adjust=1.5), lwd=3, col="blue")
# Calculate one-sided Hampel z-scores
medianv <- roll::roll_median(closep, width=look_back)
# medianv <- TTR::runMedian(closep, n=look_back)
madv <- HighFreq::roll_var(closep, look_back=look_back, method="nonparametric")
# madv <- TTR::runMAD(closep, n=look_back)
zscores <- (closep - medianv)/madv
zscores[1:look_back, ] <- 0
tail(zscores, look_back)
range(zscores)
# Calculate two-sided Hampel z-scores
half_back <- look_back %/% 2
medianv <- rutils::lagit(medianv, lagg=(-half_back))
madv <- rutils::lagit(madv, lagg=(-half_back))
zscores <- (closep - medianv)/madv
zscores[1:look_back, ] <- 0
tail(zscores, look_back)
range(zscores)
# Calculate VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
nrows <- NROW(retp)
# Define end points
endd <- 1:NROW(retp)
# Start points are multi-period lag of endd
look_back <- 11
startp <- c(rep_len(0, look_back-1), endd[1:(nrows-look_back+1)])
# Calculate trailing variance in sapply() loop - takes long
varv <- sapply(1:nrows, function(it) {
  retp <- retp[startp[it]:endd[it]]
  sum((retp - mean(retp))^2)/look_back
})  # end sapply
# Use only vectorized functions
retc <- cumsum(retp)
retc <- (retc - c(rep_len(0, look_back), retc[1:(nrows-look_back)]))
retc2 <- cumsum(retp^2)
retc2 <- (retc2 - c(rep_len(0, look_back), retc2[1:(nrows-look_back)]))
var2 <- (retc2 - retc^2/look_back)/look_back
all.equal(varv[-(1:look_back)], as.numeric(var2)[-(1:look_back)])
# Or using package rutils
retc <- rutils::roll_sum(retp, look_back=look_back)
retc2 <- rutils::roll_sum(retp^2, look_back=look_back)
var2 <- (retc2 - retc^2/look_back)/look_back
# Coerce variance into xts
tail(varv)
class(varv)
varv <- xts(varv, order.by=zoo::index(retp))
colnames(varv) <- "VTI.variance"
head(varv)
# Calculate trailing VTI variance using package HighFreq
varv <- roll::roll_var(retp, width=look_back)
colnames(varv) <- "Variance"
head(varv)
sum(is.na(varv))
varv[1:(look_back-1)] <- 0
# Benchmark calculation of trailing variance
library(microbenchmark)
summary(microbenchmark(
  sapply=sapply(1:nrows, function(it) {
    var(retp[startp[it]:endd[it]])
  }),
  roll=roll::roll_var(retp, width=look_back),
  times=10))[, c(1, 4, 5)]
# Calculate EWMA VTI variance using compiled C++ function
look_back <- 51
weightv <- exp(-0.1*1:look_back)
weightv <- weightv/sum(weights)
varv <- .Call(stats:::C_cfilter, retp^2, filter=weightv, sides=1, circular=FALSE)
varv[1:(look_back-1)] <- varv[look_back]
# Plot EWMA volatility
varv <- xts:::xts(sqrt(varv), order.by=zoo::index(retp))
dygraphs::dygraph(varv, main="VTI EWMA Volatility") %>%
  dyOptions(colors="blue") %>% dyLegend(show="always", width=500)
quantmod::chart_Series(xtsv, name="VTI EWMA Volatility")
# Calculate trailing VTI variance using package roll
library(roll)  # Load roll
varv <- roll::roll_var(retp, weights=rev(weights), width=look_back)
colnames(varv) <- "VTI.variance"
class(varv)
head(varv)
sum(is.na(varv))
varv[1:(look_back-1)] <- 0
# Calculate realized variance recursively
lambda <- 0.9
volat <- HighFreq::run_var(retp, lambda=lambda)
volat <- sqrt(volat)
# Plot EWMA volatility
volat <- xts:::xts(volat, order.by=datev)
dygraphs::dygraph(volat, main="VTI Realized Volatility") %>%
  dyOptions(colors="blue") %>% dyLegend(show="always", width=500)
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
  look_back=look_back)
# Plot range volatility
varv <- xts:::xts(sqrt(varv), order.by=zoo::index(spy))
dygraphs::dygraph(varv["2009-02"], main="SPY Trailing Range Volatility") %>%
  dyOptions(colors="blue") %>% dyLegend(show="always", width=500)
# Benchmark the speed of HighFreq vs TTR
library(microbenchmark)
summary(microbenchmark(
  ttr=TTR::volatility(rutils::etfenv$VTI, N=1, calc="yang.zhang"),
  highfreq=HighFreq::calcvar_ohlc(log(rutils::etfenv$VTI), method="yang_zhang"),
  times=2))[, c(1, 4, 5)]
# Calculate VXX log prices
vxx <- na.omit(rutils::etfenv$prices$VXX)
datev <- zoo::index(vxx)
look_back <- 41
vxx <- log(vxx)
# Calculate trailing VTI volatility
closep <- get("VTI", rutils::etfenv)[datev]
closep <- log(closep)
volat <- sqrt(HighFreq::roll_var_ohlc(ohlc=closep, look_back=look_back, scalev=FALSE))
volat[1:look_back] <- volat[look_back+1]
# Plot dygraph of VXX and VTI volatility
datav <- cbind(vxx, volat)
colnames(datav)[2] <- "VTI Volatility"
colnamev <- colnames(datav)
captiont <- "VXX and VTI Volatility"
dygraphs::dygraph(datav[, 1:2], main=captiont) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red") %>%
  dyLegend(show="always", width=500)
# Calculate VXX log prices
vxx <- na.omit(rutils::etfenv$prices$VXX)
datev <- zoo::index(vxx)
look_back <- 41
vxx <- log(vxx)
vxx <- (vxx - HighFreq::roll_mean(vxx, look_back=look_back))
vxx[1:look_back] <- vxx[look_back+1]
# Calculate trailing VTI volatility
closep <- get("VTI", rutils::etfenv)[datev]
closep <- log(closep)
volat <- sqrt(HighFreq::roll_var_ohlc(ohlc=closep, look_back=look_back, scalev=FALSE))
volat[1:look_back] <- volat[look_back+1]
# Calculate regression coefficients of XLB ~ XLE
betav <- drop(cov(vxx, volat)/var(volat))
alpha <- drop(mean(vxx) - betav*mean(volat))
# Calculate regression residuals
fitv <- (alpha + betav*volat)
residuals <- (vxx - fitv)
# Perform ADF test on residuals
tseries::adf.test(residuals, k=1)
# Plot dygraph of VXX and VTI volatility
datav <- cbind(vxx, volat)
colnamev <- colnames(datav)
captiont <- "VXX and VTI Volatility"
dygraphs::dygraph(datav[, 1:2], main=captiont) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red") %>%
  dyLegend(show="always", width=500)
x11(width=6, height=5)
par(mar=c(4, 3, 1, 1), oma=c(0, 0, 0, 0))
# Calculate VTI percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
# Calculate trailing VTI variance using package roll
look_back <- 21
varv <- HighFreq::roll_var(retp, look_back=look_back)
colnames(varv) <- "Variance"
# Number of look_backs that fit over returns
nrows <- NROW(retp)
nagg <- nrows %/% look_back
# Define end points with beginning stub
endd <- c(0, nrows-look_back*nagg + (0:nagg)*look_back)
nrows <- NROW(endd)
# Subset variance to end points
varv <- varv[endd]
# Plot autocorrelation function
rutils::plot_acf(varv, lag=10, main="ACF of Variance")
# Plot partial autocorrelation
pacf(varv, lag=10, main="PACF of Variance", ylab=NA)
# Define GARCH parameters
alpha <- 0.3; betav <- 0.5;
omega <- 1e-4*(1-alpha-betav)
nrows <- 1000
# Calculate matrix of standard normal innovations
set.seed(1121)  # Reset random numbers
innov <- rnorm(nrows)
retp <- numeric(nrows)
varv <- numeric(nrows)
varv[1] <- omega/(1-alpha-betav)
retp[1] <- sqrt(varv[1])*innov[1]
# Simulate GARCH model
for (i in 2:nrows) {
  retp[i] <- sqrt(varv[i-1])*innov[i]
  varv[i] <- omega + alpha*retp[i]^2 + betav*varv[i-1]
}  # end for
# Simulate the GARCH process using Rcpp
garch_data <- HighFreq::sim_garch(omega=omega, alpha=alpha,
  beta=betav, innov=matrix(innov))
all.equal(garch_data, cbind(retp, varv), check.attributes=FALSE)
# Define GARCH parameters
alpha <- 0.3; betav <- 0.5;
omega <- 1e-4*(1-alpha-betav)
nrows <- 1000
# Calculate matrix of standard normal innovations
set.seed(1121)  # Reset random numbers
innov <- rnorm(nrows)
retp <- numeric(nrows)
varv <- numeric(nrows)
varv[1] <- omega/(1-alpha-betav)
retp[1] <- sqrt(varv[1])*innov[1]
# Simulate GARCH model
for (i in 2:nrows) {
  retp[i] <- sqrt(varv[i-1])*innov[i]
  varv[i] <- omega + alpha*retp[i]^2 + betav*varv[i-1]
}  # end for
# Simulate the GARCH process using Rcpp
garch_data <- HighFreq::sim_garch(omega=omega, alpha=alpha,
  beta=betav, innov=matrix(innov))
all.equal(garch_data, cbind(retp, varv), check.attributes=FALSE)
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
  alpha=alpha, beta=betav))
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
omega/(1-alpha-betav)
# Calculate kurtosis of GARCH returns
mean(((retp-mean(retp))/sd(retp))^4)
# Calculate expected value of kurtosis
3 + 6*alpha^2/(1-2*alpha^2-(alpha+betav)^2)
# Calculate the distribution of GARCH kurtosis
kurt <- sapply(1:1e4, function(x) {
  garch_data <- HighFreq::sim_garch(omega=omega, alpha=alpha,
    beta=betav, innov=matrix(rnorm(nrows)))
  retp <- garch_data[, 1]
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
abline(v=(3 + 6*alpha^2/(1-2*alpha^2-(alpha+betav)^2)), lwd=3, col="red")
text(x=7.0, y=0.4, "Expected Kurtosis")
quartz.save("figure/garch_kurtosis.png", type="png", width=6, height=5)
# Simulate the GARCH process using Rcpp
garch_data <- HighFreq::sim_garch(omega=omega, alpha=alpha,
  beta=betav, innov=matrix(innov))
# Extract the returns
retp <- garch_data[, 1]
# Estimate the trailing variance from the returns
varv <- numeric(nrows)
varv[1] <- omega/(1-alpha-betav)
for (i in 2:nrows) {
  varv[i] <- omega + alpha*retp[i]^2 +
    betav*varv[i-1]
}  # end for
all.equal(garch_data[, 2], varv, check.attributes=FALSE)
library(fGarch)
# Fit returns into GARCH
garch_fit <- fGarch::garchFit(data=retp)
# Fitted GARCH parameters
garch_fit@fit$coef
# Actual GARCH parameters
c(mu=mean(retp), omega=omega,alpha=alpha, beta=betav)
# Plot GARCH fitted volatility
plot(sqrt(garch_fit@fit$series$h), t="l",
  col="blue", xlab="", ylab="",
  main="GARCH Fitted Volatility")
quartz.save("figure/garch_fGarch_fitted.png",
  type="png", width=6, height=5)
# Define likelihood function
likefun <- function(omega, alpha, betav) {
  # Estimate the trailing variance from the returns
  varv <- numeric(nrows)
  varv[1] <- omega/(1-alpha-betav)
  for (i in 2:nrows) {
    varv[i] <- omega + alpha*retp[i]^2 + betav*varv[i-1]
  }  # end for
  varv <- ifelse(varv > 0, varv, 0.000001)
  # Lag the variance
  varv <- rutils::lagit(varv, pad_zeros=FALSE)
  # Calculate the likelihood
  -sum(retp^2/varv + log(varv))
}  # end likefun
# Calculate the likelihood in R
likefun(omega, alpha, betav)
# Calculate the likelihood in Rcpp
HighFreq::lik_garch(omega=omega, alpha=alpha,
  beta=betav, returns=matrix(retp))
# Benchmark speed of likelihood calculations
library(microbenchmark)
summary(microbenchmark(
  Rcode=likefun(omega, alpha, betav),
  Rcpp=HighFreq::lik_garch(omega=omega, alpha=alpha, beta=betav, returns=matrix(retp))
  ), times=10)[, c(1, 4, 5)]
# Calculate the variance of returns
retp <- garch_data[, 1, drop=FALSE]
varv <- var(retp)
retp <- (retp - mean(retp))
# Calculate likelihood as function of alpha and betav parameters
likefun <- function(alpha, betav) {
  omega <- variance*(1 - alpha - betav)
  -HighFreq::lik_garch(omega=omega, alpha=alpha, beta=betav, returns=retp)
}  # end likefun
# Calculate matrix of likelihood values
alphas <- seq(from=0.15, to=0.35, len=50)
betav <- seq(from=0.35, to=0.5, len=50)
likmat <- sapply(alphas, function(alpha) sapply(betav,
  function(betav) likefun(alpha, betav)))
# Set rgl options and load package rgl
options(rgl.useNULL=TRUE); library(rgl)
# Draw and render 3d surface plot of likelihood function
ncols <- 100
color <- rainbow(ncols, start=2/6, end=4/6)
zcols <- cut(likmat, ncols)
rgl::persp3d(alphas, betav, likmat, col=color[zcols],
  xlab="alpha", ylab="beta", zlab="likelihood")
rgl::rglwidget(elementId="plot3drgl", width=700, height=700)
# Perform grid search
coord <- which(likmat == min(likmat), arr.ind=TRUE)
c(alphas[coord[2]], betav[coord[1]])
likmat[coord]
likefun(alphas[coord[2]], betav[coord[1]])
# Optimal and actual parameters
options(scipen=2)  # Use fixed not scientific notation
cbind(actual=c(alpha=alpha, beta=betav, omega=omega),
  optimal=c(alphas[coord[2]], betav[coord[1]], variance*(1 - sum(alphas[coord[2]], betav[coord[1]]))))
# Define vectorized likelihood function
likefun <- function(x, retp) {
  alpha <- x[1]; betav <- x[2]; omega <- x[3]
  -HighFreq::lik_garch(omega=omega, alpha=alpha, beta=betav, returns=retp)
}  # end likefun
# Initial parameters
initp <- c(alpha=0.2, beta=0.4, omega=varv/0.2)
# Find max likelihood parameters using steepest descent optimizer
fitobj <- optim(par=initp,
  fn=likefun, # Log-likelihood function
  method="L-BFGS-B", # Quasi-Newton method
  returns=retp,
  upper=c(0.35, 0.55, varv), # Upper constraint
  lower=c(0.15, 0.35, varv/100)) # Lower constraint
# Optimal and actual parameters
cbind(actual=c(alpha=alpha, beta=betav, omega=omega),
optimal=c(fitobj$par["alpha"], fitobj$par["beta"], fitobj$par["omega"]))
# Find max likelihood parameters using DEoptim
optiml <- DEoptim::DEoptim(fn=likefun,
  upper=c(0.35, 0.55, varv), # Upper constraint
  lower=c(0.15, 0.35, varv/100), # Lower constraint
  returns=retp,
  control=list(trace=FALSE, itermax=1000, parallelType=1))
# Optimal and actual parameters
cbind(actual=c(alpha=alpha, beta=betav, omega=omega),
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
par_am <- unname(optiml$optim$bestmem)
alpha <- par_am[1]; betav <- par_am[2]; omega <- par_am[3]
c(alpha, betav, omega)
# Equilibrium GARCH variance
omega/(1-alpha-betav)
drop(var(retp))
# Estimate the GARCH volatility of VTI returns
nrows <- NROW(retp)
varv <- numeric(nrows)
varv[1] <- omega/(1-alpha-betav)
for (i in 2:nrows) {
  varv[i] <- omega + alpha*retp[i]^2 + betav*varv[i-1]
}  # end for
# Estimate the GARCH volatility using Rcpp
garch_data <- HighFreq::sim_garch(omega=omega, alpha=alpha,
  beta=betav, innov=retp, is_random=FALSE)
all.equal(garch_data[, 2], varv, check.attributes=FALSE)
# Plot dygraph of the estimated GARCH volatility
dygraphs::dygraph(xts::xts(sqrt(varv), zoo::index(retp)),
  main="Estimated GARCH Volatility of VTI") %>%
  dyOptions(colors="blue") %>% dyLegend(show="always", width=500)
# Simulate GARCH model
garch_data <- HighFreq::sim_garch(omega=omega, alpha=alpha,
  beta=betav, innov=matrix(innov))
varv <- garch_data[, 2]
# Calculate the equilibrium variance
vareq <- omega/(1-alpha-betav)
# Calculate the variance forecasts
varf <- numeric(10)
varf[1] <- vareq + (alpha + betav)*(xts::last(varv) - vareq)
for (i in 2:10) {
  varf[i] <- vareq + (alpha + betav)*(varf[i-1] - vareq)
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
quartz.save("figure/garch_forecast.png", type="png",
  width=6, height=5)
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
ohlc <- rutils::etfenv$VTI
# Number of data points
nrows <- NROW(ohlc["2018-06/"])
# Define endd at each point in time
endd <- 0:nrows
# Number of data points in look_back interval
look_back <- 22
# startp are endd lagged by look_back
startp <- c(rep_len(0, look_back), endd[1:(NROW(endd)-look_back)])
head(startp, 33)
# Number of data points
closep <- quantmod::Cl(ohlc["2018/"])
nrows <- NROW(closep)
# Number of periods between endpoints
npoints <- 21
# Number of npoints that fit over nrows
nagg <- nrows %/% npoints
# If(nrows==npoints*nagg then whole number
endd <- (0:nagg)*npoints
# Stub interval at beginning
endd <- c(0, nrows-npoints*nagg + (0:nagg)*npoints)
# Else stub interval at end
endd <- c((0:nagg)*npoints, nrows)
# Or use xts::endpoints()
endd <- xts::endpoints(closep, on="weeks")
# Plot data and endpoints as vertical lines
plot.xts(closep, col="blue", lwd=2, xlab="", ylab="",
   main="Prices with Endpoints as Vertical Lines")
addEventLines(xts(rep("endpoint", NROW(endd)-1), zoo::index(closep)[endd]),
        col="red", lwd=2, pos=4)
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- "blue"
quantmod::chart_Series(closep, theme=plot_theme,
  name="prices with endpoints as vertical lines")
abline(v=endd, col="red", lwd=2)
# Number of data points
nrows <- NROW(rutils::etfenv$VTI["2019/"])
# Number of npoints that fit over nrows
npoints <- 21
nagg <- nrows %/% npoints
# Stub interval at beginning
endd <- c(0, nrows-npoints*nagg + (0:nagg)*npoints)
# look_back defined as number of data points
look_back <- 252
# startp are endd lagged by look_back
startp <- (endd - look_back + 1)
startp <- ifelse(startp < 0, 0, startp)
# look_back defined as number of endd
look_back <- 12
startp <- c(rep_len(0, look_back), endd[1:(NROW(endd)- look_back)])
# Bind startp with endd
cbind(startp, endd)
# Number of data points
nrows <- NROW(rutils::etfenv$VTI["2019/"])
# Number of data points per interval
npoints <- 21
# Number of npointss that fit over nrows
nagg <- nrows %/% npoints
# Define endd with beginning stub
endd <- c(0, nrows-npoints*nagg + (0:nagg)*npoints)
# Define contiguous startp
startp <- c(0, endd[1:(NROW(endd)-1)])
# Define exclusive startp
startp <- c(0, endd[1:(NROW(endd)-1)]+1)
# Extract time series of VTI log prices
closep <- log(na.omit(rutils::etfenv$prices$VTI))
endd <- 0:NROW(closep)  # End points at each point
npts <- NROW(endd)
look_back <- 22  # Number of data points per look-back interval
# startp are multi-period lag of endd
startp <- c(rep_len(0, look_back), endd[1:(npts - look_back)])
# Define list of look-back intervals for aggregations over past
look_backs <- lapply(2:npts, function(it) {
    startp[it]:endd[it]
})  # end lapply
# Define aggregation function
aggfun <- function(xtsv) c(max=max(xtsv), min=min(xtsv))
# Perform aggregations over look_backs list
aggs <- sapply(look_backs,
    function(look_back) aggfun(closep[look_back])
)  # end sapply
# Coerce aggs into matrix and transpose it
if (is.vector(aggs))
  aggs <- t(aggs)
aggs <- t(aggs)
# Coerce aggs into xts series
aggs <- xts(aggs, order.by=zoo::index(closep[endd]))
library(rutils)  # Load package rutils
# Perform aggregations over look_backs list
aggs <- lapply(look_backs,
    function(look_back) aggfun(closep[look_back])
)  # end lapply
# rbind list into single xts or matrix
aggs <- rutils::do_call(rbind, aggs)
# Convert into xts
aggs <- xts::xts(aggs, order.by=zoo::index(closep))
aggs <- cbind(aggs, closep)
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green")
x11(width=6, height=5)
quantmod::chart_Series(aggs, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(aggs),
  bg="white", lty=1, lwd=6, y.intersp=0.4,
  col=plot_theme$col$line.col, bty="n")
# library(rutils)  # Load package rutils
# Define functional for trailing aggregations
roll_agg <- function(xtsv, look_back, FUN, ...) {
# Define end points at every period
  endd <- 0:NROW(xtsv)
  npts <- NROW(endd)
# Define starting points as lag of endd
  startp <- c(rep_len(0, look_back), endd[1:(npts- look_back)])
# Perform aggregations over look_backs list
  aggs <- lapply(2:npts, function(it)
    FUN(xtsv[startp[it]:endd[it]], ...)
  )  # end lapply
# rbind list into single xts or matrix
  aggs <- rutils::do_call(rbind, aggs)
# Coerce aggs into xts series
  if (!is.xts(aggs))
    aggs <- xts(aggs, order.by=zoo::index(xtsv))
  aggs
}  # end roll_agg
# Define aggregation function
aggfun <- function(xtsv)
  c(max=max(xtsv), min=min(xtsv))
# Perform aggregations over trailing interval
aggs <- roll_agg(closep, look_back=look_back, FUN=aggfun)
class(aggs)
dim(aggs)
# library(rutils)  # Load package rutils
# Define aggregation function that returns a vector
agg_vector <- function(xtsv)
  c(max=max(xtsv), min=min(xtsv))
# Define aggregation function that returns an xts
agg_xts <- function(xtsv)
  xts(t(c(max=max(xtsv), min=min(xtsv))), order.by=end(xtsv))
# Benchmark the speed of aggregation functions
library(microbenchmark)
summary(microbenchmark(
  agg_vector=roll_agg(closep, look_back=look_back, FUN=agg_vector),
  agg_xts=roll_agg(closep, look_back=look_back, FUN=agg_xts),
  times=10))[, c(1, 4, 5)]
# library(rutils)  # Load package rutils
# Define aggregation function that returns a single value
aggfun <- function(xtsv)  max(xtsv)
# Perform aggregations over a trailing interval
aggs <- xts:::rollapply.xts(closep, width=look_back,
              FUN=aggfun, align="right")
# Perform aggregations over a trailing interval
library(PerformanceAnalytics)  # Load package PerformanceAnalytics
aggs <- apply.rolling(closep, width=look_back, FUN=aggfun)
# Benchmark the speed of the functionals
library(microbenchmark)
summary(microbenchmark(
  roll_agg=roll_agg(closep, look_back=look_back, FUN=max),
  roll_xts=xts:::rollapply.xts(closep, width=look_back, FUN=max, align="right"),
  apply_rolling=apply.rolling(closep, width=look_back, FUN=max),
  times=10))[, c(1, 4, 5)]
# library(rutils)  # Load package rutils
# Trailing sum using cumsum()
roll_sum <- function(xtsv, look_back) {
  cumsumv <- cumsum(na.omit(xtsv))
  output <- cumsumv - rutils::lagit(x=cumsumv, lagg=look_back)
  output[1:look_back, ] <- cumsumv[1:look_back, ]
  colnames(output) <- paste0(colnames(xtsv), "_stdev")
  output
}  # end roll_sum
aggs <- roll_sum(closep, look_back=look_back)
# Perform trailing aggregations using lapply loop
aggs <- lapply(2:npts, function(it)
    sum(closep[startp[it]:endd[it]])
)  # end lapply
# rbind list into single xts or matrix
aggs <- rutils::do_call(rbind, aggs)
head(aggs)
tail(aggs)
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
weightv <- exp(-0.1*1:look_back)
weightv <- weightv/sum(weights)
pricef <- stats::filter(closep, filter=weightv,
                   method="convolution", sides=1)
pricef <- as.numeric(pricef)
# filter() returns time series of class "ts"
class(pricef)
# Filter using compiled C++ function directly
getAnywhere(C_cfilter)
str(stats:::C_cfilter)
priceff <- .Call(stats:::C_cfilter, closep,
               filter=weightv, sides=1, circular=FALSE)
all.equal(as.numeric(pricef), priceff, check.attributes=FALSE)
# Calculate EWMA prices using HighFreq::roll_conv()
pricecpp <- HighFreq::roll_conv(closep, weightv=weightv)
all.equal(pricef[-(1:look_back)],
    as.numeric(pricecpp)[-(1:look_back)],
    check.attributes=FALSE)
# Benchmark speed of trailing calculations
library(microbenchmark)
summary(microbenchmark(
  filter=filter(closep, filter=weightv, method="convolution", sides=1),
  priceff=.Call(stats:::C_cfilter, closep, filter=weightv, sides=1, circular=FALSE),
  cumsumv=cumsum(closep),
  rcpp=HighFreq::roll_conv(closep, weightv=weightv)
  ), times=10)[, c(1, 4, 5)]
# Calculate the trailing maximum and minimum over a vector of data
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
  rcode=roll_maxminr(closep, look_back),
  ttr=TTR::runMax(closep, n=look_back),
  times=10))[, c(1, 4, 5)]
# Benchmark the speed of TTR::runSum
summary(microbenchmark(
  vector_r=cumsum(coredata(closep)),
  rutils=rutils::roll_sum(closep, look_back=look_back),
  ttr=TTR::runSum(closep, n=look_back),
  times=10))[, c(1, 4, 5)]
library(rutils)
# Calculate trailing VTI variance using package roll
library(roll)  # Load roll
retp <- na.omit(rutils::etfenv$returns$VTI)
look_back <- 22
# Calculate trailing sum using roll::roll_sum
sum_roll <- roll::roll_sum(retp, width=look_back, min_obs=1)
# Calculate trailing sum using rutils
sum_rutils <- rutils::roll_sum(retp, look_back=look_back)
all.equal(sum_roll[-(1:look_back), ],
    sum_rutils[-(1:look_back), ], check.attributes=FALSE)
# Benchmark speed of trailing calculations
library(microbenchmark)
summary(microbenchmark(
  cumsumv=cumsum(retp),
  roll=roll::roll_sum(retp, width=look_back),
  RcppRoll=RcppRoll::roll_sum(retp, n=look_back),
  rutils=rutils::roll_sum(retp, look_back=look_back),
  times=10))[, c(1, 4, 5)]
library(RcppRoll)  # Load package RcppRoll
# Calculate trailing sum using RcppRoll
sum_roll <- RcppRoll::roll_sum(retp, align="right", n=look_back)
# Calculate trailing sum using rutils
sum_rutils <- rutils::roll_sum(retp, look_back=look_back)
all.equal(sum_roll, coredata(sum_rutils[-(1:(look_back-1))]),
    check.attributes=FALSE)
# Benchmark speed of trailing calculations
library(microbenchmark)
summary(microbenchmark(
  cumsumv=cumsum(retp),
  RcppRoll=RcppRoll::roll_sum(retp, n=look_back),
  rutils=rutils::roll_sum(retp, look_back=look_back),
  times=10))[, c(1, 4, 5)]
# Calculate EWMA prices using RcppRoll
closep <- quantmod::Cl(rutils::etfenv$VTI)
weightv <- exp(0.1*1:look_back)
pricewma <- RcppRoll::roll_mean(closep,
align="right", n=look_back, weights=weightv)
pricewma <- cbind(closep,
  rbind(coredata(closep[1:(look_back-1), ]), pricewma))
colnames(pricewma) <- c("VTI", "VTI EWMA")
# Plot an interactive dygraph plot
dygraphs::dygraph(pricewma)
# Or static plot of EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red")
quantmod::chart_Series(pricewma, theme=plot_theme, name="EWMA prices")
legend("top", legend=colnames(pricewma),
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
# Vector of trailing volatilities
sigmav <- runsd(x=closep, k=look_back,
          endrule="constant", align="center")
# Vector of trailing quantiles
quantvs <- runquantile(x=closep, k=look_back,
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
colorv <- c("blue", "red", "green")
dygraphs::dygraph(datav, main=paste(colnames(closep), "max and min lines")) %>%
  dyOptions(colors=colorv) %>% dyLegend(show="always", width=500)
# Standard plot with max_min lines
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(datav["2008/2009"], theme=plot_theme,
  name=paste(colnames(closep), "max and min lines"))
legend(x="topright", title=NULL, legend=colnames(datav),
 inset=0.1, cex=0.9, bg="white", bty="n",
 lwd=6, lty=1, col=colorv)
library(rutils)  # Load package rutils
# Indices of last observations in each hour
endd <- xts::endpoints(closep, on="hours")
head(endd)
# extract the last observations in each hour
head(closep[endd, ])
# Extract time series of VTI log prices
closep <- log(na.omit(rutils::etfenv$prices$VTI))
# Number of data points
nrows <- NROW(closep)
# Number of data points per interval
look_back <- 22
# Number of look_backs that fit over nrows
nagg <- nrows %/% look_back
# Define endd with beginning stub
endd <- c(0, nrows-look_back*nagg + (0:nagg)*look_back)
# Define contiguous startp
startp <- c(0, endd[1:(NROW(endd)-1)])
# Define list of look-back intervals for aggregations over past
look_backs <- lapply(2:NROW(endd), function(it) {
    startp[it]:endd[it]
})  # end lapply
look_backs[[1]]
look_backs[[2]]
# Perform sapply() loop over look_backs list
aggs <- sapply(look_backs, function(look_back) {
  xtsv <- closep[look_back]
  c(max=max(xtsv), min=min(xtsv))
})  # end sapply
# Coerce aggs into matrix and transpose it
if (is.vector(aggs))
  aggs <- t(aggs)
aggs <- t(aggs)
# Coerce aggs into xts series
aggs <- xts(aggs, order.by=zoo::index(closep[endd]))
head(aggs)
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
quantmod::chart_Series(aggs, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(aggs),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# library(rutils)  # Load package rutils
# Perform lapply() loop over look_backs list
aggs <- lapply(look_backs, function(look_back) {
  xtsv <- closep[look_back]
  c(max=max(xtsv), min=min(xtsv))
})  # end lapply
# rbind list into single xts or matrix
aggs <- rutils::do_call(rbind, aggs)
# Coerce aggs into xts series
aggs <- xts(aggs, order.by=zoo::index(closep[endd]))
head(aggs)
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
quantmod::chart_Series(aggs, theme=plot_theme, name="price aggregations")
legend("top", legend=colnames(aggs),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
# library(rutils)  # Load package rutils
# Define functional for trailing aggregations over endd
roll_agg <- function(xtsv, endd, FUN, ...) {
  nrows <- NROW(endd)
# startp are single-period lag of endd
  startp <- c(1, endd[1:(nrows-1)])
# Perform aggregations over look_backs list
  aggs <- lapply(look_backs,
    function(look_back) FUN(xtsv[look_back], ...))  # end lapply
# rbind list into single xts or matrix
  aggs <- rutils::do_call(rbind, aggs)
  if (!is.xts(aggs))
    aggs <-  # Coerce aggs into xts series
    xts(aggs, order.by=zoo::index(xtsv[endd]))
  aggs
}  # end roll_agg
# Apply sum() over endd
aggs <- roll_agg(closep, endd=endd, FUN=sum)
aggs <- period.apply(closep, INDEX=endd, FUN=sum)
# Benchmark the speed of aggregation functions
summary(microbenchmark(
  roll_agg=roll_agg(closep, endd=endd, FUN=sum),
  period_apply=period.apply(closep, INDEX=endd, FUN=sum),
  times=10))[, c(1, 4, 5)]
aggs <- period.sum(closep, INDEX=endd)
head(aggs)
# library(rutils)  # Load package rutils
# Load package HighFreq
library(HighFreq)
# Extract closing minutely prices
closep <- quantmod::Cl(rutils::etfenv$VTI["2019"])
# Apply "mean" over daily periods
aggs <- apply.daily(closep, FUN=sum)
head(aggs)
# Define endd with beginning stub
npoints <- 5
nrows <- NROW(closep)
nagg <- nrows %/% npoints
endd <- c(0, nrows-npoints*nagg + (0:nagg)*npoints)
# Number of data points in look_back interval
look_back <- 22
# startp are endd lagged by look_back
startp <- (endd - look_back + 1)
startp <- ifelse(startp < 0, 0, startp)
# Perform lapply() loop over look_backs list
aggs <- lapply(2:NROW(endd), function(it) {
xtsv <- closep[startp[it]:endd[it]]
c(max=max(xtsv), min=min(xtsv))
})  # end lapply
# rbind list into single xts or matrix
aggs <- rutils::do_call(rbind, aggs)
# Coerce aggs into xts series
aggs <- xts(aggs, order.by=zoo::index(closep[endd]))
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
quantmod::chart_Series(aggs, theme=plot_theme,
       name="price aggregations")
legend("top", legend=colnames(aggs),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
aggs <- cbind(closep, aggs)
tail(aggs)
aggs <- na.omit(xts:::na.locf.xts(aggs))
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green")
quantmod::chart_Series(aggs, theme=plot_theme, name="price aggregations")
legend("top", legend=colnames(aggs),
  bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")
set.seed(1121)  # Reset random number generator
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(zoo)  # Load package zoo
# Create zoo time series of random returns
datev <- Sys.Date() + 0:365
zoo_series <- zoo(rnorm(NROW(datev)), order.by=datev)
# Create monthly dates
dates_agg <- as.Date(as.yearmon(zoo::index(zoo_series)))
# Perform monthly mean aggregation
zoo_agg <- aggregate(zoo_series, by=datev_agg, FUN=mean)
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
 title="Aggregated Prices", y.intersp=0.4,
 leg=c("orig prices", "agg prices"),
 lwd=2, bg="white", col=c("black", "red"))
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Perform monthly mean aggregation
zoo_agg <- aggregate(zoo_series, by=datev_agg, FUN=mean)
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
 col=c("black", "red"), bty="n", y.intersp=0.4)
par(mar=c(7, 2, 1, 2), mgp=c(2, 1, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# "mean" aggregation over interval with width=11
zoo_mean <- rollapply(zoo_series, width=11, FUN=mean, align="right")
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
 col=c("black", "red"), bty="n", y.intersp=0.4)
