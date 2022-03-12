# Summary: Calculate the performance of random sub-portfolios
# of S&P500 constituent stocks.
# 1. (20pts) Load the file sp500_prices.RData containing
# an xts series called prices, with the daily closing
# Prices of the S&P500 stock index constituents.
library(rutils)
load("/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# Subset (select) from prices only the data after the
# year 2000, and copy it over prices.
prices <- prices["2000/"]
# Copy over NA prices using the function zoo::na.locf().
prices <- zoo::na.locf(prices, na.rm=FALSE)
prices <- zoo::na.locf(prices, fromLast=TRUE)
# Create a vector of dates called dates, equal to the
# time index of prices.
dates <- zoo::index(prices)
# Normalize the columns of prices, so that the first
# row for all columns is equal to 1.
# The columns of prices will then represent the growth
# of one dollar invested in that stock.
# You can use the functions lapply(), as.numeric(), cbind(),
# and rutils::do_call().
prices <- lapply(prices, function(x) x/as.numeric(x[1]))
prices <- rutils::do_call(cbind, prices)
class(prices)
dim(prices)
sum(prices[1, ])
round(head(prices[, 1:6]), 3)
round(tail(prices[, 1:6]), 3)
# Calculate a vector equal to the dollar-weighted
# Prices of the index components, i.e. the average of
# the rows of prices, and call it indeks.
# You can use the functions NCOL() and rowSums().
ncols <- NCOL(prices)
indeks <- rowSums(prices)/ncols
# You should get the following output:
tail(indeks)
# [1] 12.27228 12.33409 12.33381 11.98787 12.11134 11.87442
# 2. (30pts) Select twenty equally dollar-weighted,
# random sub-portfolios from the columns of prices,
# with each sub-portfolio being the average of five
# randomly selected columns (stocks) of prices.
# Bind the sub-portfolio prices into a single xts
# time series called sub_portfolios.
# You can use the function sample.int() with "replace=FALSE".
# You can also use the functions sapply(), xts::xts(),
# rowSums(), paste0(), and colnames().
# You can use the vector of dates called dates.
n_portf <- 20
nstocks <- 5
set.seed(1121)
sub_portfolios <- sapply(1:n_portf, function(x) {
  prices <- prices[, sample.int(n=ncols, size=nstocks, replace=FALSE)]
  rowSums(prices)/nstocks
})  # end sapply
sub_portfolios <- xts::xts(sub_portfolios, order.by=dates)
colnames(sub_portfolios) <- paste0("portf", 1:n_portf)
# You should get output similar to the following:
#
round(head(sub_portfolios[, 1:4]), 3)
#            portf1 portf2 portf3 portf4
# 2000-01-03  1.000  1.000  1.000  1.000
# 2000-01-04  0.977  0.975  0.962  0.973
# 2000-01-05  0.967  0.992  0.979  0.988
# 2000-01-06  0.950  1.001  0.981  1.008
# 2000-01-07  0.966  1.008  0.985  1.035
# 2000-01-10  0.980  1.010  0.980  1.043
round(tail(sub_portfolios[, 1:4]), 3)
#            portf1 portf2 portf3 portf4
# 2020-06-19 30.928 11.654 36.575  8.667
# 2020-06-22 31.553 11.691 37.173  8.662
# 2020-06-23 31.262 11.606 37.022  8.660
# 2020-06-24 30.275 11.409 37.289  8.415
# 2020-06-25 30.897 11.511 37.242  8.567
# 2020-06-26 29.834 11.228 36.912  8.570
# Plot the sub_portfolios from worst to best (based
# on final price) using a color ramp from red to blue.
#
# Create a color ramp, using functions colorRampPalette()
# and order().
colors <- colorRampPalette(c("red", "blue"))(n_portf)
colors <- colors[order(order(sub_portfolios[NROW(sub_portfolios), ]))]
# Create a plot of the sub_portfolios with the custom
# color ramp, using either function zoo::plot.zoo(),
# Or functions chart_theme() and chart_Series().
# Plot using chart_theme() and chart_Series()
x11(width=6, height=5)
zoo::plot.zoo(sub_portfolios, plot.type="single",
        col=colors, xlab="", ylab="",
        main="Random S&P500 Stock Sub-portfolios (normalized)")
# Or plot using quantmod::chart_Series()
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(sub_portfolios, theme=plot_theme,
                 name="Random S&P500 Stock Sub-portfolios (normalized)")
# Your plot should be similar to sp500_sub_portfolios.png
# Calculate an xts series called above_index, with the
# percentage of sub-portfolios whose prices at the end
# of each year are above the index price indeks.
# You cannot perform any loops.
# You can use the functions xts::endpoints(), rowSums(),
# chart_theme(), and xts().
# You can calculate the end of year end points as follows:
endp <- xts::endpoints(dates, on="years")
# endp are the row numbers for the end of year dates:
dates[endp]
endp <- xts::endpoints(dates, on="years")
above_index <- (sub_portfolios[endp, ] > indeks[endp])
above_index <- rowSums(above_index)/n_portf
above_index <- xts::xts(above_index, order.by=dates[endp])
colnames(above_index) <- "percentage"
# You should get output similar to the following:
above_index
#             percentage
# 2000-12-29       0.35
# 2001-12-31       0.40
# 2002-12-31       0.35
# 2003-12-31       0.25
# 2004-12-31       0.35
# 2005-12-30       0.25
# 2006-12-29       0.25
# 2007-12-31       0.25
# 2008-12-31       0.30
# 2009-12-31       0.25
# 2010-12-31       0.30
# 2011-12-30       0.30
# 2012-12-31       0.30
# 2013-12-31       0.35
# 2014-12-31       0.30
# 2015-12-31       0.30
# 2016-12-30       0.30
# 2017-12-29       0.30
# 2018-12-31       0.40
# 2019-12-31       0.40
# 2020-06-26       0.40
# Create a plot of above_index using function
# zoo::plot.zoo().
zoo::plot.zoo(above_index, col="blue", lwd=2, xlab="", ylab="",
        main="Percentage of Random Sub-portfolios
        Above the Index")
# Your plot should be similar to sp500_sub_portfolios_above.png
library(PortfolioAnalytics)
# Use ETF returns from package rutils
library(rutils)
portf_names <- c("VTI", "IEF", "DBC", "XLF",
  "VNQ", "XLP", "XLV", "XLU", "XLB", "XLE")
# Initial portfolio to equal weights
portf_init <- rep(1/NROW(portf_names),
            NROW(portf_names))
# named vector
names(portf_init) <- portf_names
# Create portfolio object
portf_init <- portfolio.spec(
  assets=portf_init)
library(PortfolioAnalytics)
# Add constraints
portf_maxSR <- add.constraint(
  portfolio=portf_init,  # Initial portfolio
  type="weightvum",  # Constraint sum weights
  min_sum=0.9, max_sum=1.1)
# Add constraints
portf_maxSR <- add.constraint(
  portfolio=portf_maxSR,
  type="long_only")  # box constraint min=0, max=1
# Add objectives
portf_maxSR <- add.objective(
  portfolio=portf_maxSR,
  type="return",  # Maximize mean return
  name="mean")
# Add objectives
portf_maxSR <- add.objective(
  portfolio=portf_maxSR,
  type="risk",  # Minimize StdDev
  name="StdDev")
library(PortfolioAnalytics)
# Use ETF returns from package rutils
library(rutils)
portf_names <- c("VTI", "IEF", "DBC", "XLF",
  "VNQ", "XLP", "XLV", "XLU", "XLB", "XLE")
# Initial portfolio to equal weights
portf_init <- rep(1/NROW(portf_names),
            NROW(portf_names))
# named vector
names(portf_init) <- portf_names
# Create portfolio object
portf_init <- portfolio.spec(
  assets=portf_init)
library(PortfolioAnalytics)
# Add constraints
portf_maxSR <- add.constraint(
  portfolio=portf_init,  # Initial portfolio
  type="weightvum",  # Constraint sum weights
  min_sum=0.9, max_sum=1.1)
# Add constraints
portf_maxSR <- add.constraint(
  portfolio=portf_maxSR,
  type="long_only")  # box constraint min=0, max=1
# Add objectives
portf_maxSR <- add.objective(
  portfolio=portf_maxSR,
  type="return",  # Maximize mean return
  name="mean")
# Add objectives
portf_maxSR <- add.objective(
  portfolio=portf_maxSR,
  type="risk",  # Minimize StdDev
  name="StdDev")
# Linear constraint
weightv <- weightv/sum(weightv)
# Quadratic constraint
weightv <- weightv/sqrt(sum(weightv^2))
# Box constraints
weightv[weightv > 1] <- 1
weightv[weightv < 0] <- 0
library(quantmod)
library(Rglpk)
# Vector of symbol names
symbols <- c("VTI", "IEF", "DBC")
nweights <- NROW(symbols)
# Calculate mean returns
returns <- rutils::etfenv$returns[, symbols]
returns <- zoo::na.locf(returns, na.rm=FALSE)
returns <- na.omit(returns)
mean_rets <- colMeans(returns)
# Specify weight constraints
constraint_s <- matrix(c(rep(1, nweights), 1, 1, 0),
                 nc=nweights, byrow=TRUE)
direction_s <- c("==", "<=")
rh_s <- c(1, 0)
# Specify weight bounds (-1, 1) (default is c(0, Inf))
bound_s <-
  list(lower=list(ind=1:nweights, val=rep(-1, nweights)),
 upper=list(ind=1:nweights, val=rep(1, nweights)))
# Perform optimization
optimd <- Rglpk::Rglpk_solve_LP(
  obj=mean_rets,
  mat=constraint_s,
  dir=direction_s,
  rhs=rh_s,
  bounds=bound_s,
  max=TRUE)
unlist(optimd[1:2])
# Calculate covariance matrix of returns and its inverse
covmat <- cov(returns)
covinv <- solve(a=covmat)
unitv <- rep(1, NCOL(covmat))
# Minimum variance weights with constraint
# weightv <- solve(a=covmat, b=unitv)
weightv <- covinv %*% unitv
weightv <- weightv / drop(t(unitv) %*% weightv)
# Minimum variance
t(weightv) %*% covmat %*% weightv
1/(t(unitv) %*% covinv %*% unitv)
# Calculate vector of mean returns
mean_rets <- colMeans(returns)
# Specify the target return
targetr <- 1.5*mean(returns)
# Products of inverse with mean returns and unit vector
fmat <- matrix(c(
  t(unitv) %*% covinv %*% unitv,
  t(unitv) %*% covinv %*% mean_rets,
  t(mean_rets) %*% covinv %*% unitv,
  t(mean_rets) %*% covinv %*% mean_rets), nc=2)
# Solve for the Lagrange multipliers
lagm <- solve(a=fmat, b=c(2, 2*targetr))
# Calculate weights
weightv <- drop(0.5*covinv %*% cbind(unitv, mean_rets) %*% lagm)
# Calculate constraints
all.equal(1, sum(weightv))
all.equal(targetr, sum(mean_rets*weightv))
# Calculate portfolio return and standard deviation
retsp <- drop(returns %*% weightv)
c(return=mean(retsp), sd=sd(retsp))
all.equal(mean(retsp), targetr)
# Calculate portfolio variance
uu <- c(1, targetr)
finv <- solve(fmat)
all.equal(var(retsp), drop(t(uu) %*% finv %*% uu))
# Calculate vertex of variance parabola
weightv <- drop(covinv %*% unitv /
  drop(t(unitv) %*% covinv %*% unitv))
retsp <- drop(returns %*% weightv)
retsv <- drop(t(unitv) %*% covinv %*% mean_rets /
  t(unitv) %*% covinv %*% unitv)
all.equal(mean(retsp), retsv)
var_min <- drop(1/t(unitv) %*% covinv %*% unitv)
all.equal(var(retsp), var_min)
# Calculate efficient frontier
targets <- retsv*(1+seq(from=-1, to=1, by=0.1))
eff_front <- sapply(targets, function(targetr) {
  uu <- c(1, targetr)
  sqrt(drop(t(uu) %*% finv %*% uu))
})  # end sapply
# Plot efficient frontier
x11(width=6, height=5)
plot(x=eff_front, y=targets, t="l", col="blue", lwd=2,
     main="Efficient Frontier and Minimum Variance Portfolio",
     xlab="standard deviation", ylab="return")
points(x=sqrt(var_min), y=retsv, col="green", lwd=6)
text(x=sqrt(var_min), y=retsv, labels="minimum \nvariance",
     pos=4, cex=0.8)
# Calculate portfolio standard deviation
stdev <- sqrt(drop(t(uu) %*% finv %*% uu))
# Calculate the slope of the tangent line
slopev <- (stdev*det(fmat))/(fmat[1, 1]*targetr-fmat[1, 2])
# Calculate the risk-free rate as intercept of the tangent line
riskf <- targetr - slopev*stdev
# Calculate the risk-free rate from target return
riskf <- (targetr*fmat[1, 2]-fmat[2, 2]) /
  (targetr*fmat[1, 1]-fmat[1, 2])
# Plot efficient frontier
plot(x=eff_front, y=targets, t="l", col="blue", lwd=2,
     xlim=c(0.0, max(eff_front)),
     main="Efficient Frontier and Tangency Portfolio",
     xlab="standard deviation", ylab="return")
# Plot minimum variance
points(x=sqrt(var_min), y=retsv, col="green", lwd=6)
text(x=sqrt(var_min), y=retsv, labels="minimum \nvariance",
     pos=4, cex=0.8)
# Plot tangent point
points(x=stdev, y=targetr, col="red", lwd=6)
text(x=stdev, y=targetr, labels="tangency\nportfolio", pos=2, cex=0.8)
# Plot risk-free point
points(x=0, y=riskf, col="red", lwd=6)
text(x=0, y=riskf, labels="risk-free", pos=4, cex=0.8)
# Plot tangent line
abline(a=riskf, b=slopev, lwd=2, col="green")
# Calculate excess returns
riskf <- 0.03/252
excess <- returns - riskf
# Calculate covariance and inverse matrix
covmat <- cov(returns)
unitv <- rep(1, NCOL(covmat))
covinv <- solve(a=covmat)
# Calculate mean excess returns
excess <- sapply(excess, mean)
# Weights of maximum Sharpe portfolio
# weightv <- solve(a=covmat, b=returns)
weightv <- covinv %*% excess
weightv <- weightv/drop(t(unitv) %*% weightv)
# Sharpe ratios
sqrt(252)*sum(weightv * excess) /
  sqrt(drop(weightv %*% covmat %*% weightv))
sapply(returns - riskf,
  function(x) sqrt(252)*mean(x)/sd(x))
weights_maxsharpe <- weightv
library(quantmod)
# Calculate minimum variance weights
weightv <- covinv %*% unitv
weights_minvar <- weightv/drop(t(unitv) %*% weightv)
# Calculate optimal portfolio returns
optim_rets <- xts(
  x=cbind(exp(cumsum(returns %*% weights_maxsharpe)),
    exp(cumsum(returns %*% weights_minvar))),
  order.by=index(returns))
colnames(optim_rets) <- c("maxsharpe", "minvar")
# Plot optimal portfolio returns, with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "green")
x11(width=6, height=5)
chart_Series(optim_rets, theme=plot_theme,
  name="Maximum Sharpe and
  Minimum Variance portfolios")
legend("top", legend=colnames(optim_rets), cex=0.8,
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
x11(wid_th <- 6, hei_ght <- 6)
# Calculate minimum variance weights
weightv <- covinv %*% unitv
weightv <- weightv / drop(t(unitv) %*% weightv)
# Minimum standard deviation and return
stdev <- sqrt(252*drop(weightv %*% covmat %*% weightv))
min_ret <- 252*sum(weightv * mean_rets)
# Calculate maximum Sharpe portfolios
riskf <- (min_ret * seq(-10, 10, by=0.1)^3)/252
eff_front <- sapply(riskf, function(riskf) {
  weightv <- covinv %*% (mean_rets - riskf)
  weightv <- weightv/drop(t(unitv) %*% weightv)
  # Portfolio return and standard deviation
  c(return=252*sum(weightv * mean_rets),
    stddev=sqrt(252*drop(weightv %*% covmat %*% weightv)))
})  # end sapply
eff_front <- cbind(252*riskf, t(eff_front))
colnames(eff_front)[1] <- "risk-free"
eff_front <- eff_front[is.finite(eff_front[, "stddev"]), ]
eff_front <- eff_front[order(eff_front[, "return"]), ]
# Plot maximum Sharpe portfolios
plot(x=eff_front[, "stddev"],
     y=eff_front[, "return"], t="l",
     xlim=c(0.0*stdev, 3.0*stdev),
     ylim=c(0.0*min_ret, 2.0*min_ret),
     main="Efficient Frontier and Capital Market Line",
     xlab="standard deviation", ylab="return")
points(x=eff_front[, "stddev"], y=eff_front[, "return"],
 col="red", lwd=3)
# Plot minimum variance portfolio
points(x=stdev, y=min_ret, col="green", lwd=6)
text(stdev, min_ret, labels="minimum \nvariance",
     pos=4, cex=0.8)
# Draw Capital Market Line
sortv <- sort(eff_front[, 1])
riskf <-
  sortv[findInterval(x=0.5*min_ret, vec=sortv)]
points(x=0, y=riskf, col="blue", lwd=6)
text(x=0, y=riskf, labels="risk-free",
     pos=4, cex=0.8)
indeks <- match(riskf, eff_front[, 1])
points(x=eff_front[indeks, "stddev"],
 y=eff_front[indeks, "return"],
 col="blue", lwd=6)
text(x=eff_front[indeks, "stddev"],
     y=eff_front[indeks, "return"],
     labels="market portfolio",
     pos=2, cex=0.8)
sharp_e <- (eff_front[indeks, "return"]-riskf)/
  eff_front[indeks, "stddev"]
abline(a=riskf, b=sharp_e, col="blue", lwd=2)
text(x=0.7*eff_front[indeks, "stddev"],
     y=0.7*eff_front[indeks, "return"]+0.01,
     labels="Capital Market Line", pos=2, cex=0.8,
     srt=45*atan(sharp_e*hei_ght/wid_th)/(0.25*pi))
# Calculate random portfolios
n_portf <- 1000
ret_sd <- sapply(1:n_portf, function(indeks) {
  weightv <- runif(nweights-1, min=-0.25, max=1.0)
  weightv <- c(weightv, 1-sum(weightv))
  # Portfolio return and standard deviation
  c(return=252*sum(weightv * mean_rets),
    stddev=sqrt(252*drop(weightv %*% covmat %*% weightv)))
})  # end sapply
# Plot scatterplot of random portfolios
x11(wid_th <- 6, hei_ght <- 6)
plot(x=ret_sd["stddev", ], y=ret_sd["return", ],
     main="Efficient Frontier and Random Portfolios",
     xlim=c(0.5*stdev, 0.8*max(ret_sd["stddev", ])),
     xlab="standard deviation", ylab="return")
# Plot maximum Sharpe portfolios
lines(x=eff_front[, "stddev"],
     y=eff_front[, "return"], lwd=2)
points(x=eff_front[, "stddev"], y=eff_front[, "return"],
 col="red", lwd=3)
# Plot minimum variance portfolio
points(x=stdev, y=min_ret, col="green", lwd=6)
text(stdev, min_ret, labels="minimum\nvariance",
     pos=2, cex=0.8)
# Plot market portfolio
points(x=eff_front[indeks, "stddev"],
 y=eff_front[indeks, "return"], col="green", lwd=6)
text(x=eff_front[indeks, "stddev"],
     y=eff_front[indeks, "return"],
     labels="market\nportfolio",
     pos=2, cex=0.8)
# Plot individual assets
points(x=sqrt(252*diag(covmat)),
 y=252*mean_rets, col="blue", lwd=6)
text(x=sqrt(252*diag(covmat)), y=252*mean_rets,
     labels=names(mean_rets),
     col="blue", pos=1, cex=0.8)
# Vector of symbol names
symbols <- c("VTI", "IEF", "DBC")
nweights <- NROW(symbols)
# Calculate random portfolios
n_portf <- 1000
ret_sd <- sapply(1:n_portf, function(indeks) {
  weightv <- runif(nweights, min=0, max=10)
  weightv <- weightv/sum(weightv)
  retsp <- etfenv$returns[, symbols] %*% weightv
  100*c(ret=mean(retsp), sd=sd(retsp))
})  # end sapply
# Plot scatterplot of random portfolios
x11(width=6, height=5)
plot(x=ret_sd[2, ], y=ret_sd[1, ], xlim=c(0, max(ret_sd[2, ])),
     main="Random portfolios",
     ylim=c(min(0, min(ret_sd[1, ])), max(ret_sd[1, ])),
     xlab=rownames(ret_sd)[2], ylab=rownames(ret_sd)[1])
# Plot individual assets
points(x=sqrt(252*diag(covmat)),
 y=252*mean_rets, col="blue", lwd=6)
text(x=sqrt(252*diag(covmat)), y=252*mean_rets,
     labels=names(mean_rets),
     col="blue", pos=1, cex=0.8)
riskf <- 0.03
returns <- c(asset1=0.05, asset2=0.06)
stdevs <- c(asset1=0.4, asset2=0.5)
cor_rel <- 0.6
covmat <- matrix(c(1, cor_rel, cor_rel, 1), nc=2)
covmat <- t(t(stdevs*covmat)*stdevs)
weightv <- seq(from=-1, to=2, length.out=31)
weightv <- cbind(weightv, 1-weightv)
retsp <- weightv %*% returns
portf_sd <-
  sqrt(rowSums(weightv * (weightv %*% covmat)))
sharper <- (retsp-riskf)/portf_sd
indeks <- which.max(sharper)
sharpem <- max(sharper)
# Plot efficient frontier
x11(wid_th <- 6, hei_ght <- 5)
par(mar=c(3,3,2,1)+0.1, oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(portf_sd, retsp, t="l",
 main=paste0("Efficient frontier and CML for two assets\ncorrelation = ", 100*cor_rel, "%"),
 xlab="standard deviation", ylab="return",
 lwd=2, col="orange",
 xlim=c(0, max(portf_sd)),
 ylim=c(0.02, max(retsp)))
# Add Market Portfolio (maximum Sharpe ratio portfolio)
points(portf_sd[indeks], retsp[indeks],
 col="blue", lwd=3)
text(x=portf_sd[indeks], y=retsp[indeks],
     labels=paste(c("market portfolio\n",
 structure(c(weightv[indeks], 1-weightv[indeks]),
         names=names(returns))), collapse=" "),
     pos=2, cex=0.8)
# Plot individual assets
points(stdevs, returns, col="green", lwd=3)
text(stdevs, returns, labels=names(returns), pos=4, cex=0.8)
# Add point at risk-free rate and draw Capital Market Line
points(x=0, y=riskf, col="blue", lwd=3)
text(0, riskf, labels="risk-free\nrate", pos=4, cex=0.8)
abline(a=riskf, b=sharpem, lwd=2, col="blue")
range_s <- par("usr")
text(portf_sd[indeks]/2, (retsp[indeks]+riskf)/2,
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(sharpem*(range_s[2]-range_s[1])/
             (range_s[4]-range_s[3])*
             hei_ght/wid_th)/(0.25*pi))
# Plot portfolios in x11() window
x11(wid_th <- 6, hei_ght <- 5)
par(oma=c(0, 0, 0, 0), mar=c(3,3,2,1)+0.1, mgp=c(2, 1, 0), cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
# Vector of symbol names
symbols <- c("VTI", "IEF")
# Matrix of portfolio weights
weightv <- seq(from=-1, to=2, length.out=31)
weightv <- cbind(weightv, 1-weightv)
# Calculate portfolio returns and volatilities
returns <- rutils::etfenv$returns[, symbols]
ret_sd <- returns %*% t(weightv)
ret_sd <- cbind(252*colMeans(ret_sd),
  sqrt(252)*matrixStats::colSds(ret_sd))
colnames(ret_sd) <- c("returns", "stddev")
riskf <- 0.06
ret_sd <- cbind(ret_sd,
  (ret_sd[, "returns"]-riskf)/ret_sd[, "stddev"])
colnames(ret_sd)[3] <- "Sharpe"
indeks <- which.max(ret_sd[, "Sharpe"])
sharpem <- ret_sd[indeks, "Sharpe"]
plot(x=ret_sd[, "stddev"], y=ret_sd[, "returns"],
     main="Stock and Bond portfolios", t="l",
     xlim=c(0, 0.7*max(ret_sd[, "stddev"])), ylim=c(0, max(ret_sd[, "returns"])),
     xlab="standard deviation", ylab="return")
# Add blue point for market portfolio
points(x=ret_sd[indeks, "stddev"], y=ret_sd[indeks, "returns"], col="blue", lwd=6)
text(x=ret_sd[indeks, "stddev"], y=ret_sd[indeks, "returns"],
     labels=paste(c("market portfolio\n", structure(c(weightv[indeks, 1], weightv[indeks, 2]), names=symbols)), collapse=" "),
     pos=3, cex=0.8)
# Plot individual assets
mean_rets <- 252*sapply(returns, mean)
stdevs <- sqrt(252)*sapply(returns, sd)
points(stdevs, mean_rets, col="green", lwd=6)
text(stdevs, mean_rets, labels=names(returns), pos=2, cex=0.8)
# Add point at risk-free rate and draw Capital Market Line
points(x=0, y=riskf, col="blue", lwd=6)
text(0, riskf, labels="risk-free", pos=4, cex=0.8)
abline(a=riskf, b=sharpem, col="blue", lwd=2)
range_s <- par("usr")
text(max(ret_sd[, "stddev"])/3, 0.75*max(ret_sd[, "returns"]),
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(sharpem*(range_s[2]-range_s[1])/
             (range_s[4]-range_s[3])*
             hei_ght/wid_th)/(0.25*pi))
# Plot portfolios in x11() window
x11(wid_th <- 6, hei_ght <- 5)
# Calculate cumulative returns of VTI and IEF
optim_rets <- lapply(returns,
  function(returns) exp(cumsum(returns)))
optim_rets <- rutils::do_call(cbind, optim_rets)
# Calculate market portfolio returns
optim_rets <- cbind(exp(cumsum(returns %*%
    c(weightv[indeks], 1-weightv[indeks]))),
  optim_rets)
colnames(optim_rets)[1] <- "market"
# Plot market portfolio with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue", "green")
chart_Series(optim_rets, theme=plot_theme,
       name="Market portfolio for stocks and bonds")
legend("top", legend=colnames(optim_rets),
 cex=0.8, inset=0.1, bg="white", lty=1,
 lwd=6, col=plot_theme$col$line.col, bty="n")
x11(width=6, height=4)
par(mar=c(3, 2, 1, 0), oma=c(0, 0, 0, 0))
# VTI percentage returns
returns <- rutils::diffit(log(quantmod::Cl(rutils::etfenv$VTI)))
confl <- 0.1
va_r <- quantile(returns, confl)
c_var <- mean(returns[returns < va_r])
# Or
sort_ed <- sort(as.numeric(returns))
indeks <- round(confl*NROW(returns))
va_r <- sort_ed[indeks]
c_var <- mean(sort_ed[1:indeks])
# Plot histogram of VTI returns
min_var <- (-0.05)
histo_gram <- hist(returns, col="lightgrey",
  xlab="returns", breaks=100, xlim=c(min_var, 0.01),
  ylab="frequency", freq=FALSE, main="VTI Returns Histogram")
# Plot density of losses
densv <- density(returns, adjust=1.5)
lines(densv, lwd=3, col="blue")
# Add line for VaR
abline(v=va_r, col="red", lwd=3)
y_max <- max(densv$y)
text(x=va_r, y=2*y_max/3, labels="VaR", lwd=2, pos=2)
# Add shading for CVaR
rangev <- (densv$x < va_r) & (densv$x > min_var)
polygon(
  c(min_var, densv$x[rangev], va_r),
  c(0, densv$y[rangev], 0),
  col=rgb(1, 0, 0,0.5), border=NA)
text(x=1.5*va_r, y=y_max/7, labels="CVaR", lwd=2, pos=2)
library(rutils)  # Load rutils
library(Rglpk)
# Vector of symbol names and returns
symbols <- c("VTI", "IEF", "DBC")
nweights <- NROW(symbols)
returns <- rutils::etfenv$returns[((NROW(returns)-6):NROW(returns)), symbols]
mean_rets <- colMeans(returns)
confl <- 0.05
r_min <- 0 ; w_min <- 0 ; w_max <- 1
weightvum <- 1
ncols <- NCOL(returns) # number of assets
nrows <- NROW(returns) # number of rows
# Creat objective vector
obj_vector <- c(numeric(ncols), rep(-1/(confl*nrows), nrows), -1)
# Specify weight constraints
constraint_s <- rbind(
  cbind(rbind(1, mean_rets),
  matrix(data=0, nrow=2, ncol=(nrows+1))),
  cbind(coredata(returns), diag(nrows), 1))
rh_s <- c(weightvum, r_min, rep(0, nrows))
direction_s <- c("==", ">=", rep(">=", nrows))
# Specify weight bounds
bound_s <- list(
  lower=list(ind=1:ncols, val=rep(w_min, ncols)),
  upper=list(ind=1:ncols, val=rep(w_max, ncols)))
# Perform optimization
optimd <- Rglpk_solve_LP(obj=obj_vector, mat=constraint_s, dir=direction_s, rhs=rh_s, types=rep("C", NROW(obj_vector)), max=T, bounds=bound_s)
optimd$solution
constraint_s %*% optimd$solution
obj_vector %*% optimd$solution
as.numeric(optimd$solution[1:ncols])
# Calculate daily percentage returns
symbols <- c("VTI", "IEF", "DBC")
returns <- rutils::etfenv$returns[, symbols]
# Create initial vector of portfolio weights
weightv <- rep(1, NROW(symbols))
names(weightv) <- symbols
# Objective equal to minus Sharpe ratio
object <- function(weightv, returns) {
  retsp <- returns %*% weightv
  if (sd(retsp) == 0)
    return(0)
  else
    return(-mean(retsp)/sd(retsp))
}  # end object
# Objective for equal weight portfolio
object(weightv, returns=returns)
optimd <- unlist(optimize(
  f=function(weight)
    object(c(1, 1, weight), returns=returns),
  interval=c(-4, 1)))
# Vectorize objective function with respect to third weight
vec_object <- function(weights) sapply(weights,
  function(weight) object(c(1, 1, weight),
    returns=returns))
# Or
vec_object <- Vectorize(FUN=function(weight)
    object(c(1, 1, weight), returns=returns),
  vectorize.args="weight")  # end Vectorize
vec_object(1)
vec_object(1:3)
x11(width=6, height=5)
par(oma=c(1, 1, 1, 1), mgp=c(2, 1, 0), mar=c(3, 1, 1, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot objective function with respect to third weight
curve(expr=vec_object,
      type="l", xlim=c(-4.0, 1.0),
      xlab=paste("weight of", names(weightv[3])),
      ylab="", lwd=2)
title(main="Objective Function", line=-1)  # Add title
points(x=optimd[1], y=optimd[2], col="green", lwd=6)
text(x=optimd[1], y=optimd[2],
     labels="minimum objective", pos=4, cex=0.8)
#below is simplified code for plotting objective function
# Create vector of DBC weights
weightv <- seq(from=-4, to=1, by=0.1)
obj_val <- sapply(weightv,
  function(weight) object(c(1, 1, weight)))
plot(x=weightv, y=obj_val, t="l",
      xlab="weight of DBC", ylab="", lwd=2)
title(main="Objective Function", line=-1)  # Add title
points(x=optimd[1], y=optimd[2], col="green", lwd=6)
text(x=optimd[1], y=optimd[2],
     labels="minimum objective", pos=4, cex=0.8)
# Vectorize function with respect to all weights
vec_object <- Vectorize(
  FUN=function(w1, w2, w3) object(c(w1, w2, w3)),
  vectorize.args=c("w2", "w3"))  # end Vectorize
# Calculate objective on 2-d (w2 x w3) parameter grid
w2 <- seq(-3, 7, length=50)
w3 <- seq(-5, 5, length=50)
grid_object <- outer(w2, w3, FUN=vec_object, w1=1)
rownames(grid_object) <- round(w2, 2)
colnames(grid_object) <- round(w3, 2)
# Perspective plot of objective function
persp(w2, w3, -grid_object,
theta=45, phi=30, shade=0.5,
col=rainbow(50), border="green",
main="objective function")
# Interactive perspective plot of objective function
library(rgl)
rgl::persp3d(z=-grid_object, zlab="objective",
  col="green", main="objective function")
rgl::persp3d(
  x=function(w2, w3) {-vec_object(w1=1, w2, w3)},
  xlim=c(-3, 7), ylim=c(-5, 5),
  col="green", axes=FALSE)
# Vector of initial portfolio weights equal to 1
weightv <- rep(1, nweights)
names(weightv) <- symbols
# Objective function equal to standard deviation of returns
object <- function(weightv) {
  retsp <- returns %*% weightv
  sd(retsp)/sum(weightv)
}  # end object
# object() for equal weight portfolio
object(weightv)
object(2*weightv)
# Perform portfolio optimization
optimd <- optim(par=weightv,
          fn=object,
          method="L-BFGS-B",
          upper=rep(10, nweights),
          lower=rep(-10, nweights))
# Rescale the optimal weights
weightv <- optimd$par/sum(optimd$par)
# Minimum variance portfolio returns
optim_rets <- xts(x=returns %*% weightv,
            order.by=index(returns))
chart_Series(x=exp(cumsum(optim_rets)), name="minvar portfolio")
# Add green point for minimum variance portfolio
optim_sd <- 100*sd(optim_rets)
optim_ret <- 100*mean(optim_rets)
points(x=optim_sd, y=optim_ret, col="green", lwd=6)
text(x=optim_sd, y=optim_ret, labels="minvar", pos=2, cex=0.8)
# Objective function equal to minus Sharpe ratio
riskf <- 0.03
object <- function(weightv) {
  retsp <- 100*etfenv$returns[, names(weightv)] %*% weightv / sum(weightv)
  -mean(retsp-riskf)/sd(retsp)
}  # end object
# Perform portfolio optimization
optimd <- optim(par=weightv,
             fn=object,
             method="L-BFGS-B",
             upper=rep(10, nweights),
             lower=rep(-10, nweights))
# Maximum Sharpe ratio portfolio returns
weightv <- optimd$par/sum(optimd$par)
optim_rets <- xts(x=returns %*% weightv,
            order.by=index(returns))
chart_Series(x=exp(cumsum(optim_rets)), name="maxSR portfolio")
optim_sd <- 100*sd(optim_rets)
optim_ret <- 100*mean(optim_rets)
points(x=optim_sd, y=optim_ret,
 col="blue", lwd=3)
text(x=optim_sd, y=optim_ret,
     labels="maxSR", pos=2, cex=0.8)
sharpem <- (optim_ret-riskf)/optim_sd
# Plot individual assets
mean_rets <- 100*sapply(returns, mean)
stdevs <- 100*sapply(returns, sd)
points(stdevs, mean_rets, col="green", lwd=3)
text(stdevs, mean_rets, labels=names(mean_rets), pos=2, cex=0.8)
# Add point at risk-free rate and draw Capital Market Line
points(x=0, y=riskf)
text(0, riskf, labels="risk-free", pos=4, cex=0.8)
abline(a=riskf, b=sharpem, col="blue")
range_s <- par("usr")
text(optim_sd/3, (optim_ret+riskf)/2.5,
     labels="Capital Market Line", cex=0.8, , pos=3,
     srt=45*atan(sharpem*(range_s[2]-range_s[1])/
             (range_s[4]-range_s[3])*
             hei_ght/wid_th)/(0.25*pi))
# Optimization to find weights with maximum Sharpe ratio
optimd <- optim(par=weightv,
             fn=object,
             returns=returns,
             method="L-BFGS-B",
             upper=c(1.1, 10, 10),
             lower=c(0.9, -10, -10))
# Optimal parameters
optimd$par
optimd$par <- optimd$par/sum(optimd$par)
# Optimal Sharpe ratio
-object(optimd$par)
x11(width=6, height=5)
par(oma=c(1, 1, 1, 0), mgp=c(2, 1, 0), mar=c(2, 1, 2, 1), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# Plot in two vertical panels
layout(matrix(c(1,2), 2),
 widths=c(1,1), heights=c(1,3))
# barplot of optimal portfolio weights
barplot(optimd$par, col=c("red", "green", "blue"),
  main="Optimized portfolio weights")
# Calculate cumulative returns of VTI, IEF, DBC
cum_rets <- lapply(returns,
  function(returns) exp(cumsum(returns)))
cum_rets <- rutils::do_call(cbind, cum_rets)
# Calculate optimal portfolio returns with VTI, IEF, DBC
optim_rets <- cbind(
  exp(cumsum(returns %*% optimd$par)),
  cum_rets)
colnames(optim_rets)[1] <- "optim_rets"
# Plot optimal returns with VTI, IEF, DBC
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("black", "red", "green", "blue")
chart_Series(optim_rets, theme=plot_theme,
       name="Optimized portfolio performance")
legend("top", legend=colnames(optim_rets), cex=0.8,
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")
# Or plot non-compounded (simple) cumulative returns
PerformanceAnalytics::chart.CumReturns(
  cbind(returns %*% optimd$par, returns),
  lwd=2, ylab="", legend.loc="topleft", main="")
riskf <- 0.03
returns <- c(asset1=0.05, asset2=0.06)
stdevs <- c(asset1=0.4, asset2=0.5)
cor_rel <- 0.6
covmat <- matrix(c(1, cor_rel, cor_rel, 1), nc=2)
covmat <- t(t(stdevs*covmat)*stdevs)
library(quadprog)
# Minimum variance weights without constraints
optimd <- solve.QP(Dmat=2*covmat,
            dvec=rep(0, 2),
            Amat=matrix(0, nr=2, nc=1),
            bvec=0)
# Minimum variance weights sum equal to 1
optimd <- solve.QP(Dmat=2*covmat,
            dvec=rep(0, 2),
            Amat=matrix(1, nr=2, nc=1),
            bvec=1)
# Optimal value of objective function
t(optimd$solution) %*% covmat %*% optimd$solution
Perform simple optimization for reference
# Objective function for simple optimization
object <- function(x) {
  x <- c(x, 1-x)
  t(x) %*% covmat %*% x
}  # end object
unlist(optimize(f=object, interval=c(-1, 2)))
# Calculate daily percentage returns
symbols <- c("VTI", "IEF", "DBC")
returns <- rutils::etfenv$returns[, symbols]
# Calculate the covariance matrix
covmat <- cov(returns)
# Minimum variance weights, with sum equal to 1
optimd <- quadprog::solve.QP(Dmat=2*covmat,
            dvec=numeric(3),
            Amat=matrix(1, nr=3, nc=1),
            bvec=1)
# Minimum variance, maximum returns
optimd <- quadprog::solve.QP(Dmat=2*covmat,
            dvec=apply(0.1*returns, 2, mean),
            Amat=matrix(1, nr=3, nc=1),
            bvec=1)
# Minimum variance positive weights, sum equal to 1
a_mat <- cbind(matrix(1, nr=3, nc=1),
       diag(3), -diag(3))
b_vec <- c(1, rep(0, 3), rep(-1, 3))
optimd <- quadprog::solve.QP(Dmat=2*covmat,
            dvec=numeric(3),
            Amat=a_mat,
            bvec=b_vec,
            meq=1)
# Rastrigin function with vector argument for optimization
rastri_gin <- function(vectorv, pa_ram=25){
  sum(vectorv^2 - pa_ram*cos(vectorv))
}  # end rastri_gin
vectorv <- c(pi/6, pi/6)
rastri_gin(vectorv=vectorv)
library(DEoptim)
# Optimize rastri_gin using DEoptim
optimd <-  DEoptim(rastri_gin,
  upper=c(6, 6), lower=c(-6, -6),
  DEoptim.control(trace=FALSE, itermax=50))
# Optimal parameters and value
optimd$optim$bestmem
rastri_gin(optimd$optim$bestmem)
summary(optimd)
plot(optimd)
# Calculate daily percentage returns
returns <- rutils::etfenv$returns[, symbols]
# Objective equal to minus Sharpe ratio
object <- function(weightv, returns) {
  retsp <- returns %*% weightv
  if (sd(retsp) == 0)
    return(0)
  else
    return(-mean(retsp)/sd(retsp))
}  # end object
# Perform optimization using DEoptim
optimd <- DEoptim::DEoptim(fn=object,
  upper=rep(10, NCOL(returns)),
  lower=rep(-10, NCOL(returns)),
  returns=returns,
  control=list(trace=FALSE, itermax=100, parallelType=1))
weightv <- optimd$optim$bestmem/sum(abs(optimd$optim$bestmem))
names(weightv) <- colnames(returns)
# Objective with shrinkage penalty
object <- function(weightv, returns, lambda, alpha) {
  retsp <- returns %*% weightv
  if (sd(retsp) == 0)
    return(0)
  else {
    penal_ty <- lambda*((1-alpha)*sum(weightv^2) +
alpha*sum(abs(weightv)))
    return(-mean(retsp)/sd(retsp) + penal_ty)
  }
}  # end object
# Objective for equal weight portfolio
weightv <- rep(1, NROW(symbols))
names(weightv) <- symbols
lambda <- 0.5 ; alpha <- 0.5
object(weightv, returns=returns,
  lambda=lambda, alpha=alpha)
# Perform optimization using DEoptim
optimd <- DEoptim::DEoptim(fn=object,
  upper=rep(10, NCOL(returns)),
  lower=rep(-10, NCOL(returns)),
  returns=returns,
  lambda=lambda,
  alpha=alpha,
  control=list(trace=FALSE, itermax=100, parallelType=1))
weightv <- optimd$optim$bestmem/sum(abs(optimd$optim$bestmem))
names(weightv) <- colnames(returns)
load(file="/Users/jerzy/Develop/lecture_slides/data/zoo_data.RData")
stxts <- as.ts(zoo_stx)
class(stxts)
tail(stxts[, 1:4])
library(xts)
st_ox <- as.xts(zoo_stx)
class(st_ox)
tail(st_ox[, 1:4])
library(PortfolioAnalytics)  # load package "PortfolioAnalytics"
# get documentation for package "PortfolioAnalytics"
packageDescription("PortfolioAnalytics")  # get short description
help(package="PortfolioAnalytics")  # load help page
data(package="PortfolioAnalytics")  # list all datasets in "PortfolioAnalytics"
ls("package:PortfolioAnalytics")  # list all objects in "PortfolioAnalytics"
detach("package:PortfolioAnalytics")  # remove PortfolioAnalytics from search path
library(PortfolioAnalytics)
# Use ETF returns from package rutils
library(rutils)
portf_names <- c("VTI", "IEF", "DBC", "XLF",
  "VNQ", "XLP", "XLV", "XLU", "XLB", "XLE")
# Initial portfolio to equal weights
portf_init <- rep(1/NROW(portf_names), NROW(portf_names))
# named vector
names(portf_init) <- portf_names
# Create portfolio object
portf_init <- portfolio.spec(assets=portf_init)
library(PortfolioAnalytics)
# Add constraints
portf_maxSR <- add.constraint(
  portfolio=portf_init,  # Initial portfolio
  type="weightvum",  # Constraint sum weights
  min_sum=0.9, max_sum=1.1)
# Add constraints
portf_maxSR <- add.constraint(
  portfolio=portf_maxSR,
  type="long_only")  # box constraint min=0, max=1
# Add objectives
portf_maxSR <- add.objective(
  portfolio=portf_maxSR,
  type="return",  # Maximize mean return
  name="mean")
# Add objectives
portf_maxSR <- add.objective(
  portfolio=portf_maxSR,
  type="risk",  # Minimize StdDev
  name="StdDev")
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
library(PortfolioAnalytics)
# Perform optimization of weights
maxSR_DEOpt <- optimize.portfolio(
  R=etfenv$returns[, portf_names],  # Specify returns
  portfolio=portf_maxSR,  # Specify portfolio
  optimize_method="DEoptim", # Use DEoptim
  maxSR=TRUE,  # Maximize Sharpe
  trace=TRUE, traceDE=0)
# Plot optimization
chart.RiskReward(maxSR_DEOpt,
  risk.col="stddev",
  return.col="mean")
options(width=50)
library(PortfolioAnalytics)
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
maxSR_DEOpt$weights
maxSR_DEOpt$objective_measures$mean[1]
maxSR_DEOpt$objective_measures$StdDev[[1]]
library(PortfolioAnalytics)
# Plot optimization
chart.RiskReward(maxSR_DEOpt,
  risk.col="StdDev",
  return.col="mean")
# Plot risk/ret points in portfolio scatterplot
risk_ret_points <- function(rets=etfenv$returns,
  risk=c("sd", "ETL"), symbols=c("VTI", "IEF")) {
  risk <- match.arg(risk)  # Match to arg list
  if (risk=="ETL") {
    stopifnot(
"package:PerformanceAnalytics" %in% search() ||
require("PerformanceAnalytics", quietly=TRUE))
  }  # end if
  risk <- match.fun(risk)  # Match to function
  risk_ret <- t(sapply(rets[, symbols],
     function(xtes)
 c(ret=mean(xtes), risk=abs(risk(xtes)))))
  points(x=risk_ret[, "risk"], y=risk_ret[, "ret"],
   col="red", lwd=3)
  text(x=risk_ret[, "risk"], y=risk_ret[, "ret"],
 labels=rownames(risk_ret), col="red",
 lwd=2, pos=4)
}  # end risk_ret_points
risk_ret_points()
library(PortfolioAnalytics)
plot_portf <- function(portfolio,
      rets_data=etfenv$returns) {
  weightv <- portfolio$weights
  portf_names <- names(weightv)
  # Calculate xts of portfolio
  portf_max <- xts(
    rets_data[, portf_names] %*% weightv,
    order.by=index(rets_data))
  colnames(portf_max) <-
    deparse(substitute(portfolio))
  graph_params <- par(oma=c(1, 0, 1, 0),
    mgp=c(2, 1, 0), mar=c(2, 1, 2, 1),
    cex.lab=0.8, cex.axis=1.0,
    cex.main=0.8, cex.sub=0.5)
  layout(matrix(c(1,2), 2),
    widths=c(1,1), heights=c(1,3))
  barplot(weightv, names.arg=portf_names,
    las=3, ylab="", xlab="Symbol", main="")
  title(main=paste("Loadings",
          colnames(portf_max)), line=-1)
  chart.CumReturns(
    cbind(portf_max, rets_data[, c("IEF", "VTI")]),
    lwd=2, ylab="", legend.loc="topleft", main="")
  title(main=paste0(colnames(portf_max),
              ", IEF, VTI"), line=-1)
  par(graph_params)  # restore original parameters
  invisible(portf_max)
}  # end plot_portf
maxSR_DEOpt_xts <- plot_portf(portfolio=maxSR_DEOpt)
library(PortfolioAnalytics)
# Add leverage constraint abs(weightvum)
portf_maxSRN <- add.constraint(
  portfolio=portf_init, type="leverage",
  min_sum=0.9, max_sum=1.1)
# Add box constraint long/short
portf_maxSRN <- add.constraint(
  portfolio=portf_maxSRN,
  type="box", min=-0.2, max=0.2)
# Add objectives
portf_maxSRN <- add.objective(
  portfolio=portf_maxSRN,
  type="return",  # Maximize mean return
  name="mean")
# Add objectives
portf_maxSRN <- add.objective(
  portfolio=portf_maxSRN,
  type="risk",  # Minimize StdDev
  name="StdDev")
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
library(PortfolioAnalytics)
# Perform optimization of weights
maxSRN_DEOpt <- optimize.portfolio(
  R=etfenv$returns[, portf_names],  # Specify returns
  portfolio=portf_maxSRN,  # Specify portfolio
  optimize_method="DEoptim", # Use DEoptim
  maxSR=TRUE,  # Maximize Sharpe
  trace=TRUE, traceDE=0)
# Plot optimization
chart.RiskReward(maxSRN_DEOpt,
  risk.col="StdDev",
  return.col="mean",
  xlim=c(
    maxSR_DEOpt$objective_measures$StdDev[[1]]-0.001,
    0.016))
  points(x=maxSR_DEOpt$objective_measures$StdDev[[1]],
   y=maxSR_DEOpt$objective_measures$mean[1],
   col="green", lwd=3)
  text(x=maxSR_DEOpt$objective_measures$StdDev[[1]],
   y=maxSR_DEOpt$objective_measures$mean[1],
 labels="maxSR", col="green",
 lwd=2, pos=4)
# Plot risk/ret points in portfolio scatterplot
risk_ret_points()
library(PortfolioAnalytics)
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
maxSRN_DEOpt$weights
maxSRN_DEOpt$objective_measures$mean[1]
maxSRN_DEOpt$objective_measures$StdDev[[1]]
library(PortfolioAnalytics)
maxSRN_DEOpt_xts <-
  plot_portf(portfolio=maxSRN_DEOpt)
library(PerformanceAnalytics)
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
chart.CumReturns(
  cbind(maxSR_DEOpt_xts, maxSRN_DEOpt_xts),
  lwd=2, ylab="",
  legend.loc="topleft", main="")
options(width=50)
library(PerformanceAnalytics)
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
rbind(maxSR_DEOpt$weights, maxSRN_DEOpt$weights)
c(maxSR_DEOpt$objective_measures$mean,
maxSRN_DEOpt$objective_measures$mean)
c(maxSR_DEOpt$objective_measures$StdDev[[1]],
maxSRN_DEOpt$objective_measures$StdDev[[1]])
library(PortfolioAnalytics)
# Add constraints
portf_maxSTARR <- add.constraint(
  portfolio=portf_init,  # Initial portfolio
  type="weightvum",  # Constraint sum weights
  min_sum=0.9, max_sum=1.1)
# Add constraints
portf_maxSTARR <- add.constraint(
  portfolio=portf_maxSTARR,
  type="long_only")  # box constraint min=0, max=1
# Add objectives
portf_maxSTARR <- add.objective(
  portfolio=portf_maxSTARR,
  type="return",  # Maximize mean return
  name="mean")
# Add objectives
portf_maxSTARR <- add.objective(
  portfolio=portf_maxSTARR,
  type="risk",  # Minimize Expected Shortfall
  name="ES")
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
library(PortfolioAnalytics)
# Perform optimization of weights
maxSTARR_DEOpt <- optimize.portfolio(
  R=etfenv$returns[, portf_names],  # Specify returns
  portfolio=portf_maxSTARR,  # Specify portfolio
  optimize_method="DEoptim", # Use DEoptim
  maxSTARR=TRUE,  # Maximize STARR
  trace=TRUE, traceDE=0)
# Plot optimization
chart.RiskReward(maxSTARR_DEOpt,
  risk.col="ES",
  return.col="mean")
# Plot risk/ret points in portfolio scatterplot
risk_ret_points(risk="ETL")
options(width=50)
library(PortfolioAnalytics)
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
maxSTARR_DEOpt$weights
maxSTARR_DEOpt$objective_measures$mean[1]
maxSTARR_DEOpt$objective_measures$ES[[1]]
library(PortfolioAnalytics)
maxSTARR_DEOpt_xts <-
  plot_portf(portfolio=maxSTARR_DEOpt)
library(PerformanceAnalytics)
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
chart.CumReturns(
  cbind(maxSR_DEOpt_xts, maxSTARR_DEOpt_xts),
  lwd=2, ylab="",
  legend.loc="topleft", main="")
options(width=50)
library(PerformanceAnalytics)
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
rbind(maxSR_DEOpt$weights, maxSTARR_DEOpt$weights)
c(maxSR_DEOpt$objective_measures$mean,
maxSTARR_DEOpt$objective_measures$mean)
c(maxSR_DEOpt$objective_measures$StdDev[[1]],
maxSTARR_DEOpt$objective_measures$ES[[1]])
library(PortfolioAnalytics)
# Plot the efficient frontier
chart.EfficientFrontier(maxSR_DEOpt,
          match.col="StdDev",
          n.portfolios=15, type="l")
points(x=maxSRN_DEOpt$objective_measures$StdDev[[1]],
   y=maxSRN_DEOpt$objective_measures$mean[1],
   col="green", lwd=3)
text(x=maxSRN_DEOpt$objective_measures$StdDev[[1]],
   y=maxSRN_DEOpt$objective_measures$mean[1],
 labels="maxSRN", col="green",
 lwd=2, pos=4)
library(PortfolioAnalytics)
# Add constraints
portf_minES <- add.constraint(
  portfolio=portf_init,  # Initial portfolio
  type="weightvum",  # Constraint sum weights
  min_sum=0.9, max_sum=1.1)
# Add constraints
portf_minES <- add.constraint(
  portfolio=portf_minES,
  type="long_only")  # box constraint min=0, max=1
# Add objectives
portf_minES <- add.objective(
  portfolio=portf_minES,
  type="risk",  # Minimize ES
  name="ES")
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
library(PortfolioAnalytics)
# Perform optimization of weights
minESROI <- optimize.portfolio(
  R=etfenv$returns[, portf_names],  # Specify returns
  portfolio=portf_minES,  # Specify portfolio
  optimize_method="ROI", # Use ROI
  trace=TRUE, traceDE=0)
# Plot optimization
chart.RiskReward(maxSTARR_DEOpt,
  risk.col="ES",
  return.col="mean")
  points(x=minESROI$objective_measures$ES[[1]],
   y=mean(minESROI_xts),
   col="green", lwd=3)
  text(x=minESROI$objective_measures$ES[[1]],
   y=mean(minESROI_xts),
 labels="minES", col="green",
 lwd=2, pos=4)
# Plot risk/ret points in portfolio scatterplot
risk_ret_points(risk="ETL")
options(width=50)
library(PortfolioAnalytics)
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
minESROI$weights
minESROI$objective_measures$ES[[1]]
library(PortfolioAnalytics)
minESROI_xts <-
  plot_portf(portfolio=minESROI)
library(PerformanceAnalytics)
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
chart.CumReturns(
  cbind(maxSR_DEOpt_xts, minESROI_xts),
  lwd=2, ylab="",
  legend.loc="topleft", main="")
options(width=50)
library(PerformanceAnalytics)
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
rbind(maxSR_DEOpt$weights, minESROI$weights)
c(maxSR_DEOpt$objective_measures$mean,
minESROI$objective_measures$mean)
c(maxSR_DEOpt$objective_measures$StdDev[[1]],
minESROI$objective_measures$ES[[1]])
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
library(PortfolioAnalytics)
options(width=50)
# Perform optimization of weights
maxSR_DEOpt <- optimize.portfolio(
  R=etfenv$returns["/2011", portf_names],
  portfolio=portf_maxSR,  # Specify portfolio
  optimize_method="DEoptim", # Use DEoptim
  maxSR=TRUE,  # Maximize Sharpe
  trace=TRUE, traceDE=0)
weights1h <- maxSR_DEOpt$weights
# Plot optimization
maxSR_DEOpt_xts <-
  plot_portf(portfolio=maxSR_DEOpt)
load(file="/Users/jerzy/Develop/lecture_slides/data/portf_optim.RData")
library(PortfolioAnalytics)
options(width=50)
# Perform optimization of weights
maxSR_DEOpt <- optimize.portfolio(
  R=etfenv$returns["2011/", portf_names],
  portfolio=portf_maxSR,  # Specify portfolio
  optimize_method="DEoptim", # Use DEoptim
  maxSR=TRUE,  # Maximize Sharpe
  trace=TRUE, traceDE=0)
weights2h <- maxSR_DEOpt$weights
# Plot optimization
maxSR_DEOpt_xts <-
  plot_portf(portfolio=maxSR_DEOpt)
options(width=50)
weights1h
weights2h
weights1h - weights2h
par(oma=c(1,0,1,0), mgp=c(2,1,0), mar=c(2,1,2,1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
par(mfrow=c(2,1))  # Set plot panels
barplot(weights1h,
  names.arg=names(weights1h),
  las=3, ylab="", xlab="",
  main="Portfolio Weights First Half")
barplot(weights2h,
  names.arg=names(weights2h),
  las=3, ylab="", xlab="",
  main="Portfolio Weights Second Half")
# Calculate random default probabilities
set.seed(1121)
nassets <- 100
def_probs <- runif(nassets, max=0.2)
mean(def_probs)
# Simulate number of defaults
unifunc <- runif(nassets)
sum(unifunc < def_probs)
# Simulate average number of defaults using for() loop (inefficient way)
nsimu <- 1000
set.seed(1121)
de_faults <- numeric(nsimu)
for (i in 1:nsimu) {  # Perform loop
  unifunc <- runif(nassets)
  de_faults[i] <- sum(unifunc < def_probs)
}  # end for
# Calculate average number of defaults
mean(de_faults)
# Simulate using vectorized functions  (efficient way)
set.seed(1121)
unifunc <- matrix(runif(nsimu*nassets), ncol=nsimu)
de_faults <- colSums(unifunc < def_probs)
mean(de_faults)
# Plot the distribution of defaults
x11(width=6, height=5)
plot(density(de_faults), main="Distribution of Defaults",
     xlab="number of defaults", ylab="frequqncy")
abline(v=mean(de_faults), lwd=3, col="red")
# Calculate default thresholds and asset values
def_thresh <- qnorm(def_probs)
assets <- qnorm(unifunc)
# Simulate defaults
de_faults <- colSums(assets < def_thresh)
mean(de_faults)
# Plot Standard Normal distribution
x11(width=6, height=5)
xlim <- 4; def_thresh <- qnorm(0.025)
curve(expr=dnorm(x), type="l", xlim=c(-xlim, xlim),
xlab="asset value", ylab="", lwd=3,
col="blue", main="Distribution of Asset Values")
abline(v=def_thresh, col="red", lwd=3)
text(x=def_thresh-0.1, y=0.15, labels="default threshold",
 lwd=2, srt=90, pos=3)
# Plot polygon area
x_var <- seq(-xlim, xlim, length=100)
y_var <- dnorm(x_var)
are_a <- ((x_var >= (-xlim)) & (x_var <= def_thresh))
polygon(c(xlim, x_var[are_a], def_thresh),
  c(-1, y_var[are_a], -1), col="red")
# Define correlation parameters
rho <- 0.2
rho_sqrt <- sqrt(rho) ; rho_sqrtm <- sqrt(1-rho)
nassets <- 5 ; nsimu <- 10000
# Calculate vector of systematic and idiosyncratic factors
sysv <- rnorm(nsimu)
idio_syncratic <- rnorm(nsimu*nassets)
# Simulate asset values using vectorized functions (efficient way)
assets <- rho_sqrt*sysv + rho_sqrtm*idio_syncratic
dim(assets) <- c(nsimu, nassets)
# Asset values are standard normally distributed
apply(assets, MARGIN=2, function(x) c(mean=mean(x), sd=sd(x)))
# Calculate correlations between asset values
cor(assets)
# Simulate asset values using for() loop (inefficient way)
# Allocate matrix of assets
assets <- matrix(nr=nsimu, nc=nassets)
# Simulate asset values using for() loop
for (i in 1:nsimu) {  # Perform loop
  assets[i, ] <- rho_sqrt*sysv[i] + rho_sqrtm*rnorm(nassets)
}  # end for
cor(assets)
# benchmark the speed of the two methods
library(microbenchmark)
summary(microbenchmark(
  for_loop={for (i in 1:nsimu) {
    rho_sqrt*sysv[i] + rho_sqrtm*rnorm(nassets)}},
  vector_ized={rho_sqrt*sysv + rho_sqrtm*rnorm(nsimu*nassets)},
  times=10))[, c(1, 4, 5)]
# Calculate random default probabilities
nassets <- 5
def_probs <- runif(nassets, max=0.2)
mean(def_probs)
# Calculate default thresholds
def_thresh <- qnorm(def_probs)
# Calculate number of defaults using vectorized functions (efficient way)
# Calculate vector of number of defaults
rowMeans(t(assets) < def_thresh)
def_probs
# Calculate number of defaults using for() loop (inefficient way)
# allocate matrix of de_faults
de_faults <- matrix(nr=nsimu, nc=nassets)
# Simulate asset values using for() loop
for (i in 1:nsimu) {  # Perform loop
  de_faults[i, ] <- (assets[i, ] < def_thresh)
}  # end for
colSums(de_faults) / nsimu
def_probs
# Calculate correlations between defaults
cor(de_faults)
# Define default probabilities
nassets <- 2
def_prob <- 0.2
def_thresh <- qnorm(def_prob)
# Define correlation parameters
rho <- 0.2
rho_sqrt <- sqrt(rho) ; rho_sqrtm <- sqrt(1-rho)
# Calculate vector of systematic factors
nsimu <- 1000
sysv <- rnorm(nsimu)
# Simulate asset values using vectorized functions
assets <- rho_sqrt*sysv + rho_sqrtm*rnorm(nsimu*nassets)
dim(assets) <- c(nsimu, nassets)
# Calculate number of defaults using vectorized functions
de_faults <- t(t(assets) < def_thresh)
# Calculate correlations between defaults
cor(de_faults)
# Calculate average number of defaults and compare to def_prob
colSums(de_faults) / nsimu
def_prob
# Define cumulative default probability function
def_cumdistr <- function(x, def_thresh=(-2), rho=0.2)
  pnorm((sqrt(1-rho)*qnorm(x) - def_thresh)/sqrt(rho))
def_cumdistr(x=0.2, def_thresh=qnorm(def_prob), rho=rho)
# Plot cumulative default probability function
def_prob <- 0.4; def_thresh <- qnorm(def_prob)
curve(expr=def_cumdistr(x, def_thresh=def_thresh, rho=0.05),
xlim=c(0, 0.999), lwd=3,
xlab="percent default", ylab="probability",
col="green", main="Cumulative Default Probabilities")
# Plot default distribution with higher correlation
curve(expr=def_cumdistr(x, def_thresh=def_thresh, rho=0.2),
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
def_distr <- function(x, def_thresh=(-2), rho=0.2)
  sqrt((1-rho)/rho)*exp(-(sqrt(1-rho)*qnorm(x) -
  def_thresh)^2/(2*rho) + qnorm(x)^2/2)
# Define parameters
rho <- 0.2 ; rho_sqrt <- sqrt(rho) ; rho_sqrtm <- sqrt(1-rho)
def_prob <- 0.3; def_thresh <- qnorm(def_prob)
def_distr(0.03, def_thresh=def_thresh, rho=rho)
# Plot probability distribution of defaults
curve(expr=def_distr(x, def_thresh=def_thresh, rho=0.1),
xlim=c(0, 1.0), lwd=3,
xlab="percentage of defaults", ylab="density",
col="green", main="Distribution of Defaults")
# Plot default distribution with higher correlation
curve(expr=def_distr(x, def_thresh=def_thresh, rho=0.3),
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
curve(expr=def_distr(x, def_thresh=def_thresh, rho=0.01),
xlab="default percentage", ylab="", lwd=2,
col="green", main="Distribution of Defaults")
# Plot default distribution with high correlation
curve(expr=def_distr(x, def_thresh=def_thresh, rho=0.99),
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
func <- function(x) {1/((x+1)*sqrt(x))}
integrate(func, lower=0, upper=10)
integrate(func, lower=0, upper=Inf)
# Integrate function with parameter lambda
func <- function(x, lambda=1) {
  exp(-x*lambda)
}  # end func
integrate(func, lower=0, upper=Inf)
integrate(func, lower=0, upper=Inf, lambda=2)
# Cumulative probability over normal distribution
pnorm(-2)
integrate(dnorm, low=2, up=Inf)
str(dnorm)
pnorm(-1)
integrate(dnorm, low=2, up=Inf, mean=1)
# Expected value over normal distribution
integrate(function(x) x*dnorm(x), low=2, up=Inf)
# Vasicek model parameters
rho <- 0.1; lgd <- 0.4
def_prob <- 0.05; def_thresh <- qnorm(def_prob)
# Define Vasicek loss distribution function
loss_distr <- function(x, def_thresh=(-2), rho=0.2, lgd=0.4)
  sqrt((1-rho)/rho)*exp(-(sqrt(1-rho)*qnorm(x/lgd) - def_thresh)^2/(2*rho) + qnorm(x/lgd)^2/2)/lgd
integrate(loss_distr, low=0, up=lgd, def_thresh=(-2), rho=rho, lgd=lgd)
# Plot probability distribution of losses
x11(width=6, height=5)
curve(expr=loss_distr(x, def_thresh=def_thresh, rho=rho),
type="l", xlim=c(0, 0.06),
xlab="loss percentage", ylab="density", lwd=3,
col="blue", main="Portfolio Loss Density")
# Add line for expected loss
abline(v=lgd*def_prob, col="red", lwd=3)
text(x=lgd*def_prob-0.001, y=35, labels="expected loss", lwd=3, pos=4)
# Define cumulative default probability function
cum_loss <- function(x, def_thresh=(-2), rho=0.2, lgd=0.4)
  pnorm((sqrt(1-rho)*qnorm(x/lgd) - def_thresh)/sqrt(rho))
# Define Vasicek loss distribution function
# (vectorized version with error handling for x)
loss_distr <- function(x, def_thresh=-2, rho=0.1, lgd=0.4) {
  q_norm <- ifelse(x/lgd < 0.999, qnorm(x/lgd), 3.1)
  sqrt((1-rho)/rho)*exp(-(sqrt(1-rho)*q_norm - def_thresh)^2/(2*rho) + q_norm^2/2)/lgd
}  # end loss_distr
def_prob <- 0.2; def_thresh <- qnorm(def_prob)
rho <- 0.1; lgd <- 0.4
at_tach <- 0.15; de_tach <- 0.2
# Expected tranche loss is sum of two terms
tranche_loss <-
  # Loss between at_tach and de_tach
  integrate(function(x, at_tach) (x-at_tach)*loss_distr(x,
def_thresh=def_thresh, rho=rho, lgd=lgd),
low=at_tach, up=de_tach, at_tach=at_tach)$value / (de_tach-at_tach) +
  # Loss in excess of de_tach
  (1-cum_loss(x=de_tach, def_thresh=def_thresh, rho=rho, lgd=lgd))
# Plot probability distribution of losses
curve(expr=loss_distr(x, def_thresh=def_thresh, rho=rho),
type="l", xlim=c(0, 3*lgd*def_prob),
xlab="loss percentage", ylab="density", lwd=3,
col="orange", main="CDO Tranche Losses")
# Add line for expected loss
abline(v=lgd*def_prob, col="red", lwd=3)
text(x=lgd*def_prob-0.001, y=4, labels="expected loss",
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
densv <- sapply(var_s, loss_distr,
  def_thresh=def_thresh, rho=rho)
# Draw shaded polygon
polygon(c(at_tach, var_s, de_tach), density=20,
  c(-1, densv, -1), col="red", border=NA)
text(x=0.5*(at_tach+de_tach), y=0, labels="CDO tranche", cex=0.9, lwd=2, pos=3)
# Add lines for unexpected loss
abline(v=0.04, col="blue", lwd=3)
arrows(x0=0.02, y0=35, x1=0.04, y1=35, code=3, lwd=3, cex=0.5)
text(x=0.03, y=36, labels="unexpected loss", lwd=2, pos=3)
# Add lines for VaR
abline(v=0.055, col="red", lwd=3)
arrows(x0=0.0, y0=25, x1=0.055, y1=25, code=3, lwd=3, cex=0.5)
text(x=0.03, y=26, labels="VaR", lwd=2, pos=3)
text(x=0.055-0.001, y=10, labels="VaR", lwd=2, srt=90, pos=3)
va_r <- 0.04; min_var <- 4*lgd*def_prob
# Calculate CVaR
c_var <- integrate(function(x, ...) x*loss_distr(x, ...),
  low=va_r, up=lgd, def_thresh=def_thresh, rho=rho, lgd=lgd)$value
c_var <- c_var/integrate(loss_distr, low=va_r, up=lgd, def_thresh=def_thresh, rho=rho, lgd=lgd)$value
# Plot probability distribution of losses
curve(expr=loss_distr(x, def_thresh=def_thresh, rho=rho),
type="l", xlim=c(0, 0.06),
xlab="loss percentage", ylab="density", lwd=3,
col="orange", main="Conditional Value at Risk")
# Add line for expected loss
abline(v=lgd*def_prob, col="red", lwd=3)
text(x=lgd*def_prob-0.001, y=10, labels="expected loss", lwd=2, srt=90, pos=3)
# Add lines for VaR
abline(v=va_r, col="red", lwd=3)
text(x=va_r-0.001, y=10, labels="VaR",
 lwd=2, srt=90, pos=3)
# Add shading for CVaR
var_s <- seq(va_r, min_var, length=100)
densv <- sapply(var_s, loss_distr,
  def_thresh=def_thresh, rho=rho)
# Draw shaded polygon
polygon(c(va_r, var_s, min_var), density=20,
  c(-1, densv, -1), col="red", border=NA)
text(x=va_r+0.005, y=0, labels="CVaR", lwd=2, pos=3)
# VaR (quantile of the loss distribution)
var_func <- function(x, def_thresh=qnorm(0.1), rho=0.1, lgd=0.4)
  lgd*pnorm((sqrt(rho)*qnorm(x) + def_thresh)/sqrt(1-rho))
var_func(x=0.99, def_thresh=def_thresh, rho=rho, lgd=lgd)
# Plot VaR
curve(expr=var_func(x, def_thresh=def_thresh, rho=rho, lgd=lgd),
type="l", xlim=c(0, 0.999), xlab="confidence level", ylab="VaR", lwd=3,
col="orange", main="VaR versus Confidence Level")
# Add line for expected loss
abline(h=lgd*def_prob, col="red", lwd=3)
text(x=0.2, y=lgd*def_prob, labels="expected loss", lwd=2, pos=3)
# Integrate loss_distr() over full range
integrate(loss_distr, low=0.0, up=lgd,
    def_thresh=def_thresh, rho=rho, lgd=lgd)
# Calculate expected losses using loss_distr()
integrate(function(x, ...) x*loss_distr(x, ...),
    low=0.0, up=lgd,
    def_thresh=def_thresh, rho=rho, lgd=lgd)
# Calculate confidence levels corresponding to VaR values
var_s <- seq(0.07, 0.12, 0.001)
levels <- sapply(var_s, function(va_r, ...) {
  integrate(loss_distr, low=va_r, up=lgd, ...)
}, def_thresh=def_thresh, rho=rho, lgd=lgd)  # end sapply
levels <- cbind(as.numeric(t(levels)[, 1]), var_s)
colnames(levels) <- c("levels", "VaRs")
# Calculate 95% confidence level VaR value
levels[match(TRUE, levels[, "levels"] < 0.05), "VaRs"]
plot(x=1-levels[, "levels"],
     y=levels[, "VaRs"], lwd=2,
     xlab="confidence level", ylab="VaRs",
     t="l", main="VaR Values and Confidence Levels")
# Calculate CVaR values
cvar_s <- sapply(var_s, function(va_r, ...) {
  integrate(function(x, ...) x*loss_distr(x, ...),
      low=va_r, up=lgd, ...)
}, def_thresh=def_thresh, rho=rho, lgd=lgd)  # end sapply
levels <- cbind(levels, as.numeric(t(cvar_s)[, 1]))
colnames(levels)[3] <- "CVaRs"
# Divide CVaR by confidence level
levels[, "CVaRs"] <- levels[, "CVaRs"]/levels[, "levels"]
# Calculate 95% confidence level CVaR value
levels[match(TRUE,
  levels[, "levels"] < 0.05), "CVaRs"]
# Plot CVaRs
plot(x=1-levels[, "levels"],
     y=levels[, "CVaRs"],
     t="l", col="red", lwd=2,
     ylim=range(levels[, c("VaRs", "CVaRs")]),
     xlab="confidence level", ylab="CVaRs",
     main="CVaR Values and Confidence Levels")
# Add VaRs
lines(x=1-levels[, "levels"], y=levels[, "VaRs"], lwd=2)
# Add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"),
 title="default probability = 5%
correlation = 10%
loss given default = 40%",
 inset=0.1, cex=0.8, bg="white", bty="n",
 lwd=6, lty=1, col=c("red", "black"))
# Define model parameters
nassets <- 300; nsimu <- 1000; lgd <- 0.4
# Define correlation parameters
rho <- 0.2; rho_sqrt <- sqrt(rho); rho_sqrtm <- sqrt(1-rho)
# Calculate default probabilities and thresholds
set.seed(1121)
def_probs <- runif(nassets, max=0.2)
def_thresh <- qnorm(def_probs)
# Simulate losses under Vasicek model
sysv <- rnorm(nsimu)
assets <- matrix(rnorm(nsimu*nassets), ncol=nsimu)
assets <- t(rho_sqrt*sysv + t(rho_sqrtm*assets))
losses <- lgd*colSums(assets < def_thresh)/nassets
# Calculate VaR from confidence level
confl <- 0.95
va_r <- quantile(losses, confl)
# Calculate the CVaR as the mean losses in excess of VaR
c_var <- mean(losses[losses > va_r])
# Plot the density of portfolio losses
x11(width=6, height=5)
densv <- density(losses)
plot(densv, xlab="loss percentage", ylab="density",
     lwd=3, col="blue", main="Portfolio Loss Distribution")
# Add vertical line for expected loss
exploss <- lgd*mean(def_probs)
abline(v=exploss, col="red", lwd=3)
x_max <- max(densv$x); y_max <- max(densv$y)
text(x=exploss, y=(6*y_max/7), labels="expected loss",
     lwd=2, pos=4)
# Add vertical line for VaR
abline(v=va_r, col="red", lwd=3)
text(x=va_r, y=4*y_max/5, labels="VaR", lwd=2, pos=4)
# Draw shaded polygon for CVaR
indeks <- (densv$x > va_r)
x_var <- c(min(densv$x[indeks]), densv$x[indeks], max(densv$x))
polygon(x_var, c(-1, densv$y[indeks], -1), col="red", border=NA, density=10)
# Add text for CVaR
text(x=5*va_r/4, y=(y_max/7), labels="CVaR", lwd=2, pos=4)
# Add text with data
text(x_max, y_max, labels=paste0(
 "Expected Loss = ", format(100*exploss, digits=3), "%", "\n",
 "Loss severity = ", format(100*lgd, digits=3), "%", "\n",
 "Correlation = ", format(100*rho, digits=3), "%", "\n",
 "VaR = ", format(100*va_r, digits=3), "%", "\n",
 "CVaR = ", format(100*c_var, digits=3), "%"),
     adj=c(1, 1), cex=0.8, lwd=2)
# Calculate VaRs from confidence levels
levels <- seq(0.93, 0.99, 0.01)
var_s <- quantile(losses, probs=levels)
plot(x=levels, y=var_s, t="l", lwd=2,
     xlab="confidence level", ylab="VaRs",
     main="Simulated VaR and Confidence Levels")
# Calculate CVaRs
cvar_s <- sapply(var_s, function(va_r) {
  mean(losses[losses >= va_r])
})  # end sapply
cvar_s <- cbind(cvar_s, var_s)
# Alternative CVaR calculation using frequency table
# first calculate frequency table of losses
# tablev <- table(losses)/nsimu
# Calculate CVaRs from frequency table
# Cvar_s <- sapply(var_s, function(va_r) {
#   tai_l <- tablev[names(tablev) > va_r]
#   tai_l %*% as.numeric(names(tai_l)) / sum(tai_l)
# })  # end sapply
# Plot CVaRs
plot(x=levels, y=cvar_s[, "cvar_s"],
     t="l", col="red", lwd=2,
     ylim=range(cvar_s),
     xlab="confidence level", ylab="CVaRs",
     main="Simulated CVaR and Confidence Levels")
# Add VaRs
lines(x=levels, y=cvar_s[, "var_s"], lwd=2)
# Add legend
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=1, col=c("red", "black"))
calc_var <- function(def_thresh, # Default thresholds
               lgd=0.6, # loss given default
               rho_sqrt, rho_sqrtm, # asset correlation
               nsimu=1000, # number of simulations
               levels=seq(0.93, 0.99, 0.01) # Confidence levels
               ) {
  # Define model parameters
  nassets <- NROW(def_thresh)
  # Simulate losses under Vasicek model
  sysv <- rnorm(nsimu)
  assets <- matrix(rnorm(nsimu*nassets), ncol=nsimu)
  assets <- t(rho_sqrt*sysv + t(rho_sqrtm*assets))
  losses <- lgd*colSums(assets < def_thresh)/nassets
  # Calculate VaRs and CVaRs
  var_s <- quantile(losses, probs=levels)
  cvar_s <- sapply(var_s, function(va_r) {
    mean(losses[losses >= va_r])
  })  # end sapply
  names(var_s) <- levels
  names(cvar_s) <- levels
  c(var_s, cvar_s)
}  # end calc_var
# Define model parameters
nassets <- 300; nsimu <- 1000; lgd <- 0.4
rho <- 0.2; rho_sqrt <- sqrt(rho); rho_sqrtm <- sqrt(1-rho)
# Calculate default probabilities and thresholds
set.seed(1121)
def_probs <- runif(nassets, max=0.2)
def_thresh <- qnorm(def_probs)
# Define number of bootstrap simulations
nboot <- 500
# Perform bootstrap of calc_var
set.seed(1121)
boot_data <- sapply(rep(lgd, nboot), calc_var,
  def_thresh=def_thresh,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  nsimu=nsimu, levels=levels)  # end sapply
boot_data <- t(boot_data)
# Calculate vectors of standard errors of VaR and CVaR from boot_data data
stderror_var <- apply(boot_data[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
stderror_cvar <- apply(boot_data[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
stderror_var[2, ] <- stderror_var[2, ]/stderror_var[1, ]
stderror_cvar[2, ] <- stderror_cvar[2, ]/stderror_cvar[1, ]
# Plot the standard errors of VaRs and CVaRs
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(0, 0, 0, 0), mgp=c(2, 1, 0))
plot(x=colnames(stderror_cvar), y=stderror_cvar[2, ],
  t="l", col="red", lwd=2,
  ylim=range(c(stderror_var[2, ], stderror_cvar[2, ])),
  xlab="confidence level", ylab="standard error",
  main="Scaled standard errors of CVaR and VaR")
lines(x=colnames(stderror_var), y=stderror_var[2, ], lwd=2)
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=1, col=c("red", "black"))
library(parallel)  # load package parallel
ncores <- detectCores() - 1  # number of cores
cluster <- makeCluster(ncores)  # Initialize compute cluster
# Perform bootstrap of calc_var for Windows
clusterSetRNGStream(cluster, 1121)
boot_data <- parLapply(cluster, rep(lgd, nboot),
  fun=calc_var, def_thresh=def_thresh,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  nsimu=nsimu, levels=levels)  # end parLapply
# Bootstrap under Mac-OSX or Linux
boot_data <- mclapply(rep(lgd, nboot),
  FUN=calc_var, def_thresh=def_thresh,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  nsimu=nsimu, levels=levels)  # end mclapply
boot_data <- rutils::do_call(rbind, boot_data)
stopCluster(cluster)  # Stop R processes over cluster
# Calculate vectors of standard errors of VaR and CVaR from boot_data data
stderror_var <- apply(boot_data[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
stderror_cvar <- apply(boot_data[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
stderror_var_scaled <- stderror_var[2, ]/stderror_var[1, ]
stderror_cvar_scaled <- stderror_cvar[2, ]/stderror_cvar[1, ]
# Plot the standard errors of VaRs and CVaRs
x11(width=6, height=5)
plot(x=colnames(stderror_cvar),
  y=stderror_cvar_scaled, t="l", col="red", lwd=2,
  ylim=range(c(stderror_var_scaled, stderror_cvar_scaled)),
  xlab="confidence level", ylab="standard error",
  main="Scaled standard errors of CVaR and VaR")
lines(x=colnames(stderror_var), y=stderror_var_scaled, lwd=2)
legend(x="topleft", legend=c("CVaRs", "VaRs"), bty="n",
 title=NULL, inset=0.05, cex=0.8, bg="white",
 lwd=6, lty=1, col=c("red", "black"))
calc_var <- function(def_probs, # Default probabilities
               lgd=0.6, # loss given default
               rho_sqrt, rho_sqrtm, # asset correlation
               nsimu=1000, # number of simulations
               levels=seq(0.93, 0.99, 0.01) # Confidence levels
               ) {
  # Calculate random default thresholds
  def_thresh <- qnorm(runif(1, min=0.5, max=1.5)*def_probs)
  # Simulate losses under Vasicek model
  nassets <- NROW(def_probs)
  sysv <- rnorm(nsimu)
  assets <- matrix(rnorm(nsimu*nassets), ncol=nsimu)
  assets <- t(rho_sqrt*sysv + t(rho_sqrtm*assets))
  losses <- lgd*colSums(assets < def_thresh)/nassets
  # Calculate VaRs and CVaRs
  var_s <- quantile(losses, probs=levels)
  cvar_s <- sapply(var_s, function(va_r) {
    mean(losses[losses >= va_r])
  })  # end sapply
  names(var_s) <- levels
  names(cvar_s) <- levels
  c(var_s, cvar_s)
}  # end calc_var
library(parallel)  # load package parallel
ncores <- detectCores() - 1  # number of cores
cluster <- makeCluster(ncores)  # Initialize compute cluster
# Perform bootstrap of calc_var for Windows
clusterSetRNGStream(cluster, 1121)
boot_data <- parLapply(cluster, rep(lgd, nboot),
  fun=calc_var, def_probs=def_probs,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  nsimu=nsimu, levels=levels)  # end parLapply
# Bootstrap under Mac-OSX or Linux
boot_data <- mclapply(rep(lgd, nboot),
  FUN=calc_var, def_probs=def_probs,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  nsimu=nsimu, levels=levels)  # end mclapply
boot_data <- rutils::do_call(rbind, boot_data)
stopCluster(cluster)  # Stop R processes over cluster
# Calculate vectors of standard errors of VaR and CVaR from boot_data data
stderror_var_param <- apply(boot_data[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
stderror_cvar_param <- apply(boot_data[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Plot the standard errors of VaRs under uncertain default probabilities
x11(width=6, height=5)
plot(x=colnames(stderror_var),
  y=stderror_var[2, ], t="l", lwd=3,
  ylim=range(c(stderror_var[2, ], stderror_var_param[2, ])),
  xlab="confidence level", ylab="standard error",
  main="Standard Errors of VaR
  with Uncertain Default Probabilities")
lines(x=colnames(stderror_var), y=stderror_var_param[2, ],
col="red", lwd=3)
legend(x=0.95, y=0.02, bty="n",
 legend=c("VaR Fixed Def Probs", "VaR Random Def Probs"),
 title=NULL, inset=0.05, cex=1.0, bg="white",
 lwd=6, lty=1, col=c("black", "red"))
# Scale the standard errors of VaRs and CVaRs
stderror_var_scaled <- stderror_var_param[2, ]/
  stderror_var_param[1, ]
stderror_cvar_scaled <- stderror_cvar_param[2, ]/
  stderror_cvar_param[1, ]
# Plot the standard errors of VaRs and CVaRs
x11(width=6, height=5)
plot(x=colnames(stderror_cvar_param),
  y=stderror_cvar_scaled, t="l", col="red", lwd=3,
  ylim=range(c(stderror_var_scaled, stderror_cvar_scaled)),
  xlab="confidence level", ylab="standard error",
  main="Relative Standard Errors of VaR and CVaR
  with Uncertain Default Probabilities")
lines(x=names(stderror_var_scaled), y=stderror_var_scaled, lwd=3)
legend(x="topright", legend=c("CVaR", "VaR"), bty="n",
 title=NULL, inset=0.05, cex=1.0, bg="white",
 lwd=6, lty=1, col=c("red", "black"))
# Define model parameters
nassets <- 300; nsimu <- 1000; lgd <- 0.4
# Define correlation parameters
rho <- 0.2; rho_sqrt <- sqrt(rho); rho_sqrtm <- sqrt(1-rho)
# Calculate default probabilities and thresholds
set.seed(1121)
def_probs <- runif(nassets, max=0.2)
def_thresh <- qnorm(def_probs)
# Calculate vector of systematic factors
sysv <- rnorm(nsimu)
# Calculate vector of idiosyncratic factors
idio_syncratic <-
  matrix(rnorm(nsimu*nassets), ncol=nsimu)
# Simulate losses under Vasicek model
assets <-
  t(rho_sqrt*sysv + t(rho_sqrtm*idio_syncratic))
losses <-
  lgd*colSums(assets < def_thresh)/nassets
# Calculate VaRs
levels <- seq(0.93, 0.99, 0.01)
var_s <- quantile(losses, probs=levels)
# Importance sampling losses
lambda <- 3
assets <-
  t(rho_sqrt*sysv + t(rho_sqrtm*idio_syncratic))
cond_thresh <- outer(rho_sqrtm*def_thresh, -rho_sqrt*sysv, FUN="+")
cond_probs <- pnorm(cond_thresh)
tilt_probs <- lambda*cond_probs/(1 + cond_probs*(lambda - 1))
weightv <- (1 + tilt_probs*(lambda - 1))/lambda
tilt_thresh <- qnorm(tilt_probs)
losses <-
  lgd*colSums(weightv*(assets < tilt_thresh))/nassets
var_s <- quantile(losses, probs=levels)
foo <- (unifunc < def_probs)
def_thresh <- qnorm(def_probs)
assets <- t(rho_sqrt*(sysv - lambda) +
t(rho_sqrtm*idio_syncratic))
losses <-
  lgd*colSums(assets < def_thresh)/nassets
# Calculate VaRs
var_s <- quantile(losses, probs=levels)
assets <- t(rho_sqrt*(sysv - lambda) +
t(rho_sqrtm*idio_syncratic))
losses <-
  lgd*colSums(assets < def_thresh)/nassets
# Calculate VaRs
var_s <- quantile(losses, probs=levels)
plot(x=levels, y=var_s, t="l", lwd=2,
     xlab="confidence level", ylab="VaRs",
     main="Simulated VaR and Confidence Levels")
# Define model parameters
nassets <- 300; nsimu <- 1000; lgd <- 0.4
rho <- 0.2; rho_sqrt <- sqrt(rho); rho_sqrtm <- sqrt(1-rho)
# Calculate default probabilities and thresholds
set.seed(1121)
def_probs <- runif(nassets, max=0.2)
def_thresh <- qnorm(def_probs)
# Define number of bootstrap simulations
nboot <- 500
# Perform bootstrap of calc_var
set.seed(1121)
boot_data <- sapply(rep(lgd, nboot),
  calc_var,
  def_thresh=def_thresh,
  rho_sqrt=rho_sqrt, rho_sqrtm=rho_sqrtm,
  nsimu=nsimu, levels=levels)  # end sapply
boot_data <- t(boot_data)
# Calculate vectors of standard errors of VaR and CVaR from boot_data data
stderror_var <- apply(boot_data[, 1:7], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
stderror_cvar <- apply(boot_data[, 8:14], MARGIN=2,
    function(x) c(mean=mean(x), sd=sd(x)))
# Scale the standard errors of VaRs and CVaRs
stderror_var[2, ] <- stderror_var[2, ]/stderror_var[1, ]
stderror_cvar[2, ] <- stderror_cvar[2, ]/stderror_cvar[1, ]
