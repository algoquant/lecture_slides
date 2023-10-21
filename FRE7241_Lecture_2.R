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

library(rutils)
# Extract the ETF prices from rutils::etfenv$prices
pricev <- rutils::etfenv$prices
pricev <- zoo::na.locf(pricev, na.rm=FALSE)
pricev <- zoo::na.locf(pricev, fromLast=TRUE)
datev <- zoo::index(pricev)
# Calculate the dollar returns
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
pricen <- cumprod(1 + retp)
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
  dyLegend(show="always", width=300)

# Calculate the percentage VTI returns
pricev <- rutils::etfenv$prices$VTI
pricev <- na.omit(pricev)
retp <- rutils::diffit(pricev)/rutils::lagit(pricev, lagg=1, pad_zeros=FALSE)

# Funding rate per day
frate <- 0.01/252
# Margin account
marginv <- cumsum(retp)
# Cumulative funding costs
fcosts <- cumsum(frate*marginv)
# Add funding costs to margin account
marginv <- (marginv + fcosts)
# dygraph plot of margin and funding costs
datav <- cbind(marginv, fcosts)
colnamev <- c("Margin", "Cumulative Funding")
colnames(datav) <- colnamev
endd <- rutils::calc_endpoints(datav, interval="weeks")
dygraphs::dygraph(datav[endd], main="VTI Margin Funding Costs") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

# bidask equal to 1 bp for liquid ETFs
bidask <- 0.001
# Cumulative transaction costs
costs <- bidask*cumsum(abs(retp))/2
# Subtract transaction costs from margin account
marginv <- cumsum(retp)
marginv <- (marginv - costs)
# dygraph plot of margin and transaction costs
datav <- cbind(marginv, costs)
colnamev <- c("Margin", "Cumulative Transaction Costs")
colnames(datav) <- colnamev
dygraphs::dygraph(datav[endd], main="VTI Transaction Costs") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

# Calculate the VTI and IEF dollar returns
pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- na.omit(pricev)
retd <- rutils::diffit(pricev)
datev <- zoo::index(pricev)
# Calculate the VTI and IEF percentage returns
retp <- retd/rutils::lagit(pricev, lagg=1, pad_zeros=FALSE)
# Wealth of fixed shares equal to $0.5 each (without rebalancing)
weightv <- c(0.5, 0.5)  # dollar weights
wealthfs <- drop(cumprod(1 + retp) %*% weightv)
# Or using the dollar returns
pricesi <- as.numeric(pricev[1, ])
retd[1, ] <- pricev[1, ]
wealthfs2 <- cumsum(retd %*% (weightv/pricesi))
all.equal(wealthfs, drop(wealthfs2))

# Wealth of fixed dollars (with rebalancing)
wealthfd <- cumsum(retp %*% weightv)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(log(wealthfs), wealthfd)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Fixed shares", "Fixed dollars")
sqrt(252)*sapply(rutils::diffit(wealthv), function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the log wealth
colnamev <- colnames(wealthv)
endd <- rutils::calc_endpoints(retp, interval="weeks")
dygraphs::dygraph(wealthv[endd], main="Wealth of Weighted Portfolios") %>%
  dySeries(name=colnamev[1], col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Margin account for fixed dollars (with rebalancing)
marginv <- cumsum(retp %*% weightv)
# Cumulative transaction costs
costs <- bidask*cumsum(abs(retp) %*% weightv)/2
# Subtract transaction costs from margin account
marginv <- (marginv - costs)
# dygraph plot of margin and transaction costs
datav <- cbind(marginv, costs)
datav <- xts::xts(datav, datev)
colnamev <- c("Margin", "Cumulative Transaction Costs")
colnames(datav) <- colnamev
dygraphs::dygraph(datav[endd], main="Fixed Dollar Portfolio Transaction Costs") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

# Wealth of fixed shares (without rebalancing)
wealthfs <- cumsum(retd %*% (weightv/pricesi))
# Or compound the percentage returns
wealthfs <- cumprod(1 + retp) %*% weightv
# Wealth of proportional allocations (with rebalancing)
wealthpd <- cumprod(1 + retp %*% weightv)
wealthv <- cbind(wealthfs, wealthpd)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Fixed shares", "Prop dollars")
wealthv <- log(wealthv)
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv), function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the log wealth
dygraphs::dygraph(wealthv[endd],
  main="Wealth of Proportional Dollar Allocations") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Returns in excess of weighted returns
retw <- retp %*% weightv
retx <- lapply(retp, function(x) (retw - x))
retx <- do.call(cbind, retx)
sum(retx %*% weightv)
# Calculate the weighted sum of absolute excess returns
retx <- abs(retx) %*% weightv
# Total dollar amount of stocks that need to be traded
retx <- retx*rutils::lagit(wealthpd)
# Cumulative transaction costs
costs <- bidask*cumsum(retx)/2
# Subtract transaction costs from wealth
wealthpd <- (wealthpd - costs)

# dygraph plot of wealth and transaction costs
wealthv <- cbind(wealthpd, costs)
wealthv <- xts::xts(wealthv, datev)
colnamev <- c("Wealth", "Cumulative Transaction Costs")
colnames(wealthv) <- colnamev
dygraphs::dygraph(wealthv[endd],
  main="Transaction Costs With Proportional Allocations") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

# Wealth of fixed shares (without rebalancing)
wealthfs <- drop(apply(retp, 2, function(x) cumprod(1 + x)) %*% weightv)-1
# Wealth of proportional dollar allocations (with rebalancing)
wealthpd <- cumprod(1 + retp %*% weightv) - 1
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
wealthv <- cbind(wealthpd, wealthv)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Proportional Allocations", "Proportional Target")
dygraphs::dygraph(wealthv, main="Wealth of Proportional Target Allocations") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

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
wealthv <- cumsum(retp)
# Calculate the a vector of monthly end points
endd <- rutils::calc_endpoints(retp, interval="weeks")
# Plot cumulative log wealth
dygraphs::dygraph(wealthv[endd],
  main="Stocks and Bonds With Proportional Allocations") %>%
  dyOptions(colors=c("blue", "green", "blue", "red")) %>%
  dySeries("Combined", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)

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
  apply(retp, 2, function(x) prod(1 + x))
})  # end parLapply
# Calculate the distribution of terminal wealths under Mac-OSX or Linux
wealthv <- mclapply(bootd, function(retp) {
  apply(retp, 2, function(x) prod(1 + x))
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

# Define the risk-adjusted wealth measure
riskretfun <- function(wealthv) {
  riskv <- 0.01 # Risk floor
  if (min(wealthv) < 1) # Some wealth is below par
    # Calculate the mean stock wealth below par
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
weightaw <- c(0.30, 0.55, 0.15)
retp <- cbind(retp, retp %*% weightaw)
colnames(retp)[4] <- "All Weather"
# Calculate the Sharpe ratios
sqrt(252)*sapply(retp, function(x) mean(x)/sd(x))

# Calculate the cumulative wealth from returns
wealthv <- cumsum(retp)
# Calculate the a vector of monthly end points
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
# dygraph all-weather wealth
dygraphs::dygraph(wealthv[endd], main="All-Weather Portfolio") %>%
  dyOptions(colors=c("blue", "green", "orange", "red")) %>%
  dySeries("All Weather", color="red", strokeWidth=2) %>%
  dyLegend(show="always", width=400)
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
pricev <- 100*cumprod(1 + retp)
datav <- xts::xts(cbind(stockv, bondv, portfv, pricev), datev)
colnames(datav) <- c("stocks", "bonds", "CPPI", "VTI")
endd <- rutils::calc_endpoints(datav, interval="weeks")
dygraphs::dygraph(datav[endd], main="CPPI strategy") %>%
  dyOptions(colors=c("red", "green", "blue", "orange"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the dollar returns of VTI and IEF
pricev <- na.omit(rutils::etfenv$prices[, c("VTI", "IEF")])
retd <- rutils::diffit(pricev)
# Scale the stock prices to $1 at beginning
pricesi <- as.numeric(pricev[1, ]) # Initial stock prices
pricesc <- pricev
pricesc$VTI <- pricesc$VTI/pricesi[1]
pricesc$IEF <- pricesc$IEF/pricesi[2]
sum(pricesc[1, ])
retsc <- rutils::diffit(pricesc)
# Wealth of fixed number of shares (without rebalancing)
weightv <- c(0.5, 0.5) # Buy $0.5 of each stock
wealthed <- 1 + cumsum(retsc %*% weightv)
# Calculate the stock prices with unit dollar volatility
stdev <- sapply(retd, sd)
pricesd <- pricev
pricesd$VTI <- pricev$VTI/stdev["VTI"]
pricesd$IEF <- pricev$IEF/stdev["IEF"]
retsd <- rutils::diffit(pricesd)
sapply(retsd, sd)
# Or equivalent using lapply()
# Calculate the standardized dollar returns
retsd <- lapply(retd, function(x) x/sd(x))
retsd <- do.call(cbind, retsd)
sapply(retsd, sd)
# Initial stock prices with unit dollar volatility
pricesi <- as.numeric(pricev[1, ])/sapply(retd, sd)
# Wealth of shares with equal dollar volatilities
wealthev <- 1 + cumsum(retsd %*% weightv)/sum(pricesi*weightv)

# Scale the sum of stock prices at beginning to $2
pricesd <- 2*pricesd/sum(pricesd[1, ])
retsd <- rutils::diffit(pricesd)
sapply(retsd, sd)
# Wealth of shares with equal dollar volatilities
wealthev <- 1 + cumsum(retsd %*% weightv)
# Calculate the Sharpe and Sortino ratios
wealthv <- xts::xts(cbind(wealthed, wealthev), zoo::index(pricev))
colnames(wealthv) <- c("Equal dollar", "Equal volatility")
sqrt(252)*sapply(rutils::diffit(wealthv), function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the log wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(log(wealthv[endd]),
  main="Wealth of Equal Dollar And Equal Volatility") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the wealth of proportional dollar allocations (with rebalancing)
retp <- retd/rutils::lagit(pricev, lagg=1, pad_zeros=FALSE)
weightv <- c(0.5, 0.5)
wealthpd <- cumprod(1 + retp %*% weightv)
# Calculate the trailing dollar volatility
volat <- HighFreq::run_var(retd, lambda=0.2)
volat <- sqrt(volat)
volat <- rutils::lagit(volat)
volat[1:2, ] <- 1
# Calculate the standardized prices with unit dollar volatility
pricerp <- pricev/volat
# Scale the sum of stock prices to $2
pricerp <- 2*pricerp/rowSums(pricerp)
# Calculate the risk parity returns
retrp <- retp*pricerp
# Calculate the wealth of risk parity
wealthrp <- 1 + cumsum(retrp %*% weightv)

# Calculate the log wealths
wealthv <- cbind(wealthpd, wealthrp)
wealthv <- xts::xts(wealthv, zoo::index(pricev))
colnames(wealthv) <- c("PropDollars", "Risk Parity")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv), function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot a dygraph of the log wealths
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(log(wealthv[endd]),
  main="Log Wealth of Risk Parity vs Proportional Allocations") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Test risk parity market timing of VTI using Treynor-Mazuy test
retrp <- rutils::diffit(wealthv)
retvti <- retp$VTI
desm <- cbind(retrp, retvti, retvti^2)
colnames(desm)[1:2] <- c("prop", "riskp")
colnames(desm)[4] <- "treynor"
regmod <- lm(riskp ~ VTI + treynor, data=desm)
summary(regmod)
# Plot residual scatterplot
resids <- regmod$residuals
plot.default(x=retvti, y=resids, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for Risk Parity vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retvti, y=fitv, pch=16, col="red")
text(x=0.0, y=0.8*max(resids), paste("Treynor test t-value =", tvalue))

# Test for proportional allocations market timing of VTI using Treynor-Mazuy test
regmod <- lm(prop ~ VTI + treynor, data=desm)
summary(regmod)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
points.default(x=retvti, y=fitv, pch=16, col="blue")
text(x=0.0, y=0.6*max(resids), paste("Prop Alloc t-value =", round(coefreg["treynor", "t value"], 2)))

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
pnlinmay <- posv*retp
wealthv <- cbind(retp, pnlinmay)
colnames(wealthv) <- c("VTI", "sell_in_may")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot wealth of Sell in May strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Sell in May Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
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
desm <- cbind(wealth$sell_in_may, 0.5*(retp+abs(retp)), retp^2)
colnames(desm) <- c("VTI", "merton", "treynor")
# Perform Merton-Henriksson test
regmod <- lm(pnlinmay ~ VTI + merton, data=desm)
summary(regmod)
# Perform Treynor-Mazuy test
regmod <- lm(pnlinmay ~ VTI + treynor, data=desm)
summary(regmod)
# Plot Treynor-Mazuy residual scatterplot
resids <- (pnlinmay - regmod$coeff["VTI"]*retp)
plot.default(x=retp, y=resids, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for Sell in May vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retp
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retp, y=fitv, pch=16, col="red")
text(x=0.0, y=0.8*max(resids), paste("Treynor test t-value =", tvalue))

# Calculate the log of OHLC VTI prices
ohlc <- log(rutils::etfenv$VTI)
openp <- quantmod::Op(ohlc)
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
closep <- quantmod::Cl(ohlc)
# Calculate the close-to-close log returns,
# the daytime open-to-close returns
# and the overnight close-to-open returns.
retp <- rutils::diffit(closep)
colnames(retp) <- "daily"
retd <- (closep - openp)
colnames(retd) <- "intraday"
reton <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE))
colnames(reton) <- "overnight"

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, reton, retd)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the log wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Wealth of Close-to-Close, Overnight, and Intraday Strategies") %>%
  dySeries(name="daily", strokeWidth=2, col="blue") %>%
  dySeries(name="overnight", strokeWidth=2, col="red") %>%
  dySeries(name="intraday", strokeWidth=2, col="green") %>%
  dyLegend(width=500)

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
colnamev <- c("VTI", "TOM Strategy")
colnames(wealthv) <- colnamev
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot VTI Turn of the Month strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Turn of the Month Strategy") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="red")

# Calculate the VTI prices and returns
pricev <- na.omit(rutils::etfenv$prices$VTI)
nrows <- NROW(pricev)
datev <- zoo::index(pricev)
retp <- rutils::diffit(log(pricev))
# Simulate stop-loss strategy
stopl <- 0.05 # Stop-loss percentage
pricem <- cummax(pricev) # Trailing maximum prices
# Calculate the drawdown
dd <- (pricev - pricem)
pnls <- retp # Initialize PnLs
for (i in 1:(nrows-1)) {
# Check for stop-loss
  if (dd[i] < -stopl*pricem[i])
    pnls[i+1] <- 0 # Set PnLs = 0 if in stop-loss
}  # end for
# Same but without using loops in R
pnls2 <- retp
insl <- rutils::lagit(dd < -stopl*pricem)
pnls2 <- ifelse(insl, 0, pnls2)
all.equal(pnls, pnls2, check.attributes=FALSE)

# Combine the data
wealthv <- cbind(retp, pnls)
colnamev <- c("VTI", "Strategy")
colnames(wealthv) <- colnamev
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot the stop-loss strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="VTI Stop-loss Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Plot dygraph with shading
# Create colors for background shading
indic <- (rutils::diffit(insl) != 0) # Indices of stop-loss
crossd <- c(datev[indic], datev[nrows]) # Dates of stop-loss
shadev <- ifelse(insl[indic] == 1, "antiquewhite", "lightgreen")
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(cumsum(wealthv),
  main="VTI Stop-loss Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
  dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot

# Simulate multiple stop-loss strategies
dd <- (pricev - pricem)
stopv <- 0.01*(1:30)
pnlc <- sapply(stopv, function(stopl) {
  pnls <- retp
  insl <- rutils::lagit(dd < -stopl*pricem)
  pnls <- ifelse(insl, 0, pnls)
  sum(pnls)
})  # end sapply

# Plot cumulative pnls for stop-loss strategies
plot(x=stopv, y=pnlc,
   main="Cumulative PnLs for Stop-loss Strategies",
   xlab="stop-loss percent", ylab="cumulative pnl",
   t="l", lwd=3, col="blue")

# Define function for simulating a stop-start strategy
sim_stopstart <- function(stopl) {
  maxp <- pricev[1] # Trailing maximum price
  minp <- pricev[1] # Trailing minimum price
  insl <- FALSE # Is in stop-loss?
  insg <- FALSE # Is in start-gain?
  pnls <- retp # Initialize PnLs
  for (i in 1:nrows) {
    if (insl) { # In stop-loss
pnls[i] <- 0 # Set PnLs = 0 if in stop-loss
minp <- min(minp, pricev[i]) # Update minimum price to current price
if (pricev[i] > ((1 + stopl)*minp)) { # Check for start-gain
  insg <- TRUE # Is in start-gain?
  insl <- FALSE # Is in stop-loss?
  maxp <- pricev[i] # Reset trailing maximum price
}  # end if
    } else if (insg) { # In start-gain
maxp <- max(maxp, pricev[i]) # Update maximum price to current price
if (pricev[i] < ((1 - stopl)*maxp)) { # Check for stop-loss
  insl <- TRUE # Is in stop-loss?
  insg <- FALSE # Is in start-gain?
  minp <- pricev[i] # Reset trailing minimum price
}  # end if
    } else { # Warmup period
# Update the maximum and minimum prices
maxp <- max(maxp, pricev[i])
minp <- min(minp, pricev[i])
# Update the stop-loss and start-gain indicators
insl <- (pricev[i] < ((1 - stopl)*maxp)) # Is in stop-loss?
insg <- (pricev[i] > ((1 + stopl)*minp)) # Is in start-gain?
    }  # end if
  }  # end for
  return(pnls)
} # end sim_stopstart

# Simulate stop-start strategy
pnls <- sim_stopstart(0.1)
# Combine the data
wealthv <- cbind(retp, pnls)
colnamev <- c("VTI", "Strategy")
colnames(wealthv) <- colnamev
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot the stop-loss strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="VTI Stop-Start Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Plot dygraph with shading
# Create colors for background shading
insl <- (pnls == 0) # Is in stop-loss?
indic <- (rutils::diffit(insl) != 0) # Indices of crosses
crossd <- c(datev[indic], datev[nrows]) # Dates of crosses
shadev <- ifelse(insl[indic] == 1, "antiquewhite", "lightgreen")
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(cumsum(wealthv),
  main="VTI Stop-Start Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
  dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot

# Simulate multiple stop-loss strategies
stopv <- 0.01*(1:30)
pnlc <- sapply(stopv, function(stopl) {
  sum(sim_stopstart(stopl))
})  # end sapply
stopl <- stopv[which.max(pnlc)]

# Plot cumulative pnls for stop-loss strategies
plot(x=stopv, y=pnlc,
   main="Cumulative PnLs for Stop-Start Strategies",
   xlab="stop-loss percent", ylab="cumulative pnl",
   t="l", lwd=3, col="blue")

# Calculate the USO prices and returns
pricev <- na.omit(rutils::etfenv$prices$USO)
nrows <- NROW(pricev)
datev <- zoo::index(pricev)
retp <- rutils::diffit(log(pricev))
# Simulate multiple stop-start strategies
stopv <- 0.01*(1:30)
pnlc <- sapply(stopv, function(stopl) {
  sum(sim_stopstart(stopl))
})  # end sapply

# Plot cumulative pnls for stop-start strategies
plot(x=stopv, y=pnlc,
   main="Cumulative PnLs for USO Stop-Start Strategies",
   xlab="stop-loss percent", ylab="cumulative pnl",
   t="l", lwd=3, col="blue")

# Simulate optimal stop-start strategy for USO
stopl <- stopv[which.max(pnlc)]
pnls <- sim_stopstart(stopl)
# Combine the data
wealthv <- cbind(retp, pnls)
colnamev <- c("USO", "Strategy")
colnames(wealthv) <- colnamev
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# dygraph plot the stop-start strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="USO Stop-Start Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Plot dygraph with shading
# Create colors for background shading
insl <- (pnls == 0) # Is in stop-loss?
indic <- (rutils::diffit(insl) != 0) # Indices of crosses
crossd <- c(datev[indic], datev[nrows]) # Dates of crosses
shadev <- ifelse(insl[indic] == 1, "antiquewhite", "lightgreen")
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(cumsum(wealthv),
  main="USO Stop-Start Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
  dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot

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
dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="red")
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
