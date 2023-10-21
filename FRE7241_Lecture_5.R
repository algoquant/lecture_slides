# Load daily S&P500 stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Select ETF returns
retetf <- rutils::etfenv$returns[, c("VTI", "XLK", "XLF", "XLE")]
# Calculate the MSFT betas with respect to different ETFs
betas <- sapply(retetf, function(retetf) {
  retp <- na.omit(cbind(retstock$MSFT, retetf))
  # Calculate the MSFT beta
  drop(cov(retp$MSFT, retp[, 2])/var(retp[, 2]))
}) # end sapply
# Combine MSFT and XLK returns
retp <- cbind(retstock$MSFT, rutils::etfenv$returns$XLK)
retp <- na.omit(retp)
colnames(retp) <- c("MSFT", "XLK")
# Calculate the beta and alpha of returns MSFT ~ XLK
betav <- drop(cov(retp$MSFT, retp$XLK)/var(retp$XLK))
alphav <- retp$MSFT - betav*retp$XLK
# Scatterplot of returns
plot(MSFT ~ XLK, data=retp, main="MSFT ~ XLK Returns",
     xlab="XLK", ylab="MSFT", pch=1, col="blue")
abline(a=mean(alphav), b=betav, col="red", lwd=2)

# dygraph plot of MSFT idiosyncratic returns vs XLK
endd <- rutils::calc_endpoints(retp, interval="weeks")
datev <- zoo::index(retp)[endd]
dateb <- datev[findInterval(as.Date("2014-01-01"), datev)] # Steve Balmer exit date
datav <- cbind(retp$XLK, alphav)
colnames(datav)[2] <- "MSFT alpha"
colnamev <- colnames(datav)
dygraphs::dygraph(cumsum(datav)[endd], main="MSFT Cumulative Alpha vs XLK") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=2) %>%
  dyEvent(dateb, label="Balmer exit", strokePattern="solid", color="red") %>%
  dyLegend(show="always", width=300)

# Calculate the trailing alphas and betas
lambda <- 0.9
covars <- HighFreq::run_covar(retp, lambda)
covars[1, ] <- 1.0
betav <- covars[, 1]/covars[, 3]
alphav <- retp$MSFT - betav*retp$XLK
# dygraph plot of trailing MSFT idiosyncratic returns vs XLK
datav <- cbind(retp$XLK, alphav)
colnames(datav)[2] <- "MSFT alpha"
colnamev <- colnames(datav)
dygraphs::dygraph(cumsum(datav)[endd], main="MSFT Trailing Cumulative Alpha vs XLK") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=2) %>%
  dyEvent(dateb, label="Balmer exit", strokePattern="solid", color="red") %>%
  dyLegend(show="always", width=300)

# Load daily S&P500 stock prices
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# Combine MSFT and XLK prices
pricev <- cbind(rutils::etfenv$prices$XLK, pricestock$MSFT)
pricev <- log(na.omit(pricev))
colnames(pricev) <- c("XLK", "MSFT")
datev <- zoo::index(pricev)
# Calculate the beta regression coefficient of prices MSFT ~ XLK
betav <- drop(cov(pricev$MSFT, pricev$XLK)/var(pricev$XLK))
# Calculate the cointegrated portfolio prices
pricec <- pricev$MSFT - betav*pricev$XLK
colnames(pricec) <- "MSFT Coint XLK"

# Scatterplot of MSFT and XLK prices
plot(MSFT ~ XLK, data=pricev, main="MSFT and XLK Prices",
     xlab="XLK", ylab="MSFT", pch=1, col="blue")
abline(a=mean(pricec), b=betav, col="red", lwd=2)
# Plot time series of prices
endd <- rutils::calc_endpoints(pricev, interval="weeks")
dygraphs::dygraph(pricev[endd], main="MSFT and XLK Log Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2)

# Plot histogram of the cointegrated portfolio prices
hist(pricec, breaks=100, xlab="Prices",
  main="Histogram of Cointegrated Portfolio Prices")
# Plot of cointegrated portfolio prices
datav <- cbind(pricev$XLK, pricec)[endd]
colnames(datav)[2] <- "Cointegrated Portfolio"
colnamev <- colnames(datav)
dygraphs::dygraph(datav, main="MSFT and XLK Cointegrated Portfolio Prices") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Perform ADF test on the individual stocks
sapply(pricev, tseries::adf.test, k=1)
# Perform ADF test on the cointegrated portfolio
tseries::adf.test(pricec, k=1)
# Perform ADF test for vector of cointegrating factors
betas <- seq(1.2, 2.2, 0.1)
adfstat <- sapply(betas, function(betav) {
  pricec <- (pricev$MSFT - betav*pricev$XLK)
  tseries::adf.test(pricec, k=1)$statistic
})  # end sapply
# Plot ADF statistics for vector of cointegrating factors
plot(x=betas, y=adfstat, type="l", xlab="cointegrating factor", ylab="statistic",
 main="ADF Test Statistic as Function of Cointegrating Factor")

# Plot of PACF of the cointegrated portfolio returns
pricen <- zoo::coredata(pricec) # Numeric price
retd <- rutils::diffit(pricen)
pacf(retd, lag=10, xlab=NA, ylab=NA,
     main="PACF of Cointegrated Portfolio Returns")
# Dygraphs plot of cointegrated portfolio prices
endd <- rutils::calc_endpoints(pricec, interval="weeks")
dygraphs::dygraph(pricec[endd], main=
  "MSFT and XLK Cointegrated Portfolio Prices") %>%
  dyOptions(colors=c("blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=200)

# Calculate the trailing mean prices and volatilities
lambda <- 0.9
meanv <- HighFreq::run_mean(pricen, lambda=lambda)
volat <- HighFreq::run_var(pricen, lambda=lambda)
volat <- sqrt(volat)
# Simulate the pairs Bollinger strategy
pricem <- pricen - meanv # De-meaned price
nrows <- NROW(pricec)
threshd <- rutils::lagit(volat)
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(pricem > threshd, -1, posv)
posv <- ifelse(pricem < -threshd, 1, posv)
posv <- zoo::na.locf(posv)
# Lag the positions to trade in the next period
posv <- rutils::lagit(posv, lagg=1)
# Calculate the pnls and the number of trades
retp <- rutils::diffit(pricev)
pnls <-  posv*(retp$MSFT - betav*retp$XLK)
ntrades <- sum(abs(rutils::diffit(posv)) > 0)

# Calculate the Sharpe ratios
wealthv <- cbind(retp$MSFT, pnls)
colnames(wealthv) <- c("MSFT", "Strategy")
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of pairs Bollinger strategy
colnamev <- colnames(wealthv)
captiont <- paste("Pairs Strategy", "/ \n",
  paste0(paste(colnamev[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number of trades=", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=200)

# Calculate the trailing cointegrated pair prices
covars <- HighFreq::run_covar(pricev, lambda)
betav <- covars[, 1]/covars[, 3]
pricec <- (pricev$MSFT - betav*pricev$XLK)
# Recalculate the mean of cointegrated portfolio prices
meanv <- HighFreq::run_mean(pricec, lambda=lambda)
vars <- sqrt(HighFreq::run_var(pricec, lambda=lambda))
# Simulate the pairs Bollinger strategy
pricen <- zoo::coredata(pricec) # Numeric price
pricem <- pricen - meanv # De-meaned price
threshd <- rutils::lagit(volat)
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(pricem > threshd, -1, posv)
posv <- ifelse(pricem < -threshd, 1, posv)
posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv, lagg=1)
# Calculate the pnls and the number of trades
retp <- rutils::diffit(pricev)
pnls <-  posv*(retp$MSFT - betav*retp$XLK)
ntrades <- sum(abs(rutils::diffit(posv)) > 0)

# Calculate the Sharpe ratios
wealthv <- cbind(retp$MSFT, pnls)
colnames(wealthv) <- c("MSFT", "Strategy")
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of pairs Bollinger strategy
colnamev <- colnames(wealthv)
captiont <- paste("Dynamic Pairs Strategy", "/ \n",
  paste0(paste(colnamev[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number of trades=", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=200)

# Calculate the trailing cointegrated pair prices
covars <- HighFreq::run_covar(pricev, lambda=0.95)
betav <- covars[, 1]/covars[, 3]
pricec <- (pricev$MSFT - betav*pricev$XLK)
# Recalculate the mean of cointegrated portfolio prices
meanv <- HighFreq::run_mean(pricec, lambda=0.3)
vars <- sqrt(HighFreq::run_var(pricec, lambda=0.3))
# Simulate the pairs Bollinger strategy
pricen <- zoo::coredata(pricec) # Numeric price
pricem <- pricen - meanv # De-meaned price
threshd <- rutils::lagit(volat)
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(pricem > threshd, -1, posv)
posv <- ifelse(pricem < -threshd, 1, posv)
posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv, lagg=1)
# Calculate the pnls and the number of trades
retp <- rutils::diffit(pricev)
pnls <-  posv*(retp$MSFT - betav*retp$XLK)
ntrades <- sum(abs(rutils::diffit(posv)) > 0)

# Calculate the Sharpe ratios
wealthv <- cbind(retp$MSFT, pnls)
colnames(wealthv) <- c("MSFT", "Strategy")
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of pairs Bollinger strategy
colnamev <- colnames(wealthv)
captiont <- paste("Dynamic Pairs Slow Beta", "/ \n",
  paste0(paste(colnamev[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number of trades=", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=200)

# Load daily S&P500 stock prices
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# Select the prices without NAs
datev <- zoo::index(na.omit(pricestock$GOOG))
pricestock <- pricestock[datev]
numna <- sapply(pricestock, function(x) sum(is.na(x)))
pricestock <- pricestock[, numna == 0]
nrows <- NROW(pricestock)
# Calculate the percentage stock returns
retp <- lapply(pricestock, function(x) xts::diff.xts(x)/rutils::lagit(x))
retp <- rutils::do_call(cbind, retp)
retp[1, ] <- 0
# Calculate the percentage VTI returns
retvti <- rutils::etfenv$returns$VTI[datev]
colnames(retvti) <- "VTI"
retvti <- retvti[datev]

# Wealth of price-weighted (fixed shares) portfolio
wealthfs <- rowMeans(cumprod(1 + retp))
# Wealth of equal-dollar-weighted portfolio
wealthew <- cumprod(1 + rowMeans(retp))

# Calculate combined log wealth
wealthv <- cbind(wealthfs, wealthew)
wealthv <- log(wealthv)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Fixed shares", "Equal dollars")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv), function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of combined log wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(wealthv[endd],
  main="Wealth of Fixed Share and Equal Dollar Portfolios") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Select a random, fixed share sub-portfolio of 5 stocks
set.seed(1121)
nstocks <- NCOL(retp)
samplev <- sample.int(n=nstocks, size=5, replace=FALSE)
wealthr <- rowMeans(cumprod(1 + retp[, samplev]))

# Plot dygraph of all stocks and random sub-portfolio
wealthv <- cbind(wealthfs, wealthr)
wealthv <- log(wealthv)
wealthv <- xts::xts(wealthv, order.by=datev)
colnames(wealthv) <- c("All stocks", "Random sub-portfolio")
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(wealthv[endd], main="Stock Index and Random Portfolio") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Select 10 random fixed share sub-portfolios
set.seed(1121)
nportf <- 10
wealthr <- sapply(1:nportf, function(x) {
  samplev <- sample.int(n=nstocks, size=5, replace=FALSE)
  rowMeans(cumprod(1 + retp[, samplev]))
})  # end sapply
wealthr <- xts::xts(wealthr, order.by=datev)
colnames(wealthr) <- paste0("portf", 1:nportf)
# Sort the sub-portfolios according to performance
wealthr <- wealthr[, order(wealthr[nrows])]
round(head(wealthr), 3)
round(tail(wealthr), 3)

# Plot dygraph of all stocks and random sub-portfolios
colorv <- colorRampPalette(c("red", "blue"))(nportf)
colorv <- c("green", colorv)
wealthv <- cbind(wealthfs, wealthr)
wealthv <- log(wealthv)
colnames(wealthv)[1] <- "All stocks"
colnamev <- colnames(wealthv)
dygraphs::dygraph(wealthv[endd], main="All Stocks and Random Sub-Portfolios") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name=colnamev[1], label=colnamev[1], strokeWidth=3) %>%
  dyLegend(show="always", width=500)

# Define in-sample and out-of-sample intervals
cutoff <- nrows %/% 2
datev[cutoff]
insample <- 1:cutoff
outsample <- (cutoff + 1):nrows
# Calculate the 10 best performing stocks in-sample
pricev <- cumprod(1 + retp)
pricet <- pricev[cutoff, ]
pricet <- drop(coredata(pricet))
pricet <- sort(pricet, decreasing=TRUE)
symbolv <- names(head(pricet, 10))
# Calculate the wealth of the 10 best performing stocks
wealthv <- rowMeans(pricev[, symbolv])

# Combine the fixed share wealth with the 10 best performing stocks
wealthv <- cbind(wealthfs, wealthv)
wealthv <- xts::xts(log(wealthv), order.by=datev)
colnames(wealthv) <- c("All stocks", "Best performing")
# Calculate the in-sample Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv[insample, ]),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv[outsample, ]),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot out-of-sample stock portfolio returns
dygraphs::dygraph(wealthv[endd], main="Out-of-Sample Log Prices of Stock Portfolio") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(datev[cutoff], label="in-sample", strokePattern="solid", color="green") %>%
  dyLegend(width=300)

# Calculate the stock volatilities, betas, and alphas
# Perform parallel loop under Mac-OSX or Linux
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
riskret <- mclapply(retp, function(retp) {
  retp <- na.omit(retp)
  stdev <- sd(retp)
  retvti <- retvti[zoo::index(retp)]
  varvti <- drop(var(retvti))
  meanvti <- mean(retvti)
  betav <- drop(cov(retp, retvti))/varvti
  resid <- retp - betav*retvti
  alphav <- mean(retp) - betav*meanvti
  c(alpha=alphav, beta=betav, stdev=stdev, ivol=sd(resid))
}, mc.cores=ncores)  # end mclapply
riskret <- do.call(rbind, riskret)
tail(riskret)
# Calculate the median volatility
riskv <- riskret[, "stdev"]
medianv <- median(riskv)

# Calculate the returns of low and high volatility stocks
retlow <- rowMeans(retp[, names(riskv[riskv <= medianv])], na.rm=TRUE)
rethigh <- rowMeans(retp[, names(riskv[riskv > medianv])], na.rm=TRUE)
wealthv <- cbind(retlow, rethigh, retlow - 0.25*rethigh)
wealthv <- xts::xts(wealthv, order.by=datev)
colnamev <- c("low_vol", "high_vol", "long_short")
colnames(wealthv) <- colnamev
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative returns of low and high volatility stocks
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Volatility Stocks In-Sample") %>%
  dySeries(name=colnamev[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colnamev[2], col="red", strokeWidth=1) %>%
  dySeries(name=colnamev[3], col="green", strokeWidth=2) %>%
  dyLegend(width=300)

# Merton-Henriksson test
predm <- cbind(VTI=retvti, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(predm)[2:3] <- c("merton", "treynor")
regmod <- lm(wealthv$long_short ~ VTI + merton, data=predm); summary(regmod)
# Treynor-Mazuy test
regmod <- lm(wealthv$long_short ~ VTI + treynor, data=predm); summary(regmod)
# Plot residual scatterplot
resids <- regmod$residuals
plot.default(x=retvti, y=resids, xlab="VTI", ylab="Low Volatility")
title(main="Treynor-Mazuy Market Timing Test\n for Low Volatility vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retvti, y=fitv, pch=16, col="red")
text(x=0.0, y=max(resids), paste("Treynor test t-value =", tvalue))

# Calculate the in-sample stock volatilities, betas, and alphas
riskretis <- mclapply(retp[insample], function(retp) {
  combv <- na.omit(cbind(retp, retvti))
  if (NROW(combv) > 11) {
    retp <- na.omit(retp)
    stdev <- sd(retp)
    retvti <- retvti[zoo::index(retp)]
    varvti <- drop(var(retvti))
    meanvti <- mean(retvti)
    betav <- drop(cov(retp, retvti))/varvti
    resid <- retp - betav*retvti
    alphav <- mean(retp) - betav*meanvti
    return(c(alpha=alphav, beta=betav, stdev=stdev, ivol=sd(resid)))
  } else {
    return(c(alpha=0, beta=0, stdev=0, ivol=0))
  }  # end if
}, mc.cores=ncores)  # end mclapply
riskretis <- do.call(rbind, riskretis)
tail(riskretis)
# Calculate the median volatility
riskv <- riskretis[, "stdev"]
medianv <- median(riskv)
# Calculate the out-of-sample returns of low and high volatility stocks
retlow <- rowMeans(retp[outsample, names(riskv[riskv <= medianv])], na.rm=TRUE)
rethigh <- rowMeans(retp[outsample, names(riskv[riskv > medianv])], na.rm=TRUE)
wealthv <- cbind(retlow, rethigh, retlow - 0.25*rethigh)
wealthv <- xts::xts(wealthv, order.by=datev[outsample])
colnamev <- c("low_vol", "high_vol", "long_short")
colnames(wealthv) <- colnamev

# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative returns of low and high volatility stocks
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Volatility Stocks Out-Of-Sample") %>%
  dySeries(name=colnamev[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colnamev[2], col="red", strokeWidth=1) %>%
  dySeries(name=colnamev[3], col="green", strokeWidth=2) %>%
  dyLegend(width=300)

# Calculate the median idiosyncratic volatility
riskv <- riskret[, "ivol"]
medianv <- median(riskv)
# Calculate the returns of low and high idiosyncratic volatility stocks
retlow <- rowMeans(retp[, names(riskv[riskv <= medianv])], na.rm=TRUE)
rethigh <- rowMeans(retp[, names(riskv[riskv > medianv])], na.rm=TRUE)
wealthv <- cbind(retlow, rethigh, retlow - 0.25*rethigh)
wealthv <- xts::xts(wealthv, order.by=datev)
colnamev <- c("low_vol", "high_vol", "long_short")
colnames(wealthv) <- colnamev

# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of returns of low and high idiosyncratic volatility stocks
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Idiosyncratic Volatility Stocks In-Sample") %>%
  dySeries(name=colnamev[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colnamev[2], col="red", strokeWidth=1) %>%
  dySeries(name=colnamev[3], col="green", strokeWidth=2) %>%
  dyLegend(width=300)

# Merton-Henriksson test
predm <- cbind(VTI=retvti, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(predm)[2:3] <- c("merton", "treynor")
regmod <- lm(wealthv$long_short ~ VTI + merton, data=predm); summary(regmod)
# Treynor-Mazuy test
regmod <- lm(wealthv$long_short ~ VTI + treynor, data=predm); summary(regmod)
# Plot residual scatterplot
resids <- regmod$residuals
plot.default(x=retvti, y=resids, xlab="VTI", ylab="Low Volatility")
title(main="Treynor-Mazuy Market Timing Test\n for Low Idiosyncratic Volatility vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retvti, y=fitv, pch=16, col="red")
text(x=0.0, y=max(resids), paste("Treynor test t-value =", tvalue))

# Calculate the median in-sample idiosyncratic volatility
riskv <- riskretis[, "ivol"]
medianv <- median(riskv)
# Calculate the out-of-sample returns of low and high idiosyncratic volatility stocks
retlow <- rowMeans(retp[outsample, names(riskv[riskv <= medianv])], na.rm=TRUE)
rethigh <- rowMeans(retp[outsample, names(riskv[riskv > medianv])], na.rm=TRUE)
wealthv <- cbind(retlow, rethigh, retlow - 0.25*rethigh)
wealthv <- xts::xts(wealthv, order.by=datev[outsample])
colnamev <- c("low_vol", "high_vol", "long_short")
colnames(wealthv) <- colnamev

# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of out-of-sample returns of low and high volatility stocks
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Idiosyncratic Volatility Stocks Out-Of-Sample") %>%
  dySeries(name=colnamev[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colnamev[2], col="red", strokeWidth=1) %>%
  dySeries(name=colnamev[3], col="green", strokeWidth=2) %>%
  dyLegend(width=300)

# Calculate the median beta
riskv <- riskret[, "beta"]
medianv <- median(riskv)
# Calculate the returns of low and high beta stocks
betalow <- rowMeans(retp[, names(riskv[riskv <= medianv])], na.rm=TRUE)
betahigh <- rowMeans(retp[, names(riskv[riskv > medianv])], na.rm=TRUE)
wealthv <- cbind(betalow, betahigh, betalow - 0.25*betahigh)
wealthv <- xts::xts(wealthv, order.by=datev)
colnamev <- c("low_beta", "high_beta", "long_short")
colnames(wealthv) <- colnamev

# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative returns of low and high beta stocks
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Beta Stocks In-Sample") %>%
  dySeries(name=colnamev[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colnamev[2], col="red", strokeWidth=1) %>%
  dySeries(name=colnamev[3], col="green", strokeWidth=2) %>%
  dyLegend(width=300)

# Merton-Henriksson test
predm <- cbind(VTI=retvti, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(predm)[2:3] <- c("merton", "treynor")
regmod <- lm(wealthv$long_short ~ VTI + merton, data=predm); summary(regmod)
# Treynor-Mazuy test
regmod <- lm(wealthv$long_short ~ VTI + treynor, data=predm); summary(regmod)
# Plot residual scatterplot
resids <- regmod$residuals
plot.default(x=retvti, y=resids, xlab="VTI", ylab="Low Beta")
title(main="Treynor-Mazuy Market Timing Test\n for Low Beta vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retvti, y=fitv, pch=16, col="red")
text(x=0.0, y=max(resids), paste("Treynor test t-value =", tvalue))

# Calculate the median beta
riskv <- riskretis[, "beta"]
medianv <- median(riskv)
# Calculate the out-of-sample returns of low and high beta stocks
betalow <- rowMeans(retp[outsample, names(riskv[riskv <= medianv])], na.rm=TRUE)
betahigh <- rowMeans(retp[outsample, names(riskv[riskv > medianv])], na.rm=TRUE)
wealthv <- cbind(betalow, betahigh, betalow - 0.25*betahigh)
wealthv <- xts::xts(wealthv, order.by=datev[outsample])
colnamev <- c("low_beta", "high_beta", "long_short")
colnames(wealthv) <- colnamev

# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of out-of-sample returns of low and high beta stocks
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Beta Stocks Out-Of-Sample") %>%
  dySeries(name=colnamev[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colnamev[2], col="red", strokeWidth=1) %>%
  dySeries(name=colnamev[3], col="green", strokeWidth=2) %>%
  dyLegend(width=300)

# Calculate the percentage stock returns
retp <- lapply(pricestock, function(x) xts::diff.xts(x)/rutils::lagit(x))
retp <- rutils::do_call(cbind, retp)
retp[1, ] <- 0
# Calculate the dollar returns
retd <- rutils::diffit(pricestock)
# Calculate the wealth of proportional dollar allocations (with rebalancing)
wealthpd <- cumprod(1 + rowMeans(retp))
# Calculate the trailing dollar volatilities
volat <- HighFreq::run_var(retd, lambda=0.15)
volat <- sqrt(volat)
volat <- rutils::lagit(volat)
volat[volat == 0] <- 1
# Calculate the standardized prices with unit dollar volatility
pricerp <- pricestock/volat
# Scale the sum of stock prices to $1
pricerp <- pricerp/rowMeans(pricerp)
# Calculate the risk parity dollar returns
retrp <- retp*pricerp
# Calculate the wealth of risk parity
wealthrp <- 1 + cumsum(rowMeans(retrp))

# Combined wealth
wealthv <- cbind(wealthpd, wealthrp)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Prop dollar", "Risk parity")
wealthv <- log(wealthv)
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv), function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot of log wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(wealthv[endd],
  main="Wealth of Proportional Dollar and Risk Parity Portfolios") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=400)

# Calculate the median volatilities
medianv <- matrixStats::rowMedians(volat)
# Calculate the wealth of low volatility stocks
weightm <- (volat <= medianv)
weightm <- rutils::lagit(weightm)
retlow <- rowMeans(weightm*retp, na.rm=TRUE)
# Calculate the wealth of high volatility stocks
weightm <- (volat > medianv)
weightm <- rutils::lagit(weightm)
rethigh <- rowMeans(weightm*retp, na.rm=TRUE)

# Combined wealth
wealthv <- cbind(retlow, rethigh)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Low Volatility", "High Volatility")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of log wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Wealth of Low and High Volatility Stocks") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the long-short volatility returns
retls <- (retlow - 0.5*rethigh)
retls <- retls*sd(rutils::diffit(log(wealthpd)))/sd(retls)
wealthls <- cumprod(1 + retls)
# Combined wealth
wealthv <- cbind(wealthpd, wealthls)
wealthv <- xts::xts(wealthv, datev)
colnamev <- c("Prop dollar", "Long-Short Vol")
colnames(wealthv) <- colnamev
wealthv <- log(wealthv)
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv), function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot of log wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(wealthv[endd],
  main="Wealth of Price-weighted and Long-Short Vol Portfolios") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Select ETF symbols
symbolv <- c("IEF", "DBC", "XLU", "XLF", "XLP", "XLI")
# Calculate ETF prices and log returns
pricev <- rutils::etfenv$prices[, symbolv]
# Applying zoo::na.locf() can produce bias of the correlations
# pricev <- zoo::na.locf(pricev, na.rm=FALSE)
# pricev <- zoo::na.locf(pricev, fromLast=TRUE)
pricev <- na.omit(pricev)
retp <- rutils::diffit(log(pricev))
# Calculate covariance matrix
covmat <- cov(retp)
# Standardize (de-mean and scale) the returns
retp <- lapply(retp, function(x) {(x - mean(x))/sd(x)})
retp <- rutils::do_call(cbind, retp)
round(sapply(retp, mean), 6)
sapply(retp, sd)
# Alternative (much slower) center (de-mean) and scale the returns
# retp <- apply(retp, 2, scale)
# retp <- xts::xts(retp, zoo::index(pricev))
# Alternative (much slower) center (de-mean) and scale the returns
# retp <- scale(retp, center=TRUE, scale=TRUE)
# retp <- xts::xts(retp, zoo::index(pricev))
# Alternative (much slower) center (de-mean) and scale the returns
# retp <- t(retp) - colMeans(retp)
# retp <- retp/sqrt(rowSums(retp^2)/(NCOL(retp)-1))
# retp <- t(retp)
# retp <- xts::xts(retp, zoo::index(pricev))

# Calculate correlation matrix
cormat <- cor(retp)
# Reorder correlation matrix based on clusters
library(corrplot)
ordern <- corrMatOrder(cormat, order="hclust",
  hclust.method="complete")
cormat <- cormat[ordern, ordern]
# Plot the correlation matrix
colorv <- colorRampPalette(c("red", "white", "blue"))
# x11(width=6, height=6)
corrplot(cormat, title=NA, tl.col="black", mar=c(0,0,0,0),
    method="square", col=colorv(NCOL(cormat)), tl.cex=0.8,
    cl.offset=0.75, cl.cex=0.7, cl.align.text="l", cl.ratio=0.25)
title("ETF Correlation Matrix", line=2)
# Draw rectangles on the correlation matrix plot
corrRect.hclust(cormat, k=NROW(cormat) %/% 2,
  method="complete", col="red")

# Create initial vector of portfolio weights
nweights <- NROW(symbolv)
weightv <- rep(1/sqrt(nweights), nweights)
names(weightv) <- symbolv
# Objective function equal to minus portfolio variance
objfun <- function(weightv, retp) {
  retp <- retp %*% weightv
  -sum(retp^2) + 1e4*(1 - sum(weightv^2))^2
}  # end objfun
# Objective for equal weight portfolio
objfun(weightv, retp)
# Compare speed of vector multiplication methods
summary(microbenchmark(
  transp=(t(retp[, 1]) %*% retp[, 1]),
  sumv=sum(retp[, 1]^2),
  times=10))[, c(1, 4, 5)]

# Find weights with maximum variance
optiml <- optim(par=weightv,
  fn=objfun,
  retp=retp,
  method="L-BFGS-B",
  upper=rep(10.0, nweights),
  lower=rep(-10.0, nweights))
# Optimal weights and maximum variance
weights1 <- optiml$par
-objfun(weights1, retp)
# Plot first principal component weights
barplot(weights1, names.arg=names(weights1), xlab="", ylab="",
  main="First Principal Component Weights")

# PC1 returns
pc1 <- drop(retp %*% weights1)
# Redefine objective function
objfun <- function(weightv, retp) {
  retp <- retp %*% weightv
  -sum(retp^2) + 1e4*(1 - sum(weightv^2))^2 +
    1e4*(sum(weights1*weightv))^2
}  # end objfun
# Find second PC weights using parallel DEoptim
optiml <- DEoptim::DEoptim(fn=objfun,
  upper=rep(10, NCOL(retp)),
  lower=rep(-10, NCOL(retp)),
  retp=retp, control=list(parVar="weights1",
    trace=FALSE, itermax=1000, parallelType=1))

# PC2 weights
weights2 <- optiml$optim$bestmem
names(weights2) <- colnames(retp)
sum(weights2^2)
sum(weights1*weights2)
# PC2 returns
pc2 <- drop(retp %*% weights2)
# Plot second principal component loadings
barplot(weights2, names.arg=names(weights2), xlab="", ylab="",
  main="Second Principal Component Loadings")

# Calculate the eigenvalues and eigenvectors
eigend <- eigen(cormat)
eigend$vectors
# Compare with optimization
all.equal(sum(diag(cormat)), sum(eigend$values))
all.equal(abs(eigend$vectors[, 1]), abs(weights1), check.attributes=FALSE)
all.equal(abs(eigend$vectors[, 2]), abs(weights2), check.attributes=FALSE)
all.equal(eigend$values[1], var(pc1), check.attributes=FALSE)
all.equal(eigend$values[2], var(pc2), check.attributes=FALSE)
# Eigenvalue equations
(cormat %*% weights1) / weights1 / var(pc1)
(cormat %*% weights2) / weights2 / var(pc2)
# Plot eigenvalues
barplot(eigend$values, names.arg=paste0("PC", 1:nweights),
  las=3, xlab="", ylab="", main="Principal Component Variances")

# Calculate the eigen decomposition of the correlation matrix
eigend <- eigen(cormat)
# Perform PCA with scaling
pcad <- prcomp(retp, scale=TRUE)
# Compare outputs
all.equal(eigend$values, pcad$sdev^2)
all.equal(abs(eigend$vectors), abs(pcad$rotation),
    check.attributes=FALSE)
# Eigen decomposition of covariance matrix
eigend <- eigen(covmat)
# Perform PCA without scaling
pcad <- prcomp(retp, scale=FALSE)
# Compare outputs
all.equal(eigend$values, pcad$sdev^2)
all.equal(abs(eigend$vectors), abs(pcad$rotation),
    check.attributes=FALSE)

# Redefine objective function to minimize variance
objfun <- function(weightv, retp) {
  retp <- retp %*% weightv
  sum(retp^2) + 1e4*(1 - sum(weightv^2))^2
}  # end objfun
# Find highest order PC weights using parallel DEoptim
optiml <- DEoptim::DEoptim(fn=objfun,
  upper=rep(10, NCOL(retp)),
  lower=rep(-10, NCOL(retp)),
  retp=retp, control=list(trace=FALSE,
    itermax=1000, parallelType=1))
# PC6 weights and returns
weights6 <- optiml$optim$bestmem
names(weights6) <- colnames(retp)
sum(weights6^2)
sum(weights1*weights6)
# Compare with eigend vector
weights6
eigend$vectors[, 6]
# Calculate objective function
objfun(weights6, retp)
objfun(eigend$vectors[, 6], retp)

# Plot highest order principal component loadings
weights6 <- eigend$vectors[, 6]
names(weights6) <- colnames(retp)
barplot(weights6, names.arg=names(weights6), xlab="", ylab="",
  main="Highest Order Principal Component Loadings")

# Perform principal component analysis PCA
pcad <- prcomp(retp, scale=TRUE)
# Plot standard deviations of principal components
barplot(pcad$sdev, names.arg=colnames(pcad$rotation),
  las=3, xlab="", ylab="",
  main="Scree Plot: Volatilities of Principal Components \n of ETF Returns")
# Calculate the number of principal components which sum up to at least 80% of the total variance
pcavar <- pcad$sdev^2
which(cumsum(pcavar)/sum(pcavar) > 0.8)[1]

# Plot barplots with PCA loadings (weights) in multiple panels
pcad$rotation
# x11(width=6, height=7)
par(mfrow=c(nweights/2, 2))
par(mar=c(3, 2, 2, 1), oma=c(0, 0, 0, 0))
for (ordern in 1:nweights) {
  barplot(pcad$rotation[, ordern], las=3, xlab="", ylab="", main="")
  title(paste0("PC", ordern), line=-1, col.main="red")
}  # end for

# Calculate products of principal component time series
round(t(pcad$x) %*% pcad$x, 2)
# Calculate principal component time series from returns
datev <- zoo::index(pricev)
retpca <- xts::xts(retp %*% pcad$rotation, order.by=datev)
round(cov(retpca), 3)
all.equal(coredata(retpca), pcad$x, check.attributes=FALSE)
pcacum <- cumsum(retpca)
# Plot principal component time series in multiple panels
rangev <- range(pcacum)
for (ordern in 1:nweights) {
  plot.zoo(pcacum[, ordern], ylim=rangev, xlab="", ylab="")
  title(paste0("PC", ordern), line=-1, col.main="red")
}  # end for

# Invert all the principal component time series
retpca <- retp %*% pcad$rotation
solved <- retpca %*% solve(pcad$rotation)
all.equal(coredata(retp), solved)
# Invert first 3 principal component time series
solved <- retpca[, 1:3] %*% solve(pcad$rotation)[1:3, ]
solved <- xts::xts(solved, datev)
solved <- cumsum(solved)
retc <- cumsum(retp)
# Plot the solved returns
for (symbol in symbolv) {
  plot.zoo(cbind(retc[, symbol], solved[, symbol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n", legend=paste0(symbol, c("", " solved")), y.intersp=0.4,
   title=NULL, inset=0.0, cex=1.0, lwd=6, lty=1, col=c("black", "blue"))
}  # end for

# Create a matrix with low correlation
ndata <- 10
cormat <- matrix(rep(0.1, ndata^2), nc=ndata)
diag(cormat) <- rep(1, ndata)
# Calculate the condition number
eigend <- eigen(cormat)
eigenval <- eigend$values
max(eigenval)/min(eigenval)
# Create a matrix with high correlation
cormat <- matrix(rep(0.9, ndata^2), nc=ndata)
diag(cormat) <- rep(1, ndata)
# Calculate the condition number
eigend <- eigen(cormat)
eigenval <- eigend$values
max(eigenval)/min(eigenval)

# Calculate the condition numbers as function correlation
corvec <- seq(0.1, 0.9, 0.1)
condvec <- sapply(corvec, function(corv) {
  cormat <- matrix(rep(corv, ndata^2), nc=ndata)
  diag(cormat) <- rep(1, ndata)
  eigend <- eigen(cormat)
  eigenval <- eigend$values
  max(eigenval)/min(eigenval)
})  # end sapply
# Plot the condition numbers
plot(x=corvec, y=condvec, t="l",
  main="Condition Number as Function of Correlation",
  xlab="correlation", ylab="condition number")

# Simulate uncorrelated stock returns
nstocks <- 10
nrows <- 100
set.seed(1121)  # Initialize random number generator
retp <- matrix(rnorm(nstocks*nrows), nc=nstocks)
# Calculate the condition numbers as function of number of observations
obsvec <- seq(20, nrows, 10)
condvec <- sapply(obsvec, function(nobs) {
  cormat <- cor(retp[1:nobs, ])
  eigend <- eigen(cormat)
  eigenval <- eigend$values
  max(eigenval)/min(eigenval)
})  # end sapply

# Plot the condition numbers
plot(x=obsvec, y=condvec, t="l",
  main="Condition Number as Function of Number of Observations",
  xlab="number of observations", ylab="condition number")

# Load daily S&P500 log percentage stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Calculate the number of NA values in returns
retp <- retstock
colSums(is.na(retp))
# Calculate the correlations ignoring NA values
cor(retp$DAL, retp$FOXA, use="pairwise.complete.obs")
cor(na.omit(retp[, c("DAL", "FOXA")]))[2]
cormat <- cor(retp, use="pairwise.complete.obs")
sum(is.na(cormat))
cormat[is.na(cormat)] <- 0

# Perform principal component analysis PCA - produces error
pcad <- prcomp(retp, scale=TRUE)
# Calculate the eigen decomposition of the correlation matrix
eigend <- eigen(cormat)
# Calculate the eigenvalues and eigenvectors
eigenval <- eigend$values
eigenvec <- eigend$vectors
# Calculate the number of negative eigenvalues
sum(eigenval<0)
# Calculate the condition number
max(eigenval)/min(abs(eigenval))
# Calculate the number of eigenvalues which sum up to at least 80% of the total variance
which(cumsum(eigenval)/sum(eigenval) > 0.8)[1]

# Plot the eigenvalues
barplot(eigenval, xlab="", ylab="", las=3,
  names.arg=paste0("ev", 1:NROW(eigenval)),
  main="Eigenvalues of Stock Correlation Matrix")

# Calculate the stock variance
varv <- sapply(retp, var, na.rm=TRUE)
# Calculate the returns of low and high volatility stocks
nstocks <- NCOL(retp)
medianv <- median(varv)
retlow <- retp[, varv <= medianv]
rethigh <- retp[, varv > medianv]
# Calculate the correlations of low volatility stocks
cormat <- cor(retlow, use="pairwise.complete.obs")
cormat[is.na(cormat)] <- 0
# Calculate the mean correlations
mean(cormat[upper.tri(cormat)])
# Calculate the eigen decomposition of the correlation matrix
eigend <- eigen(cormat)
eigenval <- eigend$values
# Calculate the number of negative eigenvalues
sum(eigenval < 0)
# Calculate the number of eigenvalues which sum up to at least 80% of the total variance
which(cumsum(eigenval)/sum(eigenval) > 0.8)[1]
# Calculate the condition number
max(eigenval)/min(abs(eigenval))
# Calculate the correlations of high volatility stocks
cormat <- cor(rethigh, use="pairwise.complete.obs")
cormat[is.na(cormat)] <- 0
# Calculate the mean correlations
mean(cormat[upper.tri(cormat)])
# Calculate the eigen decomposition of the correlation matrix
eigend <- eigen(cormat)
eigenval <- eigend$values
# Calculate the number of negative eigenvalues
sum(eigenval < 0)
# Calculate the number of eigenvalues which sum up to at least 80% of the total variance
which(cumsum(eigenval)/sum(eigenval) > 0.8)[1]
# Calculate the condition number
max(eigenval)/min(abs(eigenval))

# Subset (select) the stock returns after the start date of VTI
retvti <- na.omit(rutils::etfenv$returns$VTI)
colnames(retvti) <- "VTI"
retp <- retstock[zoo::index(retvti)]
datev <- zoo::index(retp)
retvti <- retvti[datev]
nrows <- NROW(retp)
nstocks <- NCOL(retp)
head(retp[, 1:5])
# Calculate the monthly end points
endd <- rutils::calc_endpoints(retvti, interval="months")
retvti[head(endd)]
retvti[tail(endd)]
# Remove stub interval at the end
endd <- endd[-NROW(endd)]
npts <- NROW(endd)
# Calculate the monthly stock volatilities and correlations
stdcor <- sapply(2:npts, function(endp) {
  # cat("endp = ", endp, "\n")
  retp <- retp[endd[endp-1]:endd[endp]]
  cormat <- cor(retp, use="pairwise.complete.obs")
  cormat[is.na(cormat)] <- 0
  c(stdev=sd(retvti[endd[endp-1]:endd[endp]]),
    cor=mean(cormat[upper.tri(cormat)]))
})  # end sapply
stdcor <- t(stdcor)

# Scatterplot of stock volatilities and correlations
plot(x=stdcor[, "stdev"], y=stdcor[, "cor"],
 xlab="volatility", ylab="correlation",
 main="Monthly Stock Volatilities and Correlations")
# Plot stock volatilities and correlations
colnamev <- colnames(stdcor)
stdcor <- xts(stdcor, zoo::index(retvti[endd]))
dygraphs::dygraph(stdcor,
  main="Monthly Stock Volatilities and Correlations") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)

# Calculate the median VTI volatility
medianv <- median(stdcor[, "stdev"])
# Calculate the stock returns of low volatility intervals
retlow <- lapply(2:npts, function(endp) {
  if (stdcor[endp-1, "stdev"] <= medianv)
    retp[endd[endp-1]:endd[endp]]
})  # end lapply
retlow <- rutils::do_call(rbind, retlow)
# Calculate the stock returns of high volatility intervals
rethigh <- lapply(2:npts, function(endp) {
  if (stdcor[endp-1, "stdev"] > medianv)
    retp[endd[endp-1]:endd[endp]]
})  # end lapply
rethigh <- rutils::do_call(rbind, rethigh)

# Calculate the correlations of low volatility intervals
cormat <- cor(retlow, use="pairwise.complete.obs")
cormat[is.na(cormat)] <- 0
mean(cormat[upper.tri(cormat)])
# Calculate the eigen decomposition of the correlation matrix
eigend <- eigen(cormat)
eigenval <- eigend$values
sum(eigenval < 0)
# Calculate the number of eigenvalues which sum up to at least 80% of the total variance
which(cumsum(eigenval)/sum(eigenval) > 0.8)[1]
# Calculate the condition number
max(eigenval)/min(abs(eigenval))
# Calculate the correlations of high volatility intervals
cormat <- cor(rethigh, use="pairwise.complete.obs")
cormat[is.na(cormat)] <- 0
mean(cormat[upper.tri(cormat)])
# Calculate the eigen decomposition of the correlation matrix
eigend <- eigen(cormat)
eigenval <- eigend$values
sum(eigenval < 0)
# Calculate the number of eigenvalues which sum up to at least 80% of the total variance
which(cumsum(eigenval)/sum(eigenval) > 0.8)[1]
# Calculate the condition number
max(eigenval)/min(abs(eigenval))

# Calculate AAPL and XLK returns
retp <- na.omit(cbind(returns$AAPL, rutils::etfenv$returns$XLK))
# Calculate the trailing correlations
lambda <- 0.99
covarv <- HighFreq::run_covar(retp, lambda)
correlv <- covarv[, 1, drop=FALSE]/sqrt(covarv[, 2]*covarv[, 3])

# Plot dygraph of XLK returns and AAPL correlations
datav <- cbind(cumsum(retp$XLK), correlv)
colnames(datav)[2] <- "correlation"
colnamev <- colnames(datav)
endd <- rutils::calc_endpoints(retp, interval="weeks")
dygraphs::dygraph(datav[endd], main="AAPL Correlations With XLK") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)

# Scatterplot of trailing stock volatilities and correlations
volv <- sqrt(covarv[, 2])
plot(x=volv[endd], y=correlv[endd, ], pch=1, col="blue",
 xlab="AAPL volatility", ylab="Correlation",
 main="Trailing Volatilities and Correlations of AAPL vs XLK")
# Interactive scatterplot of trailing stock volatilities and correlations
datev <- zoo::index(retp[endd])
datav <- data.frame(datev, volv[endd], correlv[endd, ])
colnames(datav) <- c("date", "volatility", "correlation")
library(plotly)
plotly::plot_ly(data=datav, x=~volatility, y=~correlation,
  type="scatter", mode="markers", text=datev) %>%
  layout(title="Trailing Volatilities and Correlations of AAPL vs XLK")

# Plot trailing stock volatilities and correlations
datav <- xts(cbind(volv, correlv), zoo::index(retp))
colnames(datav) <- c("volatility", "correlation")
colnamev <- colnames(datav)
dygraphs::dygraph(datav[endd], main="AAPL Trailing Stock Volatility and Correlation") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)

# Calculate portfolio returns
retvti <- na.omit(rutils::etfenv$returns$VTI)
colnames(retvti) <- "VTI"
datev <- zoo::index(retvti)
retp <- returns100
retp[is.na(retp)] <- 0
retp <- retp[datev]
nrows <- NROW(retp)
nstocks <- NCOL(retp)
head(retp[, 1:5])
# Calculate the average trailing portfolio correlations
lambda <- 0.9
correlv <- sapply(retp, function(retp) {
  covarv <- HighFreq::run_covar(cbind(retvti, retp), lambda)
  covarv[, 1, drop=FALSE]/sqrt(covarv[, 2]*covarv[, 3])
})  # end sapply
correlv[is.na(correlv)] <- 0
correlp <- rowMeans(correlv)
# Scatterplot of trailing stock volatilities and correlations
volvti <- sqrt(HighFreq::run_var(retvti, lambda))
endd <- rutils::calc_endpoints(retvti, interval="weeks")
plot(x=volvti[endd], y=correlp[endd],
 xlab="volatility", ylab="correlation",
 main="Trailing Stock Volatilities and Correlations")

# Plot trailing stock volatilities and correlations
datav <- xts(cbind(volvti, correlp), datev)
colnames(datav) <- c("volatility", "correlation")
colnamev <- colnames(datav)
dygraphs::dygraph(datav[endd],
  main="Trailing Stock Volatilities and Correlations") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)
