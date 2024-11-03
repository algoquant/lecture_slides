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
betac <- drop(cov(retp$MSFT, retp$XLK)/var(retp$XLK))
alphac <- retp$MSFT - betac*retp$XLK
# Scatterplot of returns
plot(MSFT ~ XLK, data=retp, main="MSFT ~ XLK Returns",
     xlab="XLK", ylab="MSFT", pch=1, col="blue")
abline(a=mean(alphac), b=betac, col="red", lwd=2)
# dygraph plot of MSFT idiosyncratic returns vs XLK
endd <- rutils::calc_endpoints(retp, interval="weeks")
datev <- zoo::index(retp)[endd]
dateb <- datev[findInterval(as.Date("2014-01-01"), datev)] # Steve Balmer exit date
datav <- cbind(retp$XLK, alphac)
colnames(datav)[2] <- "MSFT alpha"
colv <- colnames(datav)
dygraphs::dygraph(cumsum(datav)[endd], main="MSFT Cumulative Alpha vs XLK") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyEvent(dateb, label="Balmer exit", strokePattern="solid", color="red") %>%
  dyLegend(show="always", width=300)
# Calculate the trailing alphas and betas
lambdaf <- 0.9
covarv <- HighFreq::run_covar(retp, lambdaf)
covarv[1, ] <- 1.0
betac <- covarv[, 1]/covarv[, 3]
alphac <- retp$MSFT - betac*retp$XLK
# dygraph plot of trailing MSFT idiosyncratic returns vs XLK
datav <- cbind(retp$XLK, alphac)
colnames(datav)[2] <- "MSFT alpha"
colv <- colnames(datav)
dygraphs::dygraph(cumsum(datav)[endd], main="MSFT Trailing Cumulative Alpha vs XLK") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyEvent(dateb, label="Balmer exit", strokePattern="solid", color="red") %>%
  dyLegend(show="always", width=300)
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
# Load daily S&P500 stock prices
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# Combine MSFT and XLK prices
pricev <- cbind(pricestock$MSFT, rutils::etfenv$prices$XLK)
pricev <- log(na.omit(pricev))
colnames(pricev) <- c("MSFT", "XLK")
datev <- zoo::index(pricev)
# Calculate the beta regression coefficient of prices MSFT ~ XLK
betac <- drop(cov(pricev$MSFT, pricev$XLK)/var(pricev$XLK))
# Calculate the cointegrated portfolio prices
pricec <- pricev$MSFT - betac*pricev$XLK
colnames(pricec) <- "MSFT Coint XLK"
# Scatterplot of MSFT and XLK prices
plot(MSFT ~ XLK, data=pricev, main="MSFT and XLK Prices",
     xlab="XLK", ylab="MSFT", pch=1, col="blue")
abline(a=mean(pricec), b=betac, col="red", lwd=2)
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
colv <- colnames(datav)
dygraphs::dygraph(datav, main="MSFT and XLK Cointegrated Portfolio Prices") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Perform ADF test on the individual stocks
sapply(pricev, tseries::adf.test, k=1)
# Perform ADF test on the cointegrated portfolio
tseries::adf.test(pricec, k=1)
# Perform ADF test for vector of cointegrating factors
betas <- seq(1.2, 2.2, 0.1)
adfstat <- sapply(betas, function(betac) {
  pricec <- (pricev$MSFT - betac*pricev$XLK)
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
lambdaf <- 0.9
volp <- HighFreq::run_var(pricen, lambda=lambdaf)
meanv <- volp[, 1]
volp <- sqrt(volp[, 2])
# Simulate the pairs Bollinger strategy
pricem <- pricen - meanv # De-meaned price
nrows <- NROW(pricec)
threshd <- rutils::lagit(volp)
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(pricem > threshd, -1, posv)
posv <- ifelse(pricem < -threshd, 1, posv)
posv <- zoo::na.locf(posv)
# Lag the positions to trade in the next period
posv <- rutils::lagit(posv, lagg=1)
# Calculate the pnls and the number of trades
retp <- rutils::diffit(pricev)
pnls <-  posv*(retp$MSFT - betac*retp$XLK)
ntrades <- sum(abs(rutils::diffit(posv)) > 0)
# Calculate the Sharpe ratios
wealthv <- cbind(retp$MSFT, pnls)
colnames(wealthv) <- c("MSFT", "Strategy")
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of pairs Bollinger strategy
colv <- colnames(wealthv)
captiont <- paste("Pairs Strategy", "/ \n",
  paste0(paste(colv[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number of trades=", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=200)
# Calculate the trailing cointegrated pair prices
covarv <- HighFreq::run_covar(pricev, lambdaf)
betac <- covarv[, 1]/covarv[, 3]
pricec <- (pricev$MSFT - betac*pricev$XLK)
# Recalculate the mean of cointegrated portfolio prices
volp <- HighFreq::run_var(pricec, lambda=lambdaf)
meanv <- volp[, 1]
volp <- sqrt(volp[, 2])
# Simulate the pairs Bollinger strategy
pricen <- zoo::coredata(pricec) # Numeric price
pricem <- pricen - meanv # De-meaned price
threshd <- rutils::lagit(volp)
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(pricem > threshd, -1, posv)
posv <- ifelse(pricem < -threshd, 1, posv)
posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv, lagg=1)
# Calculate the pnls and the number of trades
retp <- rutils::diffit(pricev)
pnls <-  posv*(retp$MSFT - betac*retp$XLK)
ntrades <- sum(abs(rutils::diffit(posv)) > 0)
# Calculate the Sharpe ratios
wealthv <- cbind(retp$MSFT, pnls)
colnames(wealthv) <- c("MSFT", "Strategy")
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of pairs Bollinger strategy
colv <- colnames(wealthv)
captiont <- paste("Dynamic Pairs Strategy", "/ \n",
  paste0(paste(colv[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number of trades=", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=200)
# Calculate the trailing cointegrated pair prices
covarv <- HighFreq::run_covar(pricev, lambda=0.95)
betac <- covarv[, 1]/covarv[, 3]
pricec <- (pricev$MSFT - betac*pricev$XLK)
# Recalculate the mean of cointegrated portfolio prices
volp <- HighFreq::run_var(pricec, lambda=0.3)
meanv <- volp[, 1]
volp <- sqrt(volp[, 2])
# Simulate the pairs Bollinger strategy
pricen <- zoo::coredata(pricec) # Numeric price
pricem <- pricen - meanv # De-meaned price
threshd <- rutils::lagit(volp)
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(pricem > threshd, -1, posv)
posv <- ifelse(pricem < -threshd, 1, posv)
posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv, lagg=1)
# Calculate the pnls and the number of trades
retp <- rutils::diffit(pricev)
pnls <-  posv*(retp$MSFT - betac*retp$XLK)
ntrades <- sum(abs(rutils::diffit(posv)) > 0)
# Calculate the Sharpe ratios
wealthv <- cbind(retp$MSFT, pnls)
colnames(wealthv) <- c("MSFT", "Strategy")
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Dygraphs plot of pairs Bollinger strategy
colv <- colnames(wealthv)
captiont <- paste("Dynamic Pairs Slow Beta", "/ \n",
  paste0(paste(colv[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n",
  "Number of trades=", ntrades)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=200)
# Calculate the percentage VTI returns
retvti <- na.omit(rutils::etfenv$returns$VTI)
colnames(retvti) <- "VTI"
datev <- zoo::index(retvti)
nrows <- NROW(retvti)
# Load daily S&P500 stock prices
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# Select the stock prices since VTI
pricestock <- pricestock[datev]
# Select stocks with no NA values in their prices
numna <- sapply(pricestock, function(x) sum(is.na(x)))
pricestock <- pricestock[, numna == 0]
# Drop penny stocks
pricel <- last(pricestock)
pricel <- drop(coredata(pricel))
pricestock <- pricestock[, pricel > 1]
# Calculate the dollar and percentage stock returns
retd <- rutils::diffit(pricestock)
retp <- retd/rutils::lagit(pricestock)
retp[1, ] <- 0
# Wealth of fixed shares portfolio
wealthfs <- rowMeans(cumprod(1 + retp))
# Wealth of equal wealth portfolio (with rebalancing)
wealthew <- cumprod(1 + rowMeans(retp))
# Calculate combined log wealth
wealthv <- cbind(wealthfs, wealthew)
wealthv <- log(wealthv)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Fixed shares", "Equal wealth")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv), function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of combined log wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(wealthv[endd],
  main="Wealth of Fixed Share and Equal Wealth Portfolios") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Select a random, fixed share sub-portfolio of 5 stocks
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
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
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
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
# Plot dygraph of all stock index and random sub-portfolios
colorv <- colorRampPalette(c("red", "blue"))(nportf)
colorv <- c("green", colorv)
wealthv <- cbind(wealthfs, wealthr)
wealthv <- log(wealthv)
colnames(wealthv)[1] <- "Index"
colv <- colnames(wealthv)
dygraphs::dygraph(wealthv[endd], main="Stock Index and Random Portfolios") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name=colv[1], strokeWidth=3) %>%
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
wealthb <- rowMeans(pricev[insample, symbolv])
wealthos <- wealthb[cutoff]*rowMeans(cumprod(1 + retp[outsample, symbolv]))
wealthb <- c(wealthb, wealthos)
# Combine the fixed share wealth with the 10 best performing stocks
wealthv <- cbind(wealthfs, wealthb)
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
  dyEvent(datev[cutoff], label="cutoff", strokePattern="solid", color="green") %>%
  dyLegend(width=300)
# Calculate the stock volatilities, betas, and alphas
# Perform parallel loop under Mac-OSX or Linux
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
riskret <- mclapply(retp, function(rets) {
  rets <- na.omit(rets)
  stdev <- sd(rets)
  retvti <- retvti[zoo::index(rets)]
  varvti <- drop(var(retvti))
  meanvti <- mean(retvti)
  betac <- drop(cov(rets, retvti))/varvti
  resid <- rets - betac*retvti
  alphac <- mean(rets) - betac*meanvti
  c(alpha=alphac, beta=betac, stdev=stdev, ivol=sd(resid))
}, mc.cores=ncores)  # end mclapply
riskret <- do.call(rbind, riskret)
tail(riskret)
# Calculate the median volatility
riskv <- riskret[, "stdev"]
medianv <- median(riskv)
# Calculate the returns of low and high volatility stocks
retlow <- rowMeans(retp[, (riskv <= medianv)], na.rm=TRUE)
rethigh <- rowMeans(retp[, (riskv > medianv)], na.rm=TRUE)
wealthv <- cbind(retlow, rethigh, retlow - 0.25*rethigh)
wealthv <- xts::xts(wealthv, order.by=datev)
colv <- c("low_vol", "high_vol", "long_short")
colnames(wealthv) <- colv
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative returns of low and high volatility stocks
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Volatility Stocks In-Sample") %>%
  dySeries(name=colv[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colv[2], col="red", strokeWidth=1) %>%
  dySeries(name=colv[3], col="green", strokeWidth=2) %>%
  dyLegend(width=300)
# Merton-Henriksson test
desm <- cbind(VTI=retvti, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(desm)[2:3] <- c("Merton", "Treynor")
regmod <- lm(wealthv$long_short ~ VTI + Merton, data=desm); summary(regmod)
# Treynor-Mazuy test
regmod <- lm(wealthv$long_short ~ VTI + Treynor, data=desm); summary(regmod)
# Plot residual scatterplot
resids <- regmod$residuals
plot.default(x=retvti, y=resids, xlab="VTI", ylab="Low Volatility")
title(main="Treynor-Mazuy Market Timing Test\n for Low Volatility vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["Treynor", "t value"], 2)
points.default(x=retvti, y=fitv, pch=16, col="red")
text(x=0.0, y=max(resids), paste("Treynor test t-value =", tvalue))
# Calculate the in-sample stock volatilities, betas, and alphas
riskretis <- mclapply(retp[insample], function(rets) {
  combv <- na.omit(cbind(rets, retvti))
  if (NROW(combv) > 11) {
    rets <- na.omit(rets)
    stdev <- sd(rets)
    retvti <- retvti[zoo::index(rets)]
    varvti <- drop(var(retvti))
    meanvti <- mean(retvti)
    betac <- drop(cov(rets, retvti))/varvti
    resid <- rets - betac*retvti
    alphac <- mean(rets) - betac*meanvti
    return(c(alpha=alphac, beta=betac, stdev=stdev, ivol=sd(resid)))
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
retlow <- rowMeans(retp[outsample, (riskv <= medianv)], na.rm=TRUE)
rethigh <- rowMeans(retp[outsample, (riskv > medianv)], na.rm=TRUE)
wealthv <- cbind(retlow, rethigh, retlow - 0.25*rethigh)
wealthv <- xts::xts(wealthv, order.by=datev[outsample])
colv <- c("low_vol", "high_vol", "long_short")
colnames(wealthv) <- colv
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative returns of low and high volatility stocks
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Volatility Stocks Out-Of-Sample") %>%
  dySeries(name=colv[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colv[2], col="red", strokeWidth=1) %>%
  dySeries(name=colv[3], col="green", strokeWidth=2) %>%
  dyLegend(width=300)
# Calculate the median idiosyncratic volatility
riskv <- riskret[, "ivol"]
medianv <- median(riskv)
# Calculate the returns of low and high idiosyncratic volatility stocks
retlow <- rowMeans(retp[, (riskv <= medianv)], na.rm=TRUE)
rethigh <- rowMeans(retp[, (riskv > medianv)], na.rm=TRUE)
wealthv <- cbind(retlow, rethigh, retlow - 0.25*rethigh)
wealthv <- xts::xts(wealthv, order.by=datev)
colv <- c("low_vol", "high_vol", "long_short")
colnames(wealthv) <- colv
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of returns of low and high idiosyncratic volatility stocks
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Idiosyncratic Volatility Stocks In-Sample") %>%
  dySeries(name=colv[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colv[2], col="red", strokeWidth=1) %>%
  dySeries(name=colv[3], col="green", strokeWidth=2) %>%
  dyLegend(width=300)
# Merton-Henriksson test
desm <- cbind(VTI=retvti, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(desm)[2:3] <- c("Merton", "Treynor")
regmod <- lm(wealthv$long_short ~ VTI + Merton, data=desm); summary(regmod)
# Treynor-Mazuy test
regmod <- lm(wealthv$long_short ~ VTI + Treynor, data=desm); summary(regmod)
# Plot residual scatterplot
resids <- regmod$residuals
plot.default(x=retvti, y=resids, xlab="VTI", ylab="Low Volatility")
title(main="Treynor-Mazuy Market Timing Test\n for Low Idiosyncratic Volatility vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["Treynor", "t value"], 2)
points.default(x=retvti, y=fitv, pch=16, col="red")
text(x=0.0, y=max(resids), paste("Treynor test t-value =", tvalue))
# Calculate the median in-sample idiosyncratic volatility
riskv <- riskretis[, "ivol"]
medianv <- median(riskv)
# Calculate the out-of-sample returns of low and high idiosyncratic volatility stocks
retlow <- rowMeans(retp[outsample, (riskv <= medianv)], na.rm=TRUE)
rethigh <- rowMeans(retp[outsample, (riskv > medianv)], na.rm=TRUE)
wealthv <- cbind(retlow, rethigh, retlow - 0.25*rethigh)
wealthv <- xts::xts(wealthv, order.by=datev[outsample])
colv <- c("low_vol", "high_vol", "long_short")
colnames(wealthv) <- colv
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of out-of-sample returns of low and high volatility stocks
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Idiosyncratic Volatility Stocks Out-Of-Sample") %>%
  dySeries(name=colv[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colv[2], col="red", strokeWidth=1) %>%
  dySeries(name=colv[3], col="green", strokeWidth=2) %>%
  dyLegend(width=300)
# Calculate the median beta
riskv <- riskret[, "beta"]
medianv <- median(riskv)
# Calculate the returns of low and high beta stocks
betalow <- rowMeans(retp[, names(riskv[riskv <= medianv])], na.rm=TRUE)
betahigh <- rowMeans(retp[, names(riskv[riskv > medianv])], na.rm=TRUE)
wealthv <- cbind(betalow, betahigh, betalow - 0.25*betahigh)
wealthv <- xts::xts(wealthv, order.by=datev)
colv <- c("low_beta", "high_beta", "long_short")
colnames(wealthv) <- colv
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative returns of low and high beta stocks
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Beta Stocks In-Sample") %>%
  dySeries(name=colv[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colv[2], col="red", strokeWidth=1) %>%
  dySeries(name=colv[3], col="green", strokeWidth=2) %>%
  dyLegend(width=300)
# Merton-Henriksson test
desm <- cbind(VTI=retvti, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(desm)[2:3] <- c("Merton", "Treynor")
regmod <- lm(wealthv$long_short ~ VTI + Merton, data=desm); summary(regmod)
# Treynor-Mazuy test
regmod <- lm(wealthv$long_short ~ VTI + Treynor, data=desm); summary(regmod)
# Plot residual scatterplot
resids <- regmod$residuals
plot.default(x=retvti, y=resids, xlab="VTI", ylab="Low Beta")
title(main="Treynor-Mazuy Market Timing Test\n for Low Beta vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["Treynor", "t value"], 2)
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
colv <- c("low_beta", "high_beta", "long_short")
colnames(wealthv) <- colv
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of out-of-sample returns of low and high beta stocks
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low and High Beta Stocks Out-Of-Sample") %>%
  dySeries(name=colv[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colv[2], col="red", strokeWidth=1) %>%
  dySeries(name=colv[3], col="green", strokeWidth=2) %>%
  dyLegend(width=300)
# Calculate the trailing percentage volatilities
volp <- HighFreq::run_var(retp, lambda=0.15)
volp <- sqrt(volp[, (nstocks+1):(2*nstocks)])
volp <- rutils::lagit(volp)
volp[volp == 0] <- 1
# Calculate the median volatilities
medianv <- matrixStats::rowMedians(volp)
# Calculate the wealth of low volatility stocks
weightv <- (volp <= medianv)
weightv <- rutils::lagit(weightv)
retlow <- rowMeans(weightv*retp)
# Calculate the wealth of high volatility stocks
weightv <- (volp > medianv)
weightv <- rutils::lagit(weightv)
rethigh <- rowMeans(weightv*retp)
# Combined wealth
wealthv <- cbind(retlow, rethigh)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("LowVol", "HighVol")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of log wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Wealth of Low and High Volatility Stocks") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the returns of equal wealth portfolio
retew <- rowMeans(retp, na.rm=TRUE)
retew[1] <- 0
# Calculate the long-short volatility returns
retls <- (retlow - 0.25*rethigh)
# Scale the PnL volatility to that of wealthew
retls <- retls*sd(retew)/sd(retls)
# Combined wealth
wealthv <- cbind(retew, retls)
wealthv <- xts::xts(wealthv, datev)
colv <- c("Equal Weight", "Long-Short Vol")
colnames(wealthv) <- colv
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of log wealth
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Equal Weight and Long-Short Vol Portfolios") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Calculate the trailing dollar volatilities
lambdav <- 0.99
vold <- HighFreq::run_var(retd, lambda=lambdav)
vold <- vold[, (nstocks+1):(2*nstocks)]
vold <- sqrt(vold)
vold[vold == 0] <- 1
# Calculate the rolling risk parity weights
weightv <- 1/vold
weightv <- weightv/rowSums(weightv)
# Wealth of equal wealth portfolio (with rebalancing)
wealthew <- cumprod(1 + rowMeans(retp))
# Calculate the risk parity allocations
pricerp <- pricestock*weightv
# Calculate the dollar returns of risk parity
retrp <- retp*rutils::lagit(pricerp)
retrp[1, ] <- pricerp[1, ]
# Calculate the wealth of risk parity
wealthrp <- cumsum(rowSums(retrp))
# Combined wealth
wealthv <- cbind(wealthrp[1]*wealthew, wealthrp)
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Equal wealth", "Risk parity")
wealthv <- log(wealthv)
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv), function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of log wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(wealthv[endd],
  main="Wealth of Equal Wealth and Risk Parity Portfolios") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=400)
# Objective function equal to the sum of returns
objfun <- function(retp) sum(na.omit(retp))
# Objective function equal to the Sharpe ratio
objfun <- function(retp) {
  retp <- na.omit(retp)
  if (NROW(retp) > 12) {
    stdev <- sd(retp)
    if (stdev > 0) mean(retp)/stdev else 0
  } else 0
}  # end objfun
# Objective function equal to the Kelly ratio
objfun <- function(retp) {
  retp <- na.omit(retp)
  if (NROW(retp) > 12) {
    varv <- var(retp)
    if (varv > 0) mean(retp)/varv else 0
  } else 0
}  # end objfun
# VTI returns
retv <- na.omit(rutils::etfenv$returns$VTI)
datev <- zoo::index(retv) # Dates vector
# Load daily S&P500 percentage stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
retp <- retstock[datev]
nrows <- NROW(retp) # number of rows
nstocks <- NCOL(retp) # number of stocks
# Objective function equal to the Kelly ratio
objfun <- function(retp) {
  retp <- na.omit(retp)
  if (NROW(retp) > 12) {
    varv <- var(retp)
    if (varv > 0) mean(retp)/varv else 0
  } else 0
}  # end objfun
# Calculate performance statistics for all stocks
perfstat <- sapply(retp, objfun)
sum(is.na(perfstat))
sum(!is.finite(perfstat))
hist(perfstat, breaks=100, main="Performance Statistics")
sort(perfstat, decreasing=TRUE)
# Calculate weights proportional to performance statistic
# With quadratic constraint
weightv <- perfstat/sqrt(sum(perfstat^2))
sum(weightv^2)
sum(weightv)
weightv
# Calculate weights proportional to performance statistic
# With linear constraint
weightv <- perfstat/sum(perfstat)
sum(weightv^2)
sum(weightv)
weightv
# Calculate the weighted returns using transpose
retw <- t(t(retp)*perfstat)
# Or using Rcpp
retf <- HighFreq::mult_mat(perfstat, retp)
all.equal(retw, retf, check.attributes=FALSE)
# Calculate the in-sample portfolio volatility
volis <- sd(rowMeans(retw, na.rm=TRUE))
# Calculate the equal weight portfolio volatility
volew <- sd(rowMeans(retp, na.rm=TRUE))
# Apply the volatility constraint
weightv <- volew*perfstat/volis
# Calculate the in-sample portfolio volatility
retw <- t(t(retp)*weightv)
all.equal(sd(rowMeans(retw, na.rm=TRUE)), volew)
# Apply the volatility target constraint
volt <- 0.01
weightv <- volt*perfstat/volis
retw <- t(t(retp)*weightv)
all.equal(sd(rowMeans(retw, na.rm=TRUE)), volt)
# Compare speed of R with Rcpp
library(microbenchmark)
summary(microbenchmark(
  trans=t(t(retp)*perfstat),
  rcpp=HighFreq::mult_mat(perfstat, retp),
  times=10))[, c(1, 4, 5)]
# Box constraints
weightv[weightv > 1] <- 1
weightv[weightv < 0] <- 0
weightv
# Calculate the performance statistics for all stocks
perfstat <- sapply(retp, objfun)
sum(is.na(perfstat))
# Calculate the best and worst performing stocks
perfstat <- sort(perfstat, decreasing=TRUE)
topstocks <- 10
symbolb <- names(head(perfstat, topstocks))
symbolw <- names(tail(perfstat, topstocks))
# Calculate equal weights for the best and worst performing stocks
weightv <- numeric(NCOL(retp))
names(weightv) <- colnames(retp)
weightv[symbolb] <- 1
weightv[symbolw] <- (-1)
# Calculate weights proportional to the performance statistic
weightv <- perfstat
# Center weights so sum is equal to 0
weightv <- weightv - mean(weightv)
# Scale weights so sum of squares is equal to 1
weightv <- weightv/sqrt(sum(weightv^2))
# Calculate the in-sample momentum strategy pnls
pnls <- t(t(retp)*weightv)
# Or using Rcpp
pnls2 <- HighFreq::mult_mat(weightv, retp)
all.equal(pnls, pnls2, check.attributes=FALSE)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls[1] <- 0
# Scale the pnls so their volatility is the same as equal weight
retew <- rowMeans(retp, na.rm=TRUE)
retew[1] <- 0
pnls <- sd(retew)/sd(pnls)*pnls
wealthv <- xts(cbind(retew, pnls), datev)
colnames(wealthv) <- c("Equal Weight", "Momentum")
dygraph(cumsum(wealthv))
# Calculate a vector of monthly end points
endd <- rutils::calc_endpoints(retp, interval="months")
npts <- NROW(endd)
# Perform loop over the end points
lookb <- 8
pnls <- lapply(3:(npts-1), function(tday) {
  # Select the look-back returns
  startp <- endd[max(1, tday-lookb)]
  retis <- retp[startp:endd[tday], ]
  # Calculate the best and worst performing stocks in-sample
  perfstat <- sapply(retis, objfun)
  perfstat <- sort(perfstat, decreasing=TRUE)
  symbolb <- names(head(perfstat, topstocks))
  symbolw <- names(tail(perfstat, topstocks))
  # Calculate the momentum weights
  weightv <- numeric(NCOL(retp))
  names(weightv) <- colnames(retp)
  weightv[symbolb] <- 1
  weightv[symbolw] <- (-1)
  # Calculate the in-sample momentum PnLs
  pnlis <- HighFreq::mult_mat(weightv, retis)
  pnlis <- rowMeans(pnlis, na.rm=TRUE)
  # Scale weights so in-sample pnl volatility is same as equal weight
  weightv <- weightv*sd(rowMeans(retis, na.rm=TRUE))/sd(pnlis)
  # Calculate the out-of-sample momentum returns
  pnlos <- HighFreq::mult_mat(weightv, retp[(endd[tday]+1):endd[tday+1], ])
  pnlos <- rowMeans(pnlos, na.rm=TRUE)
  drop(pnlos)
})  # end lapply
pnls <- rutils::do_call(c, pnls)
# Calculate the average of all stock returns
retew <- rowMeans(retp, na.rm=TRUE)
# Add initial startup interval to the momentum returns
pnls <- c(retew[endd[1]:endd[3]], pnls)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retew, pnls)
wealthv <- xts::xts(wealthv, order.by=datev)
colnames(wealthv) <- c("Equal", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of stock index and momentum strategy
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Stock Index and Momentum Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
btmomtop <- function(retp, objfun, lookb=12, rebalf="months", topstocks=10,
 bidask=0.0, endd=rutils::calc_endpoints(retp, interval=rebalf), ...) {
  # Perform loop over end points
  npts <- NROW(endd)
  pnls <- lapply(3:(npts-1), function(tday) {
    # Select the look-back returns
    startp <- endd[max(1, tday-lookb)]
    retis <- retp[startp:endd[tday], ]
    # Calculate the best and worst performing stocks in-sample
    perfstat <- sapply(retis, objfun)
    perfstat <- sort(perfstat, decreasing=TRUE)
    symbolb <- names(head(perfstat, topstocks))
    symbolw <- names(tail(perfstat, topstocks))
    # Calculate the momentum weights
    weightv <- numeric(NCOL(retp))
    names(weightv) <- colnames(retp)
    weightv[symbolb] <- 1
    weightv[symbolw] <- (-1)
    # Calculate the in-sample momentum pnls
    pnlis <- HighFreq::mult_mat(weightv, retis)
    pnlis <- rowMeans(pnlis, na.rm=TRUE)
    # Scale weights so in-sample pnl volatility is same as equal weight
    weightv <- weightv*sd(rowMeans(retis, na.rm=TRUE))/sd(pnlis)
    # Calculate the out-of-sample momentum returns
    pnlos <- HighFreq::mult_mat(weightv, retp[(endd[tday]+1):endd[tday+1], ])
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    drop(pnlos)
  })  # end lapply
  pnls <- rutils::do_call(c, pnls)
  pnls
}  # end btmomtop
# Perform backtests for vector of look-back intervals
lookbv <- seq(3, 15, by=1)
endd <- rutils::calc_endpoints(retp, interval="months")
# Warning - takes very long
pnll <- lapply(lookbv, btmomtop, retp=retp, endd=endd, objfun=objfun)
# Perform parallel loop under Mac-OSX or Linux
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
pnll <- mclapply(lookbv, btmomtop, retp=retp, endd=endd, objfun=objfun, mc.cores=ncores)
sharper <- sqrt(252)*sapply(pnll, function(pnl) mean(pnl)/sd(pnl))
# Plot Sharpe ratios of momentum strategies
plot(x=lookbv, y=sharper, t="l",
  main="Momentum Sharpe as Function of Look-back Interval",
  xlab="look-back (months)", ylab="Sharpe")
# Calculate best pnls of momentum strategy
whichmax <- which.max(sharper)
lookbv[whichmax]
pnls <- pnll[[whichmax]]
# Add initial startup interval to the momentum returns
pnls <- c(retew[endd[1]:endd[3]], pnls)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retew, pnls)
wealthv <- xts::xts(wealthv, order.by=datev)
colnames(wealthv) <- c("Equal", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of stock index and momentum strategy
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Momentum Strategy for Stocks") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
btmomweight <- function(retp, objfun, lookb=12, rebalf="months",
  bidask=0.0, endd=rutils::calc_endpoints(retp, interval=rebalf), ...) {
  # Perform loop over end points
  npts <- NROW(endd)
  pnls <- lapply(3:(npts-1), function(tday) {
    # Select the look-back returns
    startp <- endd[max(1, tday-lookb)]
    retis <- retp[startp:endd[tday], ]
    # Calculate weights proportional to performance
    perfstat <- sapply(retis, objfun)
    weightv <- perfstat
    # Calculate the in-sample portfolio returns
    pnlis <- HighFreq::mult_mat(weightv, retis)
    pnlis <- rowMeans(pnlis, na.rm=TRUE)
    # Scale weights so in-sample pnl volatility is same as equal weight
    weightv <- weightv*sd(rowMeans(retis, na.rm=TRUE))/sd(pnlis)
    # Calculate the out-of-sample momentum returns
    pnlos <- HighFreq::mult_mat(weightv, retp[(endd[tday]+1):endd[tday+1], ])
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    drop(pnlos)
  })  # end lapply
  rutils::do_call(c, pnls)
}  # end btmomweight
# Perform backtests for vector of look-back intervals
lookbv <- seq(3, 15, by=1)
# pnll <- lapply(lookbv, btmomweight, retp=retp, endd=endd, objfun=objfun)
# Or perform parallel loop under Mac-OSX or Linux
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
pnll <- mclapply(lookbv, btmomweight, retp=retp, endd=endd, objfun=objfun, mc.cores=ncores)
sharper <- sqrt(252)*sapply(pnll, function(pnl) mean(pnl)/sd(pnl))
# Plot Sharpe ratios of momentum strategies
plot(x=lookbv, y=sharper, t="l",
  main="Momentum Sharpe as Function of Look-back Interval",
  xlab="look-back (months)", ylab="Sharpe")
# Calculate best pnls of momentum strategy
whichmax <- which.max(sharper)
lookbv[whichmax]
pnls <- pnll[[whichmax]]
# Add initial startup interval to the momentum returns
pnls <- c(retew[endd[1]:endd[3]], pnls)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retew, pnls, 0.5*(retew + pnls))
wealthv <- xts::xts(wealthv, order.by=datev)
colnames(wealthv) <- c("Equal", "Momentum", "Combined")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of stock index and momentum strategy
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Weighted Momentum Strategy for Stocks") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Calculate the trailing average returns and variance using C++ code
lambdaf <- 0.99
varm <- HighFreq::run_var(retp, lambda=lambdaf)
meanm <- varm[, 1:nstocks]
varm <- varm[, (nstocks+1):(2*nstocks)]
# Calculate the trailing Kelly ratios
weightv <- meanm/varm
weightv <- weightv/sqrt(rowSums(weightv^2, na.rm=TRUE))
weightv <- rutils::lagit(weightv)
# Calculate the momentum profits and losses
pnls <- rowSums(weightv*retp, na.rm=TRUE)
# Calculate the transaction costs
bidask <- 0.0
costv <- 0.5*bidask*rowSums(abs(rutils::diffit(weightv)), na.rm=TRUE)
pnls <- (pnls - costv)
# Scale the momentum volatility to the equal weight index
volew <- sd(retew)
pnls <- volew*pnls/sd(pnls)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retew, pnls, 0.5*(retew + pnls))
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Equal", "Momentum", "Combined")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of stock index and momentum strategy
endd <- rutils::calc_endpoints(retp, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Daily Momentum Strategy for Stocks") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
btmomdaily <- function(retp, lambdaf=0.9, trend=1, bidask=0.0, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate the trailing Kelly ratio
  nstocks <- NCOL(retp)
  varm <- HighFreq::run_var(retp, lambda=lambdaf)
  meanm <- varm[, 1:nstocks]
  vars <- varm[, (nstocks+1):(2*nstocks)]
  weightv <- meanm/vars
  weightv <- weightv/sqrt(rowSums(weightv^2, na.rm=TRUE))
  weightv <- rutils::lagit(weightv)
  # Calculate the momentum profits and losses
  pnls <- trend*rowSums(weightv*retp, na.rm=TRUE)
  # Calculate the transaction costs
  costv <- 0.5*bidask*rowSums(abs(rutils::diffit(weightv)), na.rm=TRUE)
  (pnls - costv)
}  # end btmomdaily
# Simulate multiple daily stock momentum strategies
lambdav <- seq(0.99, 0.998, 0.001)
pnls <- sapply(lambdav, btmomdaily, retp=retp)
# Scale the momentum volatility to the equal weight index
pnls <- apply(pnls, MARGIN=2, function(pnl) volew*pnl/sd(pnl))
colnames(pnls) <- paste0("lambda=", lambdav)
pnls <- xts::xts(pnls, datev)
tail(pnls)
# Plot dygraph of daily stock momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Daily Stock Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Plot daily stock momentum strategies using quantmod
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls)[endd],
  theme=plot_theme, name="Daily Stock Momentum Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=plot_theme$col$line.col, bty="n")
# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
btmomdailyhold <- function(retp, lambdaf=0.9, trend=1, bidask=0.0, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate the trailing Kelly ratio
  nstocks <- NCOL(retp)
  varm <- HighFreq::run_var(retp, lambda=lambdaf)
  meanm <- varm[, 1:nstocks]
  vars <- varm[, (nstocks+1):(2*nstocks)]
  weightv <- meanm/vars
  weightv <- weightv/sqrt(rowSums(weightv^2, na.rm=TRUE))
  # Average the past weights
  weightv <- HighFreq::run_mean(weightv, lambda=lambdaf)
  weightv <- rutils::lagit(weightv)
  # Calculate the momentum profits and losses
  pnls <- trend*rowSums(weightv*retp, na.rm=TRUE)
  # Calculate the transaction costs
  costv <- 0.5*bidask*rowSums(abs(rutils::diffit(weightv)), na.rm=TRUE)
  pnls <- (pnls - costv)
  pnls[1:21] <- 0 # Set the warmup PnLs to zero
  return(pnls)
}  # end btmomdailyhold
# Simulate multiple daily stock momentum strategies with holding periods
lambdav <- seq(0.99, 0.998, 0.001)
pnls <- sapply(lambdav, btmomdailyhold, retp=retp)
# Scale the momentum volatility to the equal weight index
pnls <- apply(pnls, MARGIN=2, function(pnl) volew*pnl/sd(pnl))
colnames(pnls) <- paste0("lambda=", lambdav)
pnls <- xts::xts(pnls, datev)
# dygraph of daily stock momentum strategies with holding period
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Daily Stock Momentum Strategies with Holding Period") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Plot of daily stock momentum strategies with holding period
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls)[endd],
  theme=plot_theme, name="Daily Stock Momentum Strategies with Holding Period")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=plot_theme$col$line.col, bty="n")
# Calculate best pnls of momentum strategy
sharper <- sqrt(252)*sapply(pnls, function(pnl) mean(pnl)/sd(pnl))
whichmax <- which.max(sharper)
lambdav[whichmax]
pnls <- pnls[, whichmax]
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retew, pnls, 0.5*(retew + pnls))
colnames(wealthv) <- c("Equal", "Momentum", "Combined")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of stock index and momentum strategy
endd <- rutils::calc_endpoints(retp, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Daily Momentum Strategy for Stocks") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Perform sapply loop over lambdav
lambdav <- seq(0.2, 0.7, 0.1)
pnls <- sapply(lambdav, btmomdaily, retp=retp, trend=(-1))
# Scale the momentum volatility to the equal weight index
pnls <- apply(pnls, MARGIN=2, function(pnl) volew*pnl/sd(pnl))
colnames(pnls) <- paste0("lambda=", lambdav)
pnls <- xts::xts(pnls, datev)
# Plot dygraph of mean reverting daily stock momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Mean Reverting Daily Stock Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=400)
# Plot mean reverting daily stock momentum strategies using quantmod
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls)[endd],
  theme=plot_theme, name="Mean Reverting Daily Stock Momentum Strategies")
legend("topleft", legend=colnames(pnls),
  inset=0.05, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=plot_theme$col$line.col, bty="n")
# Calculate the scaled prices of VTI vs MTUM ETF
wealthv <- na.omit(rutils::etfenv$prices[, c("VTI", "MTUM")])
wealthv <- rutils::diffit(log(wealthv))
colnames(wealthv) <- c("VTI", "MTUM")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of scaled prices of VTI vs MTUM ETF
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="VTI vs MTUM ETF") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(width=300)
# Set the NA values to zero
retp[is.na(retp)] <- 0
# Calculate the standardized returns
retsc <- lapply(retp, function(x) (x - mean(x))/sd(x))
retsc <- do.call(cbind, retsc)
# Calculate the PCA loadings
covmat <- cov(retsc)
pcad <- eigen(covmat)
pcal <- pcad$vectors # The PCA loadings
rownames(pcal) <- colnames(retp)
sort(-pcal[, 1], decreasing=TRUE)
sort(pcal[, 2], decreasing=TRUE)
round((t(pcal) %*% pcal)[1:5, 1:5], 4)
# Calculate the PCA time series from the stock returns and the PCA loadings
retpca <- retp %*% pcal
colnames(retpca) <- paste0("PC", 1:nstocks)
round((t(retpca) %*% retpca)[1:5, 1:5], 4)
# Calculate the autocorrelations of the PCA time series
pacv <- apply(retpca[, 1:100], 2, function(x)
  sum(pacf(x, lag=10, plot=FALSE)$acf))
plot(pacv, type="h", main="PCA Autocorrelations",
     xlab="PC", ylab="PACF")
# Simulate daily PCA momentum strategies for multiple lambdaf parameters
dimax <- 30
lambdav <- seq(0.97, 0.99, 0.005)
pnls <- mclapply(lambdav, btmomdailyhold, retp=retpca[, 1:dimax], mc.cores=ncores)
pnls <- lapply(pnls, function(pnl) volew*pnl/sd(pnl))
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdav)
pnls <- xts::xts(pnls, datev)
# Plot Sharpe ratios of momentum strategies
sharper <- sqrt(252)*sapply(pnls, function(pnl) mean(pnl)/sd(pnl))
plot(x=lambdav, y=sharper, t="l",
  main="PCA Momentum Sharpe as Function of Decay Factor",
  xlab="lambdaf", ylab="Sharpe")
# Plot dygraph of daily PCA momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
endd <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Daily PCA Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=400)
# Calculate best pnls of PCA momentum strategy
whichmax <- which.max(sharper)
lambdav[whichmax]
pnls <- pnls[, whichmax]
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retew, pnls, 0.5*(retew + pnls))
colnames(wealthv) <- c("Equal", "Momentum", "Combined")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of stock index and PCA momentum strategy
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Daily Momentum Strategy for Stocks") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Simulate daily PCA momentum strategies for multiple lambdaf parameters
lambdav <- seq(0.4, 0.8, 0.1)
pnls <- mclapply(lambdav, btmomdailyhold, retp=retpca[, (dimax+1):(NCOL(retpca)-dimax)],
   trend=(-1), bidask=0.0001, mc.cores=ncores)
pnls <- lapply(pnls, function(pnl) volew*pnl/sd(pnl))
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdav)
pnls <- xts::xts(pnls, datev)
# Plot Sharpe ratios of momentum strategies
sharper <- sqrt(252)*sapply(pnls, function(pnl) mean(pnl)/sd(pnl))
plot(x=lambdav, y=sharper, t="l",
  main="PCA Momentum Sharpe as Function of Decay Factor",
  xlab="lambdaf", ylab="Sharpe")
# Plot dygraph of daily PCA momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Mean Reverting Daily PCA Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=400)
# Define in-sample and out-of-sample intervals
cutoff <- nrows %/% 2
datev[cutoff]
insample <- 1:cutoff
outsample <- (cutoff + 1):nrows
# Calculate the PCA loadings in-sample
covmat <- cov(retsc[insample])
pcad <- eigen(covmat)
pcal <- pcad$vectors # The PCA loadings
rownames(pcal) <- colnames(retp)
# Calculate the PCA time series from the stock returns and the PCA loadings
retpca <- retp %*% pcal
colnames(retpca) <- paste0("PC", 1:nstocks)
# Calculate the out-of-sample PCA time series
retpca <- xts::xts(retpca[outsample, ], order.by=datev[outsample])
# Simulate daily PCA momentum strategies for multiple lambdaf parameters
lambdav <- seq(0.98, 0.99, 0.003)
pnls <- mclapply(lambdav, btmomdailyhold, retp=retpca[, 1:dimax], mc.cores=ncores)
pnls <- lapply(pnls, function(pnl) volew*pnl/sd(pnl))
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdav)
pnls <- xts::xts(pnls, datev[outsample])
# Plot Sharpe ratios of momentum strategies
sharper <- sqrt(252)*sapply(pnls, function(pnl) mean(pnl)/sd(pnl))
plot(x=lambdav, y=sharper, t="l",
  main="PCA Momentum Sharpe as Function of Decay Factor",
  xlab="lambdaf", ylab="Sharpe")
# Calculate a vector of weekly end points
endd <- rutils::calc_endpoints(retpca, interval="weeks")
# Plot dygraph of daily out-of-sample PCA momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Daily Out-of-Sample PCA Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Simulate daily PCA momentum strategies for multiple lambdaf parameters
lambdav <- seq(0.4, 0.8, 0.1)
pnls <- mclapply(lambdav, btmomdailyhold,
   retp=retpca[, (dimax+1):(NCOL(retpca)-dimax)],
   trend=(-1), bidask=0.0001, mc.cores=ncores)
pnls <- lapply(pnls, function(pnl) volew*pnl/sd(pnl))
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdav)
pnls <- xts::xts(pnls, datev[outsample])
# Plot Sharpe ratios of momentum strategies
sharper <- sqrt(252)*sapply(pnls, function(pnl) mean(pnl)/sd(pnl))
plot(x=lambdav, y=sharper, t="l",
  main="PCA Momentum Sharpe as Function of Decay Factor",
  xlab="lambdaf", ylab="Sharpe")
# Calculate a vector of weekly end points
endd <- rutils::calc_endpoints(retpca, interval="weeks")
# Plot dygraph of daily S&P500 momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Mean Reverting Daily Out-of-Sample PCA Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Extract ETF returns
symbolv <- c("VTI", "IEF", "DBC")
retp <- na.omit(rutils::etfenv$returns[, symbolv])
datev <- zoo::index(retp)
# Calculate a vector of monthly end points
endd <- rutils::calc_endpoints(retp, interval="months")
npts <- NROW(endd)
# Perform backtests for vector of look-back intervals
lookbv <- seq(3, 12, by=1)
pnll <- lapply(lookbv, btmomweight, retp=retp, endd=endd, objfun=objfun)
sharper <- sqrt(252)*sapply(pnll, function(pnl) mean(pnl)/sd(pnl))
# Plot Sharpe ratios of momentum strategies
plot(x=lookbv, y=sharper, t="l",
  main="Momentum Sharpe as Function of Look-back Interval",
  xlab="look-back (months)", ylab="Sharpe")
# Calculate best pnls of momentum strategy
whichmax <- which.max(sharper)
lookbv[whichmax]
pnls <- pnll[[whichmax]]
retew <- rowMeans(retp)
pnls <- c(retew[endd[1]:endd[3]], pnls)
# Calculate returns of all-weather benchmark
weightaw <- c(0.30, 0.55, 0.15)
retaw <- retp %*% weightaw
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retaw, pnls, 0.5*(retaw+pnls))
wealthv <- xts::xts(wealthv, order.by=datev)
colnames(wealthv) <- c("All-weather", "Momentum", "Combined")
cor(wealthv)
wealthv <- xts::xts(wealthv, order.by=datev)
sharper <- sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sharper
captiont <- "Optimal Momentum for ETFs"
colnames(wealthv) <- paste(colnames(wealthv), round(sharper[1, ], 3), sep=" = ")
# Plot dygraph of all-weather benchmark and momentum strategy
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name=colnames(wealthv)[3], strokeWidth=3) %>%
  dyLegend(show="always", width=500)
# Calculate the momentum weights
lookb <- lookbv[whichmax]
weightv <- lapply(2:npts, function(tday) {
  # Select the look-back returns
  startp <- endd[max(1, tday-lookb)]
  retis <- retp[startp:endd[tday], ]
  # Calculate weights proportional to performance
  perfstat <- sapply(retis, objfun)
  weightv <- drop(perfstat)
  # Scale weights so in-sample pnl volatility is same as equal weight
  pnls <- retis %*% weightv
  weightv*sd(rowMeans(retis))/sd(pnls)
})  # end lapply
weightv <- rutils::do_call(rbind, weightv)
# Plot of momentum weights
retvti <- cumsum(retp$VTI)
datav <- cbind(retvti[endd], weightv)
colnames(datav) <- c("VTI", paste0(colnames(retp), "_weight"))
zoo::plot.zoo(datav, xlab=NULL, main="Momentum Weights")
# Calculate ETF betas
betasetf <- sapply(retp, function(x) cov(retp$VTI, x)/var(retp$VTI))
# Momentum beta is equal weights times ETF betas
betac <- weightv %*% betasetf
betac <- xts::xts(betac, order.by=datev[endd])
colnames(betac) <- "momentum_beta"
datav <- cbind(betac, retvti[endd])
zoo::plot.zoo(datav, main="Momentum Beta & VTI Price", xlab="")
# Aggregate the returns to monthly intervals
retvti <- retp$VTI
desm <- cbind(pnls, retvti, 0.5*(retvti+abs(retvti)), retvti^2)
desm <- HighFreq::roll_sumep(desm, lookb=22)
colnames(desm) <- c("pnls", "VTI", "Merton", "Treynor")
desm <- as.data.frame(desm)
# Merton-Henriksson test
regmod <- lm(pnls ~ VTI + Merton, data=desm); summary(regmod)
# Treynor-Mazuy test
regmod <- lm(pnls ~ VTI + Treynor, data=desm); summary(regmod)
# Plot residual scatterplot
resids <- regmod$residuals
plot.default(x=retvti, y=resids, xlab="VTI", ylab="momentum")
title(main="Treynor-Mazuy Market Timing Test\n for Momentum vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["Treynor", "t value"], 2)
points.default(x=retvti, y=fitv, pch=16, col="red")
text(x=0.0, y=max(resids), paste("Treynor test t-value =", tvalue))
# Standardize the returns
pnlsd <- (pnls-mean(pnls))/sd(pnls)
retvti <- (retvti-mean(retvti))/sd(retvti)
# Calculate skewness and kurtosis
apply(cbind(pnlsd, retvti), 2, function(x)
  sapply(c(skew=3, kurt=4),
    function(e) sum(x^e)))/NROW(retvti)
# Calculate kernel density of VTI
densvti <- density(retvti)
# Plot histogram of momentum returns
hist(pnlsd, breaks=80,
  main="Momentum and VTI Return Distributions (standardized)",
  xlim=c(-4, 4), ylim=range(densvti$y), xlab="", ylab="", freq=FALSE)
# Draw kernel density of histogram
lines(density(pnlsd), col='red', lwd=2)
lines(densvti, col='blue', lwd=2)
# Add legend
legend("topright", inset=0.0, cex=1.0, title=NULL,
 leg=c("Momentum", "VTI"), bty="n", y.intersp=0.5,
 lwd=6, bg="white", col=c("red", "blue"))
# Combine momentum strategy with all-weather
wealthv <- cbind(retaw, pnls, 0.5*(pnls + retaw))
colnames(wealthv) <- c("All-weather", "Momentum", "Combined")
wealthv <- xts::xts(wealthv, datev)
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate strategy correlations
cor(wealthv)
# Plot ETF momentum strategy combined with All-Weather
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Momentum Strategy and All-weather for ETFs") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Calculate the trailing variance
lookb <- 152
varm <- HighFreq::roll_var(retp, lookb=lookb)
# Calculate the trailing Kelly ratio
meanv <- HighFreq::roll_mean(retp, lookb=lookb)
weightv <- ifelse(varm > 0, meanv/varm, 0)
sum(is.na(weightv))
weightv <- weightv/sqrt(rowSums(weightv^2))
weightv <- rutils::lagit(weightv)
# Calculate the momentum profits and losses
pnls <- rowSums(weightv*retp)
# Calculate the transaction costs
bidask <- 0.0
costv <- 0.5*bidask*rowSums(abs(rutils::diffit(weightv)))
pnls <- (pnls - costv)
# Scale the momentum volatility to all-weather
pnls <- sd(retaw)*pnls/sd(pnls)
# Calculate the wealth of momentum returns
wealthv <- cbind(retaw, pnls, 0.5*(pnls + retaw))
colnames(wealthv) <- c("All-weather", "Momentum", "Combined")
wealthv <- xts::xts(wealthv, datev)
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
cor(wealthv)
# Plot dygraph of the momentum strategy returns
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Daily Momentum Strategy for ETFs vs All-Weather") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Simulate a daily ETF momentum strategy
pnls <- btmomdaily(retp=retp, lookb=152, bidask=bidask)
# Perform sapply loop over lookbv
lookbv <- seq(90, 190, by=10)
pnls <- sapply(lookbv, btmomdaily,
  retp=retp, bidask=bidask)
# Scale the momentum volatility to all-weather
pnls <- apply(pnls, MARGIN=2,
  function(pnl) sd(retaw)*pnl/sd(pnl))
colnames(pnls) <- paste0("lookb=", lookbv)
pnls <- xts::xts(pnls, datev)
tail(pnls)
# Plot dygraph of daily ETF momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Daily ETF Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Plot daily ETF momentum strategies using quantmod
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls)[endd],
  theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=plot_theme$col$line.col, bty="n")
# Extract ETF returns
symbolv <- c("VTI", "IEF", "DBC")
retp <- rutils::etfenv$returns[, symbolv]
retp <- na.omit(retp)
# Or, select rows with IEF data
# retp <- retp[zoo::index(rutils::etfenv$IEF)]
# Copy over NA values
# retp[1, is.na(retp[1, ])] <- 0
# retp <- zoo::na.locf(retp, na.rm=FALSE)
# Select all the ETF symbols except "VXX", "SVXY" "MTUM", "QUAL", "VLUE", "USMV", "AIEQ", and "VYM"
symbolv <- colnames(rutils::etfenv$returns)
symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM", "QUAL", "VLUE", "USMV", "AIEQ", "VYM"))]
# Extract columns of rutils::etfenv$returns and overwrite NA values
retp <- rutils::etfenv$returns["1999/", symbolv]
sum(is.na(retp)) # Number of NA values
nstocks <- NCOL(retp)
datev <- zoo::index(retp)
# Calculate the covariance ignoring NA values
covmat <- cov(retp, use="pairwise.complete.obs")
sum(is.na(covmat))
# Calculate the inverse of covmat
invmat <- solve(covmat)
round(invmat %*% covmat, digits=5)
# Calculate the generalized inverse of covmat
invreg <- MASS::ginv(covmat)
all.equal(unname(invmat), invreg)
# Create rectangular matrix with collinear columns
matv <- matrix(rnorm(10*8), nc=10)
# Calculate covariance matrix
covmat <- cov(matv)
# Calculate inverse of covmat - error
invmat <- solve(covmat)
# Perform eigen decomposition
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Set tolerance for determining zero singular values
precv <- sqrt(.Machine$double.eps)
# Calculate generalized inverse from the eigen decomposition
notzero <- (eigenval > (precv*eigenval[1]))
inveigen <- eigenvec[, notzero] %*%
  (t(eigenvec[, notzero]) / eigenval[notzero])
# Inverse property of invreg doesn't hold
all.equal(covmat, inveigen %*% covmat)
# Generalized inverse property of invreg holds
all.equal(covmat, covmat %*% inveigen %*% covmat)
# Calculate generalized inverse using MASS::ginv()
invreg <- MASS::ginv(covmat)
# Verify that inveigen is the same as invreg
all.equal(inveigen, invreg)
# Maximum Sharpe weights in-sample interval
retis <- retp["/2014"]
raterf <- 0.03/252
retx <- (retis - raterf)
invreg <- MASS::ginv(cov(retis, use="pairwise.complete.obs"))
weightv <- drop(invreg %*% colMeans(retx, na.rm=TRUE))
weightv <- weightv/sqrt(sum(weightv^2))
names(weightv) <- colnames(retp)
# Plot portfolio weights
barplot(sort(weightv), main="Maximum Sharpe Weights", cex.names=0.7)
# Calculate the equal weight index
retew <- xts::xts(rowMeans(retp, na.rm=TRUE), datev)
# Calculate the in-sample weighted returns using Rcpp
pnlis <- HighFreq::mult_mat(weightv, retis)
# Or using the transpose
# pnlis <- unname(t(t(retis)*weightv))
pnlis <- rowMeans(pnlis, na.rm=TRUE)
pnlis <- pnlis*sd(retew["/2014"])/sd(pnlis)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retew["/2014"], pnlis, (pnlis + retew["/2014"])/2)
colnames(wealthv) <- c("Equal Weight", "Max Sharpe", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="In-Sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(width=300)
# Calculate the equal weight index
retos <- retp["2015/"]
# Calculate out-of-sample portfolio returns
pnlos <- HighFreq::mult_mat(weightv, retos)
pnlos <- rowMeans(pnlos, na.rm=TRUE)
pnlos <- pnlos*sd(retew["2015/"])/sd(pnlos)
wealthv <- cbind(retew["2015/"], pnlos, (pnlos + retew["2015/"])/2)
colnames(wealthv) <- c("Equal Weight", "Max Sharpe", "Combined")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Out-of-Sample Optimal Portfolio Returns") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(width=300)
# Maximum Sharpe weights in-sample interval
invreg <- MASS::ginv(cov(retis, use="pairwise.complete.obs"))
weightv <- invreg %*% colMeans(retx, na.rm=TRUE)
names(weightv) <- colnames(retp)
# Calculate cumulative wealth
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("Equal Weight", "Optimal", "Combined")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Out-of-Sample Optimal Portfolio Returns for ETFs") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=400)
# Calculate in-sample covariance matrix
covmat <- cov(retis, use="pairwise.complete.obs")
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Plot the eigenvalues
barplot(eigenval, main="ETF Covariance Eigenvalues", cex.names=0.7)
# Calculate reduced inverse of covariance matrix
dimax <- 9
invred <- eigenvec[, 1:dimax] %*%
  (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
# Reduced inverse does not satisfy matrix inverse property
all.equal(covmat, covmat %*% invred %*% covmat)
# Calculate portfolio weights
weightv <- drop(invred %*% colMeans(retis, na.rm=TRUE))
names(weightv) <- colnames(retp)
# Calculate cumulative wealth
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("Equal Weight", "DimReduction", "Combined")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Portfolio Returns With Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=400)
# Shrink the in-sample returns to their mean
alphac <- 0.7
retxm <- rowMeans(retx, na.rm=TRUE)
retxis <- (1-alphac)*retx + alphac*retxm
# Calculate portfolio weights
weightv <- invred %*% colMeans(retxis, na.rm=TRUE)
# Calculate cumulative wealth
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("Equal Weight", "Optimal", "Combined")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Portfolio With Dimension Reduction and Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)
# Define monthly end and start points
endd <- rutils::calc_endpoints(retp, interval="months")
endd <- endd[endd > (nstocks+1)]
npts <- NROW(endd)
lookb <- 3
startp <- c(rep_len(0, lookb), endd[1:(npts-lookb)])
# Perform loop over end points
pnls <- lapply(1:(npts-1), function(tday) {
    # Calculate the portfolio weights
    retis <- retx[startp[tday]:endd[tday], ]
    covmat <- cov(retis, use="pairwise.complete.obs")
    covmat[is.na(covmat)] <- 0
    invreg <- MASS::ginv(covmat)
    colm <- colMeans(retis, na.rm=TRUE)
    colm[is.na(colm)] <- 0
    weightv <- invreg %*% colm
    # Calculate the in-sample portfolio returns
    pnlis <- HighFreq::mult_mat(weightv, retis)
    pnlis <- rowMeans(pnlis, na.rm=TRUE)
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    pnlos <- HighFreq::mult_mat(weightv, retos)
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    xts::xts(pnlos, zoo::index(retos))
})  # end lapply
pnls <- do.call(rbind, pnls)
pnls <- rbind(retew[paste0("/", start(pnls)-1)], pnls)
# Calculate the Sharpe and Sortino ratios
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("Equal", "PortfStrat", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endd], main="Monthly ETF Rolling Portfolio Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Perform loop over end points
dimax <- 9
pnls <- lapply(1:(npts-1), function(tday) {
    # Calculate the portfolio weights
    retis <- retx[startp[tday]:endd[tday], ]
    covmat <- cov(retis, use="pairwise.complete.obs")
    covmat[is.na(covmat)] <- 0
    eigend <- eigen(covmat)
    eigenvec <- eigend$vectors
    eigenval <- eigend$values
    invred <- eigenvec[, 1:dimax] %*%
(t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
    colm <- colMeans(retis, na.rm=TRUE)
    colm[is.na(colm)] <- 0
    weightv <- invred %*% colm
    # Calculate the in-sample portfolio returns
    pnlis <- HighFreq::mult_mat(weightv, retis)
    pnlis <- rowMeans(pnlis, na.rm=TRUE)
    weightv <- weightv*0.01/sd(pnlis)
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    pnlos <- HighFreq::mult_mat(weightv, retos)
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    xts::xts(pnlos, zoo::index(retos))
})  # end lapply
pnls <- do.call(rbind, pnls)
pnls <- rbind(retew[paste0("/", start(pnls)-1)], pnls)
# Calculate the Sharpe and Sortino ratios
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("Equal", "PortfStrat", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Rolling Portfolio Strategy With Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
alphac <- 0.7 # Return shrinkage intensity
# Perform loop over end points
pnls <- lapply(1:(npts-1), function(tday) {
    # Shrink the in-sample returns to their mean
    retis <- retx[startp[tday]:endd[tday], ]
    rowm <- rowMeans(retis, na.rm=TRUE)
    rowm[is.na(rowm)] <- 0
    retis <- (1-alphac)*retis + alphac*rowm
    # Calculate the portfolio weights
    covmat <- cov(retis, use="pairwise.complete.obs")
    covmat[is.na(covmat)] <- 0
    eigend <- eigen(covmat)
    eigenvec <- eigend$vectors
    eigenval <- eigend$values
    invred <- eigenvec[, 1:dimax] %*%
(t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
    colm <- colMeans(retis, na.rm=TRUE)
    colm[is.na(colm)] <- 0
    weightv <- invred %*% colm
    # Scale the weights to volatility target
    pnlis <- HighFreq::mult_mat(weightv, retis)
    pnlis <- rowMeans(pnlis, na.rm=TRUE)
    weightv <- weightv*0.01/sd(pnlis)
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    pnlos <- HighFreq::mult_mat(weightv, retos)
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    xts::xts(pnlos, zoo::index(retos))
})  # end lapply
pnls <- do.call(rbind, pnls)
pnls <- rbind(retew[paste0("/", start(pnls)-1)], pnls)
# Calculate the Sharpe and Sortino ratios
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("Equal", "PortfStrat", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Rolling Portfolio Strategy With Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Define weekly dates
weeks <- seq.Date(from=as.Date("2001-01-01"), to=as.Date("2021-04-01"), by="weeks")
# Perform loop over monthly dates
lookb <- 21
dimax <- 9
pnls <- lapply((lookb+1):(NROW(weeks)-1), function(tday) {
  # Define in-sample and out-of-sample returns
  insample <- (datev > weeks[tday-lookb]) & (datev < weeks[tday])
  outsample <- (datev > weeks[tday]) & (datev < weeks[tday+1])
  retis <- retp[insample]
  retos <- retp[outsample]
  # Calculate reduced inverse of covariance matrix
  # invreg <- MASS::ginv(cov(retis, use="pairwise.complete.obs"))  # if VXX and SVXY are included then no dimension reduction is better
  invred <- HighFreq::calc_invsd(cov(retis, use="pairwise.complete.obs"), dimax=dimax)
  weightv <- invred %*% colMeans(retis - raterf, na.rm=TRUE)
  # weightv <- drop(weightv/sqrt(sum(weightv^2)))
  # Calculate portfolio pnls out-of-sample
  xts::xts(retos %*% weightv, zoo::index(retos))
})  # end lapply
pnls <- do.call(rbind, pnls)
# Plot dygraph of weekly rolling ETF portfolio strategy
vti <- rutils::diffit(cumsum(retew)[zoo::index(pnls),])
wealthv <- cbind(vti, pnls)
colnames(wealthv) <- c("Equal", "Strategy")
colv <- colnames(wealthv)
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Weekly ETF Rolling Portfolio Strategy With dimension reduction") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Define backtest functional for rolling portfolio strategy
roll_portf <- function(retx, # Excess returns
                 retp, # Stock returns
                 endd, # End points
                 lookb=12, # Look-back interval
                 dimax=3, # Dimension reduction parameter
                 alphac=0.0, # Return shrinkage intensity
                 bidask=0.0, # Bid-offer spread
                 ...) {
  npts <- NROW(endd)
  startp <- c(rep_len(0, lookb), endd[1:(npts-lookb)])
  pnls <- lapply(1:(npts-1), function(tday) {
    retis <- retx[startp[tday]:endd[tday], ]
    # Shrink the in-sample returns to their mean
    if (alphac > 0) {
rowm <- rowMeans(retis, na.rm=TRUE)
rowm[is.na(rowm)] <- 0
retis <- (1-alphac)*retis + alphac*rowm
    } # end if
    # Calculate the portfolio weights
    covmat <- cov(retis, use="pairwise.complete.obs")
    covmat[is.na(covmat)] <- 0
    eigend <- eigen(covmat)
    eigenvec <- eigend$vectors
    eigenval <- eigend$values
    invred <- eigenvec[, 1:dimax] %*% (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
    colm <- colMeans(retis, na.rm=TRUE)
    colm[is.na(colm)] <- 0
    weightv <- invred %*% colm
    # Scale the weights to volatility target
    pnlis <- HighFreq::mult_mat(weightv, retis)
    pnlis <- rowMeans(pnlis, na.rm=TRUE)
    weightv <- weightv*0.01/sd(pnlis)
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    pnlos <- HighFreq::mult_mat(weightv, retos)
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    xts::xts(pnlos, zoo::index(retos))
  })  # end lapply
  pnls <- do.call(rbind, pnls)
  # Add warmup period to pnls
  rbind(retew[paste0("/", start(pnls)-1)], pnls)
}  # end roll_portf
# Simulate a monthly ETF portfolio strategy
pnls <- roll_portf(retx=retx, retp=retp, endd=endd,
  lookb=lookb, dimax=dimax)
# Perform sapply loop over lookbv
lookbv <- seq(2, 15, by=1)
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
pnls <- mclapply(lookbv, roll_portf, retp=retp, retx=retx,
  endd=endd, dimax=dimax, mc.cores=ncores)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lookb=", lookbv)
pnlsums <- sapply(pnls, sum)
lookb <- lookbv[which.max(pnlsums)]
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(pnls, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of monthly ETF portfolio strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd], main="Rolling Portfolio Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=600)
# Plot portfolio strategies using quantmod
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Rolling Portfolio Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=plot_theme$col$line.col, bty="n")
# Perform backtest for different dimax values
dimv <- 2:11
pnls <- mclapply(dimv, roll_portf, retp=retp, retx=retx,
  endd=endd, lookb=lookb, mc.zcores=ncores)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("dimax=", dimv)
pnlsums <- sapply(pnls, sum)
dimax <- dimv[which.max(pnlsums)]
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(pnls, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of monthly ETF portfolio strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Rolling Portfolio Strategies With Dimension Reduction") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Plot portfolio strategies using quantmod
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Rolling Portfolio Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=plot_theme$col$line.col, bty="n")
# Perform backtest over vector of return shrinkage intensities
alphac <- seq(from=0.0, to=0.9, by=0.1)
pnls <- lapply(alphac, roll_portf, retx=retx,
  retp=retp, endd=endd, lookb=lookb, dimax=dimax)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("alpha=", alphac)
pnlsums <- sapply(pnls, sum)
alphac <- alphac[which.max(pnlsums)]
# Plot dygraph of monthly ETF portfolio strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd],
  main="Rolling Portfolio Strategies With Return Shrinkage") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Plot portfolio strategies using quantmod
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Rolling Portfolio Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
  col=plot_theme$col$line.col, bty="n")
# Load stock returns
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
datev <- zoo::index(na.omit(retstock$GOOGL))
retp <- retstock[datev] # Subset the returns to GOOGL
# Remove the stocks with any NA values
nonas <- sapply(retp, function(x) sum(is.na(x)))
nonas <- !(nonas > 0)
retp <- retp[, nonas]
nstocks <- NCOL(retp)
datev <- zoo::index(retp)
colv <- colnames(retp)
retis <- retp["/2014"] # In-sample returns
raterf <- 0.03/252
retx <- (retis - raterf) # Excess returns
# Maximum Sharpe weights in-sample interval
covmat <- cov(retis, use="pairwise.complete.obs")
invreg <- MASS::ginv(covmat)
colmeanv <- colMeans(retx, na.rm=TRUE)
weightv <- drop(invreg %*% colmeanv)
names(weightv) <- colv
head(sort(weightv))
tail(sort(weightv))
# Calculate the portfolio returns
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- xts::xts(pnls, datev)
retew <- xts::xts(rowMeans(retp, na.rm=TRUE), datev)
pnls <- pnls*sd(retew["/2014"])/sd(pnls["/2014"])
wealthv <- cbind(retew, pnls)
colnames(wealthv) <- c("Equal Weight", "MaxSharpe")
# Calculate the in-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["/2014"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["2015/"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative portfolio returns
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Out-of-Sample Maximum Sharpe Stock Portfolio") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)
# Calculate reduced inverse of covariance matrix
dimax <- 10
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
invred <- eigenvec[, 1:dimax] %*%
  (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
# Calculate portfolio weights and returns
weightv <- invred %*% colmeanv
rownames(weightv) <- colv
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- xts::xts(pnls, datev)
pnls <- pnls*sd(retew["/2014"])/sd(pnls["/2014"])
# Combine with equal weight
wealthv <- cbind(retew, pnls)
colnames(wealthv) <- c("Equal Weight", "MaxSharpe")
# Calculate the in-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["/2014"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["2015/"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative portfolio returns
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Maximum Sharpe With Dimension Reduction") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)
# Shrink the in-sample returns to their mean
alphac <- 0.3
retxm <- rowMeans(retx, na.rm=TRUE)
retxis <- (1-alphac)*retx + alphac*retxm
# Calculate portfolio weights and returns
weightv <- drop(invred %*% colMeans(retxis, na.rm=TRUE))
names(weightv) <- colv
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- xts::xts(pnls, datev)
pnls <- pnls*sd(retew["/2014"])/sd(pnls["/2014"])
# Combine with equal weight
wealthv <- cbind(retew, pnls)
colnames(wealthv) <- c("Equal Weight", "MaxSharpe")
# Calculate the in-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["/2014"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["2015/"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative portfolio returns
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Maximum Sharpe With Return Shrinkage") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)
# Define monthly end points
endd <- rutils::calc_endpoints(retp, interval="months")
endd <- endd[endd > (nstocks+1)]
npts <- NROW(endd) ; lookb <- 12
startp <- c(rep_len(0, lookb), endd[1:(npts-lookb)])
# !!! Perform parallel loop over end points - takes very long!!!
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
pnls <- mclapply(1:(npts-1), function(tday) {
    # Subset the excess returns
    retis <- retp[startp[tday]:endd[tday], ]
    retis[is.na(retis)] <- 0
    invreg <- MASS::ginv(cov(retis, use="pairwise.complete.obs"))
    # Calculate the maximum Sharpe ratio portfolio weights
    retm <- colMeans(retis, na.rm=TRUE)
    weightv <- invreg %*% retm
    # Zero weights if sparse data
    zerov <- sapply(retis, function(x) (sum(x == 0) > 5))
    weightv[zerov] <- 0
    # Calculate in-sample portfolio returns
    pnlis <- (retis %*% weightv)
    # Scale the weights to the in-sample volatility
    weightv <- weightv*sd(retm)/sd(pnlis)
    # Calculate the out-of-sample portfolio returns
    retos <- retp[(endd[tday]+1):endd[tday+1], ]
    pnls <- HighFreq::mult_mat(weightv, retos)
    pnls <- rowMeans(pnls, na.rm=TRUE)
    xts::xts(pnls, zoo::index(retos))
})  # end lapply
pnls <- rutils::do_call(rbind, pnls)
pnls <- rbind(retew[paste0("/", start(pnls)-1)], pnls*sd(retew)/sd(pnls))
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retew, pnls)
colnames(wealthv) <- c("Equal Weight", "MaxSharpe")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot cumulative strategy returns
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Rolling Maximum Sharpe Portfolio Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
# Create random matrix of returns
matv <- matrix(rnorm(300), nc=5)
# Reduced inverse of covariance matrix
dimax <- 3
eigend <- eigen(covmat)
invred <- eigend$vectors[, 1:dimax] %*%
  (t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])
# Reduced inverse using RcppArmadillo
invarma <- HighFreq::calc_inv(covmat, dimax)
all.equal(invred, invarma)
# Microbenchmark RcppArmadillo code
library(microbenchmark)
summary(microbenchmark(
  rcode={eigend <- eigen(covmat)
    eigend$vectors[, 1:dimax] %*%
(t(eigend$vectors[, 1:dimax]) / eigend$values[1:dimax])
  },
  rcpp=calc_inv(covmat, dimax),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary
# Shift the end points to C++ convention
endd <- (endd - 1)
endd[endd < 0] <- 0
startp <- (startp - 1)
startp[startp < 0] <- 0
# Specify dimension reduction and return shrinkage using list of portfolio optimization parameters
dimax <- 9
alphac <- 0.8
controlv <- HighFreq::param_portf(method="maxsharpe",
  dimax=dimax, alpha=alphac)
# Perform backtest in Rcpp - takes very long!!!
retx <- (retp - raterf)
pnls <- HighFreq::back_test(retx=retx, retp=retp,
  startp=startp, endd=endd, controlv=controlv)
pnls <- pnls*sd(retew)/sd(pnls)
# Calculate the out-of-sample Sharpe and Sortino ratios
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("Equal", "MaxSharpe", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot cumulative strategy returns
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Rolling S&P500 Portfolio Strategy With Shrinkage") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Perform backtest over vector of dimension reduction parameters
dimv <- seq(from=3, to=40, by=2)
pnls <- mclapply(dimv, function(dimax) {
  controlv <- HighFreq::param_portf(method="maxsharpe",
    dimax=dimax, alpha=alphac)
  HighFreq::back_test(retx=retx, retp=retp,
    startp=startp, endd=endd, controlv=controlv)
})  # end lapply
profilev <- sapply(pnls, sum)
whichmax <- which.max(profilev)
dimax <- dimv[whichmax]
# Plot of rolling strategy PnL as a function of dimax
plot(x=dimv, y=profilev, t="l", xlab="dimax", ylab="pnl",
  main="Rolling Strategy PnL as Function of dimax")
# Perform backtest over vector of return shrinkage intensities
alphav <- seq(from=0.5, to=0.9, by=0.1)
pnls <- mclapply(alphav, function(alphac) {
  controlv <- HighFreq::param_portf(method="maxsharpe",
      dimax=dimax, alpha=alphac)
  HighFreq::back_test(retx=retx, retp=retp,
      startp=startp, endd=endd, controlv=controlv)
})  # end lapply
profilev <- sapply(pnls, sum)
whichmax <- which.max(profilev)
alphac <- alphav[whichmax]
# Plot of rolling strategy PnL as a function of shrinkage intensity
plot(x=alphav, y=profilev, t="l",
  main="Rolling Strategy PnL as Function of Return Shrinkage",
  xlab="Shrinkage Intensity Alpha", ylab="pnl")
# Create list of model parameters
controlv <- HighFreq::param_portf(method="maxsharpe",
      dimax=dimax, alpha=alphac)
# Perform backtest over look-backs
lookbv <- seq(from=5, to=16, by=1)
pnls <- mclapply(lookbv, function(lookb) {
  startp <- c(rep_len(0, lookb), endd[1:(npts-lookb)])
  startp <- (startp - 1) ; startp[startp < 0] <- 0
  HighFreq::back_test(retx=retx, retp=retp,
    startp=startp, endd=endd, controlv=controlv)
})  # end lapply
profilev <- sapply(pnls, sum)
whichmax <- which.max(profilev)
lookb <- lookbv[whichmax]
# Plot of rolling strategy PnL as a function of look-back interval
plot(x=lookbv, y=profilev, t="l", main="MaxSharpe PnL as Function of Look-back Interval",
  xlab="Look-back Interval", ylab="pnl")
# Calculate the out-of-sample Sharpe and Sortino ratios
pnls <- pnls[[whichmax]]
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- cbind(retew, pnls, (pnls + retew)/2)
colnames(wealthv) <- c("Equal", "MaxSharpe", "Combined")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Dygraph the cumulative wealth
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Maximum Sharpe Portfolio Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", label="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)
# Create rectangular matrix with collinear columns
matv <- matrix(rnorm(10*8), nc=10)
# Calculate covariance matrix
covmat <- cov(matv)
# Calculate inverse of covmat - error
invmat <- solve(covmat)
# Perform eigen decomposition
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
# Set tolerance for determining zero singular values
precv <- sqrt(.Machine$double.eps)
# Calculate generalized inverse matrix
notzero <- (eigenval > (precv*eigenval[1]))
invred <- eigenvec[, notzero] %*%
  (t(eigenvec[, notzero]) / eigenval[notzero])
# Verify inverse property of invred
all.equal(covmat, covmat %*% invred %*% covmat)
# Calculate generalized inverse of covmat
invreg <- MASS::ginv(covmat)
# Verify that invreg is same as invred
all.equal(invreg, invred)
