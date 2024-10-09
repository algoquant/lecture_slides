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
colnamev <- colnames(wealthv)
dygraphs::dygraph(wealthv[endd], main="Stock Index and Random Portfolios") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name=colnamev[1], strokeWidth=3) %>%
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
retlow <- rowMeans(retp[, (riskv <= medianv)], na.rm=TRUE)
rethigh <- rowMeans(retp[, (riskv > medianv)], na.rm=TRUE)
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
colnamev <- c("Equal Weight", "Long-Short Vol")
colnames(wealthv) <- colnamev
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

# Load daily S&P500 percentage stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Select returns of 100 stocks
retp <- retstock100["2000/"]
datev <- zoo::index(retp) # Dates vector
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
hist(perfstat)
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
colnames(wealthv) <- c("Index", "Strategy")
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
colnames(wealthv) <- c("Index", "Strategy")
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
pnll <- lapply(lookbv, btmomweight, retp=retp, endd=endd, objfun=objfun)
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
colnames(wealthv) <- c("Index", "Momentum", "Combined")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of stock index and momentum strategy
dygraphs::dygraph(cumsum(wealthv)[endd],
  main="Optimal Weighted Momentum Strategy for Stocks") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300)

# To simplify, set NAs to zero
retp[is.na(retp)] <- 0
# Calculate the trailing average returns and variance using C++ code
lambdaf <- 0.995
varm <- HighFreq::run_var(retp, lambda=lambdaf)
meanm <- varm[, 1:nstocks]
varm <- varm[, (nstocks+1):(2*nstocks)]
meanm <- ifelse(is.na(meanm), 0, meanm)
varm <- ifelse(is.na(varm), 1, varm)
# Calculate the trailing Kelly ratios
weightv <- ifelse(varm > 0, meanm/varm, 0)
weightv[1, ] <- 1
weightv <- weightv/sqrt(rowSums(weightv^2))
weightv <- rutils::lagit(weightv)
# Calculate the momentum profits and losses
pnls <- rowSums(weightv*retp, na.rm=TRUE)
# Calculate the transaction costs
bidask <- 0.0
costv <- 0.5*bidask*rowSums(abs(rutils::diffit(weightv)))
pnls <- (pnls - costv)

# Scale the momentum volatility to the equal weight index
volew <- sd(retew)
pnls <- volew*pnls/sd(pnls)
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retew, pnls, 0.5*(retew + pnls))
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("Index", "Momentum", "Combined")
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
  varm <- varm[, (nstocks+1):(2*nstocks)]
  meanm <- ifelse(is.na(meanm), 0, meanm)
  varm <- ifelse(is.na(varm), 1, varm)
  weightv <- ifelse(varm > 0, meanm/varm, 0)
  weightv[1, ] <- 1
  weightv <- weightv/sqrt(rowSums(weightv^2))
  weightv <- rutils::lagit(weightv)
  # Calculate the momentum profits and losses
  pnls <- trend*rowSums(weightv*retp, na.rm=TRUE)
  # Calculate the transaction costs
  costv <- 0.5*bidask*rowSums(abs(rutils::diffit(weightv)))
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
  varm <- varm[, (nstocks+1):(2*nstocks)]
  weightv <- ifelse(varm > 0, meanm/varm, 0)
  weightv[1, ] <- 1
  weightv <- weightv/sqrt(rowSums(weightv^2))
  # Average the past weights
  weightv <- HighFreq::run_mean(weightv, lambda=lambdaf)
  weightv <- rutils::lagit(weightv)
  # Calculate the momentum profits and losses
  pnls <- trend*rowSums(weightv*retp)
  # Calculate the transaction costs
  costv <- 0.5*bidask*rowSums(abs(rutils::diffit(weightv)))
  (pnls - costv)
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
colnames(wealthv) <- c("Index", "Momentum", "Combined")
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

# Perform sapply loop over lookbv
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

# Calculate the PCA loadings for standardized returns
retsc <- lapply(retp, function(x) (x - mean(x))/sd(x))
retsc <- do.call(cbind, retsc)
covmat <- cov(retsc)
pcad <- eigen(covmat)
pcal <- pcad$vectors # The PCA loadings
rownames(pcal) <- colnames(retp)
sort(-pcal[, 1], decreasing=TRUE)
sort(pcal[, 2], decreasing=TRUE)
round((t(pcal) %*% pcal)[1:5, 1:5], 4)
# Calculate the PCA time series from the stock returns and the PCA loadings
retpca <- retsc %*% pcal
round((t(retpca) %*% retpca)[1:5, 1:5], 4)
# Calculate the PCA using prcomp()
pcad <- prcomp(retsc, center=FALSE, scale=FALSE)
all.equal(abs(pcad$x), abs(retpca), check.attributes=FALSE)
retpca <- xts::xts(retpca, order.by=datev)

# Simulate daily PCA momentum strategies for multiple lambdaf parameters
dimax <- 11
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
colnames(wealthv) <- c("Index", "Momentum", "Combined")
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
pnls <- mclapply(lambdav, btmomdailyhold, retp=retpca[, (dimax+1):NCOL(retpca)],
   trend=(-1), mc.cores=ncores)
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
pcad <- prcomp(retp[insample])
# Calculate the out-of-sample PCA time series
retpca <- xts::xts(retp[outsample] %*% pcad$rotation, order.by=datev[outsample])
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
pnls <- mclapply(lambdav, btmomdailyhold, retp=retpca[, (dimax+1):(nstocks-10)],
   trend=(-1), mc.cores=ncores)
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
