# Perform regression using formula
retsp <- na.omit(rutils::etfenv$returns[, c("XLP", "VTI")])
riskfree <- 0.03/252
retsp <- (retsp - riskfree)
regmod <- lm(XLP ~ VTI, data=retsp)
regmodsum <- summary(regmod)
# Get regression coefficients
coef(regmodsum)
# Get alpha and beta
coef(regmodsum)[, 1]

# Plot scatterplot of returns with aspect ratio 1
plot(XLP ~ VTI, data=rutils::etfenv$returns,
     xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.1),
     asp=1, main="Regression XLP ~ VTI")
# Add regression line and perpendicular line
abline(regmod, lwd=2, col="red")
abline(a=0, b=-1/coef(regmodsum)[2, 1], lwd=2, col="blue")

# Get regression coefficients
coef(regmodsum)
# Calculate regression coefficients from scratch
betav <- drop(cov(retsp$XLP, retsp$VTI)/var(retsp$VTI))
alpha <- drop(mean(retsp$XLP) - betav*mean(retsp$VTI))
c(alpha, betav)
# Calculate the residuals
residuals <- (retsp$XLP - (alpha + betav*retsp$VTI))
# Calculate the standard deviation of residuals
nrows <- NROW(residuals)
residsd <- sqrt(sum(residuals^2)/(nrows - 2))
# Calculate the standard errors of beta and alpha
sum2 <- sum((retsp$VTI - mean(retsp$VTI))^2)
betasd <- residsd/sqrt(sum2)
alphasd <- residsd*sqrt(1/nrows + mean(retsp$VTI)^2/sum2)
c(alphasd, betasd)
# Perform the Durbin-Watson test of autocorrelation of residuals
lmtest::dwtest(regmod)

retsp <- rutils::etfenv$returns
symbolv <- colnames(retsp)
symbolv <- symbolv[symbolv != "VTI"]
# Perform regressions and collect statistics
betam <- sapply(symbolv, function(symbol) {
# Specify regression formula
  formulav <- as.formula(paste(symbol, "~ VTI"))
# Perform regression
  regmod <- lm(formulav, data=retsp)
# Get regression summary
  regmodsum <- summary(regmod)
# Collect regression statistics
  with(regmodsum, 
    c(beta=coefficients[2, 1], 
pbeta=coefficients[2, 4],
alpha=coefficients[1, 1], 
palpha=coefficients[1, 4], 
pdw=lmtest::dwtest(regmod)$p.value))
})  # end sapply
betam <- t(betam)
# Sort by palpha
betam <- betam[order(betam[, "palpha"]), ]

betam

library(PerformanceAnalytics)
# Calculate XLP beta
PerformanceAnalytics::CAPM.beta(Ra=retsp$XLP, Rb=retsp$VTI)
# Or
retsxlp <- na.omit(retsp[, c("XLP", "VTI")])
betav <- drop(cov(retsxlp$XLP, retsxlp$VTI)/var(retsxlp$VTI))
betav
# Calculate XLP alpha
PerformanceAnalytics::CAPM.alpha(Ra=retsp$XLP, Rb=retsp$VTI)
# Or
mean(retsp$XLP - betav*retsp$VTI)
# Calculate XLP bull beta
PerformanceAnalytics::CAPM.beta.bull(Ra=retsp$XLP, Rb=retsp$VTI)
# Calculate XLP bear beta
PerformanceAnalytics::CAPM.beta.bear(Ra=retsp$XLP, Rb=retsp$VTI)

symbolv <- rownames(betam)
betav <- betam[-match(c("VXX", "SVXY", "MTUM", "USMV", "QUAL"), symbolv), 1]
betav <- c(1, betav)
names(betav)[1] <- "VTI"
retsann <- sapply(retsp[, names(betav)], PerformanceAnalytics::Return.annualized)
# Plot scatterplot of returns vs betas
minrets <- min(retsann)
plot(retsann ~ betav, xlab="betas", ylab="returns",
     ylim=c(minrets, -minrets), main="Security Market Line for ETFs")
retsvti <- retsann["VTI"]
points(x=1, y=retsvti, col="red", lwd=3, pch=21)
# Plot Security Market Line
riskfree <- 0.01
abline(a=riskfree, b=(retsvti-riskfree), col="green", lwd=2)
# Add labels
text(x=betav, y=retsann, labels=names(betav), pos=2, cex=0.8)

# Find optimal risk-free rate by minimizing residuals
rss <- function(riskfree) {
  sum((retsann - riskfree + betav*(retsvti-riskfree))^2)
}  # end rss
optimrss <- optimize(rss, c(-1, 1))
riskfree <- optimrss$minimum
abline(a=riskfree, b=(retsvti-riskfree), col="blue", lwd=2)
legend(x="top", bty="n", title="Security Market Line",
 legend=c("optimal fit", "riskfree=0.01"),
 y.intersp=0.5, cex=1.0, lwd=6, lty=1, col=c("blue", "green"))

# Load S&P500 constituent stock returns
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
retsvti <- na.omit(rutils::etfenv$returns$VTI)
retsp <- returns[index(retsvti), ]
nrows <- NROW(retsp)
# Calculate stock betas
betav <- sapply(retsp, function(x) {
  retsp <- na.omit(cbind(x, retsvti))
  drop(cov(retsp[, 1], retsp[, 2])/var(retsp[, 2]))
})  # end sapply
mean(betav)
# Calculate annual stock returns
retsann <- retsp
retsann[1, ] <- 0
retsann <- zoo::na.locf(retsann, na.rm=FALSE)
retsann <- 252*sapply(retsann, sum)/nrows
# Remove stocks with zero returns
sum(retsann == 0)
betav <- betav[retsann > 0]
retsann <- retsann[retsann > 0]
retsvti <- 252*mean(retsvti)
# Plot scatterplot of returns vs betas
plot(retsann ~ betav, xlab="betas", ylab="returns",
     main="Security Market Line for Stocks")
points(x=1, y=retsvti, col="red", lwd=3, pch=21)
# Plot Security Market Line
riskfree <- 0.01
abline(a=riskfree, b=(retsvti-riskfree), col="green", lwd=2)

# Find optimal risk-free rate by minimizing residuals
rss <- function(riskfree) {
  sum((retsann - riskfree + betav*(retsvti-riskfree))^2)
}  # end rss
optimrss <- optimize(rss, c(-1, 1))
riskfree <- optimrss$minimum
abline(a=riskfree, b=(retsvti-riskfree), col="blue", lwd=2)
legend(x="top", bty="n", title="Security Market Line",
 legend=c("optimal fit", "riskfree=0.01"),
 y.intersp=0.5, cex=1.0, lwd=6, lty=1, col=c("blue", "green"))

library(PerformanceAnalytics)
# Calculate XLP Treynor ratio
TreynorRatio(Ra=retsp$XLP, Rb=retsp$VTI)
# Calculate XLP Information ratio
InformationRatio(Ra=retsp$XLP, Rb=retsp$VTI)

PerformanceAnalytics::table.CAPM(Ra=retsp[, c("XLP", "XLF")], 
                           Rb=retsp$VTI, scale=252)

capmstats <- table.CAPM(Ra=retsp[, symbolv],
        Rb=retsp$VTI, scale=252)
colnamev <- strsplit(colnames(capmstats), split=" ")
colnamev <- do.call(cbind, colnamev)[1, ]
colnames(capmstats) <- colnamev
capmstats <- t(capmstats)
capmstats <- capmstats[, -1]
colnamev <- colnames(capmstats)
whichv <- match(c("Annualized Alpha", "Information Ratio", "Treynor Ratio"), colnamev)
colnamev[whichv] <- c("Alpha", "Information", "Treynor")
colnames(capmstats) <- colnamev
capmstats <- capmstats[order(capmstats[, "Alpha"], decreasing=TRUE), ]
# Copy capmstats into etfenv and save to .RData file
etfenv <- rutils::etfenv
etfenv$capmstats <- capmstats
save(etfenv, file="/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")

rutils::etfenv$capmstats[, c("Beta", "Alpha", "Information", "Treynor")]

# Load the S&P500 stock prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# Subset (select) the prices after the start date of VTI
retvti <- na.omit(rutils::etfenv$returns$VTI)
colnames(retvti) <- "VTI"
prices <- prices[zoo::index(retvti)]
# Select columns with non-NA prices at start
prices <- prices[, !is.na(prices[1, ])]
dim(prices)
# Copy over NA prices using the function zoo::na.locf()
prices <- zoo::na.locf(prices, na.rm=FALSE)
sum(is.na(prices))
datev <- zoo::index(prices)
retvti <- retvti[datev]
nrows <- NROW(prices)
nstocks <- NCOL(prices)
# Normalize the prices so that they start at 1
pricesn <- lapply(prices, function(x) x/as.numeric(x[1]))
pricesn <- rutils::do_call(cbind, pricesn)
head(pricesn[, 1:5])

# Calculate the equal dollar-weighted average of all stock prices
indeks <- rowMeans(pricesn)
indeks <- xts::xts(indeks, order.by=datev)
colnames(indeks) <- "Index"
# Select a random, equal dollar-weighted portfolio of 5 stocks
set.seed(1121)
samplev <- sample.int(n=nstocks, size=5, replace=FALSE)
portf <- pricesn[, samplev]
portf <- rowMeans(portf)
portf <- xts::xts(portf, order.by=datev)
colnames(portf) <- "Random"
# Plot dygraph of stock index and random portfolio
wealthv <- cbind(indeks, portf)
colorv <- c("blue", "red")
endp <- rutils::calc_endpoints(prices, interval="months")
dygraphs::dygraph(wealthv[endp], main="Stock Index and Random Portfolio") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Select 10 random equal dollar-weighted sub-portfolios
set.seed(1121)
nportf <- 10
portfs <- sapply(1:nportf, function(x) {
  prices <- pricesn[, sample.int(n=nstocks, size=5, replace=FALSE)]
  rowMeans(prices)
})  # end sapply
portfs <- xts::xts(portfs, order.by=datev)
colnames(portfs) <- paste0("portf", 1:nportf)
# Sort the sub-portfolios according to perfomance
portfs <- portfs[, order(portfs[nrows])]
round(head(portfs), 3)
round(tail(portfs), 3)

# Plot dygraph of stock index and random portfolios
colorv <- colorRampPalette(c("red", "blue"))(nportf)
combined <- cbind(indeks, portfs)
colnames(combined)[1] <- "Index"
colnamev <- colnames(combined)
colorv <- c("green", colorv)
dygraphs::dygraph(log(combined[endp]), main="Stock Index and Random Portfolios") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=3, col="green") %>%
  dyLegend(show="always", width=500)

# Define cutoff between in-sample and out-of-sample intervals
cutoff <- nrows %/% 2
datev[cutoff]
insample <- 1:cutoff
outsample <- (cutoff + 1):nrows
# Calculate the 10 best performing stocks in-sample
perfstat <- sort(drop(coredata(pricesn[cutoff, ])), decreasing=TRUE)
symbolv <- names(head(perfstat, 10))
# Calculate the in-sample portfolio
pricis <- pricesn[insample, symbolv]
# Normalize the prices so that they are 1 at cutoff+1
pricesn <- lapply(prices, function(x) x/as.numeric(x[cutoff+1]))
pricesn <- rutils::do_call(cbind, pricesn)
# Calculate the out-of-sample portfolio
pricos <- pricesn[outsample, symbolv]
# Scale the prices to preserve the in-sample wealth
pricos <- sum(pricis[cutoff, ])*pricos/sum(pricos[1, ])

# Combine indeks with out-of-sample stock portfolio returns
wealthv <- rbind(pricis, pricos)
wealthv <- xts::xts(rowMeans(wealthv), datev)
wealthv <- cbind(indeks, wealthv)
colnames(wealthv)[2] <- "Portfolio"
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealthv[outsample, ]),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot out-of-sample stock portfolio returns
dygraphs::dygraph(log(wealthv[endp]), main="Out-of-sample Log Prices of Stock Portfolio") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(datev[cutoff], label="in-sample", strokePattern="solid", color="green") %>%
  dyLegend(width=500)

# Calculate the stock volatilities, betas, and alphas
retsp <- rutils::diffit(log(prices))
varvti <- drop(var(retvti))
meanvti <- mean(retvti)
riskret <- sapply(retsp, function(rets) {
  betav <- drop(cov(rets, retvti))/varvti
  resid <- rets - betav*retvti
  alphav <- mean(rets) - betav*meanvti
  c(alpha=alphav, beta=betav, vol=sd(rets), ivol=sd(resid))
})  # end sapply
riskret <- t(riskret)
tail(riskret)
# Sort stocks by their volatilities
riskret <- riskret[order(riskret[, "vol"]), ]
symbolv <- rownames(riskret)
# Calculate the cumulative returns of low and high volatility stocks
volow <- rowMeans(retsp[, symbolv[1:(nstocks %/% 2)]])
volhigh <- rowMeans(retsp[, symbolv[(nstocks %/% 2):nstocks]])
wealthv <- cbind(volow, volhigh, volow - 0.3*volhigh)
wealthv <- xts::xts(wealthv, order.by=datev)
colnames(wealthv) <- c("low_vol", "high_vol", "long_short")

# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the cumulative returns of low and high volatility stocks
endp <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(cumsum(wealthv)[endp], main="Low and High Volatility Stocks In-Sample") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(width=500)

# Merton-Henriksson test
predictor <- cbind(VTI=retvti, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(predictor)[2:3] <- c("merton", "treynor")
regmod <- lm(wealthv$long_short ~ VTI + merton, data=predictor); summary(regmod)
# Treynor-Mazuy test
regmod <- lm(wealthv$long_short ~ VTI + treynor, data=predictor); summary(regmod)
# Plot residual scatterplot
residv <- regmod$residuals
plot.default(x=retvti, y=residv, xlab="VTI", ylab="Low Volatility")
title(main="Treynor-Mazuy Market Timing Test\n for Low Volatility vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fittedv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retvti, y=fittedv, pch=16, col="red")
text(x=0.0, y=max(residv), paste("Treynor test t-value =", tvalue))

# Calculate the in-sample stock volatilities, betas, and alphas
varvti <- drop(var(retvti[insample]))
meanvti <- mean(retvti[insample])
riskretis <- sapply(retsp[insample], function(rets) {
  betav <- drop(cov(rets[insample], retvti[insample]))/varvti
  resid <- rets - betav*retvti[insample]
  alphav <- mean(rets[insample]) - betav*meanvti
  c(alpha=alphav, beta=betav, vol=sd(rets), ivol=sd(resid))
})  # end sapply
riskretis <- t(riskretis)
tail(riskretis)
# Sort stocks in-sample by their volatilities
riskretis <- riskretis[order(riskretis[, "vol"]), ]
head(riskretis)
symbolv <- rownames(riskretis)
# Calculate the out-of-sample returns of low and high volatility stocks
volow <- rowMeans(retsp[outsample, symbolv[1:(nstocks %/% 2)]])
volhigh <- rowMeans(retsp[outsample, symbolv[(nstocks %/% 2):nstocks]])
wealthv <- cbind(volow, volhigh, volow - 0.3*volhigh)
wealthv <- xts::xts(wealthv, order.by=datev[outsample])
colnames(wealthv) <- c("low_vol", "high_vol", "long_short")

# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the cumulative returns of low and high volatility stocks
endp <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(cumsum(wealthv)[endp], main="Low and High Volatility Stocks Out-Of-Sample") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(width=500)

# Sort stocks by their idiosyncratic volatilities
riskret <- riskret[order(riskret[, "ivol"]), ]
symbolv <- rownames(riskret)
# Calculate the cumulative returns of low and high volatility stocks
volow <- rowMeans(retsp[, symbolv[1:(nstocks %/% 2)]])
volhigh <- rowMeans(retsp[, symbolv[(nstocks %/% 2):nstocks]])
wealthv <- cbind(volow, volhigh, volow - 0.3*volhigh)
wealthv <- xts::xts(wealthv, order.by=datev)
colnames(wealthv) <- c("low_vol", "high_vol", "long_short")

# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the cumulative returns of low and high volatility stocks
endp <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(cumsum(wealthv)[endp], main="Low and High Idiosyncratic Volatility Stocks In-Sample") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(width=500)

# Merton-Henriksson test
predictor <- cbind(VTI=retvti, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(predictor)[2:3] <- c("merton", "treynor")
regmod <- lm(wealthv$long_short ~ VTI + merton, data=predictor); summary(regmod)
# Treynor-Mazuy test
regmod <- lm(wealthv$long_short ~ VTI + treynor, data=predictor); summary(regmod)
# Plot residual scatterplot
residv <- regmod$residuals
plot.default(x=retvti, y=residv, xlab="VTI", ylab="Low Volatility")
title(main="Treynor-Mazuy Market Timing Test\n for Low Idiosyncratic Volatility vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fittedv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retvti, y=fittedv, pch=16, col="red")
text(x=0.0, y=max(residv), paste("Treynor test t-value =", tvalue))

# Sort stocks in-sample by their volatilities
riskretis <- riskretis[order(riskretis[, "ivol"]), ]
head(riskretis)
symbolv <- rownames(riskretis)
# Calculate the out-of-sample returns of low and high volatility stocks
volow <- rowMeans(retsp[outsample, symbolv[1:(nstocks %/% 2)]])
volhigh <- rowMeans(retsp[outsample, symbolv[(nstocks %/% 2):nstocks]])
wealthv <- cbind(volow, volhigh, volow - 0.3*volhigh)
wealthv <- xts::xts(wealthv, order.by=datev[outsample])
colnames(wealthv) <- c("low_vol", "high_vol", "long_short")

# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the cumulative returns of low and high volatility stocks
endp <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(cumsum(wealthv)[endp], main="Low and High Idiosyncratic Volatility Stocks Out-Of-Sample") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(width=500)

# Sort stocks by their betas
riskret <- riskret[order(riskret[, "beta"]), ]
head(riskret)
symbolv <- rownames(riskret)
# Calculate the cumulative returns of low and high beta stocks
betalow <- rowMeans(retsp[, symbolv[1:(nstocks %/% 2)]])
betahigh <- rowMeans(retsp[, symbolv[(nstocks %/% 2):nstocks]])
wealthv <- cbind(betalow, betahigh, betalow - 0.3*betahigh)
wealthv <- xts::xts(wealthv, order.by=datev)
colnames(wealthv) <- c("low_beta", "high_beta", "long_short")

# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the cumulative returns of low and high beta stocks
endp <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(cumsum(wealthv)[endp], main="Low and High Beta Stocks In-Sample") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(width=500)

# Merton-Henriksson test
predictor <- cbind(VTI=retvti, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(predictor)[2:3] <- c("merton", "treynor")
regmod <- lm(wealthv$long_short ~ VTI + merton, data=predictor); summary(regmod)
# Treynor-Mazuy test
regmod <- lm(wealthv$long_short ~ VTI + treynor, data=predictor); summary(regmod)
# Plot residual scatterplot
residv <- regmod$residuals
plot.default(x=retvti, y=residv, xlab="VTI", ylab="Low Beta")
title(main="Treynor-Mazuy Market Timing Test\n for Low Beta vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fittedv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retvti, y=fittedv, pch=16, col="red")
text(x=0.0, y=max(residv), paste("Treynor test t-value =", tvalue))

# Sort stocks in-sample by their betas
riskretis <- riskretis[order(riskretis[, "beta"]), ]
head(riskretis)
symbolv <- rownames(riskretis)
# Calculate the out-of-sample returns of low and high beta stocks
betalow <- rowMeans(retsp[outsample, symbolv[1:(nstocks %/% 2)]])
betahigh <- rowMeans(retsp[outsample, symbolv[(nstocks %/% 2):nstocks]])
wealthv <- cbind(betalow, betahigh, betalow - 0.3*betahigh)
wealthv <- xts::xts(wealthv, order.by=datev[outsample])
colnames(wealthv) <- c("low_beta", "high_beta", "long_short")

# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the cumulative returns of low and high beta stocks
endp <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(cumsum(wealthv)[endp], main="Low and High Beta Stocks Out-Of-Sample") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(width=500)

# Calculate the log percentage returns
retsp <- rutils::diffit(log(prices))
# Define performance objective function as sum of returns
objfun <- function(retsp) sum(retsp)
# Define performance objective function as Sharpe ratio
objfun <- function(rets) sum(rets)/sd(rets)
# Calculate performance statistics over look-back intervals
retsis <- retsp[endp[1]:endp[2]]
perfstat <- sapply(retsis, objfun)
perfstat[!is.finite(perfstat)] <- 0
sum(is.na(perfstat))
# Calculate the best and worst performing stocks
perfstat <- sort(perfstat, decreasing=TRUE)
nstocks <- 10
symbolb <- names(head(perfstat, nstocks))
symbolw <- names(tail(perfstat, nstocks))
# Calculate equal weights for the best and worst performing stocks
weightv <- numeric(NCOL(retsp))
names(weightv) <- colnames(retsp)
weightv[symbolb] <- 1
weightv[symbolw] <- (-1)
# Calculate weights proportional to performance statistic
weightv <- perfstat
# Center weights so sum is equal to 0
weightv <- weightv - mean(weightv)
# Scale weights so sum of squares is equal to 1
weightv <- weightv/sqrt(sum(weightv^2))
# Calculate the momentum portfolio returns
retsportf <- retsp %*% weightv
# Scale weights so in-sample portfolio volatility is same as equal weight
scalef <- sd(rowMeans(retsis))/sd(retsportf)
weightv <- scalef*weightv

# Calculate a vector of monthly end points
endp <- rutils::calc_endpoints(retsp, interval="months")
npts <- NROW(endp)
# Perform loop over the end points
nstocks <- 10
look_back <- 8
pnls <- lapply(2:(npts-1), function(ep) {
  # Select the look-back returns
  startp <- endp[max(1, ep-look_back)]
  retsis <- retsp[startp:endp[ep], ]
  # Calculate the best and worst performing stocks in-sample
  perfstat <- sapply(retsis, objfun)
  perfstat[!is.finite(perfstat)] <- 0
  perfstat <- sort(perfstat, decreasing=TRUE)
  symbolb <- names(head(perfstat, nstocks))
  symbolw <- names(tail(perfstat, nstocks))
  # Calculate the momentum weights
  weightv <- numeric(NCOL(retsp))
  names(weightv) <- colnames(retsp)
  weightv[symbolb] <- 1
  # weightv[symbolw] <- (-1)
  # Calculate the in-sample portfolio returns
  retsportf <- retsis %*% weightv
  # Scale weights so in-sample portfolio volatility is same as equal weight
  weightv <- weightv*sd(rowMeans(retsis))/sd(retsportf)
  # Calculate the momentum portfolio returns
  retsportf <- retsp[(endp[ep]+1):endp[ep+1], ] %*% weightv
  rowMeans(retsportf)
})  # end lapply
pnls <- rutils::do_call(c, pnls)

# Calculate the average of all stock returns
indeks <- rowMeans(retsp)
indeks <- xts::xts(indeks, order.by=datev)
colnames(indeks) <- "Index"
# Add initial startup interval returns
pnls <- c(rowMeans(retsp[endp[1]:endp[2], ]), pnls)
pnls <- xts::xts(pnls, order.by=datev)
colnames(pnls) <- "Strategy"
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls)
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot dygraph of stock index and momentum strategy
colorv <- c("blue", "red")
endp <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(cumsum(wealthv)[endp], main="Log Stock Index and Momentum Strategy") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)

btmomtop <- function(rets,
  objfun=function(rets) (sum(rets)/sd(rets)),
  look_back=12, rfreq="months", nstocks=10, bid_offer=0.001,
  endp=rutils::calc_endpoints(rets, interval=rfreq), ...) {
  # Perform loop over end points
  npts <- NROW(endp)
  pnls <- lapply(2:(npts-1), function(ep) {
    # Select the look-back returns
    startp <- endp[max(1, ep-look_back)]
    retsis <- retsp[startp:endp[ep], ]
    # Calculate the best and worst performing stocks in-sample
    perfstat <- sapply(retsis, objfun)
    perfstat[!is.finite(perfstat)] <- 0
    perfstat <- sort(perfstat, decreasing=TRUE)
    symbolb <- names(head(perfstat, nstocks))
    symbolw <- names(tail(perfstat, nstocks))
    # Calculate the momentum weights
    weightv <- numeric(NCOL(retsp))
    names(weightv) <- colnames(retsp)
    weightv[symbolb] <- 1
    # weightv[symbolw] <- (-1)
    # Calculate the in-sample portfolio returns
    retsportf <- retsis %*% weightv
    # Scale weights so in-sample portfolio volatility is same as equal weight
    weightv <- weightv*sd(rowMeans(retsis))/sd(retsportf)
    # Calculate the momentum portfolio returns
    retsportf <- retsp[(endp[ep]+1):endp[ep+1], ] %*% weightv
    rowMeans(retsportf)
  })  # end lapply
  pnls <- rutils::do_call(c, pnls)
  pnls
}  # end btmomtop

# Perform backtests for vector of look-back intervals
look_backs <- seq(3, 15, by=1)
endp <- rutils::calc_endpoints(retsp, interval="months")
pnlsl <- lapply(look_backs, btmomtop, rets=retsp, endp=endp, objfun=objfun)
# Or perform parallel loop under Mac-OSX or Linux
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
pnlsl <- mclapply(look_backs, btmomtop, rets=retsp, endp=endp, objfun=objfun, mc.cores=ncores)
profilev <- sapply(pnlsl, function(pnl) sum(pnl)/sd(pnl))

# Plot Momentum profile
plot(x=look_backs, y=profilev, t="l",
  main="Momentum PnL as Function of Look-back Interval",
  xlab="look-back (months)", ylab="pnl")

# Calculate best pnls of momentum strategy
whichmax <- which.max(profilev)
look_backs[whichmax]
pnls <- pnlsl[[whichmax]]
pnls <- c(rowMeans(retsp[endp[1]:endp[2], ]), pnls)
pnls <- xts::xts(pnls, order.by=datev)
colnames(pnls) <- "Strategy"
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls)
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot dygraph of stock index and momentum strategy
colorv <- c("blue", "red")
dygraphs::dygraph(cumsum(wealthv)[endp], main="Optimal Momentum Strategy for Stocks") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)

btmomweight <- function(rets,
  objfun=function(rets) (sum(rets)/sd(rets)),
  look_back=12, rfreq="months", bid_offer=0.001,
  endp=rutils::calc_endpoints(rets, interval=rfreq), ...) {
  # Perform loop over end points
  npts <- NROW(endp)
  pnls <- lapply(2:(npts-1), function(ep) {
    # Select the look-back returns
    startp <- endp[max(1, ep-look_back)]
    retsis <- rets[startp:endp[ep], ]
    # Calculate weights proportional to performance
    perfstat <- sapply(retsis, objfun)
    perfstat[!is.finite(perfstat)] <- 0
    weightv <- perfstat
    # Calculate the in-sample portfolio returns
    retsportf <- retsis %*% weightv
    # Scale weights so in-sample portfolio volatility is same as equal weight
    weightv <- weightv*sd(rowMeans(retsis))/sd(retsportf)
    # Calculate the momentum portfolio returns
    rets[(endp[ep]+1):endp[ep+1], ] %*% weightv
  })  # end lapply
  rutils::do_call(c, pnls)
}  # end btmomweight

# Perform backtests for vector of look-back intervals
look_backs <- seq(3, 15, by=1)
pnlsl <- lapply(look_backs, btmomweight, rets=retsp, endp=endp, objfun=objfun)
# Or perform parallel loop under Mac-OSX or Linux
library(parallel)  # Load package parallel
ncores <- detectCores() - 1
pnlsl <- mclapply(look_backs, btmomweight, rets=retsp, endp=endp, objfun=objfun, mc.cores=ncores)
profilev <- sapply(pnlsl, function(pnl) sum(pnl)/sd(pnl))
# Plot Momentum profile
plot(x=look_backs, y=profilev, t="l",
  main="Momentum PnL as Function of Look-back Interval",
  xlab="look-back (months)", ylab="pnl")
# Calculate best pnls of momentum strategy
whichmax <- which.max(profilev)
look_backs[whichmax]
pnls <- pnlsl[[whichmax]]
pnls <- c(rowMeans(retsp[endp[1]:endp[2], ]), pnls)
pnls <- xts::xts(pnls, order.by=datev)
colnames(pnls) <- "Strategy"
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(indeks, pnls)
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot dygraph of stock index and momentum strategy
colorv <- c("blue", "red")
dygraphs::dygraph(cumsum(wealthv)[endp], main="Optimal Weighted Momentum Strategy for Stocks") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Calculate the scaled prices of VTI vs MTUM ETF
wealthv <- na.omit(rutils::etfenv$returns[, c("VTI", "MTUM")])
colnames(wealthv) <- c("VTI", "MTUM")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the scaled prices of VTI vs MTUM ETF
endp <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(cumsum(wealthv)[endp], main="VTI vs MTUM ETF") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(width=500)

# Extract ETF returns
symbolv <- c("VTI", "IEF", "DBC")
retsp <- rutils::etfenv$returns[, symbolv]
retsp <- na.omit(retsp)
datev <- zoo::index(retsp)
# Calculate a vector of monthly end points
endp <- rutils::calc_endpoints(retsp, interval="months")
npts <- NROW(endp)
# Perform backtests for vector of look-back intervals
look_backs <- seq(3, 15, by=1)
objfun <- function(retsp) sum(retsp)/sd(retsp)
pnlsl <- lapply(look_backs, btmomweight, rets=retsp, endp=endp, objfun=objfun)
profilev <- sapply(pnlsl, function(pnl) sum(pnl)/sd(pnl))
# Plot Momentum PnLs
plot(x=look_backs, y=profilev, t="l",
  main="Momentum PnL as Function of Look-back Interval",
  xlab="look-back (months)", ylab="pnl")

# Calculate best pnls of momentum strategy
whichmax <- which.max(profilev)
look_backs[whichmax]
pnls <- pnlsl[[whichmax]]
pnls <- c(rowMeans(retsp[endp[1]:endp[2], ]), pnls)
# Define all-weather benchmark
weightsaw <- c(0.30, 0.55, 0.15)
all_weather <- retsp %*% weightsaw
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(all_weather, pnls)
cor(wealthv)
wealthv <- xts::xts(wealthv, order.by=datev)
colnames(wealthv) <- c("All-weather", "Strategy")
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot dygraph of stock index and momentum strategy
colorv <- c("blue", "red")
dygraphs::dygraph(cumsum(wealthv)[endp], main="Momentum Strategy and All-weather") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Calculate the momentum weights
look_back <- look_backs[whichmax]
weightv <- lapply(1:(npts-1), function(ep) {
  # Select the look-back returns
  startp <- endp[max(1, ep-look_back)]
  retsis <- retsp[startp:endp[ep], ]
  # Calculate weights proportional to performance
  perfstat <- sapply(retsis, objfun)
  weightv <- drop(perfstat)
  # Scale weights so in-sample portfolio volatility is same as equal weight
  retsportf <- retsis %*% weightv
  weightv*sd(rowMeans(retsis))/sd(retsportf)
})  # end lapply
weightv <- rutils::do_call(rbind, weightv)
# Plot the momentum weights
retvti <- cumsum(retsp$VTI)
datav <- cbind(retvti[endp], weightv)
colnames(datav) <- c("VTI", paste0(colnames(retsp), "weight"))
zoo::plot.zoo(datav, xlab=NULL, main="Momentum Weights")

# Calculate ETF betas
betas_etf <- sapply(retsp, function(x)
  cov(retsp$VTI, x)/var(retsp$VTI))
# Momentum beta is equal weights times ETF betas
betas <- weightv %*% betas_etf
betas <- xts::xts(betas, order.by=datev[endp])
colnames(betas) <- "momentum_beta"
datav <- cbind(betas, retvti[endp])
zoo::plot.zoo(datav, main="Momentum Beta & VTI Price", xlab="")

# Merton-Henriksson test
retvti <- retsp$VTI
predictor <- cbind(VTI=retvti, 0.5*(retvti+abs(retvti)), retvti^2)
colnames(predictor)[2:3] <- c("merton", "treynor")
regmod <- lm(pnls ~ VTI + merton, data=predictor); summary(regmod)
# Treynor-Mazuy test
regmod <- lm(pnls ~ VTI + treynor, data=predictor); summary(regmod)
# Plot residual scatterplot
residv <- regmod$residuals
plot.default(x=retvti, y=residv, xlab="VTI", ylab="momentum")
title(main="Treynor-Mazuy Market Timing Test\n for Momentum vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fittedv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retvti
tvalue <- round(coefreg["treynor", "t value"], 2)
points.default(x=retvti, y=fittedv, pch=16, col="red")
text(x=0.0, y=max(residv), paste("Treynor test t-value =", tvalue))

# Standardize the returns
pnlsd <- (pnls-mean(pnls))/sd(pnls)
retvti <- (retvti-mean(retvti))/sd(retvti)
# Calculate skewness and kurtosis
apply(cbind(pnlsd, retvti), 2, function(x)
  sapply(c(skew=3, kurt=4),
    function(e) sum(x^e)))/NROW(retvti)

# Plot histogram
hist(pnlsd, breaks=80,
  main="Momentum and VTI Return Distributions (standardized",
  xlim=c(-4, 4), xlab="", ylab="", freq=FALSE)
# Draw kernel density of histogram
lines(density(pnlsd), col='red', lwd=2)
lines(density(retvti), col='blue', lwd=2)
# Add legend
legend("topright", inset=0.05, cex=1.0, title=NULL,
 leg=c("Momentum", "VTI"), bty="n",
 lwd=6, bg="white", col=c("red", "blue"))

# Combine momentum strategy with all-weather
wealthv <- cbind(pnls, all_weather, 0.5*(pnls + all_weather))
colnames(wealthv) <- c("momentum", "all_weather", "combined")
wealthv <- xts::xts(wealthv, datev)
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate strategy correlations
cor(wealthv)

# Plot ETF momentum strategy combined with All-Weather
dygraphs::dygraph(cumsum(wealthv)[endp], main="ETF Momentum Strategy Combined with All-Weather") %>%
  dyOptions(colors=c("red", "blue", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("green", "blue", "red")
quantmod::chart_Series(wealthv, theme=plot_theme,
       name="ETF Momentum Strategy Combined with All-Weather")
legend("topleft", legend=colnames(wealthv),
  inset=0.1, bg="white", lty=1, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# Calculate rolling variance
look_back <- 152
variance <- roll::roll_var(retsp, width=look_back, min_obs=1)
variance[1, ] <- variance[2, ]
variance[variance <= 0] <- 0
# Calculate rolling Sharpe
perfstat <- roll::roll_mean(retsp, width=look_back, min_obs=1)
weightv <- perfstat/sqrt(variance)
weightv <- weightv/sqrt(rowSums(weightv^2))
weightv <- rutils::lagit(weightv)
weightv[is.nan(weightv)] <- 0
weightv <- zoo::na.locf(weightv)
sum(is.na(weightv))
# Calculate momentum profits and losses
pnls <- rowSums(weightv*retsp)

# Calculate transaction costs
bid_offer <- 0.001
costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weightv)))
pnls <- (pnls - costs)
# Define all-weather benchmark
all_weather <- rowMeans(retsp)
# Scale the momentum volatility to all_weather
pnls <- sd(all_weather)*pnls/sd(pnls)
# Calculate the wealth of momentum returns
wealthv <- xts::xts(cbind(all_weather, pnls), order.by=datev)
colnames(wealthv) <- c("All-Weather", "Momentum")
cor(wealthv)
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv,
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the momentum strategy returns
dygraphs::dygraph(cumsum(wealthv)[endp], main="Daily Momentum Strategy vs All-Weather") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
btmomdaily <- function(rets, look_back=252, bid_offer=0.001, trend=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  variance <- roll::roll_var(rets, width=look_back, min_obs=1)
  variance[1, ] <- 1
  variance[variance <= 0] <- 1
# Calculate rolling Sharpe
  perfstat <- roll::roll_mean(rets, width=look_back, min_obs=1)
  weights <- perfstat/sqrt(variance)
  weights <- weights/sqrt(rowSums(weights^2))
  weights <- rutils::lagit(weights)
  # Calculate momentum profits and losses
  pnls <- trend*rowSums(weights*rets)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weights)))
  (pnls - costs)
}  # end btmomdaily

# Simulate a daily ETF momentum strategy
source("/Users/jerzy/Develop/lecture_slides/scripts/back_test.R")
pnls <- btmomdaily(rets=retsp, look_back=152,
  bid_offer=bid_offer)
# Perform sapply loop over look_backs
look_backs <- seq(90, 190, by=10)
pnls <- sapply(look_backs, btmomdaily,
  rets=retsp, bid_offer=bid_offer)
# Scale the momentum volatility to all_weather
pnls <- apply(pnls, MARGIN=2,
  function(pnl) sd(all_weather)*pnl/sd(pnl))
colnames(pnls) <- paste0("look_back=", look_backs)
pnls <- xts::xts(pnls, zoo::index(retsp))
tail(pnls)

# Plot dygraph of daily ETF momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endp], main="Daily ETF Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot daily ETF momentum strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls)[endp],
  theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retsp)),
  col=plot_theme$col$line.col, bty="n")

# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
btmomdailyhold <- function(rets, look_back=252, holdp=5, bid_offer=0.001, trend=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  variance <- roll::roll_var(rets, width=look_back, min_obs=1)
  variance[1, ] <- 1
  variance[variance <= 0] <- 1
  # Calculate rolling Sharpe
  perfstat <- roll::roll_mean(rets, width=look_back, min_obs=1)
  weightv <- perfstat/sqrt(variance)
  weightv <- weightv/sqrt(rowSums(weightv^2))
  # Average the weights over holding period
  weightv <- roll::roll_mean(weightv, width=holdp, min_obs=1)
  weightv <- rutils::lagit(weightv)
  # Calculate momentum profits and losses
  pnls <- trend*rowSums(weightv*rets)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weightv)))
  (pnls - costs)
}  # end btmomdailyhold

# Perform sapply loop over holding periods
holdpv <- seq(2, 11, by=2)
pnls <- sapply(holdpv, btmomdailyhold, look_back=100,
            rets=retsp, bid_offer=bid_offer)
# Scale the momentum volatility to all_weather
pnls <- apply(pnls, MARGIN=2,
  function(pnl) sd(all_weather)*pnl/sd(pnl))
colnames(pnls) <- paste0("holding=", holdpv)
pnls <- xts::xts(pnls, zoo::index(retsp))

# Plot dygraph of daily ETF momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endp], main="Daily ETF Momentum Strategies with Holding Period") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot daily ETF momentum strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls)[endp],
  theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retsp)),
  col=plot_theme$col$line.col, bty="n")

# Load daily S&P500 percentage stock returns.
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in returns100
retsp <- returns100["2000/"]
retsp[1, is.na(retsp[1, ])] <- 0
retsp <- zoo::na.locf(retsp, na.rm=FALSE)
# Simulate a daily S&P500 momentum strategy.
# Perform sapply loop over look_backs
look_backs <- seq(100, 170, by=10)
pnls <- sapply(look_backs, btmomdailyhold,
  holdp=5, rets=retsp, bid_offer=0)
colnames(pnls) <- paste0("look_back=", look_backs)
pnls <- xts::xts(pnls, zoo::index(retsp))

# Calculate a vector of monthly end points
endp <- rutils::calc_endpoints(retsp, interval="months")
# Plot dygraph of daily S&P500 momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endp], main="Daily S&P500 Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot daily S&P500 momentum strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls)[endp],
  theme=plot_theme, name="Daily S&P500 Momentum Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retsp)),
  col=plot_theme$col$line.col, bty="n")

# Perform sapply loop over look_backs
look_backs <- seq(3, 20, by=2)
pnls <- sapply(look_backs, btmomdaily,
  rets=retsp, bid_offer=0, trend=(-1))
colnames(pnls) <- paste0("look_back=", look_backs)
pnls <- xts::xts(pnls, zoo::index(retsp))

# Plot dygraph of daily S&P500 momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endp], main="Daily S&P500 Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot daily S&P500 momentum strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls)[endp],
  theme=plot_theme, name="Cumulative Returns of S&P500 Mean Reverting Strategies")
legend("topleft", legend=colnames(pnls),
  inset=0.05, bg="white", cex=0.7, lwd=rep(6, NCOL(retsp)),
  col=plot_theme$col$line.col, bty="n")

# Calculate XLP and VTI returns
retsp <- na.omit(rutils::etfenv$returns[, c("XLP", "VTI")])
# Calculate monthly end points
endp <- xts::endpoints(retsp, on="months")[-1]
# Calculate start points from look-back interval
look_back <- 12  # Look back 12 months
startp <- c(rep(1, look_back), endp[1:(NROW(endp)-look_back)])
head(cbind(endp, startp), look_back+2)
# Calculate rolling beta regressions every month in R
formulav <- XLP ~ VTI  # Specify regression formula
betar <- sapply(1:NROW(endp), FUN=function(ep) {
    datav <- retsp[startp[ep]:endp[ep], ]
    # coef(lm(formulav, data=datav))[2]
    drop(cov(datav$XLP, datav$VTI)/var(datav$VTI))
  })  # end sapply
# Calculate rolling betas using RcppArmadillo
reg_stats <- HighFreq::roll_reg(response=retsp$XLP, retsp=retsp$VTI, endp=(endp-1), startp=(startp-1))
betas <- reg_stats$VTI
all.equal(betas, betar)
# Compare the speed of RcppArmadillo with R code
library(microbenchmark)
summary(microbenchmark(
  Rcpp=HighFreq::roll_reg(response=retsp$XLP, retsp=retsp$VTI, endp=(endp-1), startp=(startp-1)),
  Rcode=sapply(1:NROW(endp), FUN=function(ep) {
    datav <- retsp[startp[ep]:endp[ep], ]
    drop(cov(datav$XLP, datav$VTI)/var(datav$VTI))
  }),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# dygraph plot of rolling XLP beta and VTI prices
dates <- zoo::index(retsp[endp, ])
pricev <- rutils::etfenv$prices$VTI[dates]
datav <- cbind(pricev, betas)
colnamev <- colnames(datav)
dygraphs::dygraph(datav, main="XLP Rolling 12-month Beta and VTI Prices") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=3) %>%
  dyLegend(show="always", width=500)

# Calculate XLB and XLE prices
pricev <- na.omit(rutils::etfenv$prices[, c("XLB", "XLE")])
cor(rutils::diffit(log(pricev)))
xlb <- drop(zoo::coredata(pricev$XLB))
xle <- drop(zoo::coredata(pricev$XLE))
# Calculate regression coefficients of XLB ~ XLE
betav <- cov(xlb, xle)/var(xle)
alpha <- (mean(xlb) - betav*mean(xle))
# Calculate regression residuals
fittedv <- (alpha + betav*xle)
residuals <- (xlb - fittedv)
# Perform ADF test on residuals
tseries::adf.test(residuals, k=1)

# Plot prices
dygraphs::dygraph(pricev, main="XLB and XLE Prices") %>%
  dyOptions(colors=c("blue", "red"))
# Plot cointegration residuals
residuals <- xts::xts(residuals, zoo::index(pricev))
dygraphs::dygraph(residuals, main="XLB and XLE Cointegration Residuals")

# Load S&P500 constituent stock prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# Calculate stock prices and percentage returns
pricets <- zoo::na.locf(pricets, na.rm=FALSE)
pricets <- zoo::na.locf(pricets, fromLast=TRUE)
retsp <- rutils::diffit(log(pricev))
# Standardize (de-mean and scale) the returns
retsp <- lapply(retsp, function(x) {(x - mean(x))/sd(x)})
retsp <- rutils::do_call(cbind, retsp)
# Perform principal component analysis PCA
pcad <- prcomp(retsp, scale=TRUE)
# Find number of components with variance greater than 2
ncomp <- which(pcad$sdev^2 < 2)[1]

# Plot standard deviations of principal components
barplot(pcad$sdev[1:ncomp],
  names.arg=colnames(pcad$rotation[, 1:ncomp]),
  las=3, xlab="", ylab="",
  main="Volatilities of S&P500 Principal Components")

# Calculate principal component loadings (weights)
# Plot barplots with PCA weights in multiple panels
ncomps <- 6
par(mfrow=c(ncomps/2, 2))
par(mar=c(4, 2, 2, 1), oma=c(0, 0, 0, 0))
# First principal component weights
weights <- sort(pcad$rotation[, 1], decreasing=TRUE)
barplot(weights[1:6], las=3, xlab="", ylab="", main="")
title(paste0("PC", 1), line=-2.0, col.main="red")
for (ordern in 2:ncomps) {
  weights <- sort(pcad$rotation[, ordern], decreasing=TRUE)
  barplot(weights[c(1:3, 498:500)], las=3, xlab="", ylab="", main="")
  title(paste0("PC", ordern), line=-2.0, col.main="red")
}  # end for

# Calculate principal component time series
retspca <- xts(retsp %*% pcad$rotation[, 1:ncomps],
          order.by=dates)
round(cov(retspca), 3)
pcacum <- cumsum(retspca)
# Plot principal component time series in multiple panels
par(mfrow=c(ncomps/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
rangev <- range(pcacum)
for (ordern in 1:ncomps) {
  plot.zoo(pcacum[, ordern], ylim=rangev, xlab="", ylab="")
  title(paste0("PC", ordern), line=-2.0)
}  # end for

par(mfrow=c(ncomps/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
# Invert principal component time series
invmat <- solve(pcad$rotation)
all.equal(invmat, t(pcad$rotation))
solved <- retspca %*% invmat[1:ncomps, ]
solved <- xts::xts(solved, dates)
solved <- cumsum(solved)
retc <- cumsum(retsp)
# Plot the solved returns
symbolv <- c("MSFT", "XOM", "JPM", "AAPL", "BRK_B", "JNJ")
for (symbol in symbolv) {
  plot.zoo(cbind(retc[, symbol], solved[, symbol]),
    plot.type="single", col=c("black", "blue"), xlab="", ylab="")
  legend(x="topleft", bty="n",
   legend=paste0(symbol, c("", " solved")),
   title=NULL, inset=0.05, cex=1.0, lwd=6,
   lty=1, col=c("black", "blue"))
}  # end for

par(mfrow=c(ncomps/2, 2))
par(mar=c(2, 2, 0, 1), oma=c(0, 0, 0, 0))
# Perform ADF unit root tests on original series and residuals
sapply(symbolv, function(symbol) {
  c(series=tseries::adf.test(retc[, symbol])$p.value,
    resid=tseries::adf.test(retc[, symbol] - solved[, symbol])$p.value)
})  # end sapply
# Plot the residuals
for (symbol in symbolv) {
  plot.zoo(retc[, symbol] - solved[, symbol],
    plot.type="single", col="blue", xlab="", ylab="")
  legend(x="topleft", bty="n", legend=paste(symbol, "residuals"),
   title=NULL, inset=0.05, cex=1.0, lwd=6, lty=1, col="blue")
}  # end for
# Perform ADF unit root test on principal component time series
retspca <- xts(retsp %*% pcad$rotation, order.by=dates)
pcacum <- cumsum(retspca)
adf_pvalues <- sapply(1:NCOL(pcacum), function(ordern)
  tseries::adf.test(pcacum[, ordern])$p.value)
# AdF unit root test on stationary time series
tseries::adf.test(rnorm(1e5))

library(quantmod)
#Perform pair-wise correlation analysis
# Calculate correlation matrix
cormat <- cor(retsp)
colnames(cormat) <- colnames(retsp)
rownames(cormat) <- colnames(retsp)
# Reorder correlation matrix based on clusters
# Calculate permutation vector
library(corrplot)
ordern <- corrMatOrder(cormat, order="hclust",
        hclust.method="complete")
# Apply permutation vector
cormat <- cormat[ordern, ordern]
# Plot the correlation matrix
colorv <- colorRampPalette(c("red", "white", "blue"))
corrplot(cormat, tl.col="black", tl.cex=0.8,
    method="square", col=colorv(8),
    cl.offset=0.75, cl.cex=0.7,
    cl.align.text="l", cl.ratio=0.25)
# draw rectangles on the correlation matrix plot
corrRect.hclust(cormat, k=NROW(cormat) %/% 2,
          method="complete", col="red")

# Convert correlation matrix into distance object
distancev <- as.dist(1-cormat)
# Perform hierarchical clustering analysis
cluster <- hclust(distancev)
plot(cluster, ann=FALSE, xlab="", ylab="")
title("Dendrogram representing hierarchical clustering
\nwith dissimilarity = 1-correlation", line=-0.5)
