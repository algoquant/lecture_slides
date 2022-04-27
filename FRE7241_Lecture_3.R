# Load the S&P500 stock prices
library(rutils)
load("/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# Subset (select) the prices after the start date of VTI
vti <- quantmod::Cl(rutils::etfenv$VTI)
vti <- rutils::diffit(vti)/rutils::lagit(vti, lagg=1, pad_zeros=FALSE)
colnames(vti) <- "VTI"
startd <- start(vti)
prices <- prices[startd <= zoo::index(prices)]
# Copy over NA prices using the function zoo::na.locf().
sum(is.na(prices))
prices <- zoo::na.locf(prices, na.rm=FALSE)
prices <- prices[, !is.na(prices[1, ])]
sum(is.na(prices))
dates <- zoo::index(prices)
vti <- vti[dates]
nrows <- NROW(prices)
ncols <- NCOL(prices)
# Normalize the prices so that they start at 1
pricesn <- lapply(prices, function(x) x/as.numeric(x[1]))
pricesn <- rutils::do_call(cbind, pricesn)
head(pricesn[, 1:5])

# Calculate the equal dollar-weighted average of all stock prices
indeks <- rowMeans(pricesn)
indeks <- xts::xts(indeks, order.by=dates)
colnames(indeks) <- "Index"
# Select a random, equal dollar-weighted portfolio of 5 stocks
nstocks <- 5
set.seed(1121)
samplev <- sample.int(n=ncols, size=nstocks, replace=FALSE)
portf <- pricesn[, samplev]
portf <- rowMeans(portf)
portf <- xts::xts(portf, order.by=dates)
colnames(portf) <- "Random"
# Plot dygraph of stock index and random portfolio
wealth <- cbind(indeks, portf)
colors <- c("blue", "red")
dygraphs::dygraph(wealth, main="Stock Index and Random Portfolio") %>%
  dyOptions(colors=colors, strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Select 10 random equal dollar-weighted sub-portfolios
set.seed(1121)
nportf <- 10
portfs <- sapply(1:nportf, function(x) {
  prices <- pricesn[, sample.int(n=ncols, size=nstocks, replace=FALSE)]
  rowMeans(prices)
})  # end sapply
portfs <- xts::xts(portfs, order.by=dates)
colnames(portfs) <- paste0("portf", 1:nportf)
round(head(portfs[, 1:4]), 3)
round(tail(portfs[, 1:4]), 3)

# Plot dygraph of stock index and random portfolios
colors <- colorRampPalette(c("red", "blue"))(nportf)
colors <- colors[order(order(portfs[NROW(portfs), ]))]
combined <- cbind(indeks, portfs)
colnames(combined)[1] <- "Index"
colnamev <- colnames(combined)
colors <- c("green", colors)
dygraphs::dygraph(combined, main="Stock Index and Random Portfolios") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=3, col="green") %>%
  dyLegend(show="always", width=500)

# Define in-sample and out-of-sample intervals
cutoff <- nrows %/% 2
dates[cutoff]
# Calculate the 10 best performing stocks in-sample
perfstat <- sort(drop(coredata(pricesn[cutoff, ])), decreasing=TRUE)
symbolv <- names(head(perfstat, 10))
# Calculate the in-sample portfolio
pricis <- pricesn[1:cutoff, symbolv]
# Normalize the prices so that they are 1 at cutoff+1
pricesn <- lapply(prices, function(x) x/as.numeric(x[cutoff+1]))
pricesn <- rutils::do_call(cbind, pricesn)
# Calculate the out-of-sample portfolio
pricos <- pricesn[(cutoff+1):nrows, symbolv]
# Scale the prices to preserve the in-sample wealth
pricos <- sum(pricis[cutoff, ])*pricos/sum(pricos[1, ])

# Combine indeks with out-of-sample stock portfolio returns
wealth <- rbind(pricis, pricos)
wealth <- xts::xts(rowMeans(wealth), dates)
wealth <- cbind(indeks, wealth)
colnames(wealth)[2] <- "Portfolio"
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealth[(cutoff+1):nrows, ]),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot out-of-sample stock portfolio returns
dygraphs::dygraph(log(wealth), main="Out-of-sample Log Prices of Stock Portfolio") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(dates[cutoff], label="in-sample", strokePattern="solid", color="green") %>%
  dyLegend(width=500)

# Calculate the percentage returns
retsp <- rutils::diffit(prices)/rutils::lagit(prices, lagg=1, pad_zeros=FALSE)
# Define momentum parameters
look_back <- 8
objfun <- function(returns) prod(1+returns)
nstocks <- 10
# Calculate a vector of monthly end points
endp <- rutils::calc_endpoints(retsp, interval="months")
endp[2] <- 11
npts <- NROW(endp)
# Perform loop over the end points
pnls <- lapply(2:(npts-1), function(it) {
  # Select the look-back returns
  startp <- endp[max(1, it-look_back)]
  retslb <- retsp[startp:endp[it], ]
  # Calculate the best performing stocks in-sample
  perfstat <- sapply(retslb, objfun)
  perfstat[!is.finite(perfstat)] <- 0
  perfstat <- sort(perfstat, decreasing=TRUE)
  symbolv <- names(head(perfstat, nstocks))
  # Calculate the in-sample portfolio volatility
  retst <- rowMeans(cumprod(1+retslb))
  retst <- rutils::diffit(retst)/rutils::lagit(retst, lagg=1, pad_zeros=FALSE)
  retsportf <- retslb[, symbolv]
  retsportf <- rowMeans(cumprod(1+retsportf))
  retsportf <- rutils::diffit(retsportf)/rutils::lagit(retsportf, lagg=1, pad_zeros=FALSE)
  scalef <- sd(retst)/sd(retsportf)
  # Calculate the out-of-sample portfolio returns
  retsos <- retsp[(endp[it]+1):endp[it+1], symbolv]
  retsos <- rowMeans(cumprod(1+retsos))
  retsos <- rutils::diffit(retsos)/rutils::lagit(retsos, lagg=1, pad_zeros=FALSE)
  # Scale the out-of-sample portfolio returns
  scalef*retsos
})  # end lapply
pnls <- rutils::do_call(c, pnls)

# Add initial startup interval returns
retsu <- retsp[endp[1]:endp[2], ]
retsu <- rowMeans(cumprod(1+retsu))
retsu <- rutils::diffit(retsu)/
  rutils::lagit(retsu, lagg=1, pad_zeros=FALSE)
pnls <- c(retsu, pnls)
pnls <- cumprod(1+pnls)
pnls <- xts::xts(pnls, order.by=dates)
colnames(pnls) <- "Strategy"
# Calculate the Sharpe and Sortino ratios
wealth <- cbind(indeks, pnls)
sqrt(252)*sapply(rutils::diffit(wealth),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot dygraph of stock index and momentum strategy
colors <- c("blue", "red")
dygraphs::dygraph(log(wealth[endp]), main="Log Stock Index and Momentum Strategy") %>%
  dyOptions(colors=colors, strokeWidth=2) %>%
  dyLegend(show="always", width=500)

backtestmomb <- function(returns,
  objfun=function(returns) (prod(1+returns)/sd(returns)),
  look_back=12, rfreq="months", nstocks=10, bid_offer=0.001,
  endp=rutils::calc_endpoints(returns, interval=rfreq), ...) {
  # Perform loop over end points
  npts <- NROW(endp)
  pnls <- lapply(2:(npts-1), function(it) {
    # Select the look-back returns
    startp <- endp[max(1, it-look_back)]
    retslb <- returns[startp:endp[it], ]
    # Calculate the best performing stocks in-sample
    perfstat <- sapply(retslb, objfun)
    perfstat[!is.finite(perfstat)] <- 0
    perfstat <- sort(perfstat, decreasing=TRUE)
    symbolb <- names(head(perfstat, nstocks))
    # Calculate the in-sample portfolio volatility
    retst <- rowMeans(cumprod(1+retslb))
    retst <- rutils::diffit(retst)/rutils::lagit(retst, lagg=1, pad_zeros=FALSE)
    retsportf <- retslb[, symbolb]
    retsportf <- rowMeans(cumprod(1+retsportf))
    retsportf <- rutils::diffit(retsportf)/rutils::lagit(retsportf, lagg=1, pad_zeros=FALSE)
    scalef <- sd(retst)/sd(retsportf)
    # Calculate the out-of-sample portfolio returns
    retsos <- returns[(endp[it]+1):endp[it+1], symbolb]
    retsos <- rowMeans(cumprod(1+retsos))
    retsos <- rutils::diffit(retsos)/rutils::lagit(retsos, lagg=1, pad_zeros=FALSE)
    # Scale the out-of-sample portfolio returns
    scalef*retsos
  })  # end lapply
  pnls <- rutils::do_call(c, pnls)
  pnls
}  # end backtestmomb

# Perform backtests for vector of look-back intervals
source("/Users/jerzy/Develop/R/backtest_functions.R")
look_backs <- seq(3, 15, by=1)
objfun <- function(returns) prod(1+returns)
pnls <- lapply(look_backs, backtestmomb, returns=retsp, endp=endp, objfun=objfun)
profilev <- sapply(pnls, function(pnl) sum(pnl)/sd(pnl))
# Plot momemntum PnLs
x11(width=6, height=5)
plot(x=look_backs, y=profilev, t="l",
  main="Momemntum PnL as Function of Look-back Interval",
  xlab="look-back (months)", ylab="pnl")

# Define performance function as Sharpe ratio
objfun <- function(returns) sum(returns)/sd(returns)
# Calculate performance statistics over look-back intervals
perfstat <- sapply(retsp[endp[1]:endp[2]], objfun)
perfstat[!is.finite(perfstat)] <- 0
sum(is.na(perfstat))
# Calculate the best and worst performing stocks
perfstat <- sort(perfstat, decreasing=TRUE)
nstocks <- 10
symbolb <- names(head(perfstat, nstocks))
symbolw <- names(tail(perfstat, nstocks))
# Calculate equal weights for the best and worst performing stocks
weights <- numeric(ncols)
names(weights) <- colnames(retsp)
weights[symbolb] <- 1
weights[symbolw] <- (-1)
# Calculate weights proportional to performance
weights <- perfstat
# Scale weights so sum of squares is equal to 1
weights <- weights/sqrt(sum(weights^2))
# Or scale weights so sum is equal to 1
weights <- weights/sum(weights)
# Calculate the momentum portfolio returns
retsportf <- retsp %*% weights

# Calculate the scaled prices of VTI vs MTUM ETF
wealth <- log(na.omit(rutils::etfenv$prices[, c("VTI", "MTUM")]))
wealth[, 1] <- wealth[, 1]/as.numeric(wealth[1, 1])
wealth[, 2] <- wealth[, 2]/as.numeric(wealth[1, 2])
colnames(wealth) <- c("VTI", "MTUM")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(rutils::diffit(wealth),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the scaled prices of VTI vs MTUM ETF
dygraphs::dygraph(wealth, main="VTI vs MTUM ETF") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(width=500)

# Perform regression using formula
model <- lm(XLP ~ VTI, data=rutils::etfenv$returns)
# Get regression coefficients
coef(summary(model))
# Get alpha and beta
coef(summary(model))[, 1]

# Plot scatterplot of returns with aspect ratio 1
plot(XLP ~ VTI, data=rutils::etfenv$returns,
     xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.1),
     asp=1, main="Regression XLP ~ VTI")
# Add regression line and perpendicular line
abline(model, lwd=2, col="red")
abline(a=0, b=-1/coef(summary(model))[2, 1],
 lwd=2, col="blue")

# Get regression coefficients
coef(summary(model))
# Calculate regression coefficients from scratch
design <- na.omit(rutils::etfenv$returns[, c("XLP", "VTI")])
betav <- drop(cov(design$XLP, design$VTI)/var(design$VTI))
alpha <- drop(mean(design$XLP) - betav*mean(design$VTI))
c(alpha, betav)
# Calculate the residuals
residuals <- (design$XLP - (alpha + betav*design$VTI))
# Calculate the standard deviation of residuals
nrows <- NROW(residuals)
resid_std <- sqrt(sum(residuals^2)/(nrows - 2))
# Calculate the standard errors of beta and alpha
sum2 <- sum((design$VTI - mean(design$VTI))^2)
beta_std <- resid_std/sqrt(sum2)
alpha_std <- resid_std*sqrt(1/nrows + mean(design$VTI)^2/sum2)
c(alpha_std, beta_std)
# Perform the Durbin-Watson test of autocorrelation of residuals
lmtest::dwtest(model)

library(rutils)  # Load rutils
returns <- rutils::etfenv$returns
symbolv <- colnames(returns)
symbolv <- symbolv[symbolv != "VTI"]
# Perform regressions and collect statistics
etf_betas <- sapply(symbolv, function(symbol) {
# Specify regression formula
  formulav <- as.formula(paste(symbol, "~ VTI"))
# Perform regression
  model <- lm(formulav, data=returns)
# Get regression summary
  model_sum <- summary(model)
# Collect regression statistics
  with(model_sum, 
    c(beta=coefficients[2, 1], 
pbeta=coefficients[2, 4],
alpha=coefficients[1, 1], 
palpha=coefficients[1, 4], 
pdw=lmtest::dwtest(model)$p.value))
})  # end sapply
etf_betas <- t(etf_betas)
# Sort by palpha
etf_betas <- etf_betas[order(etf_betas[, "palpha"]), ]

etf_betas

library(PerformanceAnalytics)
returns <- na.omit(returns[, c("XLP", "VTI")])
# Calculate XLP beta
PerformanceAnalytics::CAPM.beta(Ra=returns$XLP, Rb=returns$VTI)
# Or
betav <- cov(returns)[1, 2]/var(returns$VTI)[1]
# Calculate XLP bull beta
PerformanceAnalytics::CAPM.beta.bull(Ra=returns$XLP, Rb=returns$VTI)
# Calculate XLP bear beta
PerformanceAnalytics::CAPM.beta.bear(Ra=returns$XLP, Rb=returns$VTI)
# Calculate XLP alpha
PerformanceAnalytics::CAPM.alpha(Ra=returns$XLP, Rb=returns$VTI)
# Or
mean(returns$XLP - betav*returns$VTI)

library(PerformanceAnalytics)
etf_betas <- sapply(returns[, colnames(returns)!="VXX"],
  CAPM.beta, Rb=returns$VTI)
etf_annrets <- sapply(returns[, colnames(returns)!="VXX"],
  Return.annualized)
# Plot scatterplot
plot(etf_annrets ~ etf_betas, xlab="betas",
      ylab="ann. rets", xlim=c(-0.25, 1.6))
points(x=1, y=etf_annrets["VTI"], col="red", lwd=3, pch=21)
abline(a=0, b=etf_annrets["VTI"])
label_names <- rownames(etf_betas)[1:13]
# Add labels
text(x=1, y=etf_annrets["VTI"], labels="VTI", pos=2)
text(x=etf_betas[label_names], y=etf_annrets[label_names],
     labels=label_names, pos=2, cex=0.8)

library(PerformanceAnalytics)
# Calculate XLP Treynor ratio
TreynorRatio(Ra=returns$XLP, Rb=returns$VTI)
# Calculate XLP Information ratio
InformationRatio(Ra=returns$XLP, Rb=returns$VTI)

PerformanceAnalytics::table.CAPM(Ra=returns[, c("XLP", "XLF")], 
                           Rb=returns$VTI, scale=252)

capmstats <- table.CAPM(Ra=returns[, symbolv],
        Rb=returns$VTI, scale=252)
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
