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
  startp <- endp[max(1, it-look_back+1)]
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
    startp <- endp[max(1, it-look_back+1)]
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

# Define performance function as Sharpe ratio
objfun <- function(returns) sum(returns)/sd(returns)
# Or
objfun <- function(returns) prod(1+returns)/sd(returns)
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
weightv <- numeric(ncols)
names(weightv) <- colnames(retsp)
weightv[symbolb] <- 1
weightv[symbolw] <- (-1)
# Calculate weights proportional to performance
weightv <- perfstat
# Scale weights so sum of squares is equal to 1
weightv <- weightv/sqrt(sum(weightv^2))
# Or scale weights so sum is equal to 0
weightv <- weightv - mean(weightv)
# Calculate the momentum portfolio returns
retsportf <- retsp %*% weightv

backtestmomw <- function(returns,
  objfun=function(returns) (prod(1+returns)/sd(returns)),
  look_back=12, rfreq="months", nstocks=10, bid_offer=0.001,
  endp=rutils::calc_endpoints(returns, interval=rfreq), ...) {
  # Perform loop over end points
  npts <- NROW(endp)
  pnls <- lapply(1:(npts-1), function(it) {
    # Select the look-back returns
    startp <- endp[max(1, it-look_back+1)]
    retslb <- returns[startp:endp[it], ]
    # Calculate weights proportional to performance
    perfstat <- sapply(retslb, objfun)
    perfstat[!is.finite(perfstat)] <- 0
    weightv <- drop(perfstat)
    # Scale weights so sum of squares is equal to 1
    weightv <- weightv/sqrt(sum(weightv^2))
    weightv[!is.finite(weightv)] <- 0
    # Calculate the out-of-sample portfolio returns
    retsos <- returns[(endp[it]+1):endp[it+1], ] %*% weightv
    retsos
  })  # end lapply
  pnls <- rutils::do_call(c, pnls)
  pnls
}  # end backtestmomw

source("/Users/jerzy/Develop/R/backtest_functions.R")
# Extract ETF returns
symbolv <- c("VTI", "IEF", "DBC")
retsp <- rutils::etfenv$returns[, symbolv]
retsp <- na.omit(retsp)
dates <- zoo::index(retsp)
# Calculate a vector of monthly end points
endp <- rutils::calc_endpoints(retsp, interval="months")
npts <- NROW(endp)
# Perform backtests for vector of look-back intervals
look_backs <- seq(3, 15, by=1)
objfun <- function(retsp) sum(retsp)/sd(retsp)
pnlsl <- lapply(look_backs, backtestmomw, returns=retsp, endp=endp, objfun=objfun)
profilev <- sapply(pnlsl, function(pnl) prod(1+pnl)/sd(pnl))
# Plot momemntum PnLs
x11(width=6, height=5)
plot(x=look_backs, y=profilev, t="l",
  main="Momemntum PnL as Function of Look-back Interval",
  xlab="look-back (months)", ylab="pnl")

# Calculate best pnls of momentum strategy
whichlb <- which.max(profilev)
pnls <- pnlsl[[whichlb]]
# Define all-weather benchmark
weightsaw <- c(0.30, 0.55, 0.15)
all_weather <- retsp %*% weightsaw
# Scale the pnls of momentum strategy
pnls <- sd(all_weather)*pnls/sd(pnls)
# Calculate the Sharpe and Sortino ratios
wealth <- cbind(all_weather, pnls)
cor(wealth)
wealth <- xts::xts(wealth, order.by=dates)
wealth <- cumprod(1+wealth)
colnames(wealth) <- c("All-weather", "Strategy")
sqrt(252)*sapply(rutils::diffit(wealth),
  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot dygraph of stock index and momentum strategy
colors <- c("blue", "red")
dygraphs::dygraph(log(wealth)[endp], main="Momentum Strategy and All-weather") %>%
  dyOptions(colors=colors, strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Calculate the momentum weights
look_back <- look_backs[whichlb]
weightv <- lapply(1:(npts-1), function(it) {
  # Select the look-back returns
  startp <- endp[max(1, it-look_back+1)]
  retslb <- retsp[startp:endp[it], ]
  # Calculate weights proportional to performance
  perfstat <- sapply(retslb, objfun)
  perfstat[!is.finite(perfstat)] <- 0
  weightv <- drop(perfstat)
  # Scale weights so sum of squares is equal to 1
  weightv <- weightv/sqrt(sum(weightv^2))
  weightv[!is.finite(weightv)] <- 0
  weightv
})  # end lapply
weightv <- rutils::do_call(rbind, weightv)
# Plot the momentum weights
vti <- log(cumprod(1+retsp$VTI))
datav <- cbind(vti[endp], weightv)
colnames(datav) <- c("VTI", paste0(colnames(retsp), "weight"))
zoo::plot.zoo(datav, xlab=NULL, main="Momentum Weights")

# Calculate ETF betas
betas_etf <- sapply(retsp, function(x)
  cov(retsp$VTI, x)/var(retsp$VTI))
# Momentum beta is equal weights times ETF betas
betas <- weightv %*% betas_etf
betas <- xts::xts(betas, order.by=dates[endp])
colnames(betas) <- "momentum_beta"
datav <- cbind(betas, vti[endp])
zoo::plot.zoo(datav,
  main="Momentum Beta & VTI Price", xlab="")

# Merton-Henriksson test
vti <- retsp$VTI
design <- cbind(VTI=vti, 0.5*(vti+abs(vti)), vti^2)
colnames(design)[2:3] <- c("merton", "treynor")
model <- lm(pnls ~ VTI + merton, data=design); summary(model)
# Treynor-Mazuy test
model <- lm(pnls ~ VTI + treynor, data=design); summary(model)
# Plot residual scatterplot
plot.default(x=vti, y=model$residuals, xlab="VTI", ylab="momentum")
title(main="Treynor-Mazuy market timing test\n for Momentum vs VTI", line=0.5)
# Plot fitted (predicted) response values
points.default(x=vti, y=model$fitted.values, pch=16, col="red")
residuals <- model$residuals
text(x=0.0, y=max(residuals), paste("Treynor test t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))

# Standardize the returns
pnlsd <- (pnls-mean(pnls))/sd(pnls)
vti <- (vti-mean(vti))/sd(vti)
# Calculate skewness and kurtosis
apply(cbind(pnlsd, vti), 2, function(x)
  sapply(c(skew=3, kurt=4),
    function(e) sum(x^e)))/NROW(vti)

# Plot histogram
hist(pnlsd, breaks=80,
  main="Momentum and VTI Return Distributions (standardized",
  xlim=c(-4, 4), xlab="", ylab="", freq=FALSE)
# Draw kernel density of histogram
lines(density(pnlsd), col='red', lwd=2)
lines(density(vti), col='blue', lwd=2)
# Add legend
legend("topright", inset=0.05, cex=1.0, title=NULL,
 leg=c("Momentum", "VTI"), bty="n",
 lwd=6, bg="white", col=c("red", "blue"))

# Combine momentum strategy with all-weather
wealth <- cbind(pnls, all_weather, 0.5*(pnls + all_weather))
colnames(wealth) <- c("momentum", "all_weather", "combined")
# Calculate strategy annualized Sharpe ratios
apply(wealth, MARGIN=2, function(x) {
  sqrt(252)*sum(x)/sd(x)/NROW(x)
})  # end apply
# Calculate strategy correlations
cor(wealth)
# Calculate cumulative wealth
wealth <- xts::xts(wealth, dates)
wealth <- cumprod(1+wealth)

# Plot ETF momentum strategy combined with All-Weather
dygraphs::dygraph(log(wealth[endp]), main="ETF Momentum Strategy Combined with All-Weather") %>%
  dyOptions(colors=c("red", "blue", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Or
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("green", "blue", "red")
quantmod::chart_Series(wealth, theme=plot_theme,
       name="ETF Momentum Strategy Combined with All-Weather")
legend("topleft", legend=colnames(wealth),
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
sum(is.na(weightv))
# Calculate momentum profits and losses
pnls <- rowMeans(weightv*retsp)

# Calculate transaction costs
bid_offer <- 0.001
costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weightv)))
pnls <- (pnls - costs)
# Define all-weather benchmark
weightsaw <- c(0.30, 0.55, 0.15)
all_weather <- retsp %*% weightsaw
# Calculate the wealth of momentum returns
wealth <- xts::xts(cbind(all_weather, pnls), order.by=dates)
colnames(wealth) <- c("All-Weather", "Momentum")
cor(wealth)
# Plot dygraph of the momentum strategy returns
dygraphs::dygraph(log(cumprod(1+wealth))[endp], main="Daily Momentum Strategy vs All-Weather") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
momentum_daily <- function(retsp, look_back=252, bid_offer=0.001, trend=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  variance <- roll::roll_var(retsp, width=look_back, min_obs=1)
  variance[1, ] <- 1
  variance[variance <= 0] <- 1
# Calculate rolling Sharpe
  perfstat <- roll::roll_mean(retsp, width=look_back, min_obs=1)
  weights <- perfstat/sqrt(variance)
  weights <- weights/sqrt(rowSums(weights^2))
  weights <- rutils::lagit(weights)
  # Calculate momentum profits and losses
  pnls <- trend*rowMeans(weights*retsp)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weights)))
  (pnls - costs)
}  # end momentum_daily

# Simulate a daily ETF momentum strategy
source("/Users/jerzy/Develop/lecture_slides/scripts/back_test.R")
pnls <- momentum_daily(retsp=retsp, look_back=152,
  bid_offer=bid_offer)
# Perform sapply loop over look_backs
look_backs <- seq(50, 300, by=20)
pnls <- sapply(look_backs, momentum_daily,
  retsp=retsp, bid_offer=bid_offer)
colnames(pnls) <- paste0("look_back=", look_backs)
pnls <- xts::xts(pnls, zoo::index(retsp))
tail(pnls)

# Plot dygraph of daily ETF momentum strategies
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Daily ETF Momentum Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retsp)),
  col=plot_theme$col$line.col, bty="n")

# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
momentum_daily <- function(retsp, look_back=252, holdperiod=5, bid_offer=0.001, trend=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  variance <- roll::roll_var(retsp, width=look_back, min_obs=1)
  variance[1, ] <- 1
  variance[variance <= 0] <- 1
  # Calculate rolling Sharpe
  perfstat <- roll::roll_mean(retsp, width=look_back, min_obs=1)
  weights <- perfstat/sqrt(variance)
  weights <- weights/sqrt(rowSums(weights^2))
  weights <- rutils::lagit(weights)
  # Average the weights over holding period
  weights <- roll::roll_mean(weights, width=holdperiod, min_obs=1)
  # Calculate momentum profits and losses
  pnls <- trend*rowMeans(weights*retsp)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weights)))
  (pnls - costs)
}  # end momentum_daily

# Perform sapply loop over holding periods
holdperiods <- seq(2, 11, by=2)
pnls <- sapply(holdperiods, momentum_daily, look_back=120,
            retsp=retsp, bid_offer=bid_offer)
colnames(pnls) <- paste0("holding=", holdperiods)
pnls <- xts::xts(pnls, zoo::index(retsp))

# Plot dygraph of daily ETF momentum strategies
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Daily ETF Momentum Strategies with Holding Period") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retsp)),
  col=plot_theme$col$line.col, bty="n")

# Load daily S&P500 percentage stock returns.
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Overwrite NA values in returns100
returns100 <- returns100["2000/"]
returns100[1, is.na(returns100[1, ])] <- 0
returns100 <- zoo::na.locf(returns100, na.rm=FALSE)
# Simulate a daily S&P500 momentum strategy.
# Perform sapply loop over look_backs
look_backs <- seq(100, 300, by=20)
pnls <- sapply(look_backs, momentum_daily,
  holdperiod=5, retsp=returns100, bid_offer=0)
colnames(pnls) <- paste0("look_back=", look_backs)
pnls <- xts::xts(pnls, zoo::index(returns100))

# Plot dygraph of daily ETF momentum strategies
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Daily S&P500 Momentum Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot daily S&P500 momentum strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Daily S&P500 Momentum Strategies")
legend("bottomleft", legend=colnames(pnls),
  inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retsp)),
  col=plot_theme$col$line.col, bty="n")

# Perform sapply loop over look_backs
look_backs <- seq(3, 20, by=2)
pnls <- sapply(look_backs, momentum_daily,
  holdperiod=5, retsp=returns100, bid_offer=0, trend=(-1))
colnames(pnls) <- paste0("look_back=", look_backs)
pnls <- xts::xts(pnls, zoo::index(returns100))

# Plot dygraph of daily ETF momentum strategies
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Daily S&P500 Momentum Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
quantmod::chart_Series(cumsum(pnls),
  theme=plot_theme, name="Cumulative Returns of S&P500 Mean Reverting Strategies")
legend("topleft", legend=colnames(pnls),
  inset=0.05, bg="white", cex=0.7, lwd=rep(6, NCOL(retsp)),
  col=plot_theme$col$line.col, bty="n")

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

# Extract log VTI prices
ohlc <- log(rutils::etfenv$VTI)
closep <- quantmod::Cl(ohlc)
colnames(closep) <- "VTI"
nrows <- NROW(closep)
# Calculate EWMA weights
look_back <- 333
lambda <- 0.9
weights <- lambda^(1:look_back)
weights <- weights/sum(weights)
# Calculate EWMA prices as the convolution
ewmap <- HighFreq::roll_wsum(closep, weights=weights)
prices <- cbind(closep, ewmap)
colnames(prices) <- c("VTI", "VTI EWMA")

# Dygraphs plot with custom line colors
colnamev <- colnames(prices)
dygraphs::dygraph(prices["2009"], main="VTI EWMA Prices") %>%
  dySeries(name=colnamev[1], label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=500)
# Standard plot of  EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
colors <- c("blue", "red")
plot_theme$col$line.col <- colors
quantmod::chart_Series(prices["2009"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
legend("topleft", legend=colnames(prices),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")

# Calculate EWMA prices recursively using C++ code
ewma_rfilter <- .Call(stats:::C_rfilter, closep, lambda, c(as.numeric(closep[1])/(1-lambda), double(NROW(closep))))[-1]
# Or R code
# ewma_rfilter <- filter(closep, filter=lambda, init=as.numeric(closep[1, 1])/(1-lambda), method="recursive")
ewma_rfilter <- (1-lambda)*ewma_rfilter
# Calculate EWMA prices recursively using RcppArmadillo
ewmap <- HighFreq::run_mean(closep, lambda=lambda)
all.equal(drop(ewmap), ewma_rfilter)
# Compare the speed of C++ code with RcppArmadillo
library(microbenchmark)
summary(microbenchmark(
  run_mean=HighFreq::run_mean(closep, lambda=lambda),
  rfilter=.Call(stats:::C_rfilter, closep, lambda, c(as.numeric(closep[1])/(1-lambda), double(NROW(closep)))),
  times=10))[, c(1, 4, 5)]

# Dygraphs plot with custom line colors
prices <- cbind(closep, ewmap)
colnames(prices) <- c("VTI", "VTI EWMA")
colnamev <- colnames(prices)
dygraphs::dygraph(prices["2009"], main="Recursive VTI EWMA Prices") %>%
  dySeries(name=colnamev[1], label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], label=colnamev[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=500)
# Standard plot of  EWMA prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
colors <- c("blue", "red")
plot_theme$col$line.col <- colors
quantmod::chart_Series(prices["2009"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
legend("topleft", legend=colnames(prices),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")

# Calculate positions, either: -1, 0, or 1
indic <- sign(closep - ewmap)
posit <- rutils::lagit(indic, lagg=1)
# Create colors for background shading
dates <- (rutils::diffit(posit) != 0)
shad_e <- posit[dates]
dates <- c(zoo::index(shad_e), end(posit))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(prices["2007/"], main="VTI EWMA Prices") %>%
  dySeries(name=colnamev[1], label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], label=colnamev[2], strokeWidth=4, col="red") %>%
  dyLegend(show="always", width=500)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>% dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dyplot
# Equivalent code to the above
# Determine trade dates right after EWMA has crossed prices
indic <- sign(closep - ewmap)
dates <- (rutils::diffit(indic) != 0)
dates <- which(dates) + 1
dates <- dates[dates < nrows]
# Calculate positions, either: -1, 0, or 1
posit <- rep(NA_integer_, nrows)
posit[1] <- 0
posit[dates] <- indic[dates-1]
posit <- zoo::na.locf(posit, na.rm=FALSE)
posit <- xts::xts(posit, order.by=zoo::index(closep))
# Create indicator for background shading
shad_e <- posit[dates]
dates <- zoo::index(shad_e)
dates <- c(dates, end(posit))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")

# Standard plot of EWMA prices with position shading
x11(width=6, height=5)
quantmod::chart_Series(prices["2007/"], theme=plot_theme,
       lwd=2, name="VTI EWMA Prices")
add_TA(posit > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posit < 0, on=-1, col="lightgrey", border="lightgrey")
legend("topleft", legend=colnames(prices),
 inset=0.1, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# Calculate daily profits and losses of EWMA strategy
vti <- rutils::diffit(closep)  # VTI returns
pnls <- vti*posit
colnames(pnls) <- "EWMA"
wealth <- cbind(vti, pnls)
colnames(wealth) <- c("VTI", "EWMA PnL")
# Annualized Sharpe ratio of EWMA strategy
sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealth)
# Plot dygraph of EWMA strategy wealth
# Create dygraph object without plotting it
colors <- c("blue", "red")
dyplot <- dygraphs::dygraph(cumsum(wealth["2007/"]), main="Performance of EWMA Strategy") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>%
dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dyplot

# Standard plot of EWMA strategy wealth
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(cumsum(wealth["2007/"]), theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(posit > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posit < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(wealth),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# Test EWMA crossover market timing of VTI using Treynor-Mazuy test
design <- cbind(pnls, vti, vti^2)
design <- na.omit(design)
colnames(design) <- c("EWMA", "VTI", "treynor")
model <- lm(EWMA ~ VTI + treynor, data=design)
summary(model)
# Plot residual scatterplot
residuals <- (design$EWMA - model$coeff[2]*design$VTI)
residuals <- model$residuals
x11(width=6, height=6)
plot.default(x=design$VTI, y=residuals, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for EWMA Crossover vs VTI", line=0.5)
# Plot fitted (predicted) response values
fittedv <- (model$coeff["(Intercept)"] +
        model$coeff["treynor"]*vti^2)
points.default(x=design$VTI, y=fittedv, pch=16, col="red")
text(x=0.05, y=0.8*max(residuals), paste("EWMA crossover t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))

# Determine trade dates right after EWMA has crossed prices
indic <- sign(closep - ewmap)
# Calculate positions from lagged indicator
lagg <- 2
indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
# Calculate positions, either: -1, 0, or 1
posit <- rep(NA_integer_, nrows)
posit[1] <- 0
posit <- ifelse(indic == lagg, 1, posit)
posit <- ifelse(indic == (-lagg), -1, posit)
posit <- zoo::na.locf(posit, na.rm=FALSE)
posit <- xts::xts(posit, order.by=zoo::index(closep))
# Lag the positions to trade in next period
posit <- rutils::lagit(posit, lagg=1)
# Calculate PnLs of lagged strategy
pnls_lag <- vti*posit
colnames(pnls_lag) <- "Lagged Strategy"

wealth <- cbind(pnls, pnls_lag)
colnames(wealth) <- c("EWMA Strategy", "Lagged Strategy")
# Annualized Sharpe ratios of EWMA strategies
sharper <- sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))
# Plot both strategies
dygraphs::dygraph(cumsum(wealth["2007/"]), main=paste("EWMA Crossover Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=500)

# Calculate positions, either: -1, 0, or 1
indic <- sign(closep - ewmap)
posit <- rutils::lagit(indic, lagg=1)
# Calculate daily pnl for days without trades
pnls_lag <- vti*posit
# Determine trade dates right after EWMA has crossed prices
dates <- which(rutils::diffit(posit) != 0)
# Calculate realized pnl for days with trades
openp <- quantmod::Op(ohlc)
close_lag <- rutils::lagit(closep)
pos_lag <- rutils::lagit(posit)
pnls_lag[dates] <- pos_lag[dates]*
  (openp[dates] - close_lag[dates])
# Calculate unrealized pnl for days with trades
pnls_lag[dates] <- pnls_lag[dates] +
  posit[dates]*(closep[dates] - openp[dates])
# Calculate the wealth
wealth <- cbind(vti, pnls_lag)
colnames(wealth) <- c("VTI", "EWMA PnL")
# Annualized Sharpe ratio of EWMA strategy
sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealth)

# Plot dygraph of EWMA strategy wealth
dygraphs::dygraph(cumsum(wealth["2007/"]), main="EWMA Strategy Trading at the Open Price") %>%
  dyOptions(colors=colors, strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Standard plot of EWMA strategy wealth
quantmod::chart_Series(wealth, theme=plot_theme,
       name="EWMA Strategy Trading at the Open Price")
legend("top", legend=colnames(wealth),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# bid_offer equal to 10 bps for liquid ETFs
bid_offer <- 0.001
# Calculate transaction costs
costs <- 0.5*bid_offer*abs(pos_lag - posit)*closep
# Plot strategy with transaction costs
wealth <- cbind(pnls, pnls - costs)
colnames(wealth) <- c("EWMA", "EWMA w Costs")
colors <- c("blue", "red")
dygraphs::dygraph(cumsum(wealth["2007/"]), main="EWMA Strategy With Transaction Costs") %>%
  dyOptions(colors=colors, strokeWidth=2) %>%
  dyLegend(show="always", width=500)

sim_ewma <- function(ohlc, lambda=0.01, look_back=333, bid_offer=0.001,
                trend=1, lagg=1) {
  close <- quantmod::Cl(ohlc)
  returns <- rutils::diffit(close)
  nrows <- NROW(ohlc)
  # Calculate EWMA prices
  weights <- exp(-lambda*(1:look_back))
  weights <- weights/sum(weights)
  ewma <- HighFreq::roll_wsum(close, weights=weights)
  # Calculate the indicator
  indic <- trend*sign(close - ewma)
  if (lagg > 1) {
    indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
    indic[1:lagg] <- 0
  }  # end if
  # Calculate positions, either: -1, 0, or 1
  pos <- rep(NA_integer_, nrows)
  pos[1] <- 0
  pos <- ifelse(indic == lagg, 1, pos)
  pos <- ifelse(indic == (-lagg), -1, pos)
  pos <- zoo::na.locf(pos, na.rm=FALSE)
  pos <- xts::xts(pos, order.by=zoo::index(close))
  # Lag the positions to trade on next day
  pos <- rutils::lagit(pos, lagg=1)
  # Calculate PnLs of strategy
  pnls <- returns*pos
  costs <- 0.5*bid_offer*abs(rutils::diffit(pos))*close
  pnls <- (pnls - costs)
  # Calculate strategy returns
  pnls <- cbind(pos, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end sim_ewma

source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lambdas <- seq(from=0.001, to=0.008, by=0.001)
# Perform lapply() loop over lambdas
pnls <- lapply(lambdas, function(lambda) {
  # Simulate EWMA strategy and calculate returns
  sim_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back, bid_offer=0, lagg=2)[, "pnls"]
})  # end lapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdas)

# Plot dygraph of multiple EWMA strategies
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls["2007/"]), main="Cumulative Returns of Trend Following EWMA Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(cumsum(pnls), theme=plot_theme,
  name="Cumulative Returns of EWMA Strategies")
legend("topleft", legend=colnames(pnls), inset=0.1,
  bg="white", cex=0.8, lwd=rep(6, NCOL(pnls)),
  col=plot_theme$col$line.col, bty="n")

# Initialize compute cluster under Windows
library(parallel)
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster,
  varlist=c("ohlc", "look_back", "sim_ewma"))
# Perform parallel loop over lambdas under Windows
pnls <- parLapply(cluster, lambdas, function(lambda) {
  library(quantmod)
  # Simulate EWMA strategy and calculate returns
  sim_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back)[, "pnls"]
})  # end parLapply
stopCluster(cluster)  # Stop R processes over cluster under Windows
# Perform parallel loop over lambdas under Mac-OSX or Linux
pnls <- mclapply(lambdas, function(lambda) {
  library(quantmod)
  # Simulate EWMA strategy and calculate returns
  sim_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back)[, "pnls"]
})  # end mclapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdas)

# Calculate annualized Sharpe ratios of strategy returns
sharper <- sqrt(252)*sapply(pnls, function(xtes) {
  mean(xtes)/sd(xtes)
})  # end sapply
# Plot Sharpe ratios
dev.new(width=6, height=5, noRStudioGD=TRUE)
plot(x=lambdas, y=sharper, t="l",
     xlab="lambda", ylab="Sharpe",
     main="Performance of EWMA Trend Following Strategies
     as Function of the Decay Parameter Lambda")
# Find optimal lambda
lambda <- lambdas[which.max(sharper)]

# Plot optimal weights
weights <- exp(-lambda*(1:look_back))
weights <- weights/sum(weights)
plot(weights, t="l", xlab="days", ylab="weights",
     main="Optimal Weights of EWMA Trend Following Strategy")
trend_returns <- pnls
trend_sharpe <- sharper

# Simulate best performing strategy
ewma_trend <- sim_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back, bid_offer=0, lagg=2)
posit <- ewma_trend[, "positions"]
pnls <- ewma_trend[, "pnls"]
wealth <- cbind(vti, pnls)
colnames(wealth) <- c("VTI", "EWMA PnL")
# Create colors for background shading
dates <- (rutils::diffit(posit) != 0)
shad_e <- posit[dates]
dates <- c(zoo::index(shad_e), end(posit))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")
colors <- c("blue", "red")
# Plot dygraph of EWMA strategy wealth
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(cumsum(wealth["2007/"]), main="Performance of Optimal Trend Following EWMA Strategy") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>%
dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dyplot

# Plot EWMA PnL with position shading
# Standard plot of EWMA strategy wealth
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(cumsum(wealth["2007/"]), theme=plot_theme,
       name="Performance of EWMA Strategy")
add_TA(posit > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posit < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(wealth),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lambdas <- seq(0.05, 1.0, 0.05)
# Perform lapply() loop over lambdas
pnls <- lapply(lambdas, function(lambda) {
  # Simulate EWMA strategy and calculate returns
  sim_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back, trend=(-1))[, "pnls"]
})  # end lapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("lambda=", lambdas)
# Plot dygraph of mean reverting EWMA strategies
column_s <- seq(1, NCOL(pnls), by=4)
colors <- colorRampPalette(c("blue", "red"))(NROW(column_s))
dygraphs::dygraph(cumsum(pnls["2007/", column_s]), main="Cumulative Returns of Mean Reverting EWMA Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot EWMA strategies with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(pnls[, column_s],
  theme=plot_theme, name="Cumulative Returns of Mean Reverting EWMA Strategies")
legend("topleft", legend=colnames(pnls[, column_s]),
  inset=0.1, bg="white", cex=0.8, lwd=6,
  col=plot_theme$col$line.col, bty="n")

# Calculate Sharpe ratios of strategy returns
sharper <- sqrt(252)*sapply(pnls, function(xtes) {
  mean(xtes)/sd(xtes)
})  # end sapply
plot(x=lambdas, y=sharper, t="l",
     xlab="lambda", ylab="Sharpe",
     main="Performance of EWMA Mean Reverting Strategies
     as Function of the Decay Parameter Lambda")
revert_returns <- pnls
revert_sharpe <- sharper

# Find optimal lambda
lambda <- lambdas[which.max(sharper)]
# Simulate best performing strategy
ewma_revert <- sim_ewma(ohlc=ohlc, bid_offer=0.0,
  lambda=lambda, look_back=look_back, trend=(-1))
posit <- ewma_revert[, "positions"]
pnls <- ewma_revert[, "pnls"]
wealth <- cbind(vti, pnls)
colnames(wealth) <- c("VTI", "EWMA PnL")
# Plot dygraph of EWMA strategy wealth
colors <- c("blue", "red")
dygraphs::dygraph(cumsum(wealth["2007/"]), main="Optimal Mean Reverting EWMA Strategy") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)

# Standard plot of EWMA strategy wealth
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(cumsum(wealth["2007/"]), theme=plot_theme,
       name="Optimal Mean Reverting EWMA Strategy")
add_TA(posit > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(posit < 0, on=-1, col="lightgrey", border="lightgrey")
legend("top", legend=colnames(wealth),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# Calculate correlation between trend following and mean reverting strategies
trend_ing <- ewma_trend[, "pnls"]
colnames(trend_ing) <- "trend"
revert_ing <- ewma_revert[, "pnls"]
colnames(revert_ing) <- "revert"
cor(cbind(vti, trend_ing, revert_ing))
# Calculate combined strategy
com_bined <- (vti + trend_ing + revert_ing)/3
colnames(com_bined) <- "combined"
# Calculate annualized Sharpe ratio of strategy returns
returns <- cbind(vti, trend_ing, revert_ing, com_bined)
colnames(returns) <- c("VTI", "Trending", "Reverting", "EWMA combined")
sqrt(252)*sapply(returns, function(xtes) mean(xtes)/sd(xtes))

# Plot dygraph of EWMA strategy wealth
colors <- c("blue", "red", "green", "purple")
dygraphs::dygraph(cumsum(returns["2007/"]), main="Performance of Combined EWMA Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Standard plot of EWMA strategy wealth
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(pnls, theme=plot_theme,
       name="Performance of Combined EWMA Strategies")
legend("topleft", legend=colnames(pnls),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

weights <- c(trend_sharpe, revert_sharpe)
weights[weights<0] <- 0
weights <- weights/sum(weights)
returns <- cbind(trend_returns, revert_returns)
returns <- returns %*% weights
returns <- xts::xts(returns, order.by=zoo::index(vti))
returns <- cbind(vti, returns)
colnames(returns) <- c("VTI", "EWMA PnL")
# Plot dygraph of EWMA strategy wealth
colors <- c("blue", "red")
dygraphs::dygraph(cumsum(returns["2007/"]), main="Performance of Ensemble of EWMA Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Standard plot of EWMA strategy wealth
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(cumsum(returns["2007/"]), theme=plot_theme,
       name="Performance of Ensemble of EWMA Strategies")
legend("topleft", legend=colnames(pnls),
 inset=0.05, bg="white", lty=1, lwd=6,
 col=plot_theme$col$line.col, bty="n")

# Calculate fast and slow EWMAs
look_back <- 333
lambda1 <- 0.04
lambda2 <- 0.004
weights <- exp(-lambda1*(1:look_back))
weights <- weights/sum(weights)
ewma1 <- HighFreq::roll_wsum(closep, weights=weights)
weights <- exp(-lambda2*(1:look_back))
weights <- weights/sum(weights)
ewma2 <- HighFreq::roll_wsum(closep, weights=weights)
# Calculate EWMA prices
prices <- cbind(closep, ewma1, ewma2)
colnames(prices) <- c("VTI", "EWMA fast", "EWMA slow")
# Calculate positions, either: -1, 0, or 1
indic <- sign(ewma1 - ewma2)
lagg <- 2
indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
posit <- rep(NA_integer_, nrows)
posit[1] <- 0
posit <- ifelse(indic == lagg, 1, posit)
posit <- ifelse(indic == (-lagg), -1, posit)
posit <- zoo::na.locf(posit, na.rm=FALSE)
posit <- xts::xts(posit, order.by=zoo::index(closep))
posit <- rutils::lagit(posit, lagg=1)

# Create colors for background shading
dates <- (rutils::diffit(posit) != 0)
shad_e <- posit[dates]
dates <- c(zoo::index(shad_e), end(posit))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")
# Plot dygraph
colnamev <- colnames(prices)
dyplot <- dygraphs::dygraph(prices["2007/"], main="VTI Dual EWMA Prices") %>%
  dySeries(name=colnamev[1], label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], label=colnamev[2], strokeWidth=4, col="red") %>%
  dySeries(name=colnamev[3], label=colnamev[3], strokeWidth=4, col="purple") %>%
  dyLegend(show="always", width=500)
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>% dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
dyplot

# Calculate daily profits and losses of strategy
pnls <- vti*posit
colnames(pnls) <- "Strategy"
wealth <- cbind(vti, pnls)
# Annualized Sharpe ratio of Dual EWMA strategy
sharper <- sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealth)

# Plot Dual EWMA strategy
dyplot <- dygraphs::dygraph(cumsum(wealth["2007/"]), main=paste("EWMA Dual Crossover Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>% dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dyplot

sim_ewma2 <- function(ohlc, lambda_1=0.1, lambda2=0.01, look_back=333,
                bid_offer=0.001, trend=1, lagg=1) {
  close <- log(quantmod::Cl(ohlc))
  returns <- rutils::diffit(close)
  nrows <- NROW(ohlc)
  # Calculate EWMA prices
  weights <- exp(-lambda1*(1:look_back))
  weights <- weights/sum(weights)
  ewma1 <- HighFreq::roll_wsum(closep, weights=weights)
  weights <- exp(-lambda2*(1:look_back))
  weights <- weights/sum(weights)
  ewma2 <- HighFreq::roll_wsum(closep, weights=weights)
  # Calculate positions, either: -1, 0, or 1
  indic <- sign(ewma1 - ewma2)
  if (lagg > 1) {
    indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
    indic[1:lagg] <- 0
  }  # end if
  pos <- rep(NA_integer_, nrows)
  pos[1] <- 0
  pos <- ifelse(indic == lagg, 1, pos)
  pos <- ifelse(indic == (-lagg), -1, pos)
  pos <- zoo::na.locf(pos, na.rm=FALSE)
  pos <- xts::xts(pos, order.by=zoo::index(close))
  # Lag the positions to trade on next day
  pos <- rutils::lagit(pos, lagg=1)
  # Calculate PnLs of strategy
  pnls <- returns*pos
  costs <- 0.5*bid_offer*abs(rutils::diffit(pos))*close
  pnls <- (pnls - costs)
  # Calculate strategy returns
  pnls <- cbind(pos, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end sim_ewma2

source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
lambdas1 <- seq(from=0.05, to=0.15, by=0.01)
lambdas2 <- seq(from=0.03, to=0.1, by=0.01)
# Perform sapply() loops over lambdas
sharper <- sapply(lambdas1, function(lambda1) {
  sapply(lambdas2, function(lambda2) {
    if (lambda1 > lambda2) {
# Simulate Dual EWMA strategy
pnls <- sim_ewma2(ohlc=ohlc, lambda1=lambda1, lambda2=lambda2,
                    look_back=look_back, bid_offer=0.0, trend=1, lagg=2)[, "pnls"]
sqrt(252)*mean(pnls)/sd(pnls)
    } else NA
  })  # end sapply
})  # end sapply
colnames(sharper) <- lambdas1
rownames(sharper) <- lambdas2
# Calculate the PnLs for the optimal strategy
whichv <- which(sharper == max(sharper, na.rm=TRUE), arr.ind=TRUE)
lambda1 <- lambdas1[whichv[2]]
lambda2 <- lambdas2[whichv[1]]
pnls <- sim_ewma2(ohlc=ohlc, lambda1=lambda1, lambda2=lambda2,
              look_back=look_back, bid_offer=0.0, trend=1, lagg=2)[, "pnls"]
wealth <- cbind(vti, pnls)

# Annualized Sharpe ratio of Dual EWMA strategy
sharper <- sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))
# The crossover strategy has a negative correlation to VTI
cor(wealth)
# Plot Optimal Dual EWMA strategy
dyplot <- dygraphs::dygraph(cumsum(wealth["2007/"]), main=paste("Optimal EWMA Dual Crossover Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>% dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dyplot

# Calculate log OHLC prices and volumes
ohlc <- rutils::etfenv$VTI
closep <- log(quantmod::Cl(ohlc))
colnames(closep) <- "VTI"
volumes <- quantmod::Vo(ohlc)
colnames(volumes) <- "Volume"
nrows <- NROW(closep)
# Calculate the VWAP prices
look_back <- 21
vwap <- roll::roll_sum(closep*volumes, width=look_back, min_obs=1)
volume_roll <- roll::roll_sum(volumes, width=look_back, min_obs=1)
vwap <- vwap/volume_roll
colnames(vwap) <- "VWAP"
prices <- cbind(closep, vwap)

# Dygraphs plot with custom line colors
colors <- c("blue", "red")
dygraphs::dygraph(prices["2009"], main="VTI VWAP Prices") %>%
  dyOptions(colors=colors, strokeWidth=2)
# Plot VWAP prices with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(prices["2009"], theme=plot_theme,
       lwd=2, name="VTI VWAP Prices")
legend("bottomright", legend=colnames(prices),
 inset=0.1, bg="white", lty=1, lwd=6, cex=0.8,
 col=plot_theme$col$line.col, bty="n")

# Calculate VWAP prices recursively using C++ code
volume_rec <- .Call(stats:::C_rfilter, volumes, lambda, c(as.numeric(volumes[1])/(1-lambda), double(NROW(volumes))))[-1]
price_rec <- .Call(stats:::C_rfilter, volumes*closep, lambda, c(as.numeric(volumes[1]*closep[1])/(1-lambda), double(NROW(closep))))[-1]
vwap_rec <- price_rec/volume_rec
# Calculate VWAP prices recursively using RcppArmadillo
vwap_arma <- HighFreq::run_mean(closep, lambda=lambda, weights=volumes)
all.equal(vwap_rec, drop(vwap_arma))
# Dygraphs plot the VWAP prices
prices <- xts(cbind(vwap, vwap_arma), zoo::index(ohlc))
colnames(prices) <- c("VWAP rolling", "VWAP running")
dygraphs::dygraph(prices["2009"], main="VWAP Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Calculate positions from lagged indicator
indic <- sign(closep - vwap)
lagg <- 2
indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
# Calculate positions, either: -1, 0, or 1
posit <- rep(NA_integer_, nrows)
posit[1] <- 0
posit <- ifelse(indic == lagg, 1, posit)
posit <- ifelse(indic == (-lagg), -1, posit)
posit <- zoo::na.locf(posit, na.rm=FALSE)
posit <- xts::xts(posit, order.by=zoo::index(closep))
# Lag the positions to trade in next period
posit <- rutils::lagit(posit, lagg=1)
# Calculate PnLs of VWAP strategy
vti <- rutils::diffit(closep)  # VTI returns
pnls <- vti*posit
colnames(pnls) <- "VWAP Strategy"
wealth <- cbind(vti, pnls)
colnames(wealth) <- c("VTI", "VWAP Strategy")
colnamev <- colnames(wealth)
# Annualized Sharpe ratios of VTI and VWAP strategy
sharper <- sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))

# Create colors for background shading
dates <- (rutils::diffit(posit) != 0)
shad_e <- posit[dates]
dates <- c(zoo::index(shad_e), end(posit))
shad_e <- ifelse(drop(zoo::coredata(shad_e)) == 1, "lightgreen", "antiquewhite")
# Plot dygraph of VWAP strategy
# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(cumsum(wealth["2007/"]), main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Add shading to dygraph object
for (i in 1:NROW(shad_e)) {
    dyplot <- dyplot %>% dyShading(from=dates[i], to=dates[i+1], color=shad_e[i])
}  # end for
# Plot the dygraph object
dyplot

# Calculate correlation of VWAP strategy with VTI
cor(vti, pnls)
# Combine VWAP strategy with VTI
wealth <- cbind(vti, pnls, 0.5*(vti+pnls))
colnames(wealth) <- c("VTI", "VWAP", "Combined")
sharper <- sqrt(252)*sapply(wealth, function (x) mean(x)/sd(x))

# Plot dygraph of VWAP strategy combined with VTI
# wippp
colnamev <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dySeries(name=colnamev[1], label=colnamev[1], col="blue", strokeWidth=1) %>%
  dySeries(name=colnamev[2], label=colnamev[2], col="red", strokeWidth=1) %>%
  dySeries(name=colnamev[3], label=colnamev[3], col="purple", strokeWidth=2) %>%
  dyLegend(show="always", width=500)
# Or
dygraphs::dygraph(cumsum(wealth),
  main=paste("VWAP Crossover Strategy, Sharpe", paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))) %>%
  dyOptions(colors=c("blue", "red", "purple"), strokeWidth=1) %>%
  dyLegend(show="always", width=500)

# Test VWAP crossover market timing of VTI using Treynor-Mazuy test
design <- cbind(pnls, vti, vti^2)
design <- na.omit(design)
colnames(design) <- c("VWAP", "VTI", "treynor")
model <- lm(VWAP ~ VTI + treynor, data=design)
summary(model)
# Plot residual scatterplot
residuals <- (design$VWAP - model$coeff[2]*design$VTI)
residuals <- model$residuals
x11(width=6, height=6)
plot.default(x=design$VTI, y=residuals, xlab="VTI", ylab="residuals")
title(main="Treynor-Mazuy Market Timing Test\n for VWAP Crossover vs VTI", line=0.5)
# Plot fitted (predicted) response values
fittedv <- (model$coeff["(Intercept)"] + model$coeff["treynor"]*vti^2)
points.default(x=design$VTI, y=fittedv, pch=16, col="red")
text(x=0.05, y=0.8*max(residuals), paste("VWAP crossover t-value =", round(summary(model)$coeff["treynor", "t value"], 2)))

sim_vwap <- function(ohlc, look_back=333, bid_offer=0.001, trend=1, lagg=1) {
  close <- log(quantmod::Cl(ohlc))
  volumes <- quantmod::Vo(ohlc)
  returns <- rutils::diffit(close)
  nrows <- NROW(ohlc)
  # Calculate VWAP prices
  vwap <- roll::roll_sum(closep*volumes, width=look_back, min_obs=1)
  volume_roll <- roll::roll_sum(volumes, width=look_back, min_obs=1)
  vwap <- vwap/volume_roll
  # Calculate the indicator
  indic <- trend*sign(close - vwap)
  if (lagg > 1) {
    indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
    indic[1:lagg] <- 0
  }  # end if
  # Calculate positions, either: -1, 0, or 1
  pos <- rep(NA_integer_, nrows)
  pos[1] <- 0
  pos <- ifelse(indic == lagg, 1, pos)
  pos <- ifelse(indic == (-lagg), -1, pos)
  pos <- zoo::na.locf(pos, na.rm=FALSE)
  pos <- xts::xts(pos, order.by=zoo::index(close))
  # Lag the positions to trade on next day
  pos <- rutils::lagit(pos, lagg=1)
  # Calculate PnLs of strategy
  pnls <- returns*pos
  costs <- 0.5*bid_offer*abs(rutils::diffit(pos))*close
  pnls <- (pnls - costs)
  # Calculate strategy returns
  pnls <- cbind(pos, pnls)
  colnames(pnls) <- c("positions", "pnls")
  pnls
}  # end sim_vwap

source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
look_backs <- seq(70, 200, 10)
# Perform lapply() loop over lambdas
pnls <- lapply(look_backs, function(look_back) {
  # Simulate VWAP strategy and calculate returns
  sim_vwap(ohlc=ohlc, look_back=look_back, bid_offer=0, lagg=2)[, "pnls"]
})  # end lapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("look_back=", look_backs)

# Plot dygraph of multiple VWAP strategies
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls["2007/"]), main="Cumulative Returns of Trend Following VWAP Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)
# Plot VWAP strategies with custom line colors
x11(width=6, height=5)
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(cumsum(pnls), theme=plot_theme,
  name="Cumulative Returns of VWAP Strategies")
legend("topleft", legend=colnames(pnls), inset=0.1,
  bg="white", cex=0.8, lwd=rep(6, NCOL(pnls)),
  col=plot_theme$col$line.col, bty="n")
